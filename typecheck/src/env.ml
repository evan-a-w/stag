open! Import

module Level = struct
  type t = int [@@deriving sexp, compare, equal]
end

module Lock = struct
  module Kind = struct
    type t =
      | Closure of Mode.Var.t
      | Quote of Mode.Var.t
      | Splice
      | Exclave
      | Region
  end

  type t =
    { id : int
    ; kind : Kind.t
    }

  (* Each splice leaves the innermost quote outside it, so drops it with the
     locks between them. A splice whose quote is outside [locks] is dropped
     alone. *)
  let cancel_splices locks =
    let rec leave_quote stack =
      match stack with
      | [] -> None
      | { kind = Quote _; _ } :: rest -> Some rest
      | { kind = Closure _ | Splice | Exclave | Region; _ } :: rest ->
        leave_quote rest
    in
    List.fold_right locks ~init:[] ~f:(fun lock stack ->
      match lock.kind with
      | Splice -> Option.value (leave_quote stack) ~default:stack
      | Closure _ | Quote _ | Exclave | Region -> lock :: stack)
  ;;
end

module Entry = struct
  type t =
    { scheme : Type.ty
    ; level : Level.t
    ; quote_id : int option
    ; mode : Binding.t
    ; held : Binding.t option
    ; shape : (Mode_shape.t option[@sexp.opaque])
    ; locks : (Lock.t list[@sexp.opaque])
    ; binder : Typed.Binder.t
    ; defining : bool
    }
  [@@deriving sexp_of]

  let toplevel ~defining ~ident ~scheme ~mode ~held =
    { scheme
    ; level = 0
    ; quote_id = None
    ; mode
    ; held
    ; shape = None
    ; locks = []
    ; binder = Toplevel ident
    ; defining
    }
  ;;

  (* Locks are only pushed, so those enclosing the binding are a suffix of those
     enclosing a use it is visible at. *)
  let crossed t ~locks =
    List.take_while locks ~f:(fun (lock : Lock.t) ->
      not (List.exists t.locks ~f:(fun (bound : Lock.t) -> bound.id = lock.id)))
    |> Lock.cancel_splices
  ;;
end

module Slot = struct
  type t =
    | Bound of Entry.t
    | Forgotten of int (** At this level and the quotes in it. *)
  [@@deriving sexp_of]
end

module Lookup = struct
  type t =
    | Bound of Entry.t
    | Forgotten
    | Unbound
end

type t = Slot.t list String.Table.t [@@deriving sexp_of]

let create () = String.Table.create ()

(* A [forget] at a later level than [level] doesn't hide the name: a splice back
   to [level] sees through it. *)
let rec innermost (slots : Slot.t list) ~level : Lookup.t =
  match slots with
  | [] -> Unbound
  | Bound entry :: _ -> Bound entry
  | Forgotten at :: _ when at <= level -> Forgotten
  | Forgotten _ :: slots -> innermost slots ~level
;;

let lookup t name ~level =
  innermost (Hashtbl.find t name |> Option.value ~default:[]) ~level
;;

let find_binder t binder ~name =
  Hashtbl.find t name
  |> Option.bind
       ~f:
         (List.find_map ~f:(function
           | Slot.Bound entry
             when [%equal: Typed.Binder.t] entry.Entry.binder binder ->
             Some entry
           | Bound _ | Forgotten _ -> None))
;;

let visible t ~level =
  Hashtbl.to_alist t
  |> List.filter_map ~f:(fun (name, slots) ->
    match innermost slots ~level with
    | Bound entry -> Some (name, entry)
    | Forgotten | Unbound -> None)
  |> List.sort ~compare:(Comparable.lift String.compare ~f:fst)
;;

let with_slots t slots ~(local_ f) =
  List.iter slots ~f:(fun (name, slot) ->
    Hashtbl.add_multi t ~key:name ~data:slot);
  Exn.protect ~f ~finally:(fun () ->
    List.iter slots ~f:(fun (name, _) -> Hashtbl.remove_multi t name))
;;

let with_bindings t bindings ~(local_ f) =
  with_slots
    t
    (List.map bindings ~f:(fun (name, entry) -> name, Slot.Bound entry))
    ~f
;;

let forgetting t names ~level ~(local_ f) =
  with_slots t (List.map names ~f:(fun name -> name, Slot.Forgotten level)) ~f
;;
