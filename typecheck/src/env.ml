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
      | { kind = Closure _ | Splice | Exclave; _ } :: rest -> leave_quote rest
    in
    List.fold_right locks ~init:[] ~f:(fun lock stack ->
      match lock.kind with
      | Splice -> Option.value (leave_quote stack) ~default:stack
      | Closure _ | Quote _ | Exclave -> lock :: stack)
  ;;
end

module Entry = struct
  type t =
    { scheme : Type.ty
    ; level : Level.t
    ; quote_id : int option
    ; mode : Binding.t
    ; held : Binding.t option
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

type t = Entry.t list String.Table.t [@@deriving sexp_of]

let create () = String.Table.create ()
let find t name = Hashtbl.find t name |> Option.bind ~f:List.hd

let find_binder t binder ~name =
  Hashtbl.find t name
  |> Option.bind
       ~f:
         (List.find ~f:(fun (entry : Entry.t) ->
            [%equal: Typed.Binder.t] entry.binder binder))
;;

let visible t =
  Hashtbl.to_alist t
  |> List.filter_map ~f:(fun (name, entries) ->
    List.hd entries |> Option.map ~f:(fun entry -> name, entry))
  |> List.sort ~compare:(Comparable.lift String.compare ~f:fst)
;;

let with_bindings t bindings ~(local_ f) =
  List.iter bindings ~f:(fun (name, entry) ->
    Hashtbl.add_multi t ~key:name ~data:entry);
  Exn.protect ~f ~finally:(fun () ->
    List.iter bindings ~f:(fun (name, _) -> Hashtbl.remove_multi t name))
;;
