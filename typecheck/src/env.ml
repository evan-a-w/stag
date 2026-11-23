open! Import

module Level = struct
  type t = int [@@deriving sexp, compare, equal]
end

module Lock = struct
  module Kind = struct
    type t =
      | Closure of Mode.Var.t
      | Quote of Mode.Var.t
  end

  type t =
    { id : int
    ; kind : Kind.t
    }
end

module Entry = struct
  type t =
    { scheme : Type.ty
    ; level : Level.t
    ; quote_id : int option
    ; mode : Mode.Var.t
    ; locks : (Lock.t list[@sexp.opaque])
    ; binder : Typed.Binder.t
    }
  [@@deriving sexp_of]

  let toplevel ~ident ~scheme ~mode =
    { scheme
    ; level = 0
    ; quote_id = None
    ; mode
    ; locks = []
    ; binder = Toplevel ident
    }
  ;;

  (* Locks are only pushed, so those enclosing the binding are a suffix of
     those enclosing a use it is visible at, unless the use is in a splice,
     whose locks are those outside its quote. *)
  let crossed t ~locks =
    List.take_while locks ~f:(fun (lock : Lock.t) ->
      not (List.exists t.locks ~f:(fun (bound : Lock.t) -> bound.id = lock.id)))
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
