open! Import

module Level = struct
  type t = int [@@deriving sexp, compare, equal]

  let is_allowed_at t ~used_at ~which =
    match which with
    | `Any_greater -> used_at >= t
    | `Exactly_the_same -> used_at = t
  ;;
end

module Entry = struct
  type t =
    { scheme : Type.ty
    ; level : Level.t
    ; quote_id : int option
    ; persistent : bool
    ; binder : Typed.Binder.t
    }
  [@@deriving sexp_of]

  let toplevel ~ident ~scheme ~persistent =
    { scheme; level = 0; quote_id = None; persistent; binder = Toplevel ident }
  ;;

  let is_allowed_at t ~used_at ~quote_id =
    if t.persistent
    then Level.is_allowed_at t.level ~used_at ~which:`Any_greater
    else
      Level.is_allowed_at t.level ~used_at ~which:`Exactly_the_same
      && [%equal: int option] t.quote_id quote_id
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

let with_bindings t bindings ~f =
  List.iter bindings ~f:(fun (name, entry) ->
    Hashtbl.add_multi t ~key:name ~data:entry);
  let result = f () in
  List.iter bindings ~f:(fun (name, _) -> Hashtbl.remove_multi t name);
  result
;;
