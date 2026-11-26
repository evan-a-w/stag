open! Import

(* An entry is known by its type, kept as is by what the context is unified
   with: rows are rewritten around their entries. *)
type t = (Type.ty * Typecheck_error.Unusable_in_quote.t) list String.Table.t

let create () = String.Table.create ()

let record t ~name ~entry unusable =
  Hashtbl.add_multi t ~key:name ~data:(entry, unusable)
;;

let find t ~name ~entry =
  List.find_map (Hashtbl.find_multi t name) ~f:(fun (entry', unusable) ->
    Option.some_if (phys_equal entry entry') unusable)
;;
