open! Import

type t = Signature.Type_decl.t String.Map.t

let empty = String.Map.empty

let join prefix name =
  if String.is_empty prefix then name else prefix ^ "." ^ name
;;

let add_decl t ~name ~decl =
  match Map.find t name with
  | Some _ -> t
  | None -> Map.set t ~key:name ~data:decl
;;

let rec add_items declarations ~prefix items =
  List.fold
    items
    ~init:declarations
    ~f:(fun declarations (item : Signature.Item.t) ->
      match item with
      | Value _ | Exception _ -> declarations
      | Type declaration ->
        add_decl
          declarations
          ~name:(join prefix declaration.name)
          ~decl:declaration
      | Module { name; items } ->
        add_items declarations ~prefix:(join prefix name) items)
;;

let add t ~prefix signature = add_items t ~prefix signature

let of_signatures signatures =
  List.fold signatures ~init:empty ~f:(fun t (prefix, signature) ->
    add t ~prefix signature)
;;

let find t name = Map.find t name
