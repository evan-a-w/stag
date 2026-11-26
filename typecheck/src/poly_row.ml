open! Import

let present entries ~tail =
  List.fold_right entries ~init:tail ~f:(fun (label, ty) rest ->
    Type.RExtend (label, Present, Absent, ty, rest))
;;

let open_ t scope entries =
  let tail = Scope.fresh_row t scope in
  present entries ~tail, tail
;;

let variant t scope ~tag ~arg =
  Fresh.variant t (fst (open_ t scope [ tag, arg ]))
;;

let report_duplicate_labels scope entries =
  Duplicates.report scope (List.map entries ~f:fst) ~error:(fun label ->
    Duplicate_row_entry label)
;;
