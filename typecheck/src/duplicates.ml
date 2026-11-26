open! Import

let check (names : Syntax.Name.t list) ~error:kind =
  List.fold_result names ~init:String.Set.empty ~f:(fun seen { txt; loc } ->
    if Set.mem seen txt
    then Typecheck_error.error ~loc (kind txt)
    else Ok (Set.add seen txt))
  |> Result.ignore_m
;;

let report scope (names : Syntax.Name.t list) ~error:kind =
  ignore
    (List.fold names ~init:String.Set.empty ~f:(fun seen { txt; loc } ->
       if Set.mem seen txt
       then (
         Scope.report scope (Typecheck_error.create ~loc (kind txt));
         seen)
       else Set.add seen txt)
     : String.Set.t)
;;
