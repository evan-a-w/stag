open! Import

let%expect_test "prims return strings that share nothing with their arguments" =
  let s = "abc" in
  let check path args =
    let prim =
      Option.value_exn
        (Stag_ir.Prim.find
           (Syntax.Path.of_list_exn (String.split path ~on:'.')))
    in
    match Stag_ir.Prim.apply prim ~output:ignore args with
    | Ok (String r) ->
      print_s
        [%message path (r : string) ~shares_argument:(phys_equal r s : bool)]
    | Ok _ | Error _ -> print_s [%message "unexpected" path]
  in
  check "String.sub" [ String s; Int 0; Int 3 ];
  check "String.concat" [ String ","; String_list [ s ] ];
  check "String.append" [ String s; String "" ];
  [%expect
    {|
    (String.sub (r abc) (shares_argument false))
    (String.concat (r abc) (shares_argument false))
    (String.append (r abc) (shares_argument false))
    |}]
;;
