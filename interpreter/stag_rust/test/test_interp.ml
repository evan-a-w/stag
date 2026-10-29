open! Import

let source =
  {xx|
type nat = Z | S of nat
let power n c = match n with Z -> .<1>. | S m -> .<Int.mul .~c .~(power m c)>.
let cube x = .~(power (S (S (S Z))) .<x>.)
let used = cube 7

let%expect_test "used" = print_int used; [%expect {| 343 |}]
|xx}
;;

let%expect_test "the driver's sequence of operations" =
  let t = Option.value_exn (Source_helpers.typecheck ~prelude:false source) in
  let program = (Source_helpers.checked t).program in
  let interp = Interp.create ~stamps:(Stag_common.Stamp_counter.create ()) () in
  let print_summary () = print_endline (Interp.summary interp) in
  let print_result name result =
    print_s [%sexp (name : string), (result : Sexp.t Or_error.t)]
  in
  print_summary ();
  Map.iteri program.ident_info ~f:(fun ~key:ident ~data:info ->
    Interp.add_ident_info interp ~ident ~info);
  List.iter program.sccs ~f:(Interp.add_scc interp);
  print_summary ();
  let members = List.concat_map program.sccs ~f:(fun scc -> scc.members) in
  let member name =
    List.find_exn members ~f:(fun (member : Typed.Toplevel.t) ->
      String.equal member.ident.name name)
  in
  let cube = member "cube" in
  let code =
    match cube.body.desc with
    | Toplevel_splice { code; _ } -> code
    | _ -> assert false
  in
  print_result
    "splice"
    (Interp.splice
       interp
       ~code
       ~level:0
       ~entry:(fun _ -> None)
       ~reflect:(fun _ -> Or_error.error_string "no reflection")
     |> Or_error.map ~f:[%sexp_of: Typed.Expr.t]);
  print_result
    "run"
    (Interp.run interp code ~level:0 ~reflect:(fun _ ->
       Or_error.error_string "no reflection")
     |> Or_error.map ~f:[%sexp_of: Interp_value.t]);
  print_s
    [%sexp
      "global_info"
    , (Interp.global_info interp ~ident:cube.ident : Typed.Ident_info.t option)];
  print_result "eval" (Interp.eval interp |> Or_error.map ~f:[%sexp_of: unit]);
  let used = (member "used").ident in
  print_result
    "eval_toplevel"
    (Interp.eval_toplevel interp ~ident:used |> Or_error.map ~f:[%sexp_of: unit]);
  List.iter program.expect_tests ~f:(fun test ->
    print_result
      "run_expect_test"
      (Interp.run_expect_test interp test ~on_expect:(fun _ -> ())
       |> Or_error.map ~f:[%sexp_of: unit]));
  print_result
    "eval_expr"
    (Interp.eval_expr
       interp
       ~bindings:[ used, Int 1 ]
       (Option.value_exn (List.hd program.expect_tests)).body
     |> Or_error.map ~f:[%sexp_of: Interp_value.t]);
  print_summary ();
  [%expect
    {|
    sccs: 0, members: 0, ident infos: 0, arena: [0, 0, 0, 0, 0, 0, 0]
    sccs: 3, members: 3, ident infos: 8, arena: [2, 24, 89, 2, 22, 47, 0]
    (splice (Error "not implemented: splice"))
    (run (Error "not implemented: run"))
    (global_info ())
    (eval (Error "not implemented: eval"))
    (eval_toplevel (Error "not implemented: eval_toplevel"))
    (run_expect_test (Error "not implemented: run_expect_test"))
    (eval_expr (Error "not implemented: eval_expr"))
    sccs: 3, members: 3, ident infos: 8, arena: [4, 44, 131, 6, 36, 69, 0]
    |}]
;;

let%expect_test "adding again appends to the arena" =
  let t = Option.value_exn (Source_helpers.typecheck ~prelude:false source) in
  let program = (Source_helpers.checked t).program in
  let interp = Interp.create ~stamps:(Stag_common.Stamp_counter.create ()) () in
  List.iter program.sccs ~f:(Interp.add_scc interp);
  print_endline (Interp.summary interp);
  List.iter program.sccs ~f:(Interp.add_scc interp);
  print_endline (Interp.summary interp);
  [%expect
    {|
    sccs: 3, members: 3, ident infos: 0, arena: [2, 16, 60, 2, 13, 33, 0]
    sccs: 6, members: 6, ident infos: 0, arena: [4, 32, 120, 4, 26, 66, 0]
    |}]
;;

let%expect_test "types converted into one arena are built back" =
  let t = Option.value_exn (Source_helpers.typecheck ~prelude:false source) in
  let program = (Source_helpers.checked t).program in
  let ty name =
    (Map.to_alist program.ident_info
     |> List.find_exn ~f:(fun ((ident : Typed.Ident.t), _) ->
       String.equal ident.name name)
     |> snd)
      .ty
  in
  let power = ty "power"
  and cube = ty "cube" in
  List.iter Debug.Gc_stress.all ~f:(fun gc_stress ->
    let count, power', cube' = Debug.tys_into_one_arena power cube ~gc_stress in
    print_s
      [%sexp
        (gc_stress : Debug.Gc_stress.t)
      , (count : int)
      , (Debug.Round_trip.Ty.same power power' : string)
      , (Debug.Round_trip.Ty.same cube cube' : string)]);
  [%expect
    {|
    (Off 15 same same)
    (Minor 15 same same)
    (Compact 15 same same)
    |}]
;;
