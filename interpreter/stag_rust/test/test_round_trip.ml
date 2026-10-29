open! Import
module Round_trip = Debug.Round_trip

let sources =
  [ "a scheme", "let ident x = x"
  ; ( "records and prims"
    , {|
type point = { x : int; y : int }
let p = { x = Int.add 1 2; y = 3 }
let x = p.x
let q = { p with y = 4 }
|}
    )
  ; ( "constructors, matches, polymorphic variants and records"
    , {|
type t = A | B of int * string
let f v = match v with A -> `None | B (n, _) -> `Some n
let g x = match x with `Some n -> n | _ -> 0
let r = {`a = 1; `b = "s"}
let h r = r.`a
|}
    )
  ; ( "quotes, splices, expect tests and benches"
    , {xx|
let two = .~(.< 1 >.)
let%expect_test "e" = print_int two; [%expect {| 1 |}]
let%bench "b" n in [1; 2] = n
|xx}
    )
  ; ( "splices within quotes"
    , {|
let c = .< 1 >.
let q = .< fun y -> Int.add y .~c >.
let r x = .< fun y -> .~x >.
|}
    )
  ; ( "recursion, lets, funs, chars and strings"
    , {|
let len n = match Int.lt n 2 with true -> n | false -> Int.add 1 (len (Int.sub n 1))
let f = fun x y -> let z = (x, y, 'c', "s") in z
|}
    )
  ]
;;

let round_trip
      (type t)
      (module M : Debug.Round_trip with type t = t)
      (t : t)
      ~gc_stress
  =
  if not gc_stress
  then M.round_trip t ~gc_stress
  else (
    let control = Gc.get () in
    Gc.set { control with minor_heap_size = 4096 };
    let back = M.round_trip t ~gc_stress in
    Gc.set control;
    back)
;;

(** [(sexp_equal same)] of [t] and [t] built back, with and without
    [gc_stress]. *)
let check_round_trip
      (type t)
      (module M : Debug.Round_trip with type t = t)
      ~sexp_of_t
      (t : t)
  =
  List.iter [ false; true ] ~f:(fun gc_stress ->
    let back = round_trip (module M) t ~gc_stress in
    Gc.compact ();
    print_s
      [%message
        ""
          (gc_stress : bool)
          ~sexp_equal:(Sexp.equal (sexp_of_t t) (sexp_of_t back) : bool)
          ~same:(M.same t back : string)])
;;

let%expect_test "typed toplevels" =
  List.iter sources ~f:(fun (name, source) ->
    print_endline name;
    let t = Option.value_exn (Source_helpers.typecheck source) in
    check_round_trip
      (module Round_trip.Typed_toplevels)
      ~sexp_of_t:[%sexp_of: Typed.Toplevel.t list]
      (Source_helpers.members t));
  [%expect
    {|
    a scheme
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    records and prims
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    constructors, matches, polymorphic variants and records
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    quotes, splices, expect tests and benches
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    splices within quotes
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    recursion, lets, funs, chars and strings
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    |}]
;;

let%expect_test "whole programs, with the prelude and ident_info maps" =
  List.iter sources ~f:(fun (name, source) ->
    print_endline name;
    let t = Option.value_exn (Source_helpers.typecheck source) in
    let program = (Source_helpers.checked t).program in
    check_round_trip
      (module Round_trip.Typed_program)
      ~sexp_of_t:Typed.Program.sexp_of_t
      program;
    let back = Round_trip.Typed_program.round_trip program ~gc_stress:false in
    (* A real map, with the comparator, so lookups work. *)
    Map.invariants back.ident_info |> [%test_result: bool] ~expect:true;
    Map.iter_keys program.ident_info ~f:(fun ident ->
      assert (Map.mem back.ident_info ident)));
  [%expect
    {|
    a scheme
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    records and prims
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    constructors, matches, polymorphic variants and records
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    quotes, splices, expect tests and benches
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    splices within quotes
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    recursion, lets, funs, chars and strings
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    |}]
;;

let%expect_test "syntax programs" =
  List.iter sources ~f:(fun (name, source) ->
    print_endline name;
    check_round_trip
      (module Round_trip.Syntax_program)
      ~sexp_of_t:Syntax.Program.sexp_of_t
      (Source_helpers.parse_exn source));
  [%expect
    {|
    a scheme
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    records and prims
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    constructors, matches, polymorphic variants and records
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    quotes, splices, expect tests and benches
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    splices within quotes
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    recursion, lets, funs, chars and strings
    ((gc_stress false) (sexp_equal true) (same same))
    ((gc_stress true) (sexp_equal true) (same same))
    |}]
;;

let%expect_test "a generalised scheme shares its variable" =
  let t =
    Option.value_exn (Source_helpers.typecheck ~prelude:false "let id x = x")
  in
  let members = Source_helpers.members t in
  let back =
    round_trip (module Round_trip.Typed_toplevels) members ~gc_stress:true
  in
  let rec resolve : Type.ty -> Type.ty = function
    | TVar { state = Link ty; _ } -> resolve ty
    | ty -> ty
  in
  let sharing (toplevels : Typed.Toplevel.t list) =
    match toplevels with
    | [ { scheme = TArrow (a, b); _ } ] ->
      (match resolve a, resolve b with
       | TVar a, TVar b -> [%message "" ~same_var:(phys_equal a b : bool)]
       | _ -> [%message "unexpected" (toplevels : Typed.Toplevel.t list)])
    | _ -> [%message "unexpected" (toplevels : Typed.Toplevel.t list)]
  in
  print_s
    [%message
      "" ~input:(sharing members : Sexp.t) ~output:(sharing back : Sexp.t)];
  [%expect {| ((input (same_var true)) (output (same_var true))) |}]
;;

let%expect_test "a cyclic type stays cyclic" =
  let var = Type.Var.create 7 (Unbound 0) in
  let ty = Type.TVar var in
  var.state <- Link (TArrow (ty, TTuple [ ty; TCon ("int", []) ]));
  let back = round_trip (module Round_trip.Ty) ty ~gc_stress:true in
  (match back with
   | TVar ({ state = Link (TArrow (a, TTuple [ b; _ ])); _ } as var') ->
     print_s
       [%message
         ""
           ~cyclic:(phys_equal a back : bool)
           ~shared:(phys_equal b back : bool)
           ~id:(var'.id : int)
           ~copied:(not (phys_equal var var') : bool)
           ~same:(Round_trip.Ty.same ty back : string)]
   | _ -> print_s [%message "unexpected" (back : Type.ty)]);
  [%expect {| ((cyclic true) (shared true) (id 7) (copied true) (same same)) |}]
;;

let%expect_test "types shared as a DAG stay shared" =
  let ty =
    Fn.apply_n_times
      ~n:64
      (fun ty -> Type.TTuple [ ty; ty ])
      (Type.TCon ("int", []))
  in
  let back = round_trip (module Round_trip.Ty) ty ~gc_stress:true in
  let rec shared ty =
    match ty with
    | Type.TTuple [ a; b ] -> phys_equal a b && shared a
    | TCon ("int", []) -> true
    | _ -> false
  in
  print_s
    [%message
      ""
        ~shared:(shared back : bool)
        ~same:(Round_trip.Ty.same ty back : string)];
  [%expect {| ((shared true) (same same)) |}]
;;

let%expect_test "a set, in a row variable's bound" =
  let row_var = Type.Var.create 1 (Unbound 0) in
  row_var.bounds
  <- [ { Type.row = REmpty
       ; shadowed = String.Set.of_list [ "e"; "d"; "c"; "b"; "a" ]
       }
     ];
  let ty = Type.TRecord { id = 2; row = RVar row_var } in
  let back = round_trip (module Round_trip.Ty) ty ~gc_stress:true in
  (match back with
   | TRecord { row = RVar { bounds = [ { shadowed; _ } ]; _ }; _ } ->
     Set.invariants shadowed |> [%test_result: bool] ~expect:true;
     print_s
       [%message
         ""
           (shadowed : String.Set.t)
           ~mem_c:(Set.mem shadowed "c" : bool)
           ~same:(Round_trip.Ty.same ty back : string)]
   | _ -> print_s [%message "unexpected" (back : Type.ty)]);
  [%expect {| ((shadowed (a b c d e)) (mem_c true) (same same)) |}]
;;

let rec exprs (e : Typed.Expr.t) =
  e :: List.concat_map (Typed.Expr.sub_exprs e) ~f:exprs
;;

let toplevel_exprs toplevels =
  List.concat_map toplevels ~f:(fun (toplevel : Typed.Toplevel.t) ->
    exprs toplevel.body)
;;

let%expect_test "record types are shared, prims are the real ones" =
  let t =
    Option.value_exn
      (Source_helpers.typecheck
         {|
type point = { x : int; y : int }
let p = { x = Int.add 1 2; y = 3 }
let x = p.x
let q = { p with y = 4 }
|})
  in
  let members = Source_helpers.members t in
  let back =
    round_trip (module Round_trip.Typed_toplevels) members ~gc_stress:true
  in
  let record_types =
    List.concat_map (toplevel_exprs back) ~f:(fun e ->
      match e.desc with
      | Record fields | Record_update (_, fields) ->
        List.map fields ~f:(fun ((field : Typed.Field_ref.t), _) ->
          field.record)
      | Field (_, field) -> [ field.record ]
      | _ -> [])
  in
  let prims toplevels =
    List.filter_map (toplevel_exprs toplevels) ~f:(fun e ->
      match e.desc with
      | Var (Prim prim) -> Some prim
      | _ -> None)
  in
  let first = List.hd_exn record_types in
  print_s
    [%message
      ""
        ~record_types:(List.length record_types : int)
        ~all_shared:(List.for_all record_types ~f:(phys_equal first) : bool)
        ~prims:(prims back : Prim.t list)
        ~real_prims:
          (List.for_all2_exn (prims members) (prims back) ~f:phys_equal : bool)];
  [%expect
    {| ((record_types 4) (all_shared true) (prims (Int.add)) (real_prims true)) |}]
;;

let%expect_test "the implicit substitutions of splices are real maps" =
  let t =
    Option.value_exn
      (Source_helpers.typecheck
         "let q c = .< fun y -> let z = 1 in Int.add y .~c >.")
  in
  let back =
    round_trip
      (module Round_trip.Typed_toplevels)
      (Source_helpers.members t)
      ~gc_stress:true
  in
  List.iter (toplevel_exprs back) ~f:(fun e ->
    match e.desc with
    | Splice { subs = Implicit providers; _ } ->
      Map.invariants providers |> [%test_result: bool] ~expect:true;
      print_s
        [%message
          ""
            (providers : Typed.Binder.t String.Map.t)
            ~find_z:(Map.find providers "z" : Typed.Binder.t option)]
    | _ -> ());
  [%expect
    {| ((providers ((y (Local y/15)) (z (Local z/16)))) (find_z ((Local z/16)))) |}]
;;
