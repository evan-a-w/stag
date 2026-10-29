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

module Gc_stress = Debug.Gc_stress

let round_trip
      (type t)
      (module M : Debug.Round_trip with type t = t)
      (t : t)
      ~(gc_stress : Gc_stress.t)
  =
  match gc_stress with
  | Off -> M.round_trip t ~gc_stress
  | Minor | Compact ->
    let control = Gc.get () in
    Gc.set { control with minor_heap_size = 4096 };
    let back =
      Exn.protect
        ~f:(fun () -> M.round_trip t ~gc_stress)
        ~finally:(fun () -> Gc.set control)
    in
    back
;;

(** [(sexp_equal same)] of [t] and [t] built back, at each of [gc_stress]. *)
let check_round_trip
      ?(gc_stress = [ Gc_stress.Off; Minor ])
      (type t)
      (module M : Debug.Round_trip with type t = t)
      ~sexp_of_t
      (t : t)
  =
  List.iter gc_stress ~f:(fun gc_stress ->
    let back = round_trip (module M) t ~gc_stress in
    Gc.compact ();
    print_s
      [%message
        ""
          (gc_stress : Gc_stress.t)
          ~sexp_equal:(Sexp.equal (sexp_of_t t) (sexp_of_t back) : bool)
          ~same:(M.same t back : string)])
;;

let%expect_test "typed toplevels" =
  List.iter sources ~f:(fun (name, source) ->
    print_endline name;
    let t = Option.value_exn (Source_helpers.typecheck source) in
    check_round_trip
      ~gc_stress:Gc_stress.all
      (module Round_trip.Typed_toplevels)
      ~sexp_of_t:[%sexp_of: Typed.Toplevel.t list]
      (Source_helpers.members t));
  [%expect
    {|
    a scheme
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    records and prims
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    constructors, matches, polymorphic variants and records
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    quotes, splices, expect tests and benches
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    splices within quotes
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    recursion, lets, funs, chars and strings
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
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
    let back = Round_trip.Typed_program.round_trip program ~gc_stress:Off in
    print_s
      [%message
        "ident_info"
          ~invariants:(Map.invariants back.ident_info : bool)
          ~all_found:
            (Map.for_alli program.ident_info ~f:(fun ~key ~data:_ ->
               Map.mem back.ident_info key)
             : bool)]);
  [%expect
    {|
    a scheme
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    (ident_info (invariants true) (all_found true))
    records and prims
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    (ident_info (invariants true) (all_found true))
    constructors, matches, polymorphic variants and records
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    (ident_info (invariants true) (all_found true))
    quotes, splices, expect tests and benches
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    (ident_info (invariants true) (all_found true))
    splices within quotes
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    (ident_info (invariants true) (all_found true))
    recursion, lets, funs, chars and strings
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    (ident_info (invariants true) (all_found true))
    |}]
;;

let%expect_test "syntax programs" =
  List.iter sources ~f:(fun (name, source) ->
    print_endline name;
    check_round_trip
      ~gc_stress:Gc_stress.all
      (module Round_trip.Syntax_program)
      ~sexp_of_t:Syntax.Program.sexp_of_t
      (Source_helpers.parse_exn source));
  [%expect
    {|
    a scheme
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    records and prims
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    constructors, matches, polymorphic variants and records
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    quotes, splices, expect tests and benches
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    splices within quotes
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    recursion, lets, funs, chars and strings
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    |}]
;;

let%expect_test "a generalised scheme shares its variable" =
  let t =
    Option.value_exn (Source_helpers.typecheck ~prelude:false "let id x = x")
  in
  let members = Source_helpers.members t in
  let back =
    round_trip (module Round_trip.Typed_toplevels) members ~gc_stress:Compact
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
  let back = round_trip (module Round_trip.Ty) ty ~gc_stress:Compact in
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
  let back = round_trip (module Round_trip.Ty) ty ~gc_stress:Compact in
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
  let back = round_trip (module Round_trip.Ty) ty ~gc_stress:Compact in
  (match back with
   | TRecord { row = RVar { bounds = [ { shadowed; _ } ]; _ }; _ } ->
     print_s
       [%message
         ""
           (shadowed : String.Set.t)
           ~invariants:(Set.invariants shadowed : bool)
           ~mem_c:(Set.mem shadowed "c" : bool)
           ~same:(Round_trip.Ty.same ty back : string)]
   | _ -> print_s [%message "unexpected" (back : Type.ty)]);
  [%expect
    {| ((shadowed (a b c d e)) (invariants true) (mem_c true) (same same)) |}]
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
    round_trip (module Round_trip.Typed_toplevels) members ~gc_stress:Compact
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
      ~gc_stress:Compact
  in
  List.iter (toplevel_exprs back) ~f:(fun e ->
    match e.desc with
    | Splice { subs = Implicit providers; _ } ->
      print_s
        [%message
          ""
            (providers : Typed.Binder.t String.Map.t)
            ~invariants:(Map.invariants providers : bool)
            ~find_z:(Map.find providers "z" : Typed.Binder.t option)]
    | _ -> ());
  [%expect
    {|
    ((providers ((y (Local y/15)) (z (Local z/16)))) (invariants true)
     (find_z ((Local z/16))))
    |}]
;;

let%expect_test "strings are bytes, not necessarily UTF-8" =
  let ty = Type.TCon ("\255\000\xe9", []) in
  check_round_trip
    ~gc_stress:Gc_stress.all
    (module Round_trip.Ty)
    ~sexp_of_t:Type.sexp_of_ty
    ty;
  print_s [%sexp (Round_trip.Ty.round_trip ty ~gc_stress:Off : Type.ty)];
  print_endline (Debug.ty ty);
  let t =
    Option.value_exn
      (Source_helpers.typecheck ~prelude:false {|let s = "\255\000 \xe9"|})
  in
  check_round_trip
    ~gc_stress:Gc_stress.all
    (module Round_trip.Typed_toplevels)
    ~sexp_of_t:[%sexp_of: Typed.Toplevel.t list]
    (Source_helpers.members t);
  [%expect
    {|
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    (TCon "\255\000\233" ())
    #0
    Arena {
        prim: {},
        type_row: {},
        type_ty: {
            #0: TCon(
                "\255\000\233",
                [],
            ),
        },
        type_var_presence_presence_bound: {},
        type_var_row_bound: {},
        type_var_ty_nothing: {},
        typed_record_type: {},
    }
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    |}]
;;

let pos : Example.Pos.t = { line = 1; col = 2 }
let name txt : Example.Name.t = { txt; pos }

let record_type name : Example.Record_type.t =
  { name; fields = [| "x"; "y" |]; ignored = (fun () -> ()); arity = 2 }
;;

let example_ty () : Example.ty =
  let record = record_type "point" in
  let var : Example.ty Example.var = { id = 3; link = Some Int } in
  Tuple
    ( Arrow
        ( Record { record; skipped = Fn.id; fields = [ "x", Int; "y", Var var ] }
        , Record { record; skipped = Fn.id; fields = [] } )
    , name "n" )
;;

let%expect_test "example declarations" =
  let ty = example_ty () in
  check_round_trip
    ~gc_stress:Gc_stress.all
    (module Round_trip.Example_ty)
    ~sexp_of_t:Example.sexp_of_ty
    ty;
  (match Round_trip.Example_ty.round_trip ty ~gc_stress:Compact with
   | Tuple (Arrow (Record a, Record b), _) ->
     print_s
       [%message
         "rebuilt"
           ~shared_record_type:(phys_equal a.record b.record : bool)
           ~skipped_rebuilt:(a.skipped 5 : int)]
   | back -> print_s [%message "unexpected" (back : Example.ty)]);
  let rec expr n : Example.Expr.t =
    { desc =
        (if n = 0
         then Lit 0
         else if n % 2 = 0
         then App (expr (n - 1), expr (n - 1))
         else Let { name = name "x"; body = expr (n - 1); in_ = expr (n - 1) })
    ; ty
    ; env = String.Map.of_alist_exn [ "a", Example.Int; "b", ty; "\255", Unit ]
    ; free = String.Set.of_list [ "c"; "d" ]
    ; ids = Int.Map.of_alist_exn [ 1, 10; 2, 20 ]
    ; next = (if n = 1 then Some (expr 0) else None)
    }
  in
  let expr = expr 3 in
  check_round_trip
    ~gc_stress:Gc_stress.all
    (module Round_trip.Example_expr)
    ~sexp_of_t:Example.Expr.sexp_of_t
    expr;
  let back = Round_trip.Example_expr.round_trip expr ~gc_stress:Minor in
  print_s
    [%message
      "maps"
        ~env:(Map.invariants back.env : bool)
        ~free:(Set.invariants back.free : bool)
        ~ids:(Map.invariants back.ids : bool)
        ~find:(Map.find back.ids 2 : int option)];
  let defs : Example.Defs.expr =
    { txt =
        Pair
          ( { txt = Const ('c', true); pos }
          , Some { txt = Absurd []; pos = { line = 3; col = 4 } } )
    ; pos
    }
  in
  check_round_trip
    ~gc_stress:Gc_stress.all
    (module Round_trip.Example_defs_expr)
    ~sexp_of_t:Example.Defs.sexp_of_expr
    defs;
  let chained : Example.chained = { var = { id = 1; link = Some ty } } in
  check_round_trip
    ~gc_stress:Gc_stress.all
    (module Round_trip.Example_chained)
    ~sexp_of_t:Example.sexp_of_chained
    chained;
  [%expect
    {|
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    (rebuilt (shared_record_type true) (skipped_rebuilt 5))
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    (maps (env true) (free true) (ids true) (find (20)))
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    ((gc_stress Off) (sexp_equal true) (same same))
    ((gc_stress Minor) (sexp_equal true) (same same))
    ((gc_stress Compact) (sexp_equal true) (same same))
    |}]
;;

let%expect_test "errors building" =
  let show f =
    match f () with
    | (_ : _) -> print_endline "built"
    | exception Failure message -> print_endline message
  in
  (* A callback raising. *)
  show (fun () ->
    Round_trip.Example_ty.round_trip
      (Record { record = record_type "raise"; skipped = Fn.id; fields = [] })
      ~gc_stress:Off);
  (* A cycle through a value rebuilt by a callback. *)
  let var : Example.ty Example.var = { id = 1; link = None } in
  let ty : Example.ty = Var var in
  var.link
  <- Some
       (Record
          { record = record_type "r"; skipped = Fn.id; fields = [ "self", ty ] });
  show (fun () -> Round_trip.Example_ty.round_trip ty ~gc_stress:Off);
  (* A callback not registered. *)
  print_endline (Debug.missing_callback ());
  (* Building after errors still works. *)
  show (fun () ->
    Round_trip.Example_ty.round_trip (example_ty ()) ~gc_stress:Off);
  [%expect
    {|
    OCaml callback ppx_rust.rebuild.Example.Record_type.t raised Failure("rebuilding raise")
    a cycle through a value rebuilt by an OCaml callback
    error: no OCaml callback ppx_rust.map.unregistered (is its library linked?)
    built
    |}]
;;
