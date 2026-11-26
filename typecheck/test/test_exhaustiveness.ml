open! Import

(* type option = None | Some of 'a type abc = A | B of de | C of int * int
   (declared in module M) type de = D | E type empty = | type point =
   [{ x : int; y : bool; z : int }] (declared in module M) *)
let declarations =
  [ "option", [ "None", 0; "Some", 1 ]
  ; "M.abc", [ "M.A", 0; "M.B", 1; "M.C", 2 ]
  ; "de", [ "D", 0; "E", 0 ]
  ; "empty", []
  ]
;;

let variant_constructors type_name =
  List.Assoc.find declarations type_name ~equal:String.equal
  |> Option.map
       ~f:
         (List.mapi ~f:(fun index (name, arity) ->
            { Typed.Constructor_ref.type_name; name; index }, arity))
;;

let make ?(ty = Type.error) desc : Typed.Pattern.t =
  { desc; ty; loc = Loc.none }
;;

let any = make Any
let var name = make (Var { name; stamp = 0 })
let int n = make ~ty:Type.int (Lit (Int n))
let bool b = make ~ty:Type.bool (Lit (Bool b))
let char c = make ~ty:Type.char (Lit (Char c))
let string s = make ~ty:Type.string (Lit (String s))
let unit = make ~ty:Type.unit (Lit Unit)
let ( ||| ) a b = make ~ty:a.Typed.Pattern.ty (Or (a, b))
let tuple ps = make (Tuple ps)
let deref p = make (Deref p)
let error = make (Error [])

let construct type_name name args =
  let index =
    List.Assoc.find_exn declarations type_name ~equal:String.equal
    |> List.findi_exn ~f:(fun _ (name', _) -> String.equal name name')
    |> fst
  in
  make ~ty:(TCon (type_name, [])) (Construct ({ type_name; name; index }, args))
;;

let none = construct "option" "None" []
let some p = construct "option" "Some" [ p ]
let a = construct "M.abc" "M.A" []
let b p = construct "M.abc" "M.B" [ p ]
let c p q = construct "M.abc" "M.C" [ p; q ]
let d = construct "de" "D" []
let e = construct "de" "E" []

let exn name args =
  make ~ty:Type.exn (Construct (Typed.Constructor_ref.exception_ name, args))
;;

let point =
  { Typed.Record_type.name = "M.point"; fields = [| "M.x"; "M.y"; "M.z" |] }
;;

let record fields =
  make
    ~ty:(TCon (point.name, []))
    (Record
       (List.map fields ~f:(fun (name, p) ->
          let index, _ =
            Array.findi_exn point.fields ~f:(fun _ field ->
              String.equal field ("M." ^ name))
          in
          { Typed.Field_ref.record = point; index }, p)))
;;

let next_id = ref 0

let fresh_id () =
  incr next_id;
  !next_id
;;

let open_tail () = Type.RVar (Type.Var.create (fresh_id ()) (Unbound 0))

(* A polymorphic variant type of the tags, of unit argument unless given. *)
let variant ?(open_ = false) ?(presence = []) ?(args = []) tags =
  let row =
    List.fold_right
      tags
      ~init:(if open_ then open_tail () else REmpty)
      ~f:(fun tag rest ->
        let presence =
          List.Assoc.find presence tag ~equal:String.equal
          |> Option.value ~default:Type.Present
        in
        let ty =
          List.Assoc.find args tag ~equal:String.equal
          |> Option.value ~default:Type.unit
        in
        Type.RExtend (tag, presence, Absent, ty, rest))
  in
  Type.TVariant { id = fresh_id (); row }
;;

let tag ty name arg = make ~ty (Poly_variant (name, arg))

let poly_record ?(rest = Typed.Pattern.Rest.Open) fields =
  make (Poly_record { fields; rest })
;;

let check ?(scrutinee = Type.error) patterns =
  print_s
    [%sexp
      (Exhaustiveness.check_match
         ~variant_constructors
         ~name:Prelude_path.short_name
         ~scrutinee
         patterns
       : Exhaustiveness.Result.t)]
;;

let%expect_test "nominal variants" =
  check [ none; some any ];
  check [ none ];
  check [ some any ];
  check [ a; b d ];
  check [ a; b d; c any any ];
  check [ a; b d; b e; c (int 1) any ];
  check [ a; b (var "x"); c any any; b e ];
  [%expect
    {|
    ((missing ()) (unused ()))
    ((missing ("Some _")) (unused ()))
    ((missing (None)) (unused ()))
    ((missing ("C (_, _)")) (unused ()))
    ((missing ("B E")) (unused ()))
    ((missing ("C (0, _)")) (unused ()))
    ((missing ()) (unused (3)))
    |}]
;;

let%expect_test "nested counterexamples" =
  check [ a; b d; c any any ];
  check [ some (some none); none ];
  check [ some (b d); some (c any any); some a ];
  check
    [ some (c (int 0) any); some (c any (int 0)); none; some a; some (b any) ];
  [%expect
    {|
    ((missing ("B E")) (unused ()))
    ((missing ("Some None")) (unused ()))
    ((missing (None)) (unused ()))
    ((missing ("Some (C (1, 1))")) (unused ()))
    |}]
;;

let%expect_test "empty types" =
  check ~scrutinee:(TCon ("empty", [])) [];
  check ~scrutinee:(TCon ("M.abc", [])) [];
  check ~scrutinee:Type.int [];
  [%expect
    {|
    ((missing ()) (unused ()))
    ((missing (_)) (unused ()))
    ((missing (_)) (unused ()))
    |}]
;;

let%expect_test "wildcards and redundancy" =
  check [ any; a ];
  check [ var "x"; any; error ];
  check [ error; a ];
  check [ a; b any; a; c any any; b d; any ];
  check [ some any; none; some (some any) ];
  [%expect
    {|
    ((missing ()) (unused (1)))
    ((missing ()) (unused (1 2)))
    ((missing ()) (unused (1)))
    ((missing ()) (unused (2 4 5)))
    ((missing ()) (unused (2)))
    |}]
;;

let%expect_test "or-patterns" =
  check [ a ||| b any; c any any ];
  check [ a ||| b d; c any any ];
  check [ b (d ||| e) ||| a; c any any ];
  check [ a; b d ||| a ];
  check [ b d; b e; a ||| b d ];
  check [ some (none ||| some any) ||| none ];
  [%expect
    {|
    ((missing ()) (unused ()))
    ((missing ("B E")) (unused ()))
    ((missing ()) (unused ()))
    ((missing ("C (_, _)")) (unused ()))
    ((missing ("C (_, _)")) (unused ()))
    ((missing ()) (unused ()))
    |}]
;;

let%expect_test "exceptions" =
  check [ exn "Not_found" [] ];
  check [ exn "Not_found" []; exn "Failure" [ any ]; any ];
  print_s
    [%sexp
      (Exhaustiveness.check_exception_arms
         ~variant_constructors
         [ exn "Not_found" []
         ; exn "Failure" [ string "a" ]
         ; exn "Not_found" []
         ; exn "Failure" [ any ]
         ; exn "Failure" [ string "b" ]
         ; exn "Exit" [ some any ]
         ; exn "Exit" [ none ]
         ; exn "Exit" [ any ]
         ]
       : int list)];
  [%expect
    {|
    ((missing (_)) (unused ()))
    ((missing ()) (unused ()))
    (2 4 7)
    |}]
;;

let%expect_test "literals" =
  check [ int 0; int 1; int 3 ];
  check [ int 1; int 0; any; int 2 ];
  check [ char 'a'; char 'b' ];
  check [ string ""; string "a"; string "aa"; string "b" ];
  check [ bool true ];
  check [ bool false ];
  check [ bool true; bool false ];
  check [ bool true; bool false; any ];
  check [ unit ];
  check [ unit; any ];
  check [ some (int 0); some any; none ];
  [%expect
    {|
    ((missing (2)) (unused ()))
    ((missing ()) (unused (3)))
    ((missing ('c')) (unused ()))
    ((missing ("\"aaa\"")) (unused ()))
    ((missing (false)) (unused ()))
    ((missing (true)) (unused ()))
    ((missing ()) (unused ()))
    ((missing ()) (unused (2)))
    ((missing ()) (unused ()))
    ((missing ()) (unused (1)))
    ((missing ()) (unused ()))
    |}]
;;

let%expect_test "tuples" =
  check [ tuple [ none; none ]; tuple [ some any; any ] ];
  check [ tuple [ none; none ]; tuple [ some any; some any ] ];
  check
    [ tuple [ none; any ]; tuple [ any; none ]; tuple [ some any; some any ] ];
  check
    [ tuple [ bool true; bool true ]
    ; tuple [ bool false; bool false ]
    ; tuple [ any; bool false ]
    ];
  check [ tuple [ any; any ]; tuple [ none; some any ] ];
  check [ tuple [ tuple [ a; d ]; int 0 ]; tuple [ tuple [ any; e ]; any ] ];
  [%expect
    {|
    ((missing ("None, Some _")) (unused ()))
    ((missing ("None, Some _")) (unused ()))
    ((missing ()) (unused ()))
    ((missing ("false, true")) (unused ()))
    ((missing ()) (unused (1)))
    ((missing ("(B _, D), _")) (unused ()))
    |}]
;;

let%expect_test "deref" =
  check [ deref none; deref (some any) ];
  check [ deref (some (bool true)); deref none ];
  check [ deref any; deref none ];
  [%expect
    {|
    ((missing ()) (unused ()))
    ((missing ("*(Some false)")) (unused ()))
    ((missing ()) (unused (1)))
    |}]
;;

let%expect_test "records" =
  check [ record [ "x", int 0 ]; record [ "y", bool true ] ];
  check [ record [ "y", bool true ]; record [ "y", bool false; "z", any ] ];
  check [ record [ "x", int 0; "y", bool true ]; record [ "y", bool false ] ];
  check [ record []; record [ "x", int 0 ] ];
  check [ record [ "x", var "x" ]; record [ "y", bool true ] ];
  check [ some (record [ "z", int 1; "y", bool true ]); none ];
  [%expect
    {|
    ((missing ("{x = 1; y = false}")) (unused ()))
    ((missing ()) (unused ()))
    ((missing ("{x = 1; y = true}")) (unused ()))
    ((missing ()) (unused (1)))
    ((missing ()) (unused (1)))
    ((missing ("Some {y = false}")) (unused ()))
    |}]
;;

let%expect_test "polymorphic records" =
  check [ poly_record [ "a", bool true ]; poly_record [ "b", int 0 ] ];
  check [ poly_record [ "a", bool true ]; poly_record [ "a", bool false ] ];
  check
    [ poly_record ~rest:Closed [ "a", bool true; "b", any ]
    ; poly_record ~rest:Closed [ "b", none; "a", any ]
    ];
  check [ poly_record []; poly_record [ "a", bool true ] ];
  [%expect
    {|
    ((missing ("{`a = false; `b = 1; ..}")) (unused ()))
    ((missing ()) (unused ()))
    ((missing ("{`a = false; `b = Some _; ..}")) (unused ()))
    ((missing ()) (unused (1)))
    |}]
;;

let%expect_test "polymorphic variants" =
  let closed = variant [ "A"; "B" ] ~args:[ "B", Type.int ] in
  check [ tag closed "A" None; tag closed "B" (Some any) ];
  check [ tag closed "A" None ];
  check [ tag closed "B" (Some any) ];
  check [ tag closed "A" None; tag closed "B" (Some (int 0)) ];
  check
    [ tag closed "A" (Some unit)
    ; tag closed "B" (Some any)
    ; tag closed "A" None
    ];
  let open_ = variant ~open_:true [ "A"; "B" ] in
  check [ tag open_ "A" None; tag open_ "B" None ];
  check [ tag open_ "A" None; tag open_ "B" None; any ];
  let absent = variant [ "A"; "B"; "C" ] ~presence:[ "C", Absent ] in
  check [ tag absent "A" None; tag absent "B" None ];
  let unknown =
    variant
      [ "A"; "B" ]
      ~presence:[ "B", PVar (Type.Var.create (fresh_id ()) (Unbound 0)) ]
  in
  check [ tag unknown "A" None ];
  let shadowed =
    Type.TVariant
      { id = fresh_id ()
      ; row =
          RExtend
            ( "A"
            , Absent
            , Absent
            , Type.unit
            , RExtend ("A", Present, Absent, Type.unit, REmpty) )
      }
  in
  check [ tag shadowed "A" None ];
  let nested = variant [ "A"; "B" ] ~args:[ "A", TCon ("de", []) ] in
  check [ some (tag nested "A" (Some d)); some (tag nested "B" None); none ];
  check [ some (tag open_ "A" None); none ];
  [%expect
    {|
    ((missing ()) (unused ()))
    ((missing ("`B _")) (unused ()))
    ((missing (`A)) (unused ()))
    ((missing ("`B 1")) (unused ()))
    ((missing ()) (unused (2)))
    ((missing (_)) (unused ()))
    ((missing ()) (unused ()))
    ((missing ()) (unused ()))
    ((missing (`B)) (unused ()))
    ((missing ()) (unused ()))
    ((missing ("Some (`A E)")) (unused ()))
    ((missing ("Some `B")) (unused ()))
    |}]
;;

let%expect_test "many constructors" =
  let n = 300 in
  let declarations = [ "big", List.init n ~f:(fun i -> sprintf "K%d" i, 1) ] in
  let variant_constructors type_name =
    List.Assoc.find declarations type_name ~equal:String.equal
    |> Option.map
         ~f:
           (List.mapi ~f:(fun index (name, arity) ->
              { Typed.Constructor_ref.type_name; name; index }, arity))
  in
  let k i p =
    make
      (Construct
         ({ type_name = "big"; name = sprintf "K%d" i; index = i }, [ p ]))
  in
  let patterns =
    List.init n ~f:(fun i -> tuple [ k i (bool true); k ((i + 1) % n) any ])
    @ List.init n ~f:(fun i -> tuple [ k i (bool false); any ])
  in
  print_s
    [%sexp
      (Exhaustiveness.check_match
         ~variant_constructors
         ~name:Prelude_path.short_name
         ~scrutinee:Type.error
         patterns
       : Exhaustiveness.Result.t)];
  [%expect {| ((missing ("K0 true, K0 _")) (unused ())) |}]
;;
