open! Core
open Stag
open Syntax
open Helpers

let prim_names =
  List.filter_map prims ~f:(function
    | Toplevel.Let (name, _, _, _) -> Some name
    | Type _ | Module _ -> None)
;;

let members (checked : Typecheck.Checked.t) =
  List.concat_map checked.program.sccs ~f:(fun scc -> scc.members)
  |> List.filter ~f:(fun (member : Typed.Toplevel.t) ->
    not (List.mem prim_names member.ident.name ~equal:String.equal))
;;

let print_values driver =
  List.iter
    (members (Driver.checked driver))
    ~f:(fun { ident; _ } ->
      match Treewalk.find (Driver.interpreter driver) ~ident with
      | Ok value ->
        print_s [%sexp (ident.name : string), (value : Treewalk.Value.t)]
      | Error _ -> ())
;;

(* Checks [program], printing its signature, the bodies of [bodies], and the
   toplevels evaluated at compile time; then evaluates it, printing every
   toplevel's value. *)
let run ?(bodies = []) ?(eval = true) program =
  match Driver.check (prims @ program) with
  | Error error -> print_s [%sexp (error : Typecheck_error.t)]
  | Ok driver ->
    let checked = Driver.checked driver in
    print_signature (List.drop checked.signature (List.length prims));
    List.iter (members checked) ~f:(fun { ident; body; _ } ->
      if List.mem bodies ident.name ~equal:String.equal
      then print_s [%sexp (ident : Typed.Ident.t), (body : Typed.Expr.t)]);
    print_endline "-- evaluated at compile time";
    print_values driver;
    if eval
    then (
      print_endline "-- evaluated";
      let result = Driver.eval driver in
      print_values driver;
      match result with
      | Ok () -> ()
      | Error error -> print_s [%sexp (error : Error.t)])
;;

let nat = Decl.variant "nat" [ "Z", []; "S", [ Ty.con "nat" ] ]

let rec nat_of_int n =
  if n = 0 then construct "Z" else construct "S" ~args:[ nat_of_int (n - 1) ]
;;

(* [power n c] is a list of [n] copies of [c]. *)
let power =
  top
    "power"
    [ "n"; "c" ]
    (match_
       (var "n")
       [ Pat.construct "Z", quote (construct "Nil")
       ; ( Pat.construct "S" ~args:[ Pat.var "m" ]
         , quote
             (construct
                "Cons"
                ~args:
                  [ splice (var "c")
                  ; splice (app (var "power") [ var "m"; var "c" ])
                  ]) )
       ])
;;

let%expect_test "a generator used by a toplevel splice" =
  run
    [ nat
    ; Decl.list
    ; power
    ; top "three" [] (nat_of_int 3)
    ; top
        "cube"
        [ "x" ]
        (splice (app (var "power") [ var "three"; quote (var "x") ]))
    ; top "used" [] (app (var "cube") [ int 7 ])
    ];
  [%expect
    {|
    type nat = Z | S of nat
    type list['a] = Nil | Cons of 'a * list['a]
    val power : nat -> <'a>{..'b} -> <list['a]>{..'c} where ..'b <= {..'c}
    val three : nat
    val cube : 'a -> list['a]
    val used : list[int]
    -- evaluated at compile time
    (power <fun>)
    (three (S (S (S (Z)))))
    -- evaluated
    (power <fun>)
    (three (S (S (S (Z)))))
    (cube <fun>)
    (used (Cons 7 (Cons 7 (Cons 7 (Nil)))))
    |}]
;;

let%expect_test "re-inferred types are more precise than the quote's" =
  run
    ~bodies:[ "wrapped" ]
    [ top
        "wrap"
        [ "c" ]
        (quote (let_ "y" [] (splice (var "c")) (tuple [ var "y"; var "y" ])))
    ; top "wrapped" [] (splice (app (var "wrap") [ quote (int 1) ]))
    ; top "generated" [] (app (var "wrap") [ quote (int 1) ])
    ];
  [%expect
    {|
    val wrap : <'a>{y?'b : 'a; ..'c} -> <'a * 'a>{..'d} where ..'c <= {..'d}
    val wrapped : int * int
    val generated : <int * int>{..'_a}
    (wrapped/5
     (Toplevel_splice
      (code
       (App
        (Var (Bound (Toplevel wrap/4)) :
         "<int>{y?'_a : int; ..'_b} -> <int * int>{}")
        (Quote (Lit (Int 1) : int) : "<int>{y?'_a : int; ..'_b}") :
        "<int * int>{}"))
      (expansion
       (Let (ident y/18) (args ()) (generalized true) (body (Lit (Int 1) : int))
        (in_
         (Tuple
          ((Var (Bound (Local y/18)) : int) (Var (Bound (Local y/18)) : int)) :
          "int * int"))
        : "int * int"))
      : "int * int" (where "..'_b <= {}")))
    -- evaluated at compile time
    (wrap <fun>)
    -- evaluated
    (wrap <fun>)
    (wrapped (Tuple 1 1))
    (generated
     (Code
      (Let (ident y/17) (args ()) (generalized false) (body (Lit (Int 1) : int))
       (in_
        (Tuple ((Var (Bound (Local y/17)) : 'a) (Var (Bound (Local y/17)) : 'a))
         : "'a * 'a"))
       : "'a * 'a")))
    |}]
;;

let%expect_test "mutually dependent compile time helpers" =
  run
    [ nat
    ; top
        "even"
        [ "n" ]
        (match_
           (var "n")
           [ Pat.construct "Z", quote (bool true)
           ; ( Pat.construct "S" ~args:[ Pat.var "m" ]
             , app (var "odd") [ var "m" ] )
           ])
    ; top
        "odd"
        [ "n" ]
        (match_
           (var "n")
           [ Pat.construct "Z", quote (bool false)
           ; ( Pat.construct "S" ~args:[ Pat.var "m" ]
             , app (var "even") [ var "m" ] )
           ])
    ; top "five_is_odd" [] (splice (app (var "odd") [ nat_of_int 5 ]))
    ];
  [%expect
    {|
    type nat = Z | S of nat
    val even : nat -> <bool>{..'a}
    val odd : nat -> <bool>{..'a}
    val five_is_odd : bool
    -- evaluated at compile time
    (even <fun>)
    (odd <fun>)
    -- evaluated
    (even <fun>)
    (odd <fun>)
    (five_is_odd true)
    |}]
;;

let%expect_test "a toplevel used at both levels" =
  run
    [ top "pair" [ "x" ] (tuple [ var "x"; var "x" ])
    ; top "at_runtime" [] (app (var "pair") [ int 1 ])
    ; top
        "at_compile_time"
        []
        (splice
           (app (var "pair") [ quote (int 2) ]
            |> fun e -> match_ e [ Pat.tuple [ Pat.var "a"; Pat.any ], var "a" ]
           ))
    ; top
        "in_generated_code"
        []
        (splice (quote (app (var "pair") [ bool true ])))
    ];
  [%expect
    {|
    val pair : 'a -> 'a * 'a
    val at_runtime : int * int
    val at_compile_time : int
    val in_generated_code : bool * bool
    -- evaluated at compile time
    (pair <fun>)
    -- evaluated
    (pair <fun>)
    (at_runtime (Tuple 1 1))
    (at_compile_time 2)
    (in_generated_code (Tuple true true))
    |}]
;;

let%expect_test "compile time use of a toplevel being checked" =
  run [ top "f" [ "x" ] (splice (seq (var "f") (quote (var "x")))) ];
  run
    [ top "f" [ "x" ] (app (var "g") [ var "x" ])
    ; top "g" [ "x" ] (splice (seq (var "f") (quote (var "x"))))
    ];
  run [ top "f" [ "x" ] (splice (quote (app (var "f") [ var "x" ]))) ];
  [%expect
    {|
    (Compile_time_use_of_toplevel_being_checked f)
    (Compile_time_use_of_toplevel_being_checked f)
    val f : 'a -> 'b
    -- evaluated at compile time
    -- evaluated
    (f <fun>)
    |}]
;;

let%expect_test "level 0 locals are not usable at compile time" =
  run [ top "f" [ "x" ] (splice (seq (var "x") (quote (int 1)))) ];
  run
    [ top "f" [] (let_ "x" [] (int 1) (splice (seq (var "x") (quote (int 1)))))
    ];
  [%expect
    {|
    (Variable_used_at_wrong_level (name x) (var_level 0) (used_at -1))
    (Variable_used_at_wrong_level (name x) (var_level 0) (used_at -1))
    |}]
;;

let%expect_test "toplevels are only evaluated if used at compile time" =
  let abc = Decl.variant "abc" [ "A", []; "B", [] ] in
  let fails =
    top "fails" [] (match_ (construct "B") [ Pat.construct "A", int 0 ])
  in
  run
    [ abc
    ; fails
    ; top
        "loops"
        []
        (let_ "f" [ "x" ] (app (var "f") [ var "x" ]) (app (var "f") [ unit ]))
    ; top "unused_code" [] (quote (int 0))
    ; top "used_code" [] (quote (int 1))
    ; top "spliced" [] (splice (var "used_code"))
    ]
    ~eval:false;
  run [ abc; fails; top "spliced" [] (splice (quote (int 1))) ];
  run
    [ abc
    ; fails
    ; top "spliced" [] (splice (seq (var "fails") (quote (int 1))))
    ];
  [%expect
    {|
    type abc = A | B
    val fails : int
    val loops : '_a
    val unused_code : <int>{..'a}
    val used_code : <int>{..'a}
    val spliced : int
    -- evaluated at compile time
    (used_code (Code (Lit (Int 1) : int)))
    type abc = A | B
    val fails : int
    val spliced : int
    -- evaluated at compile time
    -- evaluated
    ("Match failure" (value (B)))
    (Splice_evaluation_failed
     (code
      (Let (ident _/15) (args ()) (generalized true)
       (body (Var (Bound (Toplevel fails/4)) : int))
       (in_ (Quote (Lit (Int 1) : int) : <int>{})) : <int>{}))
     (error ("Match failure" (value (B)))))
    |}]
;;

let%expect_test "splice evaluation failures" =
  let abc = Decl.variant "abc" [ "A", []; "B", [] ] in
  run
    [ abc
    ; top
        "x"
        []
        (splice (match_ (construct "B") [ Pat.construct "A", quote (int 0) ]))
    ];
  [%expect
    {|
    (Splice_evaluation_failed
     (code
      (Match (Construct ((type_name abc) (name B) (index 1)) () : abc)
       (((Construct ((type_name abc) (name A) (index 0)) () : abc)
         (Quote (Lit (Int 0) : int) : <int>{})))
       : <int>{}))
     (error ("Match failure" (value (B)))))
    |}]
;;

let%expect_test "substitutions at toplevel splices" =
  run
    ~bodies:[ "implicit"; "explicit"; "explicit_toplevel" ]
    [ top "n" [] (int 3)
    ; top "code" [] (quote (tuple [ var "n"; var "x" ]))
    ; top "implicit" [ "x" ] (splice (var "code"))
    ; top
        "explicit"
        []
        (splice ~subs:[ "n", Some (bool true); "x", Some (int 1) ] (var "code"))
    ; top
        "explicit_toplevel"
        []
        (splice ~subs:[ "n", None; "x", Some (var "n") ] (var "code"))
    ; top "applied" [] (app (var "implicit") [ bool false ])
    ];
  [%expect
    {|
    val n : int
    val code : <'a * 'b>{n : 'a; x : 'b; ..'c}
    val implicit : 'a -> int * 'a
    val explicit : bool * int
    val explicit_toplevel : int * int
    val applied : int * bool
    (implicit/6
     (Toplevel_splice
      (code (Var (Bound (Toplevel code/5)) : "<int * 'a>{n : int; x : 'a}"))
      (expansion
       (Tuple
        ((Var (Bound (Toplevel n/4)) : int) (Var (Bound (Local x/19)) : 'a)) :
        "int * 'a"))
      : "int * 'a"))
    (explicit/7
     (Toplevel_splice
      (code (Var (Bound (Toplevel code/5)) : "<bool * int>{n : bool; x : int}"))
      (expansion
       (Let (ident n/20) (args ()) (generalized false)
        (body (Lit (Bool true) : bool))
        (in_
         (Let (ident x/21) (args ()) (generalized false)
          (body (Lit (Int 1) : int))
          (in_
           (Tuple
            ((Var (Bound (Local n/20)) : bool) (Var (Bound (Local x/21)) : int))
            : "bool * int"))
          : "bool * int"))
        : "bool * int"))
      : "bool * int"))
    (explicit_toplevel/8
     (Toplevel_splice
      (code (Var (Bound (Toplevel code/5)) : "<int * int>{n : int; x : int}"))
      (expansion
       (Let (ident n/22) (args ()) (generalized false)
        (body (Var (Bound (Toplevel n/4)) : int))
        (in_
         (Let (ident x/23) (args ()) (generalized false)
          (body (Var (Bound (Toplevel n/4)) : int))
          (in_
           (Tuple
            ((Var (Bound (Local n/22)) : int) (Var (Bound (Local x/23)) : int)) :
            "int * int"))
          : "int * int"))
        : "int * int"))
      : "int * int"))
    -- evaluated at compile time
    (code (Code (Tuple ((Var (Free n) : 'a) (Var (Free x) : 'b)) : "'a * 'b")))
    -- evaluated
    (n 3)
    (code (Code (Tuple ((Var (Free n) : 'a) (Var (Free x) : 'b)) : "'a * 'b")))
    (implicit <fun>)
    (explicit (Tuple true 1))
    (explicit_toplevel (Tuple 3 3))
    (applied (Tuple 3 false))
    |}]
;;

let%expect_test "nested toplevel splices" =
  run
    ~bodies:[ "nested" ]
    [ top "inner" [] (quote (quote (int 1)))
    ; top "nested" [] (splice (splice (var "inner")))
    ; top "in_quote" [] (quote (splice (splice (var "inner"))))
    ];
  [%expect
    {|
    val inner : <<int>{..'a}>{..'b}
    val nested : int
    val in_quote : <int>{..'_a}
    (nested/5
     (Toplevel_splice
      (code
       (Toplevel_splice (code (Var (Bound (Toplevel inner/4)) : <<int>{}>{}))
        (expansion (Quote (Lit (Int 1) : int) : <int>{})) : <int>{}))
      (expansion (Lit (Int 1) : int)) : int))
    -- evaluated at compile time
    (inner (Code (Quote (Lit (Int 1) : int) : <int>{..'a})))
    -- evaluated
    (inner (Code (Quote (Lit (Int 1) : int) : <int>{..'a})))
    (nested 1)
    (in_quote (Code (Lit (Int 1) : int)))
    |}]
;;

let%expect_test "compile time values embedded in generated code" =
  run
    ~bodies:[ "embeds" ]
    [ top
        "embeds"
        []
        (splice
           (let_
              "k"
              [ "x" ]
              (tuple [ var "x"; int 1 ])
              (quote
                 (tuple
                    [ app (var "k") [ bool true ]; app (var "k") [ int 2 ] ]))))
    ];
  [%expect
    {|
    val embeds : (bool * int) * (int * int)
    (embeds/4
     (Toplevel_splice
      (code
       (Let (ident k/15) (args (x/14)) (generalized true)
        (body
         (Tuple ((Var (Bound (Local x/14)) : 'a) (Lit (Int 1) : int)) :
          "'a * int"))
        (in_
         (Quote
          (Tuple
           ((App (Var (Persistent (Local k/15)) : "bool -> bool * int")
             (Lit (Bool true) : bool) : "bool * int")
            (App (Var (Persistent (Local k/15)) : "int -> int * int")
             (Lit (Int 2) : int) : "int * int"))
           : "(bool * int) * (int * int)")
          : "<(bool * int) * (int * int)>{}"))
        : "<(bool * int) * (int * int)>{}"))
      (expansion
       (Tuple
        ((App (Var (Persistent (Local k/16)) : "bool -> bool * int")
          (Lit (Bool true) : bool) : "bool * int")
         (App (Var (Persistent (Local k/16)) : "int -> int * int")
          (Lit (Int 2) : int) : "int * int"))
        : "(bool * int) * (int * int)"))
      : "(bool * int) * (int * int)"))
    -- evaluated at compile time
    -- evaluated
    (embeds (Tuple (Tuple true 1) (Tuple 2 1)))
    |}]
;;

let%expect_test
    "closures made at compile time read the toplevels they capture only when \
     called"
  =
  let abc = Decl.variant "abc" [ "A", []; "B", [] ] in
  run
    [ abc
    ; top "fails" [] (match_ (construct "B") [ Pat.construct "A", int 0 ])
    ; top "n" [] (int 3)
    ; top
        "embeds"
        []
        (splice
           (let_
              "never_called"
              [ "x" ]
              (seq (var "x") (var "fails"))
              (let_
                 "k"
                 [ "x" ]
                 (tuple [ var "x"; var "n" ])
                 (quote (app (var "k") [ bool true ])))))
    ]
    ~eval:false;
  run
    [ top "n" [] (int 3)
    ; top
        "embeds"
        []
        (splice
           (let_
              "k"
              [ "x" ]
              (tuple [ var "x"; var "n" ])
              (quote (app (var "k") [ bool true ]))))
    ];
  [%expect
    {|
    type abc = A | B
    val fails : int
    val n : int
    val embeds : bool * int
    -- evaluated at compile time
    val n : int
    val embeds : bool * int
    -- evaluated at compile time
    -- evaluated
    (n 3)
    (embeds (Tuple true 3))
    |}]
;;

let%expect_test "spliced code using the toplevel being checked" =
  run
    [ top "f" [ "x" ] (splice (var "code"))
    ; top "code" [] (quote (app (var "f") [ var "x" ]))
    ];
  [%expect {| (Compile_time_use_of_toplevel_being_checked code) |}]
;;

let%expect_test "toplevel splices are not generalized" =
  run
    [ top "id_code" [] (quote (let_ "f" [ "x" ] (var "x") (var "f")))
    ; top "spliced_id" [] (splice (var "id_code"))
    ; top "used" [] (app (var "spliced_id") [ int 1 ])
    ];
  [%expect
    {|
    val id_code : <'a -> 'a>{..'b}
    val spliced_id : int -> int
    val used : int
    -- evaluated at compile time
    (id_code
     (Code
      (Let (ident f/17) (args (x/16)) (generalized true)
       (body (Var (Bound (Local x/16)) : 'a))
       (in_ (Var (Bound (Local f/17)) : "'b -> 'b")) : "'b -> 'b")))
    -- evaluated
    (id_code
     (Code
      (Let (ident f/17) (args (x/16)) (generalized true)
       (body (Var (Bound (Local x/16)) : 'a))
       (in_ (Var (Bound (Local f/17)) : "'b -> 'b")) : "'b -> 'b")))
    (spliced_id <fun>)
    (used 1)
    |}]
;;

let%expect_test "toplevels used at level -2" =
  run
    ~bodies:[ "deep" ]
    [ top "n" [] (int 2)
    ; top "deep" [] (splice (splice (seq (var "n") (quote (quote (var "n"))))))
    ];
  [%expect
    {|
    val n : int
    val deep : int
    (deep/5
     (Toplevel_splice
      (code
       (Toplevel_splice
        (code
         (Let (ident _/15) (args ()) (generalized true)
          (body (Var (Bound (Toplevel n/4)) : int))
          (in_
           (Quote (Quote (Var (Free n) : int) : "<int>{n : int}") :
            "<<int>{n : int}>{}"))
          : "<<int>{n : int}>{}"))
        (expansion (Quote (Var (Free n) : int) : "<int>{n : int}")) :
        "<int>{n : int}"))
      (expansion (Var (Bound (Toplevel n/4)) : int)) : int))
    -- evaluated at compile time
    (n 2)
    -- evaluated
    (n 2)
    (deep 2)
    |}]
;;
