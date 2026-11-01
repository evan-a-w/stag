open! Import
open Source_helpers

let%expect_test "check" =
  check
    {|
let a = `A 1
let f x = add x 1
|};
  check
    ~prelude:false
    {|
let add = 1
|};
  check
    {|
let x = add true
|};
  check
    {|
let x =
|};
  [%expect
    {|
    val a : [`A of int; ..'a]
    val f : int -> int
    val add : int
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:12-16)
    t.stag:3:0-0: expected an expression, found end of file
    |}]
;;

let%expect_test "run" =
  run
    ~bodies:[ "y" ]
    {|
let x = .~(print_endline "compiling"; .<1>.)
let y = .~(.<x>.)
let z = print_endline "running"; x
|};
  run
    ~eval:false
    {|
let x = 1
|};
  [%expect
    {|
    compiling
    val x : int
    val y : int
    val z : int
    (y/5
     (Toplevel_splice
      (code (Quote (Var (Free_toplevel x) : int) : "<int>{x : immut int}"))
      (expansion (Var (Bound (Toplevel x/4)) : int)) : int))
    -- evaluated at compile time
    -- evaluated
    running
    (x 1)
    (y 1)
    (z 1)
    val x : int
    -- evaluated at compile time
    |}]
;;

let%expect_test "runtime errors" =
  run
    {|
type ab = A | B
let a = 1
let b = match B with A -> 0
|};
  [%expect
    {|
    type ab = A | B
    val a : int
    val b : int
    -- evaluated at compile time
    -- evaluated
    (a 1)
    ("Match failure: no case matches the value B" @ t.stag:4:8-27)
    |}]
;;

let%expect_test "files" =
  Option.iter
    (check_files
       [ ( "a.stag"
         , {|
let x = B.y
|}
         )
       ; ( "b.stag"
         , {|
let y = 1
|}
         )
       ])
    ~f:(fun t ->
      print_signature t;
      print_bodies t;
      eval_and_print_values t);
  ignore (check_files [ "a.stag", "let x = B.z" ] : Source_helpers.t option);
  [%expect
    {|
    module A : sig
      val x : int
    end
    module B : sig
      val y : int
    end
    (B.y/1 (Lit (Int 1) : int))
    (A.x/0 (Var (Bound (Toplevel B.y/1)) : int))
    (B.y 1)
    (A.x 1)
    (Typecheck ((Unbound_module B) @ a.stag:1:8-9))
    |}]
;;

let%expect_test "print_typed is as test_typed's print" =
  Option.iter
    (typecheck
       {|
let f x =
  let x = 1 in
  match (x, x) with x -> let g y = y in g x
|})
    ~f:(print_typed ~idents:true);
  [%expect
    {|
    ((members
      (((ident f/4) (args (x/14)) (scheme "'a -> int * int")
        (body
         (Let (ident x/15) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Match
            (scrutinee
             (Tuple
              ((Var (Bound (Local x/15)) : int) (Var (Bound (Local x/15)) : int))
              : "int * int"))
            (cases
             (((Var x/16 : "int * int")
               (Let (ident g/18) (args (y/17)) (generalized true)
                (body (Var (Bound (Local y/17)) : 'b))
                (in_
                 (App (Var (Bound (Local g/18)) : "int * int -> int * int")
                  (Var (Bound (Local x/16)) : "int * int") : "int * int"))
                : "int * int"))))
            : "int * int"))
          : "int * int")))))
     (recursive false))
    ((f/4 ("'a -> int * int" 0)) (g/18 ("'a -> 'a" 0)) (x/14 ('a 0))
     (x/15 (int 0)) (x/16 ("int * int" 0)) (y/17 ('a 0)))
    |}]
;;

let%expect_test "program" =
  let typecheck = typechecker () in
  List.iter [ "let n = add 1 2"; "let n = id true" ] ~f:(fun source ->
    match Typecheck.typecheck typecheck ~program:(program source) with
    | Ok { signature; _ } -> print_string (Signature.to_string_hum signature)
    | Error error ->
      List.iter (error : Typecheck.Failed.t).errors ~f:(fun error ->
        print_s [%sexp (error : Typecheck_error.t)]));
  print_s
    [%sexp (List.length (program ~prelude:false "let n = 1 let m = 2") : int)];
  [%expect
    {|
    val add : int -> int -> int
    val eq : int -> int -> bool
    val cond : bool -> 'a -> 'a -> 'a
    val id : 'a -> 'a
    val n : int
    val add : int -> int -> int
    val eq : int -> int -> bool
    val cond : bool -> 'a -> 'a -> 'a
    val id : 'a -> 'a
    val n : bool
    2
    |}]
;;

let%expect_test "toplevels named like the prelude's" =
  run
    ~bodies:[ "M.add" ]
    {|
module M = struct let add x = x end
let id2 = M.add 1
|};
  run
    ~prelude:false
    ~bodies:[ "add" ]
    {|
let add x = x
|};
  check
    {|
let add x = x
|};
  [%expect
    {|
    module M : sig
      val add : 'a -> 'a
    end
    val id2 : int
    (M.add/4 (Var (Bound (Local x/15)) : 'a))
    -- evaluated at compile time
    -- evaluated
    (M.add <fun>)
    (id2 1)
    val add : 'a -> 'a
    (add/0 (Var (Bound (Local x/1)) : 'a))
    -- evaluated at compile time
    -- evaluated
    (add <fun>)
    ((Duplicate_toplevel add) @ t.stag:2:4-7)
    |}]
;;
