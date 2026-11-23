open! Import
open Syntax

let file = "t.stag"

let show sexp_of_t round_trip = function
  | Ok t ->
    print_s (Round_trip.strip_locs (sexp_of_t t));
    round_trip t
  | Error error -> print_endline (Parser.Error.to_string error)
;;

let expr source = show Expr.sexp_of_t Round_trip.expr (Parser.expr ~file source)

let ty source =
  show Type_expr.sexp_of_t Round_trip.type_expr (Parser.type_expr ~file source)
;;

let prog source =
  show Program.sexp_of_t Round_trip.program (Parser.program ~file source)
;;

let print ?(width = 80) source =
  match Parser.program ~file source with
  | Error error -> print_endline (Parser.Error.to_string error)
  | Ok program ->
    Round_trip.program program;
    print_string (Printer.program ~width program)
;;

let%expect_test "mode expressions" =
  prog "let x @ local = 1";
  prog "let x @ 'm = 1";
  prog "let x @ local portable gc = 1";
  prog "let x : int @ const = 1";
  prog "let x @ local local = 1";
  prog "let x @ regional global = 1";
  prog "let x @ bogus = 1";
  prog "let x @ = 1";
  [%expect
    {|
    ((Let x () ((mode (Atoms (Local)))) (Lit (Int 1))))
    ((Let x () ((mode (Var m))) (Lit (Int 1))))
    ((Let x () ((mode (Atoms (Local Portable Gc)))) (Lit (Int 1))))
    ((Let x () ((ty (Con int ())) (mode (Atoms (Const)))) (Lit (Int 1))))
    t.stag:1:14-19: `local` and `local` are both locality modes
    t.stag:1:17-23: `regional` and `global` are both locality modes
    t.stag:1:8-13: unknown mode `bogus`, expected one of `const`, `portable`, `nonportable`, `static`, `gc`, `external`, `global`, `regional`, `local`
    t.stag:1:8-9: expected a mode like `local`, or a mode variable like `'m`, found `=`
    |}]
;;

let%expect_test "annotated parameters, results and variables" =
  prog "let f (x @ local) (y : int @ 'm) : int @ global = x";
  expr "fun (x @ local) -> x";
  expr "(x : int @ local)";
  expr "(x @ local)";
  prog "let mut x @ local = 1";
  prog "let f (x @ local, y) = x";
  [%expect
    {|
    ((Let f ((x ((mode (Atoms (Local))))) (y ((ty (Con int ())) (mode (Var m)))))
      ((ty (Con int ())) (mode (Atoms (Global)))) (Var x ())))
    (Fun ((x ((mode (Atoms (Local)))))) (Var x ()))
    (Var x ((ty (Con int ())) (mode (Atoms (Local)))))
    (Var x ((mode (Atoms (Local)))))
    ((Let_mut x ((mode (Atoms (Local)))) (Lit (Int 1))))
    t.stag:1:16-17: expected `)` to close this `(` (opened at 1:6), found `,`
    |}]
;;

let%expect_test "moded arrow types" =
  ty "int @ local -> int @ local";
  ty "'a @ 'm -> 'b @ 'n";
  ty "int * int @ local -> int";
  ty "(int @ local -> int) @ local -> int";
  ty "int @ local -> int -> int @ global";
  ty "list[int @ local]";
  ty "int @ local";
  ty "(int @ local) -> int";
  ty "int @ local @ global -> int";
  [%expect
    {|
    (Arrow (Moded (Con int ()) (Atoms (Local)))
     (Moded (Con int ()) (Atoms (Local))))
    (Arrow (Moded (Var a) (Var m)) (Moded (Var b) (Var n)))
    (Arrow (Moded (Tuple ((Con int ()) (Con int ()))) (Atoms (Local)))
     (Con int ()))
    (Arrow
     (Moded (Arrow (Moded (Con int ()) (Atoms (Local))) (Con int ()))
      (Atoms (Local)))
     (Con int ()))
    (Arrow (Moded (Con int ()) (Atoms (Local)))
     (Arrow (Con int ()) (Moded (Con int ()) (Atoms (Global)))))
    t.stag:1:11-16: a mode is only written on a function type's argument or result, as in `t @ local -> u`, or on an annotation, as in `(x : t @ local)`
    t.stag:1:6-11: a mode is only written on a function type's argument or result, as in `t @ local -> u`, or on an annotation, as in `(x : t @ local)`
    t.stag:1:7-12: a mode is only written on a function type's argument or result, as in `t @ local -> u`, or on an annotation, as in `(x : t @ local)`
    t.stag:1:12-13: a type has one mode: write its modes together, as in `t @ local portable`
    |}]
;;

let%expect_test "stack_, exclave_ and [@nontail]" =
  expr "stack_ (1, 2)";
  expr "exclave_ f x";
  expr "let x = stack_ { a = 1 } in exclave_ (x, x)";
  expr "stack_ let x = 1 in x";
  expr "f (stack_ x) y";
  expr "f x [@nontail]";
  expr "(f x y [@nontail], g x [@nontail])";
  expr "x [@nontail]";
  expr "f x [@other]";
  expr "Some (f x) [@nontail]";
  expr "A f x [@nontail]";
  expr "`A (f x) [@nontail]";
  expr "Some (f x [@nontail])";
  expr "A [@nontail]";
  expr "`A [@nontail]";
  expr "g (Some (f x)) [@nontail]";
  [%expect
    {|
    (Stack (Tuple ((Lit (Int 1)) (Lit (Int 2)))))
    (Exclave (App (Var f ()) (Var x ())))
    (Let x () () (Stack (Record ((a (Lit (Int 1))))))
     (Exclave (Tuple ((Var x ()) (Var x ())))))
    (Stack (Let x () () (Lit (Int 1)) (Var x ())))
    (App (App (Var f ()) (Stack (Var x ()))) (Var y ()))
    (Nontail (App (Var f ()) (Var x ())))
    (Tuple
     ((Nontail (App (App (Var f ()) (Var x ())) (Var y ())))
      (Nontail (App (Var g ()) (Var x ())))))
    t.stag:1:2-12: `[@nontail]` is only written after an application, as in `f x [@nontail]`
    t.stag:1:6-11: unknown attribute `[@other]`, only `[@nontail]` exists
    t.stag:1:11-21: `[@nontail]` can't follow a constructor's argument: parenthesize the application, as in `C (f x [@nontail])`
    t.stag:1:6-16: `[@nontail]` can't follow a constructor's argument: parenthesize the application, as in `C (f x [@nontail])`
    t.stag:1:9-19: `[@nontail]` can't follow a constructor's argument: parenthesize the application, as in `C (f x [@nontail])`
    (Construct Some ((Nontail (App (Var f ()) (Var x ())))))
    t.stag:1:2-12: `[@nontail]` is only written after an application, as in `f x [@nontail]`
    t.stag:1:3-13: `[@nontail]` is only written after an application, as in `f x [@nontail]`
    (Nontail (App (Var g ()) (Construct Some ((App (Var f ()) (Var x ()))))))
    |}]
;;

let%expect_test "global_ fields and constructor arguments" =
  prog "type r = { x : global_ int; y : list[int] }";
  prog "type t = A of global_ int * int | B of global_ (int -> int)";
  prog "exception E of global_ int";
  prog "exception E of int @ local";
  prog "type t = A of int @ local";
  prog "type t = A of global_ int @ local";
  ty "global_ int";
  [%expect
    {|
    ((Type
      ((name r) (params ())
       (kind
        (Record ((x (Global (Con int ()))) (y (Con list ((Ty (Con int ())))))))))))
    ((Type
      ((name t) (params ())
       (kind
        (Variant
         ((A ((Global (Con int ())) (Con int ())))
          (B ((Global (Arrow (Con int ()) (Con int ())))))))))))
    t.stag:1:15-22: an exception's arguments are always global: remove `global_`
    t.stag:1:19-20: an exception's arguments have no mode, they are always global
    t.stag:1:18-19: a constructor's argument has no mode: to make it always global, write `global_ t`
    t.stag:1:26-27: a constructor's argument has no mode: to make it always global, write `global_ t`
    t.stag:1:0-7: expected a type, found `global_`
    |}]
;;

let%expect_test "printing" =
  print
    {|
let f (x @ local) (y : int @ 'm) : int @ global = exclave_ (x, stack_ (y, y))
let g (h : int @ local -> int @ local) = h 1 [@nontail]
type r = { x : global_ int; y : int }
let mut counter : int @ static = 0
|};
  [%expect
    {|
    let f (x @ local) (y : int @ 'm) : int @ global = exclave_ (x, stack_ (y, y))
    let g (h : int @ local -> int @ local) = h 1 [@nontail]

    type r = {x : global_ int; y : int}

    let mut counter : int @ static = 0
    |}];
  print
    ~width:30
    {|
let f (x @ local) = exclave_ let y = stack_ (x, x) in some_function y [@nontail]
|};
  [%expect
    {|
    let f (x @ local) =
      exclave_
        let y = stack_ (x, x) in
        some_function y [@nontail]
    |}]
;;

let%expect_test "mode constraints" =
  prog
    "let compose : ('b @ 'n -> 'c) -> ('a -> 'b @ 'm) -> 'a -> 'c with 'm <= \
     'n = f";
  prog "let f : 'a @ 'm -> 'a @ 'n with 'm <= 'n and 'n <= portable = f";
  prog "let f : 'a @ 'm -> 'a with portable <= 'm and 'm <= local portable = f";
  expr "(x : int @ 'm with 'm <= portable)";
  expr "fun (x : int @ 'm with 'm <= 'n) -> x";
  prog "let f (x : int @ 'm with const <= 'm) : int @ 'n with 'm <= 'n = x";
  (* [with] closes an annotation, so needs no parentheses in [match]. *)
  expr "match (x : int @ 'm with 'm <= portable) with y -> y";
  [%expect
    {|
    ((Let compose ()
      ((ty
        (Constrained
         (Arrow (Arrow (Moded (Var b) (Var n)) (Var c))
          (Arrow (Arrow (Var a) (Moded (Var b) (Var m))) (Arrow (Var a) (Var c))))
         (((lower (Var m)) (upper (Var n)))))))
      (Var f ())))
    ((Let f ()
      ((ty
        (Constrained (Arrow (Moded (Var a) (Var m)) (Moded (Var a) (Var n)))
         (((lower (Var m)) (upper (Var n)))
          ((lower (Var n)) (upper (Atoms (Portable))))))))
      (Var f ())))
    ((Let f ()
      ((ty
        (Constrained (Arrow (Moded (Var a) (Var m)) (Var a))
         (((lower (Atoms (Portable))) (upper (Var m)))
          ((lower (Var m)) (upper (Atoms (Local Portable))))))))
      (Var f ())))
    (Var x
     ((ty
       (Constrained (Con int ()) (((lower (Var m)) (upper (Atoms (Portable)))))))
      (mode (Var m))))
    (Fun
     ((x
       ((ty (Constrained (Con int ()) (((lower (Var m)) (upper (Var n))))))
        (mode (Var m)))))
     (Var x ()))
    ((Let f
      ((x
        ((ty
          (Constrained (Con int ()) (((lower (Atoms (Const))) (upper (Var m))))))
         (mode (Var m)))))
      ((ty (Constrained (Con int ()) (((lower (Var m)) (upper (Var n))))))
       (mode (Var n)))
      (Var x ())))
    (Match
     (Var x
      ((ty
        (Constrained (Con int ()) (((lower (Var m)) (upper (Atoms (Portable)))))))
       (mode (Var m))))
     (((Var y) (Var y ()))))
    |}]
;;

let%expect_test "mode constraint errors" =
  prog "let f : int with portable <= const = f";
  prog "let f : int with 'm = f";
  prog "let f : int with 'm < 'n = f";
  prog "let f : int with 'm <= 'n and = f";
  prog "let f (x @ 'm with 'm <= portable) = x";
  ty "int with 'm <= 'n";
  ty "list[int with 'm <= 'n]";
  [%expect
    {|
    t.stag:1:17-34: a mode constraint relates a mode variable, as in `'m <= 'n` or `'m <= portable`
    t.stag:1:20-21: expected `<=`, found `=`
    t.stag:1:20-21: expected `<=`, found `<`
    t.stag:1:30-31: expected a mode like `local`, or a mode variable like `'m`, found `=`
    t.stag:1:14-18: mode constraints follow an annotation's type, as in `(x : t @ 'm with 'm <= portable)`
    t.stag:1:4-8: expected end of file, found `with`
    t.stag:1:9-13: expected `]` to close this `[` (opened at 1:4), found `with`
    |}]
;;

let%expect_test "printing mode constraints" =
  print
    {|
let compose : ('b @ 'n -> 'c) -> ('a -> 'b @ 'm) -> 'a -> 'c with 'm <= 'n and portable <= 'm = fun f g x -> f (g x)
let f (x : int @ 'm with 'm <= portable) = x
|};
  print
    ~width:40
    {|
let compose : ('b @ 'n -> 'c) -> ('a -> 'b @ 'm) -> 'a -> 'c with 'm <= 'n and portable <= 'm = fun f g x -> f (g x)
|};
  [%expect
    {|
    let compose : ('b @ 'n -> 'c) -> ('a -> 'b @ 'm) -> 'a -> 'c
          with 'm <= 'n and portable <= 'm =
      fun f g x -> f (g x)

    let f (x : int @ 'm with 'm <= portable) = x
    let compose : ('b @ 'n -> 'c) ->
        ('a -> 'b @ 'm) ->
        'a ->
        'c
          with 'm <= 'n and portable <= 'm =
      fun f g x -> f (g x)
    |}]
;;
