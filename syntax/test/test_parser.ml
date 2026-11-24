open! Import
open Syntax

let file = "t.stag"
let strip_locs = Round_trip.strip_locs

(* Successfully parsed syntax is also checked to round trip through [Printer]. *)
let show sexp_of_t round_trip = function
  | Ok t ->
    print_s (strip_locs (sexp_of_t t));
    round_trip t
  | Error error -> print_endline (Parser.Error.to_string error)
;;

let expr source = show Expr.sexp_of_t Round_trip.expr (Parser.expr ~file source)

let pat source =
  show Pattern.sexp_of_t Round_trip.pattern (Parser.pattern ~file source)
;;

let ty source =
  show Type_expr.sexp_of_t Round_trip.type_expr (Parser.type_expr ~file source)
;;

let prog source =
  show Program.sexp_of_t Round_trip.program (Parser.program ~file source)
;;

(* The source a [Loc.to_string] spans. *)
let slice source loc =
  let line_starts =
    0
    :: List.filter_mapi (String.to_list source) ~f:(fun i c ->
      if Char.equal c '\n' then Some (i + 1) else None)
    |> Array.of_list
  in
  let offset line col =
    line_starts.(Int.of_string line - 1) + Int.of_string col
  in
  let start, stop =
    match String.split loc ~on:':' with
    | [ _; line; cols ] ->
      let start, stop = String.lsplit2_exn cols ~on:'-' in
      offset line start, offset line stop
    | [ _; line; start_col_stop_line; stop_col ] ->
      let start_col, stop_line =
        String.lsplit2_exn start_col_stop_line ~on:'-'
      in
      offset line start_col, offset stop_line stop_col
    | _ -> raise_s [%message "bad loc" loc]
  in
  String.sub source ~pos:start ~len:(stop - start)
;;

(* Every located node, indented by depth, with the source it spans. *)
let show_locs sexp_of_t parse source =
  match parse ~file source with
  | Error error -> print_endline (Parser.Error.to_string error)
  | Ok t ->
    let rec go depth : Sexp.t -> unit = function
      | List [ txt; Atom "@"; Atom loc ] ->
        let head =
          match strip_locs txt with
          | Atom atom | List (Atom atom :: _) -> atom
          | List _ -> "_"
        in
        printf "%s%s %S\n" (String.make (2 * depth) ' ') head (slice source loc);
        go (depth + 1) txt
      | List [ txt; Atom "@"; List _ ] -> go depth txt
      | List sexps -> List.iter sexps ~f:(go depth)
      | Atom _ -> ()
    in
    go 0 (sexp_of_t t)
;;

let%expect_test "literals and variables" =
  expr "1";
  expr "4611686018427387903";
  expr "true";
  expr "false";
  expr "'c'";
  expr {|"s\n"|};
  expr "()";
  expr "x";
  expr "M.N.x";
  expr "(x)";
  expr "(x : int)";
  expr "(M.x : 'a -> 'a)";
  [%expect
    {|
    (Lit (Int 1))
    (Lit (Int 4611686018427387903))
    (Lit (Bool true))
    (Lit (Bool false))
    (Lit (Char c))
    (Lit (String "s\n"))
    (Lit Unit)
    (Var x ())
    (Var M.N.x ())
    (Var x ())
    (Var x ((ty (Con int ()))))
    (Var M.x ((ty (Arrow (Var a) (Var a)))))
    |}]
;;

let%expect_test "negative literals" =
  expr "-1";
  expr "-4611686018427387904";
  expr "f -1";
  expr "f -1 -2_000";
  expr "Some -1";
  expr "`A -1";
  expr "(-1, -2)";
  expr "match x with -1 -> 0 | 0 -> -1 | Some -2 -> 1 | (-3, _) -> 2";
  expr "-x";
  expr "--1";
  expr "f - 1";
  pat "-1";
  pat "Some -1";
  pat "-";
  [%expect
    {|
    (Lit (Int -1))
    (Lit (Int -4611686018427387904))
    (App (Var f ()) (Lit (Int -1)))
    (App (App (Var f ()) (Lit (Int -1))) (Lit (Int -2000)))
    (Construct Some ((Lit (Int -1))))
    (Poly_variant A ((Lit (Int -1))))
    (Tuple ((Lit (Int -1)) (Lit (Int -2))))
    (Match (Var x ())
     (((Lit (Int -1)) (Lit (Int 0))) ((Lit (Int 0)) (Lit (Int -1)))
      ((Construct Some ((Lit (Int -2)))) (Lit (Int 1)))
      ((Tuple ((Lit (Int -3)) Any)) (Lit (Int 2)))))
    t.stag:1:0-1: illegal character '-'
    t.stag:1:0-1: illegal character '-'
    t.stag:1:2-3: illegal character '-'
    (Lit (Int -1))
    (Construct Some ((Lit (Int -1))))
    t.stag:1:0-1: illegal character '-'
    |}]
;;

let%expect_test "application, constructors and variants" =
  expr "f x y";
  expr "f (g x) y";
  expr "C";
  expr "M.C";
  expr "C x";
  expr "C (a, b)";
  expr "C ((a, b))";
  expr "C ()";
  expr "C f x";
  expr "S S Z";
  expr "f C x";
  expr "`A";
  expr "`A x";
  expr "`A (a, b)";
  expr "f `A x";
  expr "`A f x";
  [%expect
    {|
    (App (App (Var f ()) (Var x ())) (Var y ()))
    (App (App (Var f ()) (App (Var g ()) (Var x ()))) (Var y ()))
    (Construct C ())
    (Construct M.C ())
    (Construct C ((Var x ())))
    (Construct C ((Var a ()) (Var b ())))
    (Construct C ((Tuple ((Var a ()) (Var b ())))))
    (Construct C ((Lit Unit)))
    (Construct C ((App (Var f ()) (Var x ()))))
    (Construct S ((Construct S ((Construct Z ())))))
    (App (App (Var f ()) (Construct C ())) (Var x ()))
    (Poly_variant A ())
    (Poly_variant A ((Var x ())))
    (Poly_variant A ((Tuple ((Var a ()) (Var b ())))))
    (App (App (Var f ()) (Poly_variant A ())) (Var x ()))
    (Poly_variant A ((App (Var f ()) (Var x ()))))
    |}]
;;

let%expect_test "a constructor's arguments, or its one tuple argument" =
  List.iter
    ~f:expr
    [ "C (a, b)"
    ; "C ((a, b))"
    ; "C (((a, b)))"
    ; "C ((a), b)"
    ; "C ((a, b), c)"
    ; "C (a, (b, c))"
    ; "C ((a, b)).x"
    ; "C (a, b) x"
    ; "C (let x = 1 in a, b)"
    ; "C ((let x = 1 in a, b))"
    ; "C (a; b, c)"
    ; "C D (a, b)"
    ; "C (D (a, b))"
    ; "C (D ((a, b)))"
    ];
  List.iter
    ~f:pat
    [ "C (a, b)"; "C ((a, b))"; "C ((a), b)"; "C (a, (b, c))"; "C D ((a, b))" ];
  [%expect
    {|
    (Construct C ((Var a ()) (Var b ())))
    (Construct C ((Tuple ((Var a ()) (Var b ())))))
    (Construct C ((Tuple ((Var a ()) (Var b ())))))
    (Construct C ((Var a ()) (Var b ())))
    (Construct C ((Tuple ((Var a ()) (Var b ()))) (Var c ())))
    (Construct C ((Var a ()) (Tuple ((Var b ()) (Var c ())))))
    (Construct C ((Field (Tuple ((Var a ()) (Var b ()))) x)))
    (Construct C ((App (Tuple ((Var a ()) (Var b ()))) (Var x ()))))
    (Construct C ((Let x () () (Lit (Int 1)) (Tuple ((Var a ()) (Var b ()))))))
    (Construct C ((Let x () () (Lit (Int 1)) (Tuple ((Var a ()) (Var b ()))))))
    (Construct C ((Seq (Var a ()) (Tuple ((Var b ()) (Var c ()))))))
    (Construct C ((Construct D ((Var a ()) (Var b ())))))
    (Construct C ((Construct D ((Var a ()) (Var b ())))))
    (Construct C ((Construct D ((Tuple ((Var a ()) (Var b ())))))))
    (Construct C ((Var a) (Var b)))
    (Construct C ((Tuple ((Var a) (Var b)))))
    (Construct C ((Var a) (Var b)))
    (Construct C ((Var a) (Tuple ((Var b) (Var c)))))
    (Construct C ((Construct D ((Tuple ((Var a) (Var b)))))))
    |}]
;;

let%expect_test "tuples, sequences, let and match" =
  expr "a, b, c";
  expr "(a, b), c";
  expr "f a, C b, `A c";
  expr "a; b; c";
  expr "a, b; c";
  expr "let x = 1 in x";
  expr "let f x (y : int) _ (_ : bool) : t = e in f";
  expr "let _ = a in b";
  expr "let x = 1 in a; b";
  expr "a; let x = 1 in b; c";
  expr "let x = let y = 1 in y in x";
  expr "a, let x = 1 in x, y";
  expr "match x with A -> 1 | B y -> 2";
  expr "match x with | A -> a; b | B -> c";
  expr "match x with A -> (match y with B -> 1) | C -> 2";
  expr "match x with A -> match y with B -> 1 | C -> 2";
  expr "match a, b with x, y -> x";
  expr "f (match x with _ -> 1) y";
  [%expect
    {|
    (Tuple ((Var a ()) (Var b ()) (Var c ())))
    (Tuple ((Tuple ((Var a ()) (Var b ()))) (Var c ())))
    (Tuple
     ((App (Var f ()) (Var a ())) (Construct C ((Var b ())))
      (Poly_variant A ((Var c ())))))
    (Seq (Var a ()) (Seq (Var b ()) (Var c ())))
    (Seq (Tuple ((Var a ()) (Var b ()))) (Var c ()))
    (Let x () () (Lit (Int 1)) (Var x ()))
    (Let f ((x ()) (y ((ty (Con int ())))) (_ ()) (_ ((ty (Con bool ())))))
     ((ty (Con t ()))) (Var e ()) (Var f ()))
    (Let _ () () (Var a ()) (Var b ()))
    (Let x () () (Lit (Int 1)) (Seq (Var a ()) (Var b ())))
    (Seq (Var a ()) (Let x () () (Lit (Int 1)) (Seq (Var b ()) (Var c ()))))
    (Let x () () (Let y () () (Lit (Int 1)) (Var y ())) (Var x ()))
    (Tuple
     ((Var a ()) (Let x () () (Lit (Int 1)) (Tuple ((Var x ()) (Var y ()))))))
    (Match (Var x ())
     (((Construct A ()) (Lit (Int 1))) ((Construct B ((Var y))) (Lit (Int 2)))))
    (Match (Var x ())
     (((Construct A ()) (Seq (Var a ()) (Var b ())))
      ((Construct B ()) (Var c ()))))
    (Match (Var x ())
     (((Construct A ()) (Match (Var y ()) (((Construct B ()) (Lit (Int 1))))))
      ((Construct C ()) (Lit (Int 2)))))
    (Match (Var x ())
     (((Construct A ())
       (Match (Var y ())
        (((Construct B ()) (Lit (Int 1))) ((Construct C ()) (Lit (Int 2))))))))
    (Match (Tuple ((Var a ()) (Var b ())))
     (((Tuple ((Var x) (Var y))) (Var x ()))))
    (App (App (Var f ()) (Match (Var x ()) ((Any (Lit (Int 1)))))) (Var y ()))
    |}]
;;

let%expect_test "anonymous functions" =
  expr "fun x -> x";
  expr "fun x (y : int) _ (_ : bool) -> f x y";
  expr "fun x -> a; b";
  expr "fun x -> fun y -> x, y";
  expr "f (fun x -> x) y";
  expr "a, fun x -> x, y";
  expr "match x with A -> fun y -> y | B -> b";
  expr "{x = fun y -> y; z = 1}";
  expr "fun -> x";
  expr "fun x";
  expr "fun 1 -> x";
  expr "fun (x, y) -> x";
  expr "fun () -> x";
  expr "fun (x) -> x";
  expr "fun x (A y) -> x";
  expr "fun {x = y} -> y";
  expr "fun in";
  expr "f fun x -> x";
  [%expect
    {|
    (Fun ((x ())) (Var x ()))
    (Fun ((x ()) (y ((ty (Con int ())))) (_ ()) (_ ((ty (Con bool ())))))
     (App (App (Var f ()) (Var x ())) (Var y ())))
    (Fun ((x ())) (Seq (Var a ()) (Var b ())))
    (Fun ((x ())) (Fun ((y ())) (Tuple ((Var x ()) (Var y ())))))
    (App (App (Var f ()) (Fun ((x ())) (Var x ()))) (Var y ()))
    (Tuple ((Var a ()) (Fun ((x ())) (Tuple ((Var x ()) (Var y ()))))))
    (Match (Var x ())
     (((Construct A ()) (Fun ((y ())) (Var y ()))) ((Construct B ()) (Var b ()))))
    (Record ((x (Fun ((y ())) (Var y ()))) (z (Lit (Int 1)))))
    t.stag:1:4-6: expected a parameter, found `->`
    t.stag:1:5-5: expected `->`, found end of file
    t.stag:1:4-5: patterns are not allowed as parameters, only `x`, `_`, `(x : t)` or `(x @ m)`; use `match`
    t.stag:1:4-5: patterns are not allowed as parameters, only `x`, `_`, `(x : t)` or `(x @ m)`; use `match`
    t.stag:1:4-5: patterns are not allowed as parameters, only `x`, `_`, `(x : t)` or `(x @ m)`; use `match`
    t.stag:1:4-5: patterns are not allowed as parameters, only `x`, `_`, `(x : t)` or `(x @ m)`; use `match`
    t.stag:1:6-7: patterns are not allowed as parameters, only `x`, `_`, `(x : t)` or `(x @ m)`; use `match`
    t.stag:1:4-5: patterns are not allowed as parameters, only `x`, `_`, `(x : t)` or `(x @ m)`; use `match`
    t.stag:1:4-6: expected a parameter, found `in`
    t.stag:1:2-5: expected end of file, found `fun`
    |}]
;;

let%expect_test "records" =
  expr "{x = 1; M.y = 2}";
  expr "{x = 1, 2;}";
  expr "r.x";
  expr "r.M.x";
  expr "r.x.y";
  expr "f r.x";
  expr "M.r.x";
  expr "{}";
  expr "{`x = 1; `y = true}";
  expr "{`x = 1;}";
  expr "r.`x";
  expr "r.`x.`y";
  expr "{`x = v; `y = w | r}";
  expr "{`x = v | f r}";
  expr "r \\ `x";
  expr "r \\ `x \\ `y";
  expr "f r \\ `x";
  expr "r.`a \\ `b";
  expr "{r with `x = v}";
  expr "{r with `x = v; `y = w;}";
  expr "{r.`a with `x = v}";
  expr "{r with x = v}";
  expr "{r.M.a with M.x = v; y = w;}";
  [%expect
    {|
    (Record ((x (Lit (Int 1))) (M.y (Lit (Int 2)))))
    (Record ((x (Tuple ((Lit (Int 1)) (Lit (Int 2)))))))
    (Field (Var r ()) x)
    (Field (Var r ()) M.x)
    (Field (Field (Var r ()) x) y)
    (App (Var f ()) (Field (Var r ()) x))
    (Field (Var M.r ()) x)
    (Poly_record ())
    (Poly_record ((x (Lit (Int 1))) (y (Lit (Bool true)))))
    (Poly_record ((x (Lit (Int 1)))))
    (Poly_field (Var r ()) x)
    (Poly_field (Poly_field (Var r ()) x) y)
    (Poly_extend ((x (Var v ())) (y (Var w ()))) (Var r ()))
    (Poly_extend ((x (Var v ()))) (App (Var f ()) (Var r ())))
    (Poly_restrict (Var r ()) x)
    (Poly_restrict (Poly_restrict (Var r ()) x) y)
    (App (Var f ()) (Poly_restrict (Var r ()) x))
    (Poly_restrict (Poly_field (Var r ()) a) b)
    (Poly_update (Var r ()) ((x (Var v ()))))
    (Poly_update (Var r ()) ((x (Var v ())) (y (Var w ()))))
    (Poly_update (Poly_field (Var r ()) a) ((x (Var v ()))))
    (Record_update (Var r ()) ((x (Var v ()))))
    (Record_update (Field (Var r ()) M.a) ((M.x (Var v ())) (y (Var w ()))))
    |}]
;;

let%expect_test "staging" =
  expr ".< x >.";
  expr ".< f .~x >.";
  expr ".~e";
  expr ".~f x";
  expr ".~(f x)";
  expr ".~r.x";
  expr ".~.~e";
  expr ".~e.{}";
  expr ".~e.{x = e1; y}";
  expr ".~e.{x = 1, 2; y;}";
  expr ".~(f x).{y}";
  expr ".~f.{y} x";
  expr ".~f {`x = 1}";
  expr ".~f{`x = 1}";
  expr ".~f .{x}";
  expr ".~.~e.{x}";
  expr ".~r.x.{x}";
  expr ".~f.{x} .~g.{y = 1}";
  expr ".~f.{x = let a = 1 in a; y}";
  expr ".~f.{x = match a with A -> 1 | B -> 2; y}";
  expr "C .~x";
  expr "`A .~x";
  [%expect
    {|
    (Quote (Var x ()))
    (Quote (App (Var f ()) (Splice (expr (Var x ())) (subs ()))))
    (Splice (expr (Var e ())) (subs ()))
    (App (Splice (expr (Var f ())) (subs ())) (Var x ()))
    (Splice (expr (App (Var f ()) (Var x ()))) (subs ()))
    (Splice (expr (Field (Var r ()) x)) (subs ()))
    (Splice (expr (Splice (expr (Var e ())) (subs ()))) (subs ()))
    (Splice (expr (Var e ())) (subs (())))
    (Splice (expr (Var e ())) (subs (((x ((Var e1 ()))) (y ())))))
    (Splice (expr (Var e ()))
     (subs (((x ((Tuple ((Lit (Int 1)) (Lit (Int 2)))))) (y ())))))
    (Splice (expr (App (Var f ()) (Var x ()))) (subs (((y ())))))
    (App (Splice (expr (Var f ())) (subs (((y ()))))) (Var x ()))
    (App (Splice (expr (Var f ())) (subs ())) (Poly_record ((x (Lit (Int 1))))))
    (App (Splice (expr (Var f ())) (subs ())) (Poly_record ((x (Lit (Int 1))))))
    (Splice (expr (Var f ())) (subs (((x ())))))
    (Splice (expr (Splice (expr (Var e ())) (subs (((x ())))))) (subs ()))
    (Splice (expr (Field (Var r ()) x)) (subs (((x ())))))
    (App (Splice (expr (Var f ())) (subs (((x ())))))
     (Splice (expr (Var g ())) (subs (((y ((Lit (Int 1)))))))))
    (Splice (expr (Var f ()))
     (subs (((x ((Let a () () (Lit (Int 1)) (Var a ())))) (y ())))))
    (Splice (expr (Var f ()))
     (subs
      (((x
         ((Match (Var a ())
           (((Construct A ()) (Lit (Int 1))) ((Construct B ()) (Lit (Int 2)))))))
        (y ())))))
    (Construct C ((Splice (expr (Var x ())) (subs ()))))
    (Poly_variant A ((Splice (expr (Var x ())) (subs ()))))
    |}]
;;

let%expect_test "patterns" =
  pat "_";
  pat "x";
  pat "1";
  pat "'c'";
  pat {|"s"|};
  pat "true";
  pat "()";
  pat "(p, q)";
  pat "p, q, r";
  pat "C";
  pat "M.C";
  pat "C p";
  pat "C (p, q)";
  pat "C ((p, q))";
  pat "C D x";
  pat "exception E";
  pat "exception E x";
  pat "exception M.E (a, b)";
  pat "exception e, x";
  pat "exception (e, x)";
  pat "C exception e";
  pat "{x = p; M.y = q}";
  pat "{x = p; _}";
  pat "`A";
  pat "`A p";
  pat "`A (p, q)";
  pat "{}";
  pat "{`x = p}";
  pat "{`x = p; ..}";
  pat "{`x = p; `y = _; ..rest}";
  pat "{..}";
  pat "{..rest}";
  pat "{`x = {`y = C z; ..}}";
  [%expect
    {|
    Any
    (Var x)
    (Lit (Int 1))
    (Lit (Char c))
    (Lit (String s))
    (Lit (Bool true))
    (Lit Unit)
    (Tuple ((Var p) (Var q)))
    (Tuple ((Var p) (Var q) (Var r)))
    (Construct C ())
    (Construct M.C ())
    (Construct C ((Var p)))
    (Construct C ((Var p) (Var q)))
    (Construct C ((Tuple ((Var p) (Var q)))))
    (Construct C ((Construct D ((Var x)))))
    (Exception (Construct E ()))
    (Exception (Construct E ((Var x))))
    (Exception (Construct M.E ((Var a) (Var b))))
    (Tuple ((Exception (Var e)) (Var x)))
    (Exception (Tuple ((Var e) (Var x))))
    t.stag:1:2-11: expected end of file, found `exception`
    (Record ((x (Var p)) (M.y (Var q))))
    (Record ((x (Var p))))
    (Poly_variant A ())
    (Poly_variant A ((Var p)))
    (Poly_variant A ((Tuple ((Var p) (Var q)))))
    (Poly_record (fields ()) (rest Closed))
    (Poly_record (fields ((x (Var p)))) (rest Closed))
    (Poly_record (fields ((x (Var p)))) (rest Open))
    (Poly_record (fields ((x (Var p)) (y Any))) (rest (Bind rest)))
    (Poly_record (fields ()) (rest Open))
    (Poly_record (fields ()) (rest (Bind rest)))
    (Poly_record
     (fields
      ((x (Poly_record (fields ((y (Construct C ((Var z)))))) (rest Open)))))
     (rest Closed))
    |}]
;;

let%expect_test "types" =
  ty "int";
  ty "M.t";
  ty "'a";
  ty "list['a]";
  ty "list[list['a]]";
  ty "map[k, v]";
  ty "with_x[{..'a}]";
  ty "with_x[{`y : int; ..'a}]";
  ty "with_x[{y : int; ..'a}]";
  ty "'a -> 'b -> 'c";
  ty "('a -> 'b) -> 'c";
  ty "'a * 'b * 'c";
  ty "('a * 'b) * 'c";
  ty "'a * 'b -> 'c * 'd";
  ty "()";
  ty "{}";
  ty "{..'r}";
  ty "{`x : int; `y : bool; ..'r}";
  ty "{`x : int;}";
  ty "[]";
  ty "[..'r]";
  ty "[`A of int | `B; ..'r]";
  ty "[| `A]";
  ty "[`A of 'a -> 'b | `B of int * int]";
  ty "<'a>{x : int; ..'r}";
  ty "<int -> int>{}";
  ty "<<'a>{..'b}>{..'c}";
  [%expect
    {|
    (Con int ())
    (Con M.t ())
    (Var a)
    (Con list ((Ty (Var a))))
    (Con list ((Ty (Con list ((Ty (Var a)))))))
    (Con map ((Ty (Con k ())) (Ty (Con v ()))))
    (Con with_x ((Row (Var a))))
    (Con with_x ((Row (Extend y (Con int ()) (Var a)))))
    t.stag:1:8-9: polymorphic record labels are written with a backtick: `y
    (Arrow (Var a) (Arrow (Var b) (Var c)))
    (Arrow (Arrow (Var a) (Var b)) (Var c))
    (Tuple ((Var a) (Var b) (Var c)))
    (Tuple ((Tuple ((Var a) (Var b))) (Var c)))
    (Arrow (Tuple ((Var a) (Var b))) (Tuple ((Var c) (Var d))))
    (Tuple ())
    (Record Empty)
    (Record (Var r))
    (Record (Extend x (Con int ()) (Extend y (Con bool ()) (Var r))))
    (Record (Extend x (Con int ()) Empty))
    (Variant Empty)
    (Variant (Var r))
    (Variant (Extend A (Con int ()) (Extend B (Con .unit ()) (Var r))))
    (Variant (Extend A (Con .unit ()) Empty))
    (Variant
     (Extend A (Arrow (Var a) (Var b))
      (Extend B (Tuple ((Con int ()) (Con int ()))) Empty)))
    (Code (inner (Var a)) (context (Extend x (Con int ()) (Var r))))
    (Code (inner (Arrow (Con int ()) (Con int ()))) (context Empty))
    (Code (inner (Code (inner (Var a)) (context (Var b)))) (context (Var c)))
    |}]
;;

let%expect_test "declarations" =
  prog
    {|
    let f x y = e
    let g : int = 1
    type nat = Z | S of nat
    type list['a] = Nil | Cons of ptr['a * list['a]]
    type pair = P of ('a * 'b)
    type with_x[{..'r}] = {`x : int; ..'r}
    type point = {x : int; y : int}
    type t
    type m = M.t
    type e = |
    type b = | A | B
    type c = A
    exception E
    exception F of int * M.t
    exception G of (int * string)
    module M = struct
      let x = 1
      module N = struct end
    end
    |};
  [%expect
    {|
    ((Let f ((x ()) (y ())) () (Var e ()))
     (Let g () ((ty (Con int ()))) (Lit (Int 1)))
     (Type ((name nat) (params ()) (kind (Variant ((Z ()) (S ((Con nat ()))))))))
     (Type
      ((name list) (params ((Ty a)))
       (kind
        (Variant
         ((Nil ())
          (Cons ((Con ptr ((Ty (Tuple ((Var a) (Con list ((Ty (Var a))))))))))))))))
     (Type
      ((name pair) (params ())
       (kind (Variant ((P ((Tuple ((Var a) (Var b))))))))))
     (Type
      ((name with_x) (params ((Row r)))
       (kind (Alias (Record (Extend x (Con int ()) (Var r)))))))
     (Type
      ((name point) (params ())
       (kind (Record ((x (Con int ())) (y (Con int ())))))))
     (Type ((name t) (params ()) (kind Abstract)))
     (Type ((name m) (params ()) (kind (Alias (Con M.t ())))))
     (Type ((name e) (params ()) (kind (Variant ()))))
     (Type ((name b) (params ()) (kind (Variant ((A ()) (B ()))))))
     (Type ((name c) (params ()) (kind (Variant ((A ())))))) (Exception E ())
     (Exception F ((Con int ()) (Con M.t ())))
     (Exception G ((Tuple ((Con int ()) (Con string ())))))
     (Module M ((Let x () () (Lit (Int 1))) (Module N ()))))
    |}]
;;

let%expect_test "locations" =
  show_locs
    Expr.sexp_of_t
    Parser.expr
    "let f (x : int) y : t = g x.M.a in\n  (a, b); C (c, d)";
  [%expect
    {|
    Let "let f (x : int) y : t = g x.M.a in\n  (a, b); C (c, d)"
      f "f"
      x "x"
      Con "int"
        int "int"
      y "y"
      Con "t"
        t "t"
      App "g x.M.a"
        Var "g"
        Field "x.M.a"
          Var "x"
          M.a "M.a"
      Seq "(a, b); C (c, d)"
        Tuple "(a, b)"
          Var "a"
          Var "b"
        Construct "C (c, d)"
          C "C"
          Var "c"
          Var "d"
    |}];
  show_locs
    Expr.sexp_of_t
    Parser.expr
    "match .~e.{x = 1; y} with `A p -> {`x = r.`y | {r with `z = ()}} \\ `x | \
     _ -> .< (f) >.";
  [%expect
    {|
    Match "match .~e.{x = 1; y} with `A p -> {`x = r.`y | {r with `z = ()}} \\ `x | _ -> .< (f) >."
      Splice ".~e.{x = 1; y}"
        Var "e"
        x "x"
        Lit "1"
        y "y"
      Poly_variant "`A p"
        A "`A"
        Var "p"
      Poly_restrict "{`x = r.`y | {r with `z = ()}} \\ `x"
        Poly_extend "{`x = r.`y | {r with `z = ()}}"
          x "`x"
          Poly_field "r.`y"
            Var "r"
            y "`y"
          Poly_update "{r with `z = ()}"
            Var "r"
            z "`z"
            Lit "()"
        x "`x"
      Any "_"
      Quote ".< (f) >."
        Var "(f)"
    |}];
  show_locs
    Pattern.sexp_of_t
    Parser.pattern
    "(C (x, _), {`a = `B 1; ..rest}, {M.x = 'c'})";
  [%expect
    {|
    Tuple "(C (x, _), {`a = `B 1; ..rest}, {M.x = 'c'})"
      Construct "C (x, _)"
        C "C"
        Var "x"
        Any "_"
      Poly_record "{`a = `B 1; ..rest}"
        a "`a"
        Poly_variant "`B 1"
          B "`B"
          Lit "1"
        rest "rest"
      Record "{M.x = 'c'}"
        M.x "M.x"
        Lit "'c'"
    |}];
  show_locs
    Type_expr.sexp_of_t
    Parser.type_expr
    "M.t['a, {`x : int; ..'r}] -> <(int)>{y : [`A of 'a | `B; ..'s]} * ()";
  [%expect
    {|
    Arrow "M.t['a, {`x : int; ..'r}] -> <(int)>{y : [`A of 'a | `B; ..'s]} * ()"
      Con "M.t['a, {`x : int; ..'r}]"
        M.t "M.t"
        Var "'a"
        Extend "{`x : int; ..'r}"
          x "`x"
          Con "int"
            int "int"
          Var "..'r"
            r "'r"
      Tuple "<(int)>{y : [`A of 'a | `B; ..'s]} * ()"
        Code "<(int)>{y : [`A of 'a | `B; ..'s]}"
          Con "(int)"
            int "int"
          Extend "{y : [`A of 'a | `B; ..'s]}"
            y "y"
            Variant "[`A of 'a | `B; ..'s]"
              Extend "[`A of 'a | `B; ..'s]"
                A "`A"
                Var "'a"
                Extend "`B; ..'s"
                  B "`B"
                  Con "`B"
                    .unit "`B"
                  Var "..'s"
                    s "'s"
            Empty ""
        Tuple "()"
    |}];
  show_locs
    Program.sexp_of_t
    Parser.program
    "type list['a] = Nil | Cons of ptr['a * list['a]]\n\
     type t[{..'r}] = {x : int}\n\
     module M = struct let x = 1 end";
  [%expect
    {|
    Type "type list['a] = Nil | Cons of ptr['a * list['a]]"
      list "list"
      Ty "'a"
      Nil "Nil"
      Cons "Cons"
      Con "ptr['a * list['a]]"
        ptr "ptr"
        Tuple "'a * list['a]"
          Var "'a"
          Con "list['a]"
            list "list"
            Var "'a"
    Type "type t[{..'r}] = {x : int}"
      t "t"
      Row "{..'r}"
      x "x"
      Con "int"
        int "int"
    Module "module M = struct let x = 1 end"
      M "M"
      Let "let x = 1"
        x "x"
        Lit "1"
    |}]
;;

let%expect_test "errors" =
  expr "";
  expr "(a, b";
  expr "f (x\n  in";
  expr "let x = in x";
  expr "let x = 1";
  expr "let x 1 = 1 in x";
  expr "let = 1 in x";
  expr "match x with A -> 1 |";
  expr "match x A -> 1";
  expr "match x with A 1";
  expr "(f x : int)";
  expr "(1 : int)";
  expr "a )";
  expr "a b c in";
  expr "{x = 1; `y = 2}";
  expr "{`x = 1; y = 2}";
  expr "{r with x = 1; `y = 2}";
  expr "{r}";
  expr "{`x = 1 | r";
  expr ".< x";
  expr "r.";
  expr "r.M";
  expr "r \\ x";
  expr ".~e.{1}";
  expr "C let x = 1 in x";
  expr "\"abc";
  expr "a # b";
  pat "(x : int)";
  pat "{`x = p; .. ; `y = q}";
  pat "M.x";
  pat "{x}";
  pat "{x = p; _; y = q}";
  pat "{x = p; _ ;}";
  ty "'_a";
  ty "list['_weak]";
  ty "'a['b]";
  ty "list[]";
  ty "int ->";
  ty "{x : int}";
  ty "<int>{`x : int}";
  ty "<int>";
  ty "<int";
  ty "[`A of int; `B]";
  ty "M";
  prog "let x = 1 in x";
  prog "type t[a] = int";
  prog "module M = struct let x = 1";
  prog "module m = struct end";
  prog "x";
  [%expect
    {|
    t.stag:1:0-0: expected an expression, found end of file
    t.stag:1:5-5: expected `)` to close this `(` (opened at 1:0), found end of file
    t.stag:2:2-4: expected `)` to close this `(` (opened at 1:2), found `in`
    t.stag:1:8-10: expected an expression, found `in`
    t.stag:1:9-9: expected `in` for the `let` at 1:0, found end of file
    t.stag:1:6-7: expected `=`, found `1`
    t.stag:1:4-5: expected a name to bind, found `=`
    t.stag:1:21-21: expected a pattern, found end of file
    t.stag:1:10-12: expected `with` for the `match` at 1:0, found `->`
    t.stag:1:16-16: expected `->`, found end of file
    (Annotated (App (Var f ()) (Var x ())) ((ty (Con int ()))))
    (Annotated (Lit (Int 1)) ((ty (Con int ()))))
    t.stag:1:2-3: unmatched `)`
    t.stag:1:6-8: expected end of file, found `in`
    t.stag:1:8-10: expected a field name, found the label `y
    t.stag:1:9-10: expected a polymorphic record label like `x, found `y`
    t.stag:1:15-17: expected a field name, found the label `y
    t.stag:1:2-3: expected `with` or a record field, found `}`
    t.stag:1:11-11: expected `}` to close this `{` (opened at 1:0), found end of file
    t.stag:1:4-4: expected `>.` to close this `.<` (opened at 1:0), found end of file
    t.stag:1:2-2: expected a field name, found end of file
    t.stag:1:2-3: expected a field name, found `M`
    t.stag:1:4-5: expected a polymorphic record label like `x, found `x`
    t.stag:1:5-6: expected a variable to provide, found `1`
    t.stag:1:2-5: expected end of file, found `let`
    t.stag:1:0-1: unterminated string literal
    t.stag:1:2-3: illegal character '#'
    t.stag:1:3-4: type annotations on patterns are not supported
    t.stag:1:12-13: expected `}` to close this `{` (opened at 1:0), found `;`
    t.stag:1:2-3: expected a constructor, found `x`
    t.stag:1:2-3: expected `=`, found `}`
    t.stag:1:8-9: expected a field name, found `_`
    t.stag:1:8-9: expected a field name, found `_`
    t.stag:1:0-3: weak type variables like `'_a` cannot be written, they are only printed
    t.stag:1:5-11: weak type variables like `'_weak` cannot be written, they are only printed
    t.stag:1:2-3: only type constructors like `list` take type arguments
    t.stag:1:5-6: expected a type argument, found `]`
    t.stag:1:6-6: expected a type, found end of file
    t.stag:1:1-2: polymorphic record labels are written with a backtick: `x
    t.stag:1:6-8: the variables of a code context are written without a backtick: x
    t.stag:1:5-5: expected the code type's context, like `{..'r}`, found end of file
    t.stag:1:4-4: expected `>` to close this `<` (opened at 1:0), found end of file
    t.stag:1:12-14: expected a row variable like `..'r` after `;`, found the label `B
    t.stag:1:0-1: expected a type name, found `M`
    t.stag:1:10-12: unexpected `in`: a toplevel `let` is a declaration, without `in`
    t.stag:1:7-8: expected a type parameter like `'a` or `{..'r}`, found `a`
    t.stag:1:27-27: expected `end` to close this `struct` (opened at 1:11), found end of file
    t.stag:1:7-8: expected a module name, found `m`
    t.stag:1:0-1: expected a declaration (`let`, `type`, `exception`, `module` or `include`), found `x`
    |}]
;;

let%expect_test "expect tests" =
  prog
    {x|
let%expect_test "add" =
  print_int 1;
  [%expect {| 1 |}];
  [%expect];
  [%expect "s"]
let%expect_test {|q|} = ()
|x};
  prog {|let%expect_test name = ()|};
  prog {|let%expect_test "x" ()|};
  expr {|[%expect 1]|};
  expr "[%expect {||}";
  [%expect
    {|
    ((Expect_test (name add)
      (body
       (Seq (App (Var print_int ()) (Lit (Int 1)))
        (Seq (Expect " 1 ") (Seq (Expect "") (Expect s))))))
     (Expect_test (name q) (body (Lit Unit))))
    t.stag:1:16-20: expected the name of the test, a string, found `name`
    t.stag:1:20-21: expected `=`, found `(`
    t.stag:1:9-10: expected `]` to close this `[%expect` (opened at 1:0), found `1`
    t.stag:1:13-13: expected `]` to close this `[%expect` (opened at 1:0), found end of file
    |}]
;;

let%expect_test "benches" =
  prog
    {|
let%bench "fib" = fib 20
let%bench "sort" n in [10; (f x, 3); g y] = sort (list n)
let%bench "one" n in [1;] = n
|};
  prog {|let % bench "x" = 1|};
  prog {|let%bench name = ()|};
  prog {|let%bench "x" ()|};
  prog {|let%bench "x" n = n|};
  prog {|let%bench "x" n in [] = n|};
  prog {|let%bench "x" n in 1 = n|};
  prog {|let%bench "x" N in [1] = n|};
  prog {|let%bench "x" n in [1; 2 = n|};
  [%expect
    {|
    ((Bench (name fib) (param ()) (body (App (Var fib ()) (Lit (Int 20)))))
     (Bench (name sort)
      (param
       (((name n)
         (args
          ((Lit (Int 10)) (Tuple ((App (Var f ()) (Var x ())) (Lit (Int 3))))
           (App (Var g ()) (Var y ())))))))
      (body (App (Var sort ()) (App (Var list ()) (Var n ())))))
     (Bench (name one) (param (((name n) (args ((Lit (Int 1)))))))
      (body (Var n ()))))
    t.stag:1:4-5: illegal character '%'
    t.stag:1:10-14: expected the name of the benchmark, a string, found `name`
    t.stag:1:14-15: expected `=`, found `(`
    t.stag:1:16-17: expected `in`, found `=`
    t.stag:1:19-20: a parameterised benchmark needs an argument
    t.stag:1:19-20: expected `[`, found `1`
    t.stag:1:14-15: expected `=`, found `N`
    t.stag:1:25-26: expected `]` to close this `[` (opened at 1:19), found `=`
    |}]
;;

let%expect_test "let and match in braces end at `;`" =
  expr "{x = let a = 1 in a; y = 2}";
  expr "{`x = let a = 1 in a, b; `y = 2}";
  expr "{`x = 1 | let r = s in r}";
  expr "{r with `x = match a with A -> 1 | B -> let b = 2 in b; `y = 2}";
  expr "{x = (let a = 1 in a; b); y = 2}";
  expr "{x = let a = 1 in let b = a in b; y = 2}";
  expr "let a = {x = 1} in a; b";
  expr "match a with A -> {x = 1}; b | B -> c";
  [%expect
    {|
    (Record ((x (Let a () () (Lit (Int 1)) (Var a ()))) (y (Lit (Int 2)))))
    (Poly_record
     ((x (Let a () () (Lit (Int 1)) (Tuple ((Var a ()) (Var b ())))))
      (y (Lit (Int 2)))))
    (Poly_extend ((x (Lit (Int 1)))) (Let r () () (Var s ()) (Var r ())))
    (Poly_update (Var r ())
     ((x
       (Match (Var a ())
        (((Construct A ()) (Lit (Int 1)))
         ((Construct B ()) (Let b () () (Lit (Int 2)) (Var b ()))))))
      (y (Lit (Int 2)))))
    (Record
     ((x (Let a () () (Lit (Int 1)) (Seq (Var a ()) (Var b ()))))
      (y (Lit (Int 2)))))
    (Record
     ((x (Let a () () (Lit (Int 1)) (Let b () () (Var a ()) (Var b ()))))
      (y (Lit (Int 2)))))
    (Let a () () (Record ((x (Lit (Int 1))))) (Seq (Var a ()) (Var b ())))
    (Match (Var a ())
     (((Construct A ()) (Seq (Record ((x (Lit (Int 1))))) (Var b ())))
      ((Construct B ()) (Var c ()))))
    |}]
;;

let%expect_test "slack and misplaced syntax" =
  expr "let rec f x = f x in f";
  prog "let rec f x = f x";
  expr "f.{x}";
  expr ".~f.{x = 1; y = 2, 3; }";
  expr "f @ x";
  expr "f @@ x";
  ty "int @ m";
  prog "let x @ const = 1";
  prog "type t[] = int";
  ty "[`A; ]";
  ty "[`A | ]";
  ty "[ | ]";
  ty "[ | ..'r]";
  ty "[`A | ..'r]";
  ty "[..'r]";
  ty "[`A; ..'r]";
  prog "type t = |";
  prog "type t = C of int -> int";
  prog "type t = C of int * int -> int";
  prog "type t = C of (int -> int)";
  ty "int list";
  ty "'a list";
  ty "int M.t";
  prog "type t = C of int * int list";
  ty "list['a][int]";
  ty "with_x[{x : int}]";
  ty "with_x[{`x : int; ..'r}]";
  ty "list[{}]";
  ty "{x : int}";
  ty "<int>{`x : int}";
  ty "<int>{x : int; ..'r}";
  [%expect
    {|
    t.stag:1:4-7: lets are always recursive; remove `rec`
    t.stag:1:4-7: lets are always recursive; remove `rec`
    t.stag:1:1-3: substitutions `.{...}` are only written after a splice `.~e`
    (Splice (expr (Var f ()))
     (subs (((x ((Lit (Int 1)))) (y ((Tuple ((Lit (Int 2)) (Lit (Int 3))))))))))
    t.stag:1:2-3: expected end of file, found `@`
    t.stag:1:2-4: a modality (`@@`) is only written on a type argument, as in `ptr[t @@ external]`
    t.stag:1:6-7: unknown mode `m`, expected one of `const`, `portable`, `nonportable`, `static`, `gc`, `external`, `global`, `regional`, `local`
    ((Let x () ((mode (Atoms (Const)))) (Lit (Int 1))))
    t.stag:1:7-8: expected a type parameter, found `]`
    t.stag:1:5-6: expected a row variable like `..'r` after `;`, found `]`
    t.stag:1:6-7: expected a polymorphic variant tag like `A, found `]`
    t.stag:1:4-5: expected a polymorphic variant tag like `A, found `]`
    t.stag:1:4-6: expected a polymorphic variant tag like `A, found `..`
    t.stag:1:6-8: expected a polymorphic variant tag like `A, found `..`
    (Variant (Var r))
    (Variant (Extend A (Con .unit ()) (Var r)))
    ((Type ((name t) (params ()) (kind (Variant ())))))
    t.stag:1:18-20: a constructor argument of function type must be parenthesized: `C of (a -> b)`
    t.stag:1:24-26: a constructor argument of function type must be parenthesized: `C of (a -> b)`
    ((Type
      ((name t) (params ())
       (kind (Variant ((C ((Arrow (Con int ()) (Con int ()))))))))))
    t.stag:1:4-8: type arguments are written after the name, in brackets: `list[int]`
    t.stag:1:3-7: type arguments are written after the name, in brackets: `list[int]`
    t.stag:1:4-5: type arguments are written after the name, in brackets: `list[int]`
    t.stag:1:24-28: type arguments are written after the name, in brackets: `list[int]`
    t.stag:1:8-9: type arguments are written once
    t.stag:1:8-9: polymorphic record labels are written with a backtick: `x
    (Con with_x ((Row (Extend x (Con int ()) (Var r)))))
    (Con list ((Row Empty)))
    t.stag:1:1-2: polymorphic record labels are written with a backtick: `x
    t.stag:1:6-8: the variables of a code context are written without a backtick: x
    (Code (inner (Con int ())) (context (Extend x (Con int ()) (Var r))))
    |}]
;;

let%expect_test "types as values" =
  expr "type t";
  expr "type M.t";
  expr "type list[int]";
  expr "type (int * t)";
  expr "type (int -> t)";
  (* At constructor application's precedence: an argument needs parens. *)
  expr "f (type t) x";
  expr "f type t";
  expr "C (type t)";
  expr "(type t, type u)";
  expr "type t; x";
  expr "type int -> t";
  expr "type";
  expr "type list int";
  [%expect
    {|
    (Type (Con t ()))
    (Type (Con M.t ()))
    (Type (Con list ((Ty (Con int ())))))
    (Type (Tuple ((Con int ()) (Con t ()))))
    (Type (Arrow (Con int ()) (Con t ())))
    (App (App (Var f ()) (Type (Con t ()))) (Var x ()))
    t.stag:1:2-6: expected end of file, found `type`
    (Construct C ((Type (Con t ()))))
    (Tuple ((Type (Con t ())) (Type (Con u ()))))
    (Seq (Type (Con t ())) (Var x ()))
    t.stag:1:9-11: expected end of file, found `->`
    t.stag:1:4-4: expected a type, found end of file
    t.stag:1:10-13: type arguments are written after the name, in brackets: `list[int]`
    |}]
;;

let%expect_test "root paths" =
  expr ".x";
  expr ".M.N.x";
  expr ".M.C (.x, .C)";
  expr "r.(.M.x).y.(.z)";
  expr "{.M.x = 1; y = .y}";
  expr "{r with .M.x = 1}";
  expr ".< .x .~.y >.";
  pat ".M.C {.M.x = .D; y = _}";
  ty ".int -> .M.t[.list[.option[int]]]";
  [%expect
    {|
    (Var .x ())
    (Var .M.N.x ())
    (Construct .M.C ((Var .x ()) (Construct .C ())))
    (Field (Field (Field (Var r ()) .M.x) y) .z)
    (Record ((.M.x (Lit (Int 1))) (y (Var .y ()))))
    (Record_update (Var r ()) ((.M.x (Lit (Int 1)))))
    (Quote (App (Var .x ()) (Splice (expr (Var .y ())) (subs ()))))
    (Construct .M.C ((Record ((.M.x (Construct .D ())) (y Any)))))
    (Arrow (Con .int ())
     (Con .M.t ((Ty (Con .list ((Ty (Con .option ((Ty (Con int ())))))))))))
    |}];
  expr "r.(x)";
  expr "r.(.M.x";
  expr ". x";
  pat ".x";
  [%expect
    {|
    t.stag:1:3-4: expected a root path like `.M.x`: `e.(.M.x)`, found `x`
    t.stag:1:7-7: expected `)` to close this `(` (opened at 1:2), found end of file
    t.stag:1:0-1: expected an expression, found `.`
    t.stag:1:1-2: expected a constructor, found `x`
    |}]
;;

let%expect_test "inline expansions" =
  prog
    {|
include%inline .~(gen (type t))
  type t = A
  let x = 1
end%inline
module%inline M = .~e end%inline
|};
  [%expect
    {|
    ((Include (App (Var gen ()) (Type (Con t ())))
      (((Type ((name t) (params ()) (kind (Variant ((A ()))))))
        (Let x () () (Lit (Int 1))))))
     (Module_splice M (Var e ()) (())))
    |}];
  List.iter
    [ "include%inline x end%inline"
    ; "module%inline M = struct end"
    ; "include%inline .~e let x = 1"
    ; "include%inline .~e end"
    ; "end%inline"
    ; "include .~e end%inline"
    ; "module%inline M = .~e end%inline end%inline"
    ; "module M = struct include%inline .~e let x = 1 end"
    ; "include%inline .~e module M = struct let x = 1 end%inline"
    ]
    ~f:prog;
  [%expect
    {|
    t.stag:1:15-16: expected a declaration splice `.~e` after `include%inline`, found `x`
    t.stag:1:18-24: expected a declaration splice `.~e` after `module%inline M =`, found `struct`
    t.stag:1:28-28: expected `end%inline` to close this `include%inline` (opened at 1:0), found end of file
    t.stag:1:19-22: expected `end%inline` to close this `include%inline` (opened at 1:0), found `end`
    t.stag:1:0-10: unmatched `end%inline`
    t.stag:1:12-22: unmatched `end%inline`
    t.stag:1:33-43: unmatched `end%inline`
    t.stag:1:47-50: expected `end%inline` to close this `include%inline` (opened at 1:18), found `end`
    t.stag:1:47-57: expected `end` to close this `struct` (opened at 1:30), found `end%inline`
    |}]
;;

let%expect_test "an expansion spans from the splice to [end%inline]" =
  (match Parser.program ~file "include%inline .~e\n  let x = 1\nend%inline" with
   | Ok [ { txt = Include (_, Some expansion); _ } ] ->
     print_endline (Loc.to_string expansion.loc)
   | _ -> print_endline "unexpected");
  [%expect {| t.stag:1:18-3:0 |}]
;;

let%expect_test "an expansion starts after the comments on its splice's line" =
  List.iter
    [ "include%inline .~e (* a *) (* b *)\n  let x = 1\nend%inline"
    ; "include%inline .~e\n  (* c *) let x = 1\nend%inline"
    ; "include%inline .~e (* a *) let x = 1 end%inline"
    ; "include%inline .~e (* a *) end%inline"
    ]
    ~f:(fun source ->
      match Parser.program ~file source with
      | Ok [ { txt = Include (_, Some expansion); _ } ] ->
        print_s
          [%sexp
            (String.sub
               source
               ~pos:expansion.loc.start.#offset
               ~len:(expansion.loc.stop.#offset - expansion.loc.start.#offset)
             : string)]
      | _ -> print_endline "unexpected");
  [%expect
    {|
     "\
    \n  let x = 1\
    \n"
     "\
    \n  (* c *) let x = 1\
    \n"
    " let x = 1 "
    " "
    |}]
;;

let%expect_test "a type splice defining a type is the whole definition" =
  prog "type r = .~t * int";
  prog "type r = .~t -> int";
  prog "type r = (.~t * int)";
  [%expect
    {xxx|
    t.stag:1:13-14: a type splice `.~e` defines the whole type: to splice part of an alias, write it in parentheses, as in `type t = (.~e * u)`
    t.stag:1:13-15: a type splice `.~e` defines the whole type: to splice part of an alias, write it in parentheses, as in `type t = (.~e * u)`
    ((Type
      ((name r) (params ())
       (kind (Alias (Tuple ((Splice (Var t ())) (Con int ()))))))))
    |xxx}]
;;

let%expect_test "list literal locations" =
  show_locs Expr.sexp_of_t Parser.expr "[a; b;]";
  show_locs Expr.sexp_of_t Parser.expr "[ ]";
  show_locs Pattern.sexp_of_t Parser.pattern "[a]";
  [%expect
    {|
    Construct "[a; b;]"
      <Cons> "["
      App "[a; b;]"
        Var "[a; b;]"
        Tuple "[a; b;]"
          Var "a"
          Construct "; b;]"
            <Cons> ";"
            App "; b;]"
              Var "; b;]"
              Tuple "; b;]"
                Var "b"
                Construct "]"
                  <Nil> "]"
    Construct "[ ]"
      <Nil> "[ ]"
    Construct "[a]"
      <Cons> "["
      Deref "[a]"
        Tuple "[a]"
          Var "a"
          Construct "]"
            <Nil> "]"
    |}]
;;

let%expect_test "mutable variables" =
  expr "let mut x = 1 in x <- add x 1; x";
  expr "let mut x : int = 1 in x";
  expr "M.x <- 1, 2";
  expr "x <- y <- 1";
  expr "f (x <- 1)";
  expr "match x with _ -> x <- 1; 2";
  expr "fun u -> x <- 1; 2";
  expr "[x <- 2; y <- 3, 4]";
  expr "{`a = x <- 2; `b = 1}";
  expr "{r with M.f = x <- 2}";
  expr "let mut f x = 1 in f";
  expr "let mut (x : int) = 1 in x";
  expr "f x <- 1";
  prog "let mut x = 1\nlet mut y : list[int] = []\nlet f u = x <- add x 1";
  prog "let mut f x = 1";
  [%expect
    {|
    (Let_mut x () (Lit (Int 1))
     (Seq (Assign x (App (App (Var add ()) (Var x ())) (Lit (Int 1))))
      (Var x ())))
    (Let_mut x ((ty (Con int ()))) (Lit (Int 1)) (Var x ()))
    (Assign M.x (Tuple ((Lit (Int 1)) (Lit (Int 2)))))
    (Assign x (Assign y (Lit (Int 1))))
    (App (Var f ()) (Assign x (Lit (Int 1))))
    (Match (Var x ()) ((Any (Seq (Assign x (Lit (Int 1))) (Lit (Int 2))))))
    (Fun ((u ())) (Seq (Assign x (Lit (Int 1))) (Lit (Int 2))))
    (Construct <Cons>
     ((App (Var <Ptr.make> ())
       (Tuple
        ((Assign x (Lit (Int 2)))
         (Construct <Cons>
          ((App (Var <Ptr.make> ())
            (Tuple
             ((Assign y (Tuple ((Lit (Int 3)) (Lit (Int 4)))))
              (Construct <Nil> ())))))))))))
    (Poly_record ((a (Assign x (Lit (Int 2)))) (b (Lit (Int 1)))))
    (Record_update (Var r ()) ((M.f (Assign x (Lit (Int 2))))))
    t.stag:1:8-9: a mutable variable has no parameters
    t.stag:1:8-9: expected a name to bind, found `(`
    t.stag:1:0-3: only a variable or a dereference can be assigned to
    ((Let_mut x () (Lit (Int 1)))
     (Let_mut y ((ty (Con list ((Ty (Con int ())))))) (Construct <Nil> ()))
     (Let f ((u ())) ()
      (Assign x (App (App (Var add ()) (Var x ())) (Lit (Int 1))))))
    t.stag:1:8-9: a mutable variable has no parameters
    |}]
;;
