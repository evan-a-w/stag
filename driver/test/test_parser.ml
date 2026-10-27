open! Import
open Syntax
open Helpers

let file = "t.stag"

let rec strip_locs : Sexp.t -> Sexp.t = function
  | List [ txt; Atom "@"; Atom _ ] -> strip_locs txt
  | List sexps -> List (List.map sexps ~f:strip_locs)
  | Atom _ as atom -> atom
;;

let show sexp_of_t = function
  | Ok t -> print_s (strip_locs (sexp_of_t t))
  | Error error -> print_endline (Parser.Error.to_string error)
;;

let expr source = show Expr.sexp_of_t (Parser.expr ~file source)
let pat source = show Pattern.sexp_of_t (Parser.pattern ~file source)
let ty source = show Type_expr.sexp_of_t (Parser.type_expr ~file source)
let prog source = show Program.sexp_of_t (Parser.program ~file source)

let parse_exn source =
  match Parser.program ~file source with
  | Ok program -> program
  | Error error -> raise_s [%message (Parser.Error.to_string error : string)]
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
    (Var x ((Con int ())))
    (Var M.x ((Arrow (Var a) (Var a))))
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
    (Construct C ((Var a ()) (Var b ())))
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
    (Let _ () () (Var a ()) (Let _ () () (Var b ()) (Var c ())))
    (Let _ () () (Tuple ((Var a ()) (Var b ()))) (Var c ()))
    (Let x () () (Lit (Int 1)) (Var x ()))
    (Let f ((x ()) (y ((Con int ()))) (_ ()) (_ ((Con bool ())))) ((Con t ()))
     (Var e ()) (Var f ()))
    (Let _ () () (Var a ()) (Var b ()))
    (Let x () () (Lit (Int 1)) (Let _ () () (Var a ()) (Var b ())))
    (Let _ () () (Var a ())
     (Let x () () (Lit (Int 1)) (Let _ () () (Var b ()) (Var c ()))))
    (Let x () () (Let y () () (Lit (Int 1)) (Var y ())) (Var x ()))
    (Tuple
     ((Var a ()) (Let x () () (Lit (Int 1)) (Tuple ((Var x ()) (Var y ()))))))
    (Match (Var x ())
     (((Construct A ()) (Lit (Int 1))) ((Construct B ((Var y))) (Lit (Int 2)))))
    (Match (Var x ())
     (((Construct A ()) (Let _ () () (Var a ()) (Var b ())))
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
  pat "{x = p; M.y = q}";
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
    (Construct C ((Var p) (Var q)))
    (Construct C ((Construct D ((Var x)))))
    (Record ((x (Var p)) (M.y (Var q))))
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
    (Variant (Extend A (Con int ()) (Extend B (Con unit ()) (Var r))))
    (Variant (Extend A (Con unit ()) Empty))
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
    type list['a] = Nil | Cons of 'a * list['a]
    type pair = P of ('a * 'b)
    type with_x[{..'r}] = {`x : int; ..'r}
    type point = {x : int; y : int}
    type t
    type m = M.t
    type e = |
    type b = | A | B
    type c = A
    module M = struct
      let x = 1
      module N = struct end
    end
    |};
  [%expect
    {|
    ((Let f ((x ()) (y ())) () (Var e ()))
     (Let g () ((Con int ())) (Lit (Int 1)))
     (Type ((name nat) (params ()) (kind (Variant ((Z ()) (S ((Con nat ()))))))))
     (Type
      ((name list) (params ((Ty a)))
       (kind (Variant ((Nil ()) (Cons ((Var a) (Con list ((Ty (Var a)))))))))))
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
     (Type ((name c) (params ()) (kind (Variant ((A ()))))))
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
      Let "(a, b); C (c, d)"
        _ ";"
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
                    unit "`B"
                  Var "..'s"
                    s "'s"
            Empty ""
        Tuple "()"
    |}];
  show_locs
    Program.sexp_of_t
    Parser.program
    "type list['a] = Nil | Cons of 'a * list['a]\n\
     type t[{..'r}] = {x : int}\n\
     module M = struct let x = 1 end";
  [%expect
    {|
    Type "type list['a] = Nil | Cons of 'a * list['a]"
      list "list"
      Ty "'a"
      Nil "Nil"
      Cons "Cons"
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
  expr "{r with x = 1}";
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
    t.stag:1:1-4: type annotations are only supported on variables, as in `(x : t)`
    t.stag:1:1-2: type annotations are only supported on variables, as in `(x : t)`
    t.stag:1:2-3: unmatched `)`
    t.stag:1:6-8: expected end of file, found `in`
    t.stag:1:8-10: expected a field name, found the label `y
    t.stag:1:9-10: expected a polymorphic record label like `x, found `y`
    t.stag:1:8-9: expected a polymorphic record label like `x, found `x`
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
    t.stag:1:0-1: expected a declaration (`let`, `type` or `module`), found `x`
    |}]
;;

let signature program =
  match Typecheck.typecheck (typechecker ()) ~program:(prims @ program) with
  | Ok { signature; _ } ->
    Signature.to_string_hum (List.drop signature (List.length prims))
  | Error error -> Sexp.to_string_hum [%sexp (error : Typecheck_error.t)]
;;

let type_decls =
  {|type nat = Z | S of nat
type list['a] = Nil | Cons of 'a * list['a]
type option['a] = None | Some of 'a
type stmt[{..'a}] = Stmt of <unit>{..'a} * list[stmt[{..'a}]]
|}
;;

(* [let f (x : ty) = x] has type [ty -> ty], printed as [ty] is. *)
let%expect_test "printed types parse back" =
  List.iter
    [ "{`x : 'a; ..'b} -> 'a"
    ; "nat -> <'a>{..'b} -> <list['a]>{..'c}"
    ; "[`A of int; ..'a] -> ([..'a] -> int) -> int"
    ; "[`A of 'a * 'b; ..'c] -> ([..'c] -> 'a) -> 'a"
    ; "option[[`A; ..'a]] -> int"
    ; "[`None | `Some of option[int]] -> int"
    ; "{`x : [`A; ..'a]} -> int"
    ; "<{`y : 'a}>{y : 'a; ..'b}"
    ; "<'a>{r : {`x : 'a; ..'b}; ..'c}"
    ; "<'a * {..'b}>{r : {`x : 'a; ..'b}; ..'c}"
    ; "stmt[{`x : int; ..'a}]"
    ; "stmt[{..'a}] -> <unit>{..'a}"
    ; "{`A : int} * [`A of int]"
    ; "{`a : {`b : 'a; ..'b}; ..'c} -> 'a"
    ; "('a * 'b) * 'c"
    ; "'a * ('b -> 'c)"
    ; "() * {} * []"
    ; "<<'a>{..'b}>{..'c}"
    ]
    ~f:(fun ty ->
      let program = parse_exn (type_decls ^ sprintf "let f (x : %s) = x" ty) in
      let printed = signature program |> String.split_lines |> List.last_exn in
      let parens =
        match Parser.type_expr ~file ty with
        | Ok { txt = Arrow _; _ } -> sprintf "(%s)" ty
        | _ -> ty
      in
      let expected = sprintf "val f : %s -> %s" parens ty in
      if String.equal printed expected
      then print_endline ty
      else print_s [%message "mismatch" (printed : string) (expected : string)]);
  [%expect
    {|
    {`x : 'a; ..'b} -> 'a
    nat -> <'a>{..'b} -> <list['a]>{..'c}
    [`A of int; ..'a] -> ([..'a] -> int) -> int
    [`A of 'a * 'b; ..'c] -> ([..'c] -> 'a) -> 'a
    option[[`A; ..'a]] -> int
    [`None | `Some of option[int]] -> int
    {`x : [`A; ..'a]} -> int
    <{`y : 'a}>{y : 'a; ..'b}
    <'a>{r : {`x : 'a; ..'b}; ..'c}
    <'a * {..'b}>{r : {`x : 'a; ..'b}; ..'c}
    stmt[{`x : int; ..'a}]
    stmt[{..'a}] -> <unit>{..'a}
    {`A : int} * [`A of int]
    {`a : {`b : 'a; ..'b}; ..'c} -> 'a
    ('a * 'b) * 'c
    'a * ('b -> 'c)
    () * {} * []
    <<'a>{..'b}>{..'c}
    |}]
;;

let%expect_test "printed declarations parse back" =
  let source =
    type_decls
    ^ {|type point = {`x : int; `y : int}
type with_x[{..'a}] = {`x : int; ..'a}
type holder = { p : {`x : int; `y : int} }
type stream['a] = {`head : 'a; `tail : unit -> stream['a]}
type tree = [`Leaf of int | `Node of list[tree]]
type pair = [`A of pair] * int
type rows[{..'a}] = {`x : rows[{..'a}]; ..'a}
type e = |
type abstract
|}
  in
  let printed = signature (parse_exn source) in
  print_string printed;
  print_s [%message (String.equal printed source : bool)];
  [%expect
    {|
    type nat = Z | S of nat
    type list['a] = Nil | Cons of 'a * list['a]
    type option['a] = None | Some of 'a
    type stmt[{..'a}] = Stmt of <unit>{..'a} * list[stmt[{..'a}]]
    type point = {`x : int; `y : int}
    type with_x[{..'a}] = {`x : int; ..'a}
    type holder = { p : {`x : int; `y : int} }
    type stream['a] = {`head : 'a; `tail : unit -> stream['a]}
    type tree = [`Leaf of int | `Node of list[tree]]
    type pair = [`A of pair] * int
    type rows[{..'a}] = {`x : rows[{..'a}]; ..'a}
    type e = |
    type abstract
    ("String.equal printed source" true)
    |}]
;;

let%expect_test "the staged power example" =
  let program =
    parse_exn
      {|
      type nat = Z | S of nat
      type list['a] = Nil | Cons of 'a * list['a]

      (* [power n c] is a list of [n] copies of [c]. *)
      let power n c =
        match n with
        | Z -> .< Nil >.
        | S m -> .< Cons (.~c, .~(power m c)) >.

      let three = S (S (S Z))
      let cube x = .~(power three .<x>.)
      let used = cube 7
      |}
  in
  print_s
    [%message
      (Program.equal
         (List.take program 3)
         Test_compile_time.[ nat; Decl.list; power ]
       : bool)];
  match Driver.check ~limits (prims @ program) with
  | Error error -> print_s [%sexp (error : Typecheck_error.t)]
  | Ok driver ->
    print_signature
      (List.drop (Driver.checked driver).signature (List.length prims));
    Driver.eval ~limits driver |> ok_exn;
    Test_compile_time.print_values driver;
    [%expect
      {|
      ( "Program.equal (List.take program 3)\
       \n  (let open Test_compile_time in [nat; Decl.list; power])" true)
      type nat = Z | S of nat
      type list['a] = Nil | Cons of 'a * list['a]
      val power : nat -> <'a>{..'b} -> <list['a]>{..'c} where ..'b <= {..'c}
      val three : nat
      val cube : 'a -> list['a]
      val used : list[int]
      (power <fun>)
      (three (S (S (S (Z)))))
      (cube <fun>)
      (used (Cons 7 (Cons 7 (Cons 7 (Nil)))))
      |}]
;;

let%expect_test "polymorphic records, variants and modules" =
  print_string
    (signature
       (parse_exn
          {|
          let lit = {`x = 1; `y = true}
          let empty = {}
          let get_x r = r.`x
          let extend r = {`z = 1 | r}
          let restrict r = r \ `x
          let update r = {r with `x = true}
          let nested r = r.`a.`b
          let rest r = match r with {`a = a; `b = _; ..others} -> (a, others)
          let open_ r = match r with {`a = a; ..} -> a
          let tags x = match x with `A n -> n | `B -> 0 | `C (a, b) -> add a b
          let sequence x = id x; eq x 1

          type point = {x : int; y : int}
          let origin = {x = 0; y = 0}
          let get_y p = p.y

          module M = struct
            type t = A | B of int * bool
            let make n = B (n, true)
            let first t = match t with A -> 0 | B (n, _) -> n
            module N = struct let deep = first (make 1) end
          end
          let deep = M.N.deep
          let annotated (t : M.t) = t
          let quoted (c : <int>{..'r}) = .< (.~c.{}, x) >.
          |}));
  [%expect
    {|
    val lit : {`x : int; `y : bool}
    val empty : {}
    val get_x : {`x : 'a; ..'b} -> 'a
    val extend : {..'a} -> {`z : int; ..'a}
    val restrict : {`x : 'a; ..'b} -> {..'b}
    val update : {`x : 'a; ..'b} -> {`x : bool; ..'b}
    val nested : {`a : {`b : 'a; ..'b}; ..'c} -> 'a
    val rest : {`a : 'a; `b : 'b; ..'c} -> 'a * {..'c}
    val open_ : {`a : 'a; ..'b} -> 'a
    val tags : [`A of int | `B | `C of int * int] -> int
    val sequence : int -> bool
    type point = { x : int; y : int }
    val origin : point
    val get_y : point -> int
    module M : sig
      type t = A | B of int * bool
      val make : int -> t
      val first : t -> int
      module N : sig
        val deep : int
      end
    end
    val deep : int
    val annotated : M.t -> M.t
    val quoted : <int>{} -> <int * 'a>{x : 'a; ..'b}
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
       (Let _ () () (App (Var print_int ()) (Lit (Int 1)))
        (Let _ () () (Expect " 1 ") (Let _ () () (Expect "") (Expect s))))))
     (Expect_test (name q) (body (Lit Unit))))
    t.stag:1:16-20: expected the name of the test, a string, found `name`
    t.stag:1:20-21: expected `=`, found `(`
    t.stag:1:9-10: expected `]` to close this `[%expect` (opened at 1:0), found `1`
    t.stag:1:13-13: expected `]` to close this `[%expect` (opened at 1:0), found end of file
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
     ((x (Let a () () (Lit (Int 1)) (Let _ () () (Var a ()) (Var b ()))))
      (y (Lit (Int 2)))))
    (Record
     ((x (Let a () () (Lit (Int 1)) (Let b () () (Var a ()) (Var b ()))))
      (y (Lit (Int 2)))))
    (Let a () () (Record ((x (Lit (Int 1)))))
     (Let _ () () (Var a ()) (Var b ())))
    (Match (Var a ())
     (((Construct A ()) (Let _ () () (Record ((x (Lit (Int 1))))) (Var b ())))
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
    t.stag:1:2-3: `@` is reserved (for modes)
    t.stag:1:2-4: `@` is reserved (for modes)
    t.stag:1:4-5: `@` is reserved (for modes)
    t.stag:1:6-7: `@` is reserved (for modes)
    t.stag:1:7-8: expected a type parameter, found `]`
    t.stag:1:5-6: expected a row variable like `..'r` after `;`, found `]`
    t.stag:1:6-7: expected a polymorphic variant tag like `A, found `]`
    t.stag:1:4-5: expected a polymorphic variant tag like `A, found `]`
    t.stag:1:4-6: expected a polymorphic variant tag like `A, found `..`
    t.stag:1:6-8: expected a polymorphic variant tag like `A, found `..`
    (Variant (Var r))
    (Variant (Extend A (Con unit ()) (Var r)))
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
