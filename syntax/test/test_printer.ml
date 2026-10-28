open! Import

let parse source =
  match Parser.program ~file:"t.stag" source with
  | Ok program -> program
  | Error error -> raise_s [%sexp (Parser.Error.to_string error : string)]
;;

(* Prints [source] at [width], checking it round trips. *)
let print ?(width = 80) source =
  let program = parse source in
  Round_trip.program program;
  print_string (Printer.program ~width program)
;;

let print_expr ?(width = 80) source =
  match Parser.expr ~file:"t.stag" source with
  | Error error -> print_endline (Parser.Error.to_string error)
  | Ok expr ->
    Round_trip.expr expr;
    print_endline (Printer.expr ~width expr)
;;

let%expect_test "short forms" =
  print
    {|
let x = 1
let f x (y : int) : int = g x y
let s = "a\"b\\c\n\t\001é"
let bytes = "\255é"
let c = '\''
let d = '\\'
let chars = ('\n', '\t', '\000', '\255', '"', 'a', ' ')
let n = -1
let e = (f (-1), Some (-1), `A (-1), (-1).x, f x, ())
let a = (x : 'a -> 'b)
type t = A | B of int | C of (int -> int) * (int * int) | D of list['a]
type r = {x : int; y : list[int]}
type v = |
type abs
type p['a, {..'r}] = <'a>{x : int; ..'r}
type q = {`x : int; `y : bool; ..'r} -> [`A of int | `B; ..'r] -> [..'r] * []
type w = with_x[({`x : int}), {`y : int}, {}]
|};
  [%expect
    {|
    let x = 1
    let f x (y : int) : int = g x y
    let s = "a\"b\\c\n\t\001é"
    let bytes = "\255\195\169"
    let c = '\''
    let d = '\\'
    let chars = ('\n', '\t', '\000', '\255', '"', 'a', ' ')
    let n = -1
    let e = (f (-1), Some (-1), `A (-1), (-1).x, f x, ())
    let a = (x : 'a -> 'b)

    type t = A | B of int | C of (int -> int) * (int * int) | D of list['a]
    type r = {x : int; y : list[int]}
    type v = |
    type abs
    type p['a, {..'r}] = <'a>{x : int; ..'r}
    type q = {`x : int; `y : bool; ..'r} -> [`A of int | `B; ..'r] -> [..'r] * []
    type w = with_x[({`x : int}), {`y : int}, {}]
    |}]
;;

let%expect_test "minimal parentheses" =
  List.iter
    ~f:print_expr
    [ "f (g x) (h (-1)) .~y (.~z w)"
    ; "(f x).y"
    ; "(C).x"
    ; "(M.C).M.x"
    ; "(C).`x"
    ; "(C) \\ `x"
    ; "(C) x"
    ; "(`A) x"
    ; "C (D x)"
    ; "C (f x)"
    ; "`A (`B x)"
    ; "C ((a, b))"
    ; "C ((a, b), c)"
    ; "C (let x = 1 in x)"
    ; "(a, b), c"
    ; "a, (b, c)"
    ; "(a; b), c"
    ; "(a; b); c"
    ; "a; (b; c)"
    ; "(let x = 1 in x), y"
    ; "a, (let x = 1 in x)"
    ; "(a, let x = 1 in x); y"
    ; "(match x with A -> a), b"
    ; "(match x with A -> a); b"
    ; "match x with A -> (match y with B -> b) | C -> c"
    ; "match x with A -> (let y = 1 in match y with B -> b) | C -> c"
    ; "match x with A -> (a, match y with B -> b) | C -> c"
    ; "match x with A -> (let y = 1 in y) | C -> c"
    ; "match x with A -> match y with B -> b | C -> c"
    ; "match match x with A -> a with B -> b"
    ; "let x = let y = 1 in y in x"
    ; ".~(.~e)"
    ; ".~(.~e.{x})"
    ; ".~(.~e).{x}"
    ; ".~(.~e.{x}).{y}"
    ; ".~(r.x).{y; z = 1, 2}"
    ; ".~(f x)"
    ; "(.~f) x"
    ; "(.~e).x"
    ; ".~e.{}"
    ; "{x = (a; b); M.y = let z = 1 in z; w = match a with B -> b}"
    ; "{`x = a, b; `y = (match a with B -> b) | r}"
    ; "{`x = 1 | match r with A -> a}"
    ; "{(`A) with `x = 1}"
    ; "{(`A).`b with `x = 1}"
    ; "{(`A (f x)) with `x = 1}"
    ; "{C with `x = 1}"
    ; "{x = match a with B -> b; y = let z = 1 in z; w = 2}"
    ; "{x = (match a with B -> b; c); y = (let z = 1 in z; w)}"
    ; "(`A).`x"
    ; "f (`A).x (C).`y"
    ; ".~(C).x"
    ; "(.< x >.) \\ `x"
    ; "{(f x) with `x = 1}"
    ; "{.~r with `x = 1; `y = (a; b)}"
    ; "{r.x with `x = 1}"
    ; "{r with x = 1; M.y = (a; b)}"
    ; "{(`A) with x = 1}"
    ; "{r.M.x with M.y = {s with z = 2}}"
    ; "f r \\ `x"
    ; "(f r) \\ `x"
    ; ".~r \\ `x"
    ; "r.`x.y \\ `z"
    ; ".< .< .~(.~x) >. >."
    ; "(.< x >.).x"
    ; "f (x : int) (M.y : list['a])"
    ; "(-1, -2)"
    ; "Some (-1)"
    ; "f [%expect] [%expect {|a|}]"
    ; "let _ = a in b"
    ; "let _ x = a in b"
    ; "let _ : int = a in b"
    ; "fun x -> x"
    ; "fun x (y : int) _ -> fun z -> x, y; z"
    ; "f (fun x -> x) (fun x -> x)"
    ; "f (fun x -> x) y"
    ; "(fun x -> x) y"
    ; "(fun x -> x).y"
    ; "C (fun x -> x)"
    ; "(fun x -> x), y"
    ; "a, fun x -> x"
    ; "(fun x -> x); y"
    ; "a; fun x -> x"
    ; "match x with A -> (fun y -> y) | B -> b"
    ; "match x with A -> fun y -> y"
    ; "match fun y -> y with A -> a"
    ; "let f = fun x -> x in f"
    ; "{x = fun y -> y; z = (fun y -> y; y)}"
    ; "{`x = fun y -> y}"
    ; "{`x = (fun y -> y) | r}"
    ; ".~(fun x -> x)"
    ; ".< fun x -> .~(f .< x >.) >."
    ]
  [@ocamlformat "disable"];
  [%expect
    {xxx|
    f (g x) (h (-1)) .~y (.~z w)
    (f x).y
    (C).x
    (M.C).M.x
    (C).`x
    (C) \ `x
    (C) x
    (`A) x
    C (D x)
    C (f x)
    `A (`B x)
    C ((a, b))
    C ((a, b), c)
    C (let x = 1 in x)
    ((a, b), c)
    (a, (b, c))
    ((a; b), c)
    (a; b);
    c
    a;
    b;
    c
    ((let x = 1 in x), y)
    (a, let x = 1 in x)
    (a, let x = 1 in x);
    y
    ((match x with A -> a), b)
    (match x with A -> a);
    b
    match x with
    | A -> (match y with B -> b)
    | C -> c
    match x with
    | A -> (let y = 1 in match y with B -> b)
    | C -> c
    match x with
    | A -> (a, match y with B -> b)
    | C -> c
    match x with
    | A ->
      let y = 1 in
      y
    | C -> c
    match x with
    | A ->
      match y with
      | B -> b
      | C -> c
    match (match x with A -> a) with B -> b
    let x =
      let y = 1 in
      y
    in
    x
    .~(.~e)
    .~(.~e.{x})
    .~(.~e).{x}
    .~(.~e.{x}).{y}
    .~r.x.{y; z = (1, 2)}
    .~(f x)
    .~f x
    (.~e).x
    .~e.{}
    {x = (a; b); M.y = let z = 1 in z; w = match a with B -> b}
    {`x = (a, b); `y = (match a with B -> b) | r}
    {`x = 1 | match r with A -> a}
    {(`A) with `x = 1}
    {(`A).`b with `x = 1}
    {(`A (f x)) with `x = 1}
    {C with `x = 1}
    {x = match a with B -> b; y = let z = 1 in z; w = 2}
    {x = match a with B -> (b; c); y = let z = 1 in (z; w)}
    (`A).`x
    f (`A).x (C).`y
    .~(C).x
    (.< x >.) \ `x
    {(f x) with `x = 1}
    {.~r with `x = 1; `y = (a; b)}
    {r.x with `x = 1}
    {r with x = 1; M.y = (a; b)}
    {(`A) with x = 1}
    {r.M.x with M.y = {s with z = 2}}
    f (r \ `x)
    (f r) \ `x
    .~r \ `x
    r.`x.y \ `z
    .< .< .~(.~x) >. >.
    (.< x >.).x
    f (x : int) (M.y : list['a])
    (-1, -2)
    Some (-1)
    f [%expect] [%expect {|a|}]
    a;
    b
    let _ x = a in
    b
    let _ : int = a in
    b
    fun x -> x
    fun x (y : int) _ ->
      fun z ->
        (x, y);
        z
    f (fun x -> x) (fun x -> x)
    f (fun x -> x) y
    (fun x -> x) y
    (fun x -> x).y
    C (fun x -> x)
    ((fun x -> x), y)
    (a, fun x -> x)
    (fun x -> x);
    y
    a;
    fun x -> x
    match x with
    | A -> fun y -> y
    | B -> b
    match x with A -> fun y -> y
    match (fun y -> y) with A -> a
    let f = fun x -> x in
    f
    {x = fun y -> y; z = fun y -> (y; y)}
    {`x = fun y -> y}
    {`x = fun y -> y | r}
    .~(fun x -> x)
    .< fun x -> .~(f .< x >.) >.
    |xxx}]
;;

let%expect_test "patterns" =
  print
    {|
let f x =
  match x with
  | _ -> 1
  | (a, (b, c)) -> 1
  | C ((a, b)) -> 1
  | C ((a, b), c) -> 1
  | C (D x) -> 1
  | C (-1) -> 1
  | `A (`B (-1)) -> 1
  | (-1, 'c', "s", true, ()) -> 1
  | {x = (a, b); M.y = C z} -> 1
  | {`x = a; `y = b} -> 1
  | {`x = a; ..} -> 1
  | {`x = a; ..rest} -> 1
  | {..} -> 1
  | {} -> 1
  | exception E -> 1
  | exception (E x) -> 1
  | exception M.E (a, b) -> 1
  | exception `A x -> 1
  | exception (a, b) -> 1
  | exception exception e -> 1
  | (exception e, exception C x) -> 1
  | C (exception e) -> 1
|};
  [%expect
    {|
    let f x =
      match x with
      | _ -> 1
      | a, (b, c) -> 1
      | C ((a, b)) -> 1
      | C ((a, b), c) -> 1
      | C (D x) -> 1
      | C (-1) -> 1
      | `A (`B (-1)) -> 1
      | -1, 'c', "s", true, () -> 1
      | {x = (a, b); M.y = C z} -> 1
      | {`x = a; `y = b} -> 1
      | {`x = a; ..} -> 1
      | {`x = a; ..rest} -> 1
      | {..} -> 1
      | {} -> 1
      | exception E -> 1
      | exception E x -> 1
      | exception M.E (a, b) -> 1
      | exception `A x -> 1
      | exception (a, b) -> 1
      | exception exception e -> 1
      | exception e, exception C x -> 1
      | C (exception e) -> 1
    |}]
;;

let%expect_test "long forms" =
  print
    {xx|
let f x = some_function with_a_long_argument (and_another one) (and_a_third two three)
let g x = Some_constructor (with_a_long_argument, and_another one, and_a_third two three)
let h = {first_field = some_value; second_field = another_value; third = f x y z}
let nested = {first_field = {inner = some_value; other = another_value}; second_field = 1}
let poly = {`first = some_value; `second = another_value; `third = yet_another | rest}
let update = {record with `first = some_value; `second = another_value; `third = f x}
let quote = .< some_function with_a_long_argument (.~(splice_this here) and_another) >.
let splice = .~(make_code something).{first = some_value; second = another_value; third}
let lets =
  let x = some_function with_a_long_argument (and_another one) (and_a_third two) in
  let y = match x with A -> 1 | B -> 2 in
  let z = {first_field = some_value; second_field = another_value; third = 1} in
  x; y; z
let deep = f (g (h (some_function with_a_long_argument (and_another one) (and_a_third two))))
type t = First_constructor of int | Second_constructor of string * list[int] | Third
type r = {first_field : int; second_field : list[string]; third_field : int -> int}
type a = {`first : int; `second : string; `third : int -> int -> int; ..'r} -> int
let%expect_test "a test" =
  print_endline (String.append "some long string" (String.append "another" "more"));
  [%expect {|
    some long string
    |}];
  print_endline "x";
  [%expect {| x |}];
  [%expect]
|xx};
  [%expect
    {xxx|
    let f x =
      some_function with_a_long_argument (and_another one) (and_a_third two three)

    let g x =
      Some_constructor
        (with_a_long_argument, and_another one, and_a_third two three)

    let h =
      {first_field = some_value; second_field = another_value; third = f x y z}

    let nested =
      {first_field = {inner = some_value; other = another_value}; second_field = 1}

    let poly =
      {`first = some_value; `second = another_value; `third = yet_another | rest}

    let update =
      {record with `first = some_value; `second = another_value; `third = f x}

    let quote =
      .< some_function with_a_long_argument (.~(splice_this here) and_another) >.

    let splice =
      .~(make_code something).{first = some_value; second = another_value; third}

    let lets =
      let x =
        some_function with_a_long_argument (and_another one) (and_a_third two)
      in
      let y =
        match x with
        | A -> 1
        | B -> 2
      in
      let z = {first_field = some_value; second_field = another_value; third = 1} in
      x;
      y;
      z

    let deep =
      f
        (g
           (h
              (some_function
                 with_a_long_argument
                 (and_another one)
                 (and_a_third two))))

    type t =
      | First_constructor of int
      | Second_constructor of string * list[int]
      | Third

    type r = {
      first_field : int;
      second_field : list[string];
      third_field : int -> int;
    }

    type a =
      {`first : int; `second : string; `third : int -> int -> int; ..'r} -> int

    let%expect_test "a test" =
      print_endline
        (String.append "some long string" (String.append "another" "more"));
      [%expect {|
        some long string
        |}];
      print_endline "x";
      [%expect {| x |}];
      [%expect]
    |xxx}]
;;

let%expect_test "records after =, on the line, the next line, or docked" =
  print
    {|
let a = {x = 1; y = 2}
let h = {first_field = some_value; second_field = another_value; third = f x y z}
let docked = {first_field = some_value; second_field = another_value; third_field = f x y z}
let poly = {`first = some_value; `second = another_value; `third = yet_another_value | rest}
let update = {record with `first = some_value; `second = another_value; `third = f x y z}
let lets =
  let z = {first_field = some_value; second_field = another_value; third = 1} in
  let z = {first_field = some_value; second_field = another_value; third = 1, 2} in
  {first_field = {inner = some_value; other = another_value}; second = {a = 1; b = 2}}
|};
  [%expect
    {|
    let a = {x = 1; y = 2}

    let h =
      {first_field = some_value; second_field = another_value; third = f x y z}

    let docked = {
      first_field = some_value;
      second_field = another_value;
      third_field = f x y z;
    }

    let poly = {
      `first = some_value;
      `second = another_value;
      `third = yet_another_value | rest
    }

    let update =
      {record with `first = some_value; `second = another_value; `third = f x y z}

    let lets =
      let z = {first_field = some_value; second_field = another_value; third = 1} in
      let z =
        {first_field = some_value; second_field = another_value; third = (1, 2)}
      in
      {
        first_field = {inner = some_value; other = another_value};
        second = {a = 1; b = 2};
      }
    |}]
;;

let%expect_test "anonymous functions" =
  print
    {|
let f = List.map (fun element -> some_function element (another_function element)) list
let g = fun (a_long_parameter : int) (another_long_parameter : string) (third : bool) -> body
let h = fun a -> fun b -> match a with A -> b | B -> fun c -> c
|};
  [%expect
    {|
    let f =
      List.map
        (fun element -> some_function element (another_function element))
        list

    let g =
      fun
          (a_long_parameter : int)
          (another_long_parameter : string)
          (third : bool) ->
        body

    let h =
      fun a ->
        fun b ->
          match a with
          | A -> b
          | B -> fun c -> c
    |}]
;;

let%expect_test "constructor chains" =
  print
    {|
let s = S (S (S (S Z)))
let c = Some_constructor (Other_constructor (with_a_long_argument, and_another one, and_a_third two three))
let v = `Some_tag (`Other_tag (`Third_tag (Fourth (with_a_long_argument, and_another one))))
let t = Some_constructor (Other_constructor ((with_a_long_argument, and_another one, and_a_third two)))
let f = Some_constructor (some_function with_a_long_argument (and_another one) (and_a_third two))
let l = Lit (Lit.Int n)
|};
  [%expect
    {|
    let s = S (S (S (S Z)))

    let c =
      Some_constructor (Other_constructor
        (with_a_long_argument, and_another one, and_a_third two three))

    let v =
      `Some_tag (`Other_tag (`Third_tag (Fourth
        (with_a_long_argument, and_another one))))

    let t =
      Some_constructor (Other_constructor
        ((with_a_long_argument, and_another one, and_a_third two)))

    let f =
      Some_constructor
        (some_function with_a_long_argument (and_another one) (and_a_third two))

    let l = Lit (Lit.Int n)
    |}];
  let rec nat n = if n = 0 then "Z" else [%string "S (%{nat (n - 1)})"] in
  print ~width:20 ("let deep = " ^ nat 12);
  [%expect
    {|
    let deep =
      S (S (S (S (S (S (S (S (S (S (S (S
        Z)))))))))))
    |}]
;;

let%expect_test "long parameter lists" =
  print
    {|
let some_function (first_argument : int) (second_argument : list[string]) third : int = first_argument
let f x y = let some_local_function first_argument second_argument (third : int) : int = x in f
|};
  [%expect
    {|
    let some_function
        (first_argument : int)
        (second_argument : list[string])
        third : int =
      first_argument

    let f x y =
      let some_local_function first_argument second_argument (third : int) : int =
        x
      in
      f
    |}]
;;

let%expect_test "narrow" =
  print
    ~width:30
    {|
let f x = match g x with A (a, b) -> h a b | B -> some_function with_an_argument
let r = {a = f x y; b = .< g .~(h x) >.; c = C (x, y, z)}
|};
  [%expect
    {|
    let f x =
      match g x with
      | A (a, b) -> h a b
      | B ->
        some_function
          with_an_argument

    let r = {
      a = f x y;
      b = .< g .~(h x) >.;
      c = C (x, y, z);
    }
    |}]
;;

let%expect_test "modules and spacing" =
  print
    {|
let a = 1
let b = 2
type t = A
type u = B
let c = 3
module M = struct
  let x = 1
  let y = 2
  module N = struct end
  let z = match x with A -> 1 | B -> 2
  type t = int
end
let%expect_test "t" = [%expect]
let%expect_test "u" = [%expect]
|};
  [%expect
    {|
    let a = 1
    let b = 2

    type t = A
    type u = B

    let c = 3

    module M = struct
      let x = 1
      let y = 2

      module N = struct end

      let z =
        match x with
        | A -> 1
        | B -> 2

      type t = int
    end

    let%expect_test "t" = [%expect]

    let%expect_test "u" = [%expect]
    |}]
;;

(* Every [.stag] file of the project, which [dune] copies here. *)
let stag_files () =
  let rec walk dir =
    Stdlib.Sys.readdir dir
    |> Array.to_list
    |> List.sort ~compare:String.compare
    |> List.concat_map ~f:(fun name ->
      let path = dir ^/ name in
      if Stdlib.Sys.is_directory path
      then walk path
      else if String.is_suffix name ~suffix:".stag"
      then [ path ]
      else [])
  in
  List.concat_map
    [ "tests"; "examples"; "prelude"; "core"; "bench" ]
    ~f:(fun dir -> walk ("../.." ^/ dir))
;;

let%expect_test "the project's sources round trip and are formatted" =
  List.iter (stag_files ()) ~f:(fun path ->
    let source = In_channel.read_all path in
    let program = parse source in
    Round_trip.program ~widths:[ 80; 40; 20; 1 ] program;
    Round_trip.source ~widths:[ 80; 40; 20; 1 ] source;
    let formatted =
      match Formatter.format ~file:path source with
      | Ok formatted -> String.equal formatted source
      | Error _ -> false
    in
    let long_lines =
      String.split_lines (Printer.program program)
      |> List.count ~f:(fun line -> String.length line > 80)
    in
    print_s [%message path (formatted : bool) (long_lines : int)]);
  [%expect
    {|
    (../../tests/basics.stag (formatted true) (long_lines 0))
    (../../tests/core_basics.stag (formatted true) (long_lines 0))
    (../../tests/core_derive_sexp.stag (formatted true) (long_lines 10))
    (../../tests/core_exn.stag (formatted true) (long_lines 1))
    (../../tests/core_list.stag (formatted true) (long_lines 0))
    (../../tests/core_option.stag (formatted true) (long_lines 0))
    (../../tests/core_sexp.stag (formatted true) (long_lines 1))
    (../../tests/core_shadowing.stag (formatted true) (long_lines 0))
    (../../tests/core_string.stag (formatted true) (long_lines 0))
    (../../tests/evaluation.stag (formatted true) (long_lines 1))
    (../../tests/list_literals.stag (formatted true) (long_lines 0))
    (../../examples/calc/bin/main.stag (formatted true) (long_lines 0))
    (../../examples/calc/fmt/show.stag (formatted true) (long_lines 0))
    (../../examples/calc/math/arith.stag (formatted true) (long_lines 0))
    (../../examples/calc/math/power.stag (formatted true) (long_lines 0))
    (../../examples/calc/test/calc_test.stag (formatted true) (long_lines 0))
    (../../prelude/prelude.stag (formatted true) (long_lines 0))
    (../../core/bool.stag (formatted true) (long_lines 0))
    (../../core/char.stag (formatted true) (long_lines 0))
    (../../core/int.stag (formatted true) (long_lines 0))
    (../../core/list.stag (formatted true) (long_lines 0))
    (../../core/option.stag (formatted true) (long_lines 0))
    (../../core/result.stag (formatted true) (long_lines 0))
    (../../core/sexp.stag (formatted true) (long_lines 1))
    (../../core/string.stag (formatted true) (long_lines 0))
    (../../bench/programs/fib.stag (formatted true) (long_lines 0))
    (../../bench/programs/matching.stag (formatted true) (long_lines 0))
    (../../bench/programs/power.stag (formatted true) (long_lines 0))
    (../../bench/programs/sort.stag (formatted true) (long_lines 0))
    (../../bench/programs/strings.stag (formatted true) (long_lines 0))
    |}]
;;

let%expect_test "syntax the parser does not produce" =
  let e txt : Syntax.Expr.t = Located.create txt in
  let x = e (Var (Path (Syntax.Path.name "x"), None)) in
  let c = Located.create (Syntax.Decl_path.Path (Syntax.Path.name "C")) in
  let show expr =
    let printed = Printer.expr expr in
    let reparsed =
      match Parser.expr ~file:"t.stag" printed with
      | Ok parsed -> Round_trip.strip_locs (Syntax.Expr.sexp_of_t parsed)
      | Error error -> Atom (Parser.Error.to_string error)
    in
    print_s [%message printed (reparsed : Sexp.t)]
  in
  show (e (Tuple [ x ]));
  show (e (Tuple []));
  show (e (Record []));
  show (e (Construct (c, [ e (Tuple [ x; x ]) ])));
  show (e (App (x, e (Tuple [ e (Tuple [ x; x ]) ]))));
  show (e (Expect (Located.create "a |} b |x} c")));
  [%expect
    {xxx|
    (x (reparsed (Var x ())))
    ("()" (reparsed (Lit Unit)))
    ({} (reparsed (Poly_record ())))
    ("C ((x, x))" (reparsed (Construct C ((Tuple ((Var x ()) (Var x ())))))))
    ("x (x, x)" (reparsed (App (Var x ()) (Tuple ((Var x ()) (Var x ()))))))
    ("[%expect {xx|a |} b |x} c|xx}]" (reparsed (Expect "a |} b |x} c")))
    |xxx}]
;;

let%expect_test "types as values and declaration splices" =
  print
    {|
let types = (type t, f (type M.t[int]) (type (int -> t)), C (type (a * b)), (type 'a).x, [%expect])
module M = .~(gen (type t))
include .~Gen.decls
include .~(.~x)
module N = .~((.< x >.))
|};
  [%expect
    {|
    let types =
      (type t,
       f (type M.t[int]) (type (int -> t)),
       C (type (a * b)),
       (type 'a).x,
       [%expect])

    module M = .~(gen (type t))

    include .~Gen.decls
    include .~(.~x)

    module N = .~(.< x >.)
    |}]
;;

let%expect_test "resolved paths are printed as root paths" =
  let at txt = Located.create txt in
  let resolved name = at (Syntax.Decl_path.Resolved name) in
  let ty name args = at (Syntax.Type_expr.Con (resolved name, args)) in
  let int = ty "prelude.int" [] in
  let option = ty "prelude.option" [ Ty int ] in
  let t = ty "Gen.M.t" [] in
  let x = at (Syntax.Expr.Var (Path (Syntax.Path.name "x"), None)) in
  let program : Syntax.Program.t =
    [ at
        (Syntax.Toplevel.Let
           ( at "f"
           , [ at "r", Some option ]
           , Some t
           , at
               (Syntax.Expr.Match
                  ( at (Syntax.Expr.Field (x, resolved "Gen.M.field"))
                  , [ ( at
                          (Syntax.Pattern.Construct
                             ( resolved "prelude.Some"
                             , [ at
                                   (Syntax.Pattern.Record
                                      [ ( resolved "Gen.M.x"
                                        , at Syntax.Pattern.Any )
                                      ])
                               ] ))
                      , at
                          (Syntax.Expr.Construct
                             (resolved "Gen.M.A", [ at (Syntax.Expr.Type t) ]))
                      )
                    ] )) ))
    ]
  in
  print_string (Printer.program program);
  Round_trip.program program;
  [%expect
    {|
    let f (r : .option[.int]) : .Gen.M.t =
      match x.(.Gen.M.field) with .Some {.Gen.M.x = _} -> .Gen.M.A (type .Gen.M.t)
    |}]
;;

let%expect_test "resolved paths with no root path" =
  let var name =
    Located.create (Syntax.Expr.Var (Syntax.Decl_path.Resolved name, None))
  in
  List.iter
    [ "prelude.Core.Int.to_string"
    ; "prelude.Prim.String.length"
    ; "prelude.raise"
    ; "prelude.Intrinsic.raise"
    ; "Core.x"
    ]
    ~f:(fun name ->
      printf
        "%s: %s, %s\n"
        name
        (Printer.expr (var name))
        (Printer.expr
           ~root_modules:(String.Set.of_list [ "Core"; "String" ])
           (var name)));
  [%expect
    {|
    prelude.Core.Int.to_string: .Core.Int.to_string, <prelude.Core.Int.to_string>
    prelude.Prim.String.length: .String.length, <prelude.String.length>
    prelude.raise: .raise, .raise
    prelude.Intrinsic.raise: <prelude.Intrinsic.raise>, <prelude.Intrinsic.raise>
    Core.x: .Core.x, .Core.x
    |}]
;;

let%expect_test "sequences in parentheses stay on one line only if all fits" =
  print
    {|let f x =
  (match x with a, b -> g a; h b);
  (match partial_twice with a, b -> print_pair a; print_string " "; print_pair b);
  (match x with A -> a; b | B -> c);
  (let x = 1 in g x; h x);
  ((a; b), c)
|};
  [%expect
    {|
    let f x =
      (match x with a, b -> g a; h b);
      (match partial_twice with
       | a, b ->
         print_pair a;
         print_string " ";
         print_pair b);
      (match x with
       | A ->
         a;
         b
       | B -> c);
      (let x = 1 in g x; h x);
      ((a; b), c)
    |}]
;;

let%expect_test "benches" =
  print
    {|
let%bench "fib" = fib 20
let%bench "sort" n in [100; 1000] = sort (random_list n)
let%bench "a long benchmark name" some_parameter in [first_argument_value; (second, argument); let x = 1 in x; match x with A -> 1 | B -> 2] = some_function some_parameter (and_another_argument some_parameter)
|};
  [%expect
    {|
    let%bench "fib" = fib 20

    let%bench "sort" n in [100; 1000] = sort (random_list n)

    let%bench "a long benchmark name" some_parameter in [
      first_argument_value;
      (second, argument);
      let x = 1 in
      x;
      match x with
      | A -> 1
      | B -> 2;
    ] =
      some_function some_parameter (and_another_argument some_parameter)
    |}];
  print
    ~width:30
    {|let%bench "sort" n in [100; 1000; fun x -> x] = sort (random_list n)|};
  [%expect
    {|
    let%bench "sort" n in [
      100;
      1000;
      fun x -> x;
    ] =
      sort (random_list n)
    |}]
;;

let%expect_test "inline expansions" =
  print
    {|
include%inline .~(gen (type t)) type t = A let x = 1 let y = 2 end%inline
include%inline .~e end%inline
module M = struct
  module%inline N = .~(e) let x = some_function with_a_long_argument and_another_long_argument end%inline
end
|};
  [%expect
    {|
    include%inline .~(gen (type t))
      type t = A

      let x = 1
      let y = 2
    end%inline

    include%inline .~e
    end%inline

    module M = struct
      module%inline N = .~e
        let x = some_function with_a_long_argument and_another_long_argument
      end%inline
    end
    |}]
;;

let%expect_test "list literals" =
  print
    {|let a = []
let b = [x]
let c = [[1]; []; [2; 3;]]
let d = [1, "one"; 2, "two"]
let e = Cons (x, Cons (y, Nil))
let f = Cons (x, [y])
let g l = match l with [] -> [] | [x] -> [x] | [[x]; (y, z)] -> f [x] [] | Cons (x, [_; _]) -> C [x]
let h = [let x = 1 in x; fun x -> x; match x with A -> 1 | B -> 2; 3].x
let long_name = [first_element; second_element; third_element; fourth_element]
let long = {field = [first_element; second_element; third_element; fourth_element]}
let nested = [[first_element; second_element]; [third_element; fourth_element; fifth]]
|};
  [%expect
    {|
    let a = []
    let b = [x]
    let c = [[1]; []; [2; 3]]
    let d = [(1, "one"); (2, "two")]
    let e = Cons (x, Cons (y, Nil))
    let f = Cons (x, [y])

    let g l =
      match l with
      | [] -> []
      | [x] -> [x]
      | [[x]; (y, z)] -> f [x] []
      | Cons (x, [_; _]) -> C [x]

    let h =
      [
        let x = 1 in
        x;
        fun x -> x;
        match x with
        | A -> 1
        | B -> 2;
        3;
      ].x

    let long_name = [first_element; second_element; third_element; fourth_element]

    let long =
      {field = [first_element; second_element; third_element; fourth_element]}

    let nested =
      [[first_element; second_element]; [third_element; fourth_element; fifth]]
    |}];
  print
    ~width:20
    {|let f = function_name [first; second] [third] (fun x -> [x; x; x; x; x; x])|};
  [%expect
    {|
    let f =
      function_name
        [first; second]
        [third]
        (fun x ->
           [
             x;
             x;
             x;
             x;
             x;
             x;
           ])
    |}]
;;

let%expect_test "resolved Cons and Nil not ending a list literal" =
  let at txt = Located.create txt in
  let cons args = at (Syntax.Expr.Construct (at List_literal.cons, args)) in
  let nil = at (Syntax.Expr.Construct (at List_literal.nil, [])) in
  let var name = at (Syntax.Expr.Var (Path (Syntax.Path.name name), None)) in
  let pcons args = at (Syntax.Pattern.Construct (at List_literal.cons, args)) in
  let pvar name = at (Syntax.Pattern.Var name) in
  let show e =
    Round_trip.expr e;
    print_endline (Printer.expr e)
  in
  show (cons [ var "x"; var "rest" ]);
  show (cons [ var "x"; cons [ var "y"; var "rest" ] ]);
  show (cons [ var "x"; cons [ nil; nil ] ]);
  show (cons [ var "x" ]);
  show (cons [ var "x"; nil; nil ]);
  show (at (Syntax.Expr.Construct (at List_literal.nil, [ var "x" ])));
  show
    (at
       (Syntax.Expr.Field
          ( cons [ var "x"; var "rest" ]
          , at (Syntax.Decl_path.Path (Syntax.Path.name "f")) )));
  let p = pcons [ pvar "x"; pcons [ pvar "y"; pvar "rest" ] ] in
  Round_trip.pattern p;
  print_endline (Printer.pattern p);
  [%expect
    {|
    .Cons (x, rest)
    .Cons (x, .Cons (y, rest))
    [x; []]
    .Cons x
    .Cons (x, [], [])
    .Nil x
    (.Cons (x, rest)).f
    .Cons (x, .Cons (y, rest))
    |}]
;;
