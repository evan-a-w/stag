open! Import

(* The errors of [source], else its signature and values. *)
let check source =
  match Mode_helpers.located_errors source with
  | Error errors ->
    List.iter errors ~f:(fun (loc, message) ->
      printf "%s: %s\n" (Loc.to_string loc) message)
  | Ok () -> Source_helpers.run source
;;

let round_trip source =
  let program = Source_helpers.parse_exn source in
  print_string (Printer.program program);
  Round_trip.program program;
  print_s
    (Round_trip.strip_locs
       [%sexp (List.map program ~f:(fun t -> t.txt) : Syntax.Toplevel.desc list)])
;;

let%expect_test "parsing and printing" =
  round_trip
    {|
let nonrec f x = let nonrec x = Int.add x 1 in x
type nonrec t['a] = list[t['a]]
type nonrec u = | A | B
|};
  [%expect {| |}]
;;

let%expect_test "let mut is never recursive, so takes no nonrec" =
  let parse source =
    match Parser.program ~file:"t.stag" source with
    | Ok _ -> print_endline "parsed"
    | Error error -> print_endline (Parser.Error.to_string error)
  in
  parse {|let x = let mut nonrec y = 1 in y|};
  parse {|let x = let nonrec mut y = 1 in y|};
  parse {|let mut nonrec y = 1|};
  parse {|let nonrec mut y = 1|};
  parse {|let nonrec nonrec y = 1|};
  [%expect {| |}]
;;

let%expect_test "a nonrec local refers to the binding it shadows" =
  check
    {|
let s = "toplevel"
let main (u : unit) =
  let s = "abc" in
  let nonrec s = String.length s in
  s
let from_toplevel (u : unit) = let nonrec s = String.length s in s
let from_prelude (u : unit) = let nonrec not = not true in not
let function_ (u : unit) =
  let f x = Int.add x 1 in
  let nonrec f x = f (f x) in
  f 0
let result = (main (), from_toplevel (), from_prelude (), function_ ())
|};
  [%expect {| |}]
;;

let%expect_test "a nonrec toplevel refers to the outer one" =
  check
    {|
let n = 1
module M = struct
  let nonrec n = Int.add n 10
  let nonrec length s = Int.add (String.length s) n
end
let result = (M.n, M.length "ab")
|};
  [%expect {| |}]
;;

let%expect_test "a nonrec binding is not in scope in its definition" =
  check {|let main (u : unit) = let nonrec x = x in x|};
  check {|let main (u : unit) = let nonrec f x = f x in f|};
  check {|let nonrec y = y|};
  check {|let nonrec g x = g x|};
  [%expect {| |}]
;;

let%expect_test "a nonrec type refers to the outer type" =
  check
    {|
type t = A | B
module M = struct
  type nonrec t = list[t]
  let x : t = [A; B]
end
type nonrec option = option[int]
let y : option = Some 1
let result = (M.x, y)
|};
  check {|type nonrec v = list[v]|};
  [%expect {| |}]
;;

let%expect_test "the shadowing hint suggests let nonrec" =
  check {|let main (u : unit) = let s = "a" in let s = String.length s in s|};
  [%expect {| |}]
;;

let%expect_test "quotes of nonrec lets" =
  check
    {|
let code = .< fun (s : string) -> let nonrec s = String.length s in s >.
let result = .~code "abc"
let lifted = Stag.Code.erase .< fun (s : string) -> let nonrec s = String.length s in s >.
let%expect_test "generated" =
  print_endline (Core.Bool.to_string (match lifted with Stag.Expr.Fun *(_, Stag.Expr.Let_nonrec *(_, _, _, _, _)) -> true | _ -> false))
|};
  [%expect {| |}]
;;
