open! Import

let parse source =
  match Parser.program ~file:"t.stag" source with
  | Ok program -> program
  | Error error -> raise_s [%sexp (Parser.Error.to_string error : string)]
;;

let print ?(width = 80) source =
  let program = parse source in
  Round_trip.program program;
  print_string (Printer.program ~width program)
;;

let%expect_test "struct quotes" =
  print
    {|
let ds = .< struct type t = .~(variants names) let f (x : .~ty) = .~body .~items end >.
type t = .~(Gen.record fields)
let x : .~ty = (e : .~(Stag.Type_expr.con "int" Nil))
let nested = .< struct let q = .< struct .~a end >. include .~(gen .~names) end >.
let e = .< struct end >.
|};
  [%expect {| |}]
;;
