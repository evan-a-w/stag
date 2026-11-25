open! Import
open Syntax

let bump_lits ~into_expansions program =
  let bump : Lit.t -> Lit.t = function
    | Int n -> Int (n + 1)
    | lit -> lit
  in
  let default =
    Traverse.Visitor.default ~enter:(fun () _ -> ()) ~into_expansions
  in
  let visitor =
    { default with
      expr =
        (fun v () _ e ->
          match e.txt with
          | Lit lit -> { e with txt = Lit (bump lit) }
          | _ -> Traverse.expr v () e)
    ; pattern =
        (fun v () p ->
          match p.txt with
          | Lit lit -> { p with txt = Lit (bump lit) }
          | _ -> Traverse.pattern v () p)
    }
  in
  List.map program ~f:(visitor.toplevel visitor ())
;;

(* Every literal, of an expression or a pattern, wherever it is. *)
let rec bump_sexp_lits (sexp : Sexp.t) : Sexp.t =
  match sexp with
  | List [ Atom "Lit"; List [ Atom "Int"; Atom n ] ] ->
    List
      [ Atom "Lit"
      ; List [ Atom "Int"; Atom (Int.to_string (Int.of_string n + 1)) ]
      ]
  | Atom _ -> sexp
  | List sexps -> List (List.map sexps ~f:bump_sexp_lits)
;;

let%expect_test "the traversal visits every expression and pattern" =
  let random = Random.State.make [| 3 |] in
  let differing =
    List.init 2000 ~f:(fun size ->
      let gen = { Gen.random; size = size % 60; deep = false } in
      List.init 3 ~f:(fun _ -> Gen.toplevel gen))
    |> List.filter ~f:(fun program ->
      not
        (Sexp.equal
           [%sexp (bump_lits ~into_expansions:true program : Program.t)]
           (bump_sexp_lits [%sexp (program : Program.t)])))
  in
  print_s [%sexp (List.length differing : int)];
  [%expect {| 0 |}]
;;

let%expect_test "children with how their scope differs" =
  (* The context is the quote depth and the locals in scope, of each child. *)
  let show source =
    match Parser.program ~file:"t.stag" source with
    | Error error -> print_endline (Parser.Error.to_string error)
    | Ok program ->
      let default =
        Traverse.Visitor.default
          ~enter:(fun (depth, locals) (child : Traverse.Child.t) ->
            depth + child.depth, locals @ child.binds)
          ~into_expansions:true
      in
      let visitor =
        { default with
          expr =
            (fun v ctx (child : Traverse.Child.t) e ->
              let depth, locals = ctx in
              print_s
                [%message
                  (Printer.expr e : string)
                    (depth : int)
                    (locals : string list)
                    ~splice:
                      (Option.map child.splice ~f:(fun s -> s.kind)
                       : Traverse.Splice.Kind.t option)];
              Traverse.expr v ctx e)
        }
      in
      ignore
        (List.map program ~f:(visitor.toplevel visitor (0, [])) : Program.t)
  in
  show "let a = .< .~(f x) >.";
  [%expect
    {|
    (("Printer.expr e" ".< .~(f x) >.") (depth 0) (locals ()) (splice ()))
    (("Printer.expr e" ".~(f x)") (depth 1) (locals ()) (splice ()))
    (("Printer.expr e" "f x") (depth 0) (locals ()) (splice (Expr)))
    (("Printer.expr e" f) (depth 0) (locals ()) (splice ()))
    (("Printer.expr e" x) (depth 0) (locals ()) (splice ()))
    |}];
  show "let a = .< struct let x = 1 module M = struct let y = x end end >.";
  [%expect
    {|
    (("Printer.expr e"
       ".< struct\
      \n  let x = 1\
      \n\
      \n  module M = struct\
      \n    let y = x\
      \n  end\
      \nend >.")
     (depth 0) (locals ()) (splice ()))
    (("Printer.expr e" 1) (depth 1) (locals ()) (splice ()))
    (("Printer.expr e" x) (depth 1) (locals (x)) (splice ()))
    |}];
  show
    "let f x = let g y = (x, y) in match g x with (a, b) -> fun c -> (a, b, c)";
  [%expect
    {|
    (("Printer.expr e"
       "let g y = (x, y) in\
      \nmatch g x with a, b -> fun c -> (a, b, c)")
     (depth 0) (locals (x)) (splice ()))
    (("Printer.expr e" "(x, y)") (depth 0) (locals (x g y)) (splice ()))
    (("Printer.expr e" x) (depth 0) (locals (x g y)) (splice ()))
    (("Printer.expr e" y) (depth 0) (locals (x g y)) (splice ()))
    (("Printer.expr e" "match g x with a, b -> fun c -> (a, b, c)") (depth 0)
     (locals (x g)) (splice ()))
    (("Printer.expr e" "g x") (depth 0) (locals (x g)) (splice ()))
    (("Printer.expr e" g) (depth 0) (locals (x g)) (splice ()))
    (("Printer.expr e" x) (depth 0) (locals (x g)) (splice ()))
    (("Printer.expr e" "fun c -> (a, b, c)") (depth 0) (locals (x g a b))
     (splice ()))
    (("Printer.expr e" "(a, b, c)") (depth 0) (locals (x g a b c)) (splice ()))
    (("Printer.expr e" a) (depth 0) (locals (x g a b c)) (splice ()))
    (("Printer.expr e" b) (depth 0) (locals (x g a b c)) (splice ()))
    (("Printer.expr e" c) (depth 0) (locals (x g a b c)) (splice ()))
    |}];
  show "include%inline .~e let z = 1 end%inline";
  [%expect
    {|
    (("Printer.expr e" e) (depth -1) (locals ()) (splice (Item)))
    (("Printer.expr e" 1) (depth 0) (locals ()) (splice ()))
    |}]
;;
