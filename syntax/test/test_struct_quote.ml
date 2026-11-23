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

let%expect_test "struct quotes and type splices" =
  print
    {|
let ds = .< struct type t = .~(variants names) let f (x : .~ty) = .~body include .~items end >.
type t = .~(Gen.record fields)
let x : .~ty = (e : .~(Stag.Type_expr.con "int" Nil))
let nested = .< struct let q = .< struct module M = .~a end >. include .~(gen .~names) end >.
let e = .< struct end >.
let types = (type .~t, type list[.~t], f (type .~t))
|};
  [%expect
    {|
    let ds =
      .< struct
        type t = .~(variants names)

        let f (x : .~ty) = .~body

        include .~items
      end >.

    type t = .~(Gen.record fields)

    let x : .~ty = (e : .~(Stag.Type_expr.con "int" Nil))

    let nested =
      .< struct
        let q =
          .< struct
            module M = .~a
          end >.

        include .~(gen .~names)
      end >.

    let e = .< struct end >.
    let types = (type .~t, type list[.~t], f (type .~t))
    |}]
;;

(* The expression a struct quote is sugar for, printed. *)
let expand source =
  match Parser.expr ~file:"t.stag" source with
  | Error error -> print_endline (Parser.Error.to_string error)
  | Ok { txt = Syntax.Expr.Struct_quote toplevels; loc } ->
    (match Struct_quote.expand toplevels ~loc with
     | Ok expr -> print_endline (Printer.expr ~width:80 expr)
     | Error (_, message) -> print_endline message)
  | Ok _ -> print_endline "not a struct quote"
;;

let%expect_test "expansion" =
  expand {|.< struct type t = .~kind let x : .~ty = .~e include .~items end >.|};
  [%expect
    {|
    let antiquote_0 : .Stag.Type_decl.Kind.t = kind in
    let antiquote_1 : .Stag.Type_expr.t = ty in
    let antiquote_2 : .Stag.Expr.t = e in
    let antiquote_3 : .list[.Stag.Toplevel.t] = items in
    .Cons
      (.Ptr.make
         (.Stag.Toplevel.Type {`name = "t"; `params = []; `kind = antiquote_0},
          .Cons
            (.Ptr.make
               (.Stag.Toplevel.Let
                  ("x", [], {`ty = .Some antiquote_1; `mode = .None}, antiquote_2),
                antiquote_3))))
    |}];
  expand {|.< struct let q = .< .~x >. module M = .~items end >.|};
  [%expect
    {|
    let antiquote_0 : .list[.Stag.Toplevel.t] = items in
    [
      .Stag.Toplevel.Let
        ("q",
         [],
         {`ty = .None; `mode = .None},
         .Stag.Expr.Quote
           (.Ptr.make
              (.Stag.Expr.Splice
                 (.Ptr.make
                    {
                      `expr =
                        .Stag.Expr.Var
                          (.Stag.Path.Name "x", {`ty = .None; `mode = .None});
                      `subs = .None;
                    }))));
      .Stag.Toplevel.Module ("M", antiquote_0);
    ]
    |}];
  expand {|.< struct include%inline .~e end%inline end >.|};
  [%expect
    {| A struct quote can't hold an inline expansion: generated declarations have none |}];
  expand {|.< struct let x = .~e.{y} end >.|};
  [%expect
    {| An antiquote of a struct quote takes no substitutions: write `.~e`, of a `Stag.Expr.t` |}]
;;
