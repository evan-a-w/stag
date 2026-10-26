open! Core
open Stag
open Syntax
open Helpers

let prim_names =
  List.filter_map prims ~f:(function
    | Toplevel.Let (name, _, _, _) -> Some name
    | Type _ | Module _ -> None)
;;

let sub_exprs (expr : Typed.Expr.t) =
  let snds = List.map ~f:snd in
  match expr.desc with
  | Lit _ | Var _ | Poly_variant (_, None) -> []
  | App (a, b) | Let { body = a; in_ = b; _ } -> [ a; b ]
  | Splice { code; subs = Explicit subs } -> code :: snds subs
  | Splice { code; subs = Implicit _ } -> [ code ]
  | Quote e
  | Field (e, _)
  | Poly_field (e, _)
  | Poly_restrict (e, _)
  | Poly_variant (_, Some e) -> [ e ]
  | Tuple es | Construct (_, es) -> es
  | Record fields -> snds fields
  | Poly_record fields -> snds fields
  | Poly_extend (fields, e) | Poly_update (e, fields) -> e :: snds fields
  | Match (e, cases) -> e :: snds cases
;;

(* The captures of each closure in [program]: its toplevel functions and the
   local functions within them. *)
let print program =
  match
    Typecheck.typecheck (Typecheck.create ()) ~program:(prims @ program)
  with
  | Error error -> print_s [%sexp (error : Typecheck_error.t)]
  | Ok { program; _ } ->
    let t = Treewalk.create' ~program in
    let print_captures ~ident ~args ~body =
      let captures =
        Treewalk.captures t ~ident ~args ~body
        |> Hash_set.to_list
        |> List.sort ~compare:Typed.Ident.compare
      in
      print_s [%sexp (ident : Typed.Ident.t), (captures : Typed.Ident.t list)]
    in
    let rec locals (expr : Typed.Expr.t) =
      (match expr.desc with
       | Let { ident; args = _ :: _ as args; body; _ } ->
         print_captures ~ident ~args ~body
       | _ -> ());
      List.iter (sub_exprs expr) ~f:locals
    in
    List.iter program.sccs ~f:(fun scc ->
      List.iter scc.members ~f:(fun (member : Typed.Toplevel.t) ->
        if not (List.mem prim_names member.ident.name ~equal:String.equal)
        then (
          if not (List.is_empty member.args)
          then
            print_captures
              ~ident:member.ident
              ~args:member.args
              ~body:member.body;
          locals member.body)))
;;

let%expect_test "arguments, locals and toplevels" =
  print
    [ top "k" [] (int 1)
    ; top
        "g"
        [ "a"; "b" ]
        (let_
           "f"
           [ "x" ]
           (app (var "add") [ var "x"; var "a" ])
           (let_
              "unused"
              []
              (var "b")
              (let_
                 "h"
                 [ "y" ]
                 (app (var "add") [ app (var "f") [ var "y" ]; var "k" ])
                 (int 0))))
    ];
  [%expect
    {|
    (g/5 (add/0 k/4))
    (f/18 (a/15 add/0))
    (h/21 (add/0 f/18 k/4))
    |}]
;;

let%expect_test "recursion" =
  print
    [ top "loop" [ "x" ] (app (var "loop") [ var "x" ])
    ; top "even" [ "x" ] (app (var "odd") [ var "x" ])
    ; top "odd" [ "x" ] (app (var "even") [ var "x" ])
    ; top
        "local"
        [ "n" ]
        (let_ "go" [ "x" ] (app (var "go") [ var "n" ]) (var "go"))
    ];
  [%expect
    {|
    (loop/4 (loop/4))
    (even/5 (odd/6))
    (odd/6 (even/5))
    (local/7 ())
    (go/22 (go/22 n/20))
    |}]
;;

let%expect_test "pattern binders are not captures" =
  print
    [ top
        "f"
        [ "p"; "c" ]
        (let_
           "g"
           [ "q" ]
           (match_
              (var "q")
              [ ( Pat.tuple [ Pat.var "x"; Pat.any ]
                , app (var "add") [ var "x"; var "c" ] )
              ])
           (let_
              "h"
              [ "r" ]
              (match_
                 (var "r")
                 [ ( Pat.poly_record ~rest:(Bind "rest") [ "a", Pat.var "a" ]
                   , tuple [ var "a"; var "rest"; var "p" ] )
                 ])
              (let_
                 "v"
                 [ "w" ]
                 (match_
                    (var "w")
                    [ Pat.tag "A" ~arg:(Pat.var "z"), var "z"
                    ; Pat.var "other", var "c"
                    ])
                 (int 0))))
    ];
  [%expect
    {|
    (f/4 (add/0))
    (g/17 (add/0 c/15))
    (h/20 (p/14))
    (v/24 (c/15))
    |}]
;;

let%expect_test "poly records and variants" =
  print
    [ top
        "f"
        [ "r"; "s"; "x" ]
        (let_
           "g"
           [ "u" ]
           (tuple
              [ extend [ "a", var "x" ] (var "r")
              ; update (var "s") [ "b", var "u" ]
              ; restrict (var "r") "c"
              ; dot (var "s") "d"
              ; poly_record [ "e", var "x" ]
              ; tag "T" ~arg:(var "x")
              ])
           (int 0))
    ];
  [%expect
    {|
    (f/4 ())
    (g/18 (r/14 s/15 x/16))
    |}]
;;

let%expect_test "quotes and splices" =
  print
    [ top
        "q"
        [ "c" ]
        (quote
           (let_
              "inner"
              [ "z" ]
              (app (var "add") [ var "z"; splice (var "c") ])
              (int 0)))
    ; top "h" [ "u" ] (quote (let_ "w" [] (int 1) (splice (var "u"))))
    ; top
        "build"
        [ "c" ]
        (let_
           "mk"
           [ "x" ]
           (quote (app (var "add") [ splice (var "c"); var "x" ]))
           (int 0))
    ; top
        "persistent"
        [ "n" ]
        (let_
           "k"
           [ "y" ]
           (var "y")
           (quote
              (let_
                 "g"
                 [ "x" ]
                 (app (var "k") [ var "x" ])
                 (app (var "g") [ int 1 ]))))
    ; top
        "explicit"
        [ "c"; "d" ]
        (let_
           "e"
           [ "x" ]
           (quote (splice ~subs:[ "y", Some (splice (var "d")) ] (var "c")))
           (int 0))
    ];
  [%expect
    {|
    (q/4 (add/0))
    (inner/20 (add/0 inner/20))
    (h/5 ())
    (build/6 (add/0))
    (mk/25 (add/0 c/23))
    (persistent/7 ())
    (k/28 ())
    (g/30 (k/28))
    (explicit/8 ())
    (e/34 (c/31 d/32))
    |}]
;;

let%expect_test "toplevel splices" =
  print
    [ top "n" [] (int 3)
    ; top
        "f"
        [ "x" ]
        (let_
           "g"
           [ "y" ]
           (splice (quote (app (var "add") [ var "x"; var "y" ])))
           (int 0))
    ];
  [%expect
    {|
    (f/5 (add/0))
    (g/17 (add/0 x/15))
    |}]
;;
