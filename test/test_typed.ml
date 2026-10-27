open! Core
open Stag
open Syntax
open Helpers

let prim_names = List.filter_map prims ~f:let_name

let typecheck_exn program =
  match Typecheck.typecheck (typechecker ()) ~program with
  | Ok { program; _ } -> program
  | Error error -> raise_s [%sexp (error : Typecheck_error.t)]
;;

let is_prim (ident : Typed.Ident.t) =
  List.mem prim_names ident.name ~equal:String.equal
;;

let toplevel_idents (program : Typed.Program.t) =
  List.concat_map program.sccs ~f:(fun scc ->
    List.map scc.members ~f:(fun member -> member.ident))
  |> Typed.Ident.Set.of_list
;;

let prim_locals =
  let program = typecheck_exn prims in
  Map.length program.ident_info - Set.length (toplevel_idents program)
;;

(* The components of [program] after those of [prims], and with [~idents]
   the types and levels of its binders. Toplevels get the first stamps, then prims'
   locals, as prims are checked first. *)
let print ?(idents = false) program =
  match Typecheck.typecheck (typechecker ()) ~program:(prims @ program) with
  | Error error -> print_s [%sexp (error : Typecheck_error.t)]
  | Ok { program; _ } ->
    List.iter program.sccs ~f:(fun (scc : Typed.Scc.t) ->
      if not (List.for_all scc.members ~f:(fun member -> is_prim member.ident))
      then print_s [%sexp (scc : Typed.Scc.t)]);
    if idents
    then (
      let toplevels = toplevel_idents program in
      print_s
        [%sexp
          (Map.filter_keys program.ident_info ~f:(fun ident ->
             if Set.mem toplevels ident
             then not (is_prim ident)
             else ident.stamp >= Set.length toplevels + prim_locals)
           |> Map.map ~f:(fun { Typed.Ident_info.ty; level } ->
             Type.to_string_hum ty, level)
           : (string * int) Typed.Ident.Map.t)])
;;

let%expect_test "lets and shadowing" =
  print
    ~idents:true
    [ top
        "f"
        [ "x" ]
        (let_
           "x"
           []
           (int 1)
           (match_
              (tuple [ var "x"; var "x" ])
              [ ( Pat.var "x"
                , let_ "g" [ "y" ] (var "y") (app (var "g") [ var "x" ]) )
              ]))
    ];
  [%expect
    {|
    ((members
      (((ident f/4) (args (x/14)) (scheme "'a -> int * int")
        (body
         (Let (ident x/15) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Match
            (Tuple
             ((Var (Bound (Local x/15)) : int) (Var (Bound (Local x/15)) : int))
             : "int * int")
            (((Var x/16 : "int * int")
              (Let (ident g/18) (args (y/17)) (generalized true)
               (body (Var (Bound (Local y/17)) : 'b))
               (in_
                (App (Var (Bound (Local g/18)) : "int * int -> int * int")
                 (Var (Bound (Local x/16)) : "int * int") : "int * int"))
               : "int * int")))
            : "int * int"))
          : "int * int")))))
     (recursive false))
    ((f/4 ("'a -> int * int" 0)) (g/18 ("'a -> 'a" 0)) (x/14 ('a 0))
     (x/15 (int 0)) (x/16 ("int * int" 0)) (y/17 ('a 0)))
    |}]
;;

let%expect_test "instantiation at variables" =
  print
    [ top
        "use"
        []
        (let_
           "pair"
           [ "x" ]
           (tuple [ var "x"; var "x" ])
           (tuple
              [ app (var "id") [ int 1 ]
              ; app (var "id") [ bool true ]
              ; app (var "pair") [ unit ]
              ; var "pair"
              ]))
    ];
  [%expect
    {|
    ((members
      (((ident use/4) (args ())
        (scheme "int * bool * (unit * unit) * ('_a -> '_a * '_a)")
        (body
         (Let (ident pair/15) (args (x/14)) (generalized true)
          (body
           (Tuple
            ((Var (Bound (Local x/14)) : 'b) (Var (Bound (Local x/14)) : 'b)) :
            "'b * 'b"))
          (in_
           (Tuple
            ((App (Var (Bound (Toplevel id/3)) : "int -> int")
              (Lit (Int 1) : int) : int)
             (App (Var (Bound (Toplevel id/3)) : "bool -> bool")
              (Lit (Bool true) : bool) : bool)
             (App (Var (Bound (Local pair/15)) : "unit -> unit * unit")
              (Lit Unit : unit) : "unit * unit")
             (Var (Bound (Local pair/15)) : "'_a -> '_a * '_a"))
            : "int * bool * (unit * unit) * ('_a -> '_a * '_a)"))
          : "int * bool * (unit * unit) * ('_a -> '_a * '_a)")))))
     (recursive false))
    |}]
;;

let%expect_test "constructors and fields in modules" =
  print
    [ module_
        "M"
        [ Decl.variant
            "t"
            [ "A", []; "B", [ Ty.int ]; "C", [ Ty.bool; Ty.int ] ]
        ; Decl.record "r" [ "x", Ty.int; "y", Ty.bool ]
        ; top "in_m" [] (construct "C" ~args:[ bool true; int 1 ])
        ]
    ; top
        "outside"
        [ "v" ]
        (match_
           (var "v")
           [ Pat.construct "M.A", record [ "M.y", bool true; "M.x", int 1 ]
           ; ( Pat.construct "M.B" ~args:[ Pat.var "n" ]
             , record [ "M.x", var "n"; "M.y", bool false ] )
           ; ( Pat.construct "M.C" ~args:[ Pat.any; Pat.int 2 ]
             , let_
                 "r"
                 []
                 (record [ "M.x", int 2; "M.y", bool true ])
                 (match_
                    (var "r")
                    [ Pat.record [ "M.y", Pat.var "y" ], var "r" ]) )
           ])
    ; top "get" [ "r" ] (field (var "r") "M.x")
    ];
  [%expect
    {|
    ((members
      (((ident M.in_m/4) (args ()) (scheme M.t)
        (body
         (Construct ((type_name M.t) (name M.C) (index 2))
          ((Lit (Bool true) : bool) (Lit (Int 1) : int)) : M.t)))))
     (recursive false))
    ((members
      (((ident outside/5) (args (v/16)) (scheme "M.t -> M.r")
        (body
         (Match (Var (Bound (Local v/16)) : M.t)
          (((Construct ((type_name M.t) (name M.A) (index 0)) () : M.t)
            (Record
             ((((type_name M.r) (name M.y) (index 1)) (Lit (Bool true) : bool))
              (((type_name M.r) (name M.x) (index 0)) (Lit (Int 1) : int)))
             : M.r))
           ((Construct ((type_name M.t) (name M.B) (index 1)) ((Var n/17 : int))
             : M.t)
            (Record
             ((((type_name M.r) (name M.x) (index 0))
               (Var (Bound (Local n/17)) : int))
              (((type_name M.r) (name M.y) (index 1)) (Lit (Bool false) : bool)))
             : M.r))
           ((Construct ((type_name M.t) (name M.C) (index 2))
             ((Any : bool) (Lit (Int 2) : int)) : M.t)
            (Let (ident r/18) (args ()) (generalized true)
             (body
              (Record
               ((((type_name M.r) (name M.x) (index 0)) (Lit (Int 2) : int))
                (((type_name M.r) (name M.y) (index 1)) (Lit (Bool true) : bool)))
               : M.r))
             (in_
              (Match (Var (Bound (Local r/18)) : M.r)
               (((Record
                  ((((type_name M.r) (name M.y) (index 1)) (Var y/19 : bool))) :
                  M.r)
                 (Var (Bound (Local r/18)) : M.r)))
               : M.r))
             : M.r)))
          : M.r)))))
     (recursive false))
    ((members
      (((ident get/6) (args (r/20)) (scheme "M.r -> int")
        (body
         (Field (Var (Bound (Local r/20)) : M.r)
          ((type_name M.r) (name M.x) (index 0)) : int)))))
     (recursive false))
    |}]
;;

let%expect_test "polymorphic records" =
  print
    ~idents:true
    [ top
        "ops"
        [ "r" ]
        (tuple
           [ dot (var "r") "a"
           ; extend [ "c", int 1 ] (var "r")
           ; restrict (var "r") "b"
           ; update (var "r") [ "a", bool true ]
           ; poly_record [ "a", int 1; "b", unit ]
           ])
    ; top
        "rest"
        [ "r" ]
        (match_
           (var "r")
           [ ( Pat.poly_record ~bind:"others" [ "a", Pat.var "a"; "b", Pat.any ]
             , tuple [ var "a"; var "others" ] )
           ])
    ; top
        "open_"
        [ "r" ]
        (match_
           (var "r")
           [ Pat.poly_record ~rest:Open [ "a", Pat.var "a" ], var "a" ])
    ];
  [%expect
    {|
    ((members
      (((ident ops/4) (args (r/16))
        (scheme
         "{a : 'a; b : 'b; ..'c} -> 'a * {a : 'a; b : 'b; c : int; ..'c} * {a : 'a; ..'c} * {a : bool; b : 'b; ..'c} * {a : int; b : unit}")
        (body
         (Tuple
          ((Poly_field (Var (Bound (Local r/16)) : "{a : 'a; b : 'b; ..'c}") a :
            'a)
           (Poly_extend ((c (Lit (Int 1) : int)))
            (Var (Bound (Local r/16)) : "{a : 'a; b : 'b; ..'c}") :
            "{a : 'a; b : 'b; c : int; ..'c}")
           (Poly_restrict (Var (Bound (Local r/16)) : "{a : 'a; b : 'b; ..'c}") b
            : "{a : 'a; ..'c}")
           (Poly_update (Var (Bound (Local r/16)) : "{a : 'a; b : 'b; ..'c}")
            ((a (Lit (Bool true) : bool))) : "{a : bool; b : 'b; ..'c}")
           (Poly_record ((a (Lit (Int 1) : int)) (b (Lit Unit : unit))) :
            "{a : int; b : unit}"))
          :
          "'a * {a : 'a; b : 'b; c : int; ..'c} * {a : 'a; ..'c} * {a : bool; b : 'b; ..'c} * {a : int; b : unit}")))))
     (recursive false))
    ((members
      (((ident rest/5) (args (r/17))
        (scheme "{a : 'a; b : 'b; ..'c} -> 'a * {..'c}")
        (body
         (Match (Var (Bound (Local r/17)) : "{a : 'a; b : 'b; ..'c}")
          (((Poly_record (fields ((a (Var a/18 : 'a)) (b (Any : 'b))))
             (rest (Bind others/19)) : "{a : 'a; b : 'b; ..'c}")
            (Tuple
             ((Var (Bound (Local a/18)) : 'a)
              (Var (Bound (Local others/19)) : {..'c}))
             : "'a * {..'c}")))
          : "'a * {..'c}")))))
     (recursive false))
    ((members
      (((ident open_/6) (args (r/20)) (scheme "{a : 'a; ..'b} -> 'a")
        (body
         (Match (Var (Bound (Local r/20)) : "{a : 'a; ..'b}")
          (((Poly_record (fields ((a (Var a/21 : 'a)))) (rest Open) :
             "{a : 'a; ..'b}")
            (Var (Bound (Local a/21)) : 'a)))
          : 'a)))))
     (recursive false))
    ((a/18 ('a 0)) (a/21 ('a 0)) (open_/6 ("{a : 'a; ..'b} -> 'a" 0))
     (ops/4
      ("{a : 'a; b : 'b; ..'c} -> 'a * {a : 'a; b : 'b; c : int; ..'c} * {a : 'a; ..'c} * {a : bool; b : 'b; ..'c} * {a : int; b : unit}"
       0))
     (others/19 ({..'a} 0)) (r/16 ("{a : 'a; b : 'b; ..'c}" 0))
     (r/17 ("{a : 'a; b : 'b; ..'c}" 0)) (r/20 ("{a : 'a; ..'b}" 0))
     (rest/5 ("{a : 'a; b : 'b; ..'c} -> 'a * {..'c}" 0)))
    |}]
;;

let%expect_test "polymorphic variants" =
  print
    ~idents:true
    [ top "tags" [] (tuple [ tag "A"; tag "B" ~arg:(int 1) ])
    ; top
        "handle"
        [ "v" ]
        (match_
           (var "v")
           [ Pat.tag "A", int 0
           ; Pat.tag "B" ~arg:(Pat.var "n"), var "n"
           ; Pat.var "other", int 2
           ])
    ];
  [%expect
    {|
    ((members
      (((ident tags/4) (args ()) (scheme "[`A; ..'a] * [`B of int; ..'b]")
        (body
         (Tuple
          ((Poly_variant A () : "[`A; ..'a]")
           (Poly_variant B ((Lit (Int 1) : int)) : "[`B of int; ..'b]"))
          : "[`A; ..'a] * [`B of int; ..'b]")))))
     (recursive false))
    ((members
      (((ident handle/5) (args (v/15)) (scheme "[`A | `B of int; ..'a] -> int")
        (body
         (Match (Var (Bound (Local v/15)) : "[`A | `B of int; ..'a]")
          (((Poly_variant A () : "[`A | `B of int; ..'a]") (Lit (Int 0) : int))
           ((Poly_variant B ((Var n/16 : int)) : "[`A | `B of int; ..'a]")
            (Var (Bound (Local n/16)) : int))
           ((Var other/17 : "[`A | `B of int; ..'a]") (Lit (Int 2) : int)))
          : int)))))
     (recursive false))
    ((handle/5 ("[`A | `B of int; ..'a] -> int" 0)) (n/16 (int 0))
     (other/17 ([..'a] 0)) (tags/4 ("[`A; ..'a] * [`B of int; ..'b]" 0))
     (v/15 ("[`A | `B of int; ..'a]" 0)))
    |}]
;;

let%expect_test "quotes" =
  print
    ~idents:true
    [ top "closed" [] (quote (let_ "y" [] (int 1) (var "y")))
    ; top "nested" [] (quote (quote (app (var "add") [ var "x"; int 1 ])))
    ];
  [%expect
    {|
    ((members
      (((ident closed/4) (args ()) (scheme <int>{..'a})
        (body
         (Quote
          (Let (ident y/15) (args ()) (generalized true)
           (body (Lit (Int 1) : int)) (in_ (Var (Bound (Local y/15)) : int)) :
           int)
          : <int>{..'a})))))
     (recursive false))
    ((members
      (((ident nested/5) (args ()) (scheme "<<int>{x : int; ..'a}>{..'b}")
        (body
         (Quote
          (Quote
           (App
            (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
             (Var (Free x) : int) : "int -> int")
            (Lit (Int 1) : int) : int)
           : "<int>{x : int; ..'a}")
          : "<<int>{x : int; ..'a}>{..'b}")))))
     (recursive false))
    ((closed/4 (<int>{..'a} 0)) (nested/5 ("<<int>{x : int; ..'a}>{..'b}" 0))
     (y/15 (int 1)))
    |}]
;;

let%expect_test "level crossing" =
  print
    [ top "n" [] (int 3)
    ; top "not_persistent" [] (quote (var "n"))
    ; top "persistent" [] (quote (app (var "id") [ int 1 ]))
    ; top "outer_arg" [ "x" ] (quote (var "x"))
    ; top
        "local_persistent"
        [ "n" ]
        (let_
           "g"
           [ "x" ]
           (app (var "add") [ var "x"; var "n" ])
           (quote (app (var "g") [ int 1 ])))
    ; top "local_not_persistent" [] (let_ "k" [] (int 1) (quote (var "k")))
    ];
  [%expect
    {|
    ((members (((ident n/4) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((ident not_persistent/5) (args ()) (scheme "<'a>{n : 'a; ..'b}")
        (body (Quote (Var (Free n) : 'a) : "<'a>{n : 'a; ..'b}")))))
     (recursive false))
    ((members
      (((ident persistent/6) (args ()) (scheme <int>{..'a})
        (body
         (Quote
          (App (Var (Persistent (Toplevel id/3)) : "int -> int")
           (Lit (Int 1) : int) : int)
          : <int>{..'a})))))
     (recursive false))
    ((members
      (((ident outer_arg/7) (args (x/19)) (scheme "'a -> <'b>{x : 'b; ..'c}")
        (body (Quote (Var (Free x) : 'b) : "<'b>{x : 'b; ..'c}")))))
     (recursive false))
    ((members
      (((ident local_persistent/8) (args (n/20)) (scheme "int -> <int>{..'a}")
        (body
         (Let (ident g/22) (args (x/21)) (generalized true)
          (body
           (App
            (App (Var (Bound (Toplevel add/0)) : "int -> int -> int")
             (Var (Bound (Local x/21)) : int) : "int -> int")
            (Var (Bound (Local n/20)) : int) : int))
          (in_
           (Quote
            (App (Var (Persistent (Local g/22)) : "int -> int")
             (Lit (Int 1) : int) : int)
            : <int>{..'a}))
          : <int>{..'a})))))
     (recursive false))
    ((members
      (((ident local_not_persistent/9) (args ()) (scheme "<'a>{k : 'a; ..'b}")
        (body
         (Let (ident k/23) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_ (Quote (Var (Free k) : 'a) : "<'a>{k : 'a; ..'b}")) :
          "<'a>{k : 'a; ..'b}")))))
     (recursive false))
    |}]
;;

let%expect_test "explicit splices" =
  let code = quote (app (var "add") [ var "x"; var "y" ]) in
  print
    [ top
        "explicit"
        []
        (quote
           (let_
              "x"
              []
              (int 1)
              (splice ~subs:[ "x", None; "y", Some (var "z") ] code)))
    ];
  [%expect
    {|
    ((members
      (((ident explicit/4) (args ()) (scheme "<int>{z : int; ..'a}")
        (body
         (Quote
          (Let (ident x/14) (args ()) (generalized true)
           (body (Lit (Int 1) : int))
           (in_
            (Splice
             (code
              (Quote
               (App
                (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
                 (Var (Free x) : int) : "int -> int")
                (Var (Free y) : int) : int)
               : "<int>{x : int; y : int}"))
             (subs
              (Explicit
               ((x (Var (Bound (Local x/14)) : int)) (y (Var (Free z) : int)))))
             : int))
           : int)
          : "<int>{z : int; ..'a}")))))
     (recursive false))
    |}]
;;

let%expect_test "implicit splices" =
  let code = quote (app (var "add") [ var "x"; int 1 ]) in
  print
    ~idents:true
    [ top "bound" [] (quote (let_ "x" [] (int 1) (splice code)))
    ; top "propagates" [] (quote (splice code))
    ; top
        "tail"
        [ "c" ]
        (let_
           "outer"
           []
           (int 1)
           (quote
              (let_
                 "y"
                 []
                 (int 1)
                 (let_ "f" [ "a" ] (var "a") (splice (var "c"))))))
    ; top
        "later_level"
        [ "c" ]
        (quote (quote (let_ "y" [] (int 1) (splice (splice (var "c"))))))
    ];
  [%expect
    {|
    ((members
      (((ident bound/4) (args ()) (scheme <int>{..'a})
        (body
         (Quote
          (Let (ident x/17) (args ()) (generalized true)
           (body (Lit (Int 1) : int))
           (in_
            (Splice
             (code
              (Quote
               (App
                (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
                 (Var (Free x) : int) : "int -> int")
                (Lit (Int 1) : int) : int)
               : "<int>{x : int; ..'_b}"))
             (subs (Implicit ((x (Local x/17))))) : int))
           : int)
          : <int>{..'a}))
        (where "..'_b <= {..'a}"))))
     (recursive false))
    ((members
      (((ident propagates/5) (args ()) (scheme "<int>{x : int; ..'a}")
        (body
         (Quote
          (Splice
           (code
            (Quote
             (App
              (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
               (Var (Free x) : int) : "int -> int")
              (Lit (Int 1) : int) : int)
             : "<int>{x : int; ..'_b}"))
           (subs (Implicit ())) : int)
          : "<int>{x : int; ..'a}"))
        (where "..'_b <= {x : int; ..'a}"))))
     (recursive false))
    ((members
      (((ident tail/6) (args (c/18))
        (scheme "<'a>{f?'b : 'c -> 'c; y?'d : int; ..'e} -> <'a>{..'f}")
        (body
         (Let (ident outer/19) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Quote
            (Let (ident y/20) (args ()) (generalized true)
             (body (Lit (Int 1) : int))
             (in_
              (Let (ident f/22) (args (a/21)) (generalized true)
               (body (Var (Bound (Local a/21)) : 'g))
               (in_
                (Splice
                 (code
                  (Var (Bound (Local c/18)) :
                   "<'a>{f?'b : 'c -> 'c; y?'d : int; ..'e}"))
                 (subs (Implicit ((f (Local f/22)) (y (Local y/20))))) : 'a))
               : 'a))
             : 'a)
            : <'a>{..'f}))
          : <'a>{..'f}))
        (where "..'e <= {..'f}"))))
     (recursive false))
    ((members
      (((ident later_level/7) (args (c/23))
        (scheme "<<'a>{y?'b : int; ..'c}>{-y; ..'d} -> <<'a>{..'e}>{..'f}")
        (body
         (Quote
          (Quote
           (Let (ident y/24) (args ()) (generalized true)
            (body (Lit (Int 1) : int))
            (in_
             (Splice
              (code
               (Splice
                (code
                 (Var (Bound (Local c/23)) :
                  "<<'a>{y?'b : int; ..'c}>{-y; ..'d}"))
                (subs (Implicit ())) : "<'a>{y?'b : int; ..'c}"))
              (subs (Implicit ((y (Local y/24))))) : 'a))
            : 'a)
           : <'a>{..'e})
          : <<'a>{..'e}>{..'f}))
        (where "..'c <= {..'e}, ..'d <= {..'f}"))))
     (recursive false))
    ((a/21 ('a 1)) (bound/4 (<int>{..'a} 0))
     (c/18 ("<'a>{f?'b : 'c -> 'c; y?'d : int; ..'e} where ..'e <= {..'f}" 0))
     (c/23
      ("<<'a>{y?'b : int; ..'c}>{-y; ..'d} where ..'c <= {..'e}, ..'d <= {..'f}"
       0))
     (f/22 ("'a -> 'a" 1))
     (later_level/7
      ("<<'a>{y?'b : int; ..'c}>{-y; ..'d} -> <<'a>{..'e}>{..'f} where ..'c <= {..'e}, ..'d <= {..'f}"
       0))
     (outer/19 (int 0)) (propagates/5 ("<int>{x : int; ..'a}" 0))
     (tail/6
      ("<'a>{f?'b : 'c -> 'c; y?'d : int; ..'e} -> <'a>{..'f} where ..'e <= {..'f}"
       0))
     (x/17 (int 1)) (y/20 (int 1)) (y/24 (int 2)))
    |}]
;;

let%expect_test "toplevel splices" =
  print
    ~idents:true
    [ top "n" [] (int 3)
    ; top "m" [] (var "n")
    ; top
        "f"
        [ "x" ]
        (splice
           (quote
              (app (var "add") [ var "x"; app (var "add") [ var "n"; int 1 ] ])))
    ; top "closed" [] (splice (quote (int 1)))
    ];
  [%expect
    {|
    ((members (((ident n/4) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((ident m/5) (args ()) (scheme int)
        (body (Var (Bound (Toplevel n/4)) : int)))))
     (recursive false))
    ((members
      (((ident f/6) (args (x/17)) (scheme "int -> int")
        (body
         (Toplevel_splice
          (code
           (Quote
            (App
             (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
              (Var (Free x) : int) : "int -> int")
             (App
              (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
               (Var (Free n) : int) : "int -> int")
              (Lit (Int 1) : int) : int)
             : int)
            : "<int>{n : int; x : int}"))
          (expansion
           (App
            (App (Var (Bound (Toplevel add/0)) : "int -> int -> int")
             (Var (Bound (Local x/17)) : int) : "int -> int")
            (App
             (App (Var (Bound (Toplevel add/0)) : "int -> int -> int")
              (Var (Bound (Toplevel n/4)) : int) : "int -> int")
             (Lit (Int 1) : int) : int)
            : int))
          : int)))))
     (recursive false))
    ((members
      (((ident closed/7) (args ()) (scheme int)
        (body
         (Toplevel_splice (code (Quote (Lit (Int 1) : int) : <int>{}))
          (expansion (Lit (Int 1) : int)) : int)))))
     (recursive false))
    ((closed/7 (int 0)) (f/6 ("int -> int" 0)) (m/5 (int 0)) (n/4 (int 0))
     (x/17 (int 0)))
    |}]
;;

let%expect_test "components in evaluation order" =
  print
    [ top "main" [] (app (var "even") [ int 3 ])
    ; top
        "even"
        [ "n" ]
        (app
           (var "cond")
           [ app (var "eq") [ var "n"; int 0 ]
           ; bool true
           ; app (var "odd") [ var "n" ]
           ])
    ; top
        "odd"
        [ "n" ]
        (app
           (var "cond")
           [ app (var "eq") [ var "n"; int 0 ]
           ; bool false
           ; app (var "even") [ var "n" ]
           ])
    ; top "a" [] (var "b")
    ; top "b" [] (int 1)
    ; top "gen" [ "n" ] (quote (app (var "gen") [ var "n" ]))
    ];
  [%expect
    {|
    ((members
      (((ident even/5) (args (n/19)) (scheme "int -> bool")
        (body
         (App
          (App
           (App (Var (Bound (Toplevel cond/2)) : "bool -> bool -> bool -> bool")
            (App
             (App (Var (Bound (Toplevel eq/1)) : "int -> int -> bool")
              (Var (Bound (Local n/19)) : int) : "int -> bool")
             (Lit (Int 0) : int) : bool)
            : "bool -> bool -> bool")
           (Lit (Bool true) : bool) : "bool -> bool")
          (App (Var (Bound (Toplevel odd/6)) : "int -> bool")
           (Var (Bound (Local n/19)) : int) : bool)
          : bool)))
       ((ident odd/6) (args (n/20)) (scheme "int -> bool")
        (body
         (App
          (App
           (App (Var (Bound (Toplevel cond/2)) : "bool -> bool -> bool -> bool")
            (App
             (App (Var (Bound (Toplevel eq/1)) : "int -> int -> bool")
              (Var (Bound (Local n/20)) : int) : "int -> bool")
             (Lit (Int 0) : int) : bool)
            : "bool -> bool -> bool")
           (Lit (Bool false) : bool) : "bool -> bool")
          (App (Var (Bound (Toplevel even/5)) : "int -> bool")
           (Var (Bound (Local n/20)) : int) : bool)
          : bool)))))
     (recursive true))
    ((members
      (((ident main/4) (args ()) (scheme bool)
        (body
         (App (Var (Bound (Toplevel even/5)) : "int -> bool") (Lit (Int 3) : int)
          : bool)))))
     (recursive false))
    ((members (((ident b/8) (args ()) (scheme int) (body (Lit (Int 1) : int)))))
     (recursive false))
    ((members
      (((ident a/7) (args ()) (scheme int)
        (body (Var (Bound (Toplevel b/8)) : int)))))
     (recursive false))
    ((members
      (((ident gen/9) (args (n/21))
        (scheme "'a -> <'b>{gen : 'c -> 'b; n : 'c; ..'d}")
        (body
         (Quote (App (Var (Free gen) : "'c -> 'b") (Var (Free n) : 'c) : 'b) :
          "<'b>{gen : 'c -> 'b; n : 'c; ..'d}")))))
     (recursive false))
    |}]
;;

let%expect_test "recursive components" =
  print
    [ top "gen" [ "n" ] (quote (app (var "gen") [ var "n" ]))
    ; top "a" [] (quote (var "b"))
    ; top "b" [] (quote (var "a"))
    ; top "loop" [ "x" ] (app (var "loop") [ var "x" ])
    ; top "captured" [ "x" ] (splice (quote (app (var "captured") [ var "x" ])))
    ];
  [%expect
    {|
    ((members
      (((ident gen/4) (args (n/18))
        (scheme "'a -> <'b>{gen : 'c -> 'b; n : 'c; ..'d}")
        (body
         (Quote (App (Var (Free gen) : "'c -> 'b") (Var (Free n) : 'c) : 'b) :
          "<'b>{gen : 'c -> 'b; n : 'c; ..'d}")))))
     (recursive false))
    ((members
      (((ident a/5) (args ()) (scheme "<'a>{b : 'a; ..'b}")
        (body (Quote (Var (Free b) : 'a) : "<'a>{b : 'a; ..'b}")))))
     (recursive false))
    ((members
      (((ident b/6) (args ()) (scheme "<'a>{a : 'a; ..'b}")
        (body (Quote (Var (Free a) : 'a) : "<'a>{a : 'a; ..'b}")))))
     (recursive false))
    ((members
      (((ident loop/7) (args (x/19)) (scheme "'a -> 'b")
        (body
         (App (Var (Bound (Toplevel loop/7)) : "'a -> 'b")
          (Var (Bound (Local x/19)) : 'a) : 'b)))))
     (recursive true))
    ((members
      (((ident captured/8) (args (x/20)) (scheme "'a -> 'b")
        (body
         (Toplevel_splice
          (code
           (Quote
            (App (Var (Free captured) : "'a -> 'b") (Var (Free x) : 'a) : 'b) :
            "<'b>{captured : 'a -> 'b; x : 'a}"))
          (expansion
           (App (Var (Bound (Toplevel captured/8)) : "'a -> 'b")
            (Var (Bound (Local x/20)) : 'a) : 'b))
          : 'b)))))
     (recursive true))
    |}]
;;

let%expect_test "literals that look like placeholders" =
  print
    [ top
        "lits"
        [ "s" ]
        (match_
           (var "s")
           [ Pat.string "\0000", tuple [ char '\000'; string "\0003" ] ])
    ];
  [%expect
    {|
    ((members
      (((ident lits/4) (args (s/14)) (scheme "string -> char * string")
        (body
         (Match (Var (Bound (Local s/14)) : string)
          (((Lit (String "\0000") : string)
            (Tuple ((Lit (Char "\000") : char) (Lit (String "\0003") : string)) :
             "char * string")))
          : "char * string")))))
     (recursive false))
    |}]
;;

let%expect_test "level crossing at splices" =
  print
    ~idents:true
    [ top "n" [] (int 3)
    ; top
        "persistent_local"
        []
        (quote
           (let_
              "f"
              [ "x" ]
              (var "x")
              (splice (app (var "id") [ quote (app (var "f") [ int 1 ]) ]))))
    ; top
        "outside_binder"
        []
        (let_ "x" [] (int 1) (quote (splice (quote (var "x")))))
    ; top
        "shadowed"
        [ "c" ]
        (quote (let_ "x" [] (int 1) (let_ "x" [] (int 2) (splice (var "c")))))
    ; top "n_shadowed" [] (let_ "n" [] (int 1) (splice (quote (var "n"))))
    ];
  [%expect
    {|
    ((members (((ident n/4) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((ident persistent_local/5) (args ()) (scheme <int>{..'_a})
        (body
         (Quote
          (Let (ident f/19) (args (x/18)) (generalized true)
           (body (Var (Bound (Local x/18)) : 'b))
           (in_
            (Splice
             (code
              (App
               (Var (Bound (Toplevel id/3)) :
                "<int>{f?'_c : '_d -> '_d; ..'_e} -> <int>{f?'_c : '_d -> '_d; ..'_e}")
               (Quote
                (App (Var (Persistent (Local f/19)) : "int -> int")
                 (Lit (Int 1) : int) : int)
                : "<int>{f?'_c : '_d -> '_d; ..'_e}")
               : "<int>{f?'_c : '_d -> '_d; ..'_e}"))
             (subs (Implicit ((f (Local f/19))))) : int))
           : int)
          : <int>{..'_a}))
        (where "..'_e <= {..'_a}"))))
     (recursive false))
    ((members
      (((ident outside_binder/6) (args ()) (scheme "<'a>{x : 'a; ..'b}")
        (body
         (Let (ident x/20) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Quote
            (Splice (code (Quote (Var (Free x) : 'a) : "<'a>{x : 'a; ..'_c}"))
             (subs (Implicit ())) : 'a)
            : "<'a>{x : 'a; ..'b}"))
          : "<'a>{x : 'a; ..'b}"))
        (where "..'_c <= {x : 'a; ..'b}"))))
     (recursive false))
    ((members
      (((ident shadowed/7) (args (c/21))
        (scheme "<'a>{x?'b : int; ..'c} -> <'a>{..'d}")
        (body
         (Quote
          (Let (ident x/22) (args ()) (generalized true)
           (body (Lit (Int 1) : int))
           (in_
            (Let (ident x/23) (args ()) (generalized true)
             (body (Lit (Int 2) : int))
             (in_
              (Splice
               (code (Var (Bound (Local c/21)) : "<'a>{x?'b : int; ..'c}"))
               (subs (Implicit ((x (Local x/23))))) : 'a))
             : 'a))
           : 'a)
          : <'a>{..'d}))
        (where "..'c <= {..'d}"))))
     (recursive false))
    ((members
      (((ident n_shadowed/8) (args ()) (scheme int)
        (body
         (Let (ident n/24) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Toplevel_splice
            (code (Quote (Var (Free n) : int) : "<int>{n : int}"))
            (expansion (Var (Bound (Local n/24)) : int)) : int))
          : int)))))
     (recursive false))
    ((c/21 ("<'a>{x?'b : int; ..'c} where ..'c <= {..'d}" 0))
     (f/19 ("'a -> 'a" 1)) (n/4 (int 0)) (n/24 (int 0)) (n_shadowed/8 (int 0))
     (outside_binder/6 ("<'a>{x : 'a; ..'b}" 0))
     (persistent_local/5 (<int>{..'_a} 0))
     (shadowed/7 ("<'a>{x?'b : int; ..'c} -> <'a>{..'d} where ..'c <= {..'d}" 0))
     (x/18 ('a 1)) (x/20 (int 0)) (x/22 (int 1)) (x/23 (int 1)))
    |}]
;;

let%expect_test "toplevel splice in a level -1 let" =
  print
    ~idents:true
    [ top "n" [] (int 3)
    ; top
        "deep"
        []
        (splice
           (let_
              "x"
              []
              (quote (int 1))
              (splice
                 (let_
                    "k"
                    [ "c" ]
                    (seq
                       (quote (let_ "n" [] (int 1) (splice (var "c"))))
                       (var "c"))
                    (app (var "k") [ quote (var "x") ])))))
    ];
  [%expect
    {|
    ((members (((ident n/4) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((ident deep/5) (args ()) (scheme int)
        (body
         (Toplevel_splice
          (code
           (Let (ident x/15) (args ()) (generalized true)
            (body (Quote (Lit (Int 1) : int) : <int>{..'a}))
            (in_
             (Toplevel_splice
              (code
               (Let (ident k/17) (args (c/16)) (generalized true)
                (body
                 (Let (ident _/18) (args ()) (generalized true)
                  (body
                   (Quote
                    (Let (ident n/19) (args ()) (generalized true)
                     (body (Lit (Int 1) : int))
                     (in_
                      (Splice
                       (code
                        (Var (Bound (Local c/16)) : "<'b>{n?'c : int; ..'d}"))
                       (subs (Implicit ((n (Local n/19))))) : 'b))
                     : 'b)
                    : <'b>{..'e}))
                  (in_ (Var (Bound (Local c/16)) : "<'b>{n?'c : int; ..'d}")) :
                  "<'b>{n?'c : int; ..'d}"))
                (in_
                 (App
                  (Var (Bound (Local k/17)) :
                   "<<int>{}>{-n; x : <int>{}} -> <<int>{}>{-n; x : <int>{}}")
                  (Quote (Var (Free x) : <int>{}) : "<<int>{}>{-n; x : <int>{}}")
                  : "<<int>{}>{-n; x : <int>{}}"))
                : "<<int>{}>{-n; x : <int>{}}"))
              (expansion (Var (Bound (Local x/15)) : <int>{})) : <int>{}))
            : <int>{}))
          (expansion (Lit (Int 1) : int)) : int))
        (where "..'d <= {..'e}"))))
     (recursive false))
    ((_/18 (<'a>{..'b} -2))
     (c/16 ("<'a>{n?'b : int; ..'c} where ..'c <= {..'d}" -2)) (deep/5 (int 0))
     (k/17
      ("<'a>{n?'b : int; ..'c} -> <'a>{n?'b : int; ..'c} where ..'c <= {..'d}"
       -2))
     (n/4 (int 0)) (n/19 (int -1)) (x/15 (<int>{..'a} -1)))
    |}]
;;

let%expect_test "toplevel used at level -1 by spliced code" =
  print
    [ top "n" [] (int 3)
    ; top
        "deep"
        []
        (splice
           (let_
              "x"
              []
              (quote (int 1))
              (splice (let_ "n" [] (int 5) (quote (seq (var "n") (var "x")))))))
    ];
  [%expect
    {|
    ((members (((ident n/4) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((ident deep/5) (args ()) (scheme int)
        (body
         (Toplevel_splice
          (code
           (Let (ident x/15) (args ()) (generalized true)
            (body (Quote (Lit (Int 1) : int) : <int>{..'a}))
            (in_
             (Toplevel_splice
              (code
               (Let (ident n/16) (args ()) (generalized true)
                (body (Lit (Int 5) : int))
                (in_
                 (Quote
                  (Let (ident _/17) (args ()) (generalized true)
                   (body (Var (Free n) : int)) (in_ (Var (Free x) : <int>{})) :
                   <int>{})
                  : "<<int>{}>{n : int; x : <int>{}}"))
                : "<<int>{}>{n : int; x : <int>{}}"))
              (expansion
               (Let (ident _/18) (args ()) (generalized true)
                (body (Var (Bound (Toplevel n/4)) : int))
                (in_ (Var (Bound (Local x/15)) : <int>{})) : <int>{}))
              : <int>{}))
            : <int>{}))
          (expansion (Lit (Int 1) : int)) : int)))))
     (recursive false))
    |}]
;;

let%expect_test "explicit substitutions" =
  print
    [ top "n" [] (int 3)
    ; top
        "persistent_shorthand"
        []
        (let_
           "f"
           [ "y" ]
           (var "y")
           (quote
              (splice
                 ~subs:[ "f", None; "id", None ]
                 (quote (app (var "id") [ app (var "f") [ int 1 ] ])))))
    ; top
        "at_toplevel"
        []
        (splice
           ~subs:[ "n", None; "y", Some (int 1) ]
           (quote (app (var "add") [ var "n"; var "y" ])))
    ];
  [%expect
    {|
    ((members (((ident n/4) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((ident persistent_shorthand/5) (args ()) (scheme <int>{..'a})
        (body
         (Let (ident f/17) (args (y/16)) (generalized true)
          (body (Var (Bound (Local y/16)) : 'b))
          (in_
           (Quote
            (Splice
             (code
              (Quote
               (App (Var (Persistent (Toplevel id/3)) : "int -> int")
                (App (Var (Persistent (Local f/17)) : "int -> int")
                 (Lit (Int 1) : int) : int)
                : int)
               : "<int>{f?'_c : '_d -> '_d; id?'_e : '_f -> '_f}"))
             (subs
              (Explicit
               ((f (Var (Persistent (Local f/17)) : "'_d -> '_d"))
                (id (Var (Persistent (Toplevel id/3)) : "'_f -> '_f")))))
             : int)
            : <int>{..'a}))
          : <int>{..'a})))))
     (recursive false))
    ((members
      (((ident at_toplevel/6) (args ()) (scheme int)
        (body
         (Toplevel_splice
          (code
           (Quote
            (App
             (App (Var (Persistent (Toplevel add/0)) : "int -> int -> int")
              (Var (Free n) : int) : "int -> int")
             (Var (Free y) : int) : int)
            : "<int>{n : int; y : int}"))
          (expansion
           (Let (ident n/18) (args ()) (generalized false)
            (body (Var (Bound (Toplevel n/4)) : int))
            (in_
             (Let (ident y/19) (args ()) (generalized false)
              (body (Lit (Int 1) : int))
              (in_
               (App
                (App (Var (Bound (Toplevel add/0)) : "int -> int -> int")
                 (Var (Bound (Local n/18)) : int) : "int -> int")
                (Var (Bound (Local y/19)) : int) : int))
              : int))
            : int))
          : int)))))
     (recursive false))
    |}]
;;

let%expect_test "local recursion" =
  print
    [ top
        "direct"
        [ "y" ]
        (let_
           "f"
           [ "x" ]
           (app (var "f") [ var "x" ])
           (app (var "f") [ var "y" ]))
    ; top
        "in_quote"
        []
        (let_ "f" [ "x" ] (quote (app (var "f") [ var "x" ])) (quote (var "f")))
    ];
  [%expect
    {|
    ((members
      (((ident direct/4) (args (y/15)) (scheme "'a -> 'b")
        (body
         (Let (ident f/17) (args (x/16)) (generalized true)
          (body
           (App (Var (Bound (Local f/17)) : "'c -> 'd")
            (Var (Bound (Local x/16)) : 'c) : 'd))
          (in_
           (App (Var (Bound (Local f/17)) : "'a -> 'b")
            (Var (Bound (Local y/15)) : 'a) : 'b))
          : 'b)))))
     (recursive false))
    ((members
      (((ident in_quote/5) (args ())
        (scheme "<'a -> <'b>{f : 'c -> 'b; x : 'c; ..'d}>{..'e}")
        (body
         (Let (ident f/19) (args (x/18)) (generalized true)
          (body
           (Quote (App (Var (Free f) : "'f -> 'g") (Var (Free x) : 'f) : 'g) :
            "<'g>{f : 'f -> 'g; x : 'f; ..'h}"))
          (in_
           (Quote
            (Var (Persistent (Local f/19)) :
             "'a -> <'b>{f : 'c -> 'b; x : 'c; ..'d}")
            : "<'a -> <'b>{f : 'c -> 'b; x : 'c; ..'d}>{..'e}"))
          : "<'a -> <'b>{f : 'c -> 'b; x : 'c; ..'d}>{..'e}")))))
     (recursive false))
    |}]
;;

let%expect_test "tuple patterns and annotations" =
  print
    ~idents:true
    [ top
        "first"
        [ "p" ]
        (match_
           (var "p")
           [ Pat.tuple [ Pat.var "a"; Pat.any ], var ~annot:Ty.int "a" ])
    ];
  [%expect
    {|
    ((members
      (((ident first/4) (args (p/14)) (scheme "int * 'a -> int")
        (body
         (Match (Var (Bound (Local p/14)) : "int * 'a")
          (((Tuple ((Var a/15 : int) (Any : 'a)) : "int * 'a")
            (Var (Bound (Local a/15)) : int)))
          : int)))))
     (recursive false))
    ((a/15 (int 0)) (first/4 ("int * 'a -> int" 0)) (p/14 ("int * 'a" 0)))
    |}]
;;
