open! Core
open Stag
open Syntax
open Helpers

let prim_names =
  List.filter_map prims ~f:(function
    | Toplevel.Let (name, _, _, _) -> Some name
    | Type _ | Module _ -> None)
;;

let typecheck_exn program =
  match Typecheck.typecheck (Typecheck.create ()) ~program with
  | Ok { program; _ } -> program
  | Error error -> raise_s [%sexp (error : Typecheck_error.t)]
;;

let prim_idents = Map.length (typecheck_exn prims).idents

(* The components of [program] after those of [prims], and with [~idents]
   the types of its binders. *)
let print ?(idents = false) program =
  match
    Typecheck.typecheck (Typecheck.create ()) ~program:(prims @ program)
  with
  | Error error -> print_s [%sexp (error : Typecheck_error.t)]
  | Ok { program; _ } ->
    List.iter program.sccs ~f:(fun (scc : Typed.Scc.t) ->
      if
        not
          (List.for_all scc.members ~f:(fun member ->
             List.mem prim_names member.name ~equal:String.equal))
      then print_s [%sexp (scc : Typed.Scc.t)]);
    if idents
    then
      print_s
        [%sexp
          (Map.filter_keys program.idents ~f:(fun ident ->
             ident.stamp >= prim_idents)
           |> Map.map ~f:Type.to_string_hum
           : string Typed.Ident.Map.t)]
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
      (((name f) (args (x/9)) (scheme "'a -> int * int")
        (body
         (Let (ident x/10) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Match
            (Tuple
             ((Var (Bound (Local x/10)) : int) (Var (Bound (Local x/10)) : int))
             : "int * int")
            (((Var x/11 : "int * int")
              (Let (ident g/13) (args (y/12)) (generalized true)
               (body (Var (Bound (Local y/12)) : 'b))
               (in_
                (App (Var (Bound (Local g/13)) : "int * int -> int * int")
                 (Var (Bound (Local x/11)) : "int * int") : "int * int"))
               : "int * int")))
            : "int * int"))
          : "int * int")))))
     (recursive false))
    ((g/13 "'a -> 'a") (x/9 'a) (x/10 int) (x/11 "int * int") (y/12 'a))
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
      (((name use) (args ())
        (scheme "int * bool * (unit * unit) * ('_a -> '_a * '_a)")
        (body
         (Let (ident pair/10) (args (x/9)) (generalized true)
          (body
           (Tuple ((Var (Bound (Local x/9)) : 'b) (Var (Bound (Local x/9)) : 'b))
            : "'b * 'b"))
          (in_
           (Tuple
            ((App (Var (Bound (Toplevel id)) : "int -> int") (Lit (Int 1) : int)
              : int)
             (App (Var (Bound (Toplevel id)) : "bool -> bool")
              (Lit (Bool true) : bool) : bool)
             (App (Var (Bound (Local pair/10)) : "unit -> unit * unit")
              (Lit Unit : unit) : "unit * unit")
             (Var (Bound (Local pair/10)) : "'_a -> '_a * '_a"))
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
      (((name M.in_m) (args ()) (scheme M.t)
        (body
         (Construct ((type_name M.t) (name M.C) (index 2))
          ((Lit (Bool true) : bool) (Lit (Int 1) : int)) : M.t)))))
     (recursive false))
    ((members
      (((name outside) (args (v/9)) (scheme "M.t -> M.r")
        (body
         (Match (Var (Bound (Local v/9)) : M.t)
          (((Construct ((type_name M.t) (name M.A) (index 0)) () : M.t)
            (Record
             ((((type_name M.r) (name M.y) (index 1)) (Lit (Bool true) : bool))
              (((type_name M.r) (name M.x) (index 0)) (Lit (Int 1) : int)))
             : M.r))
           ((Construct ((type_name M.t) (name M.B) (index 1)) ((Var n/10 : int))
             : M.t)
            (Record
             ((((type_name M.r) (name M.x) (index 0))
               (Var (Bound (Local n/10)) : int))
              (((type_name M.r) (name M.y) (index 1)) (Lit (Bool false) : bool)))
             : M.r))
           ((Construct ((type_name M.t) (name M.C) (index 2))
             ((Any : bool) (Lit (Int 2) : int)) : M.t)
            (Let (ident r/11) (args ()) (generalized true)
             (body
              (Record
               ((((type_name M.r) (name M.x) (index 0)) (Lit (Int 2) : int))
                (((type_name M.r) (name M.y) (index 1)) (Lit (Bool true) : bool)))
               : M.r))
             (in_
              (Match (Var (Bound (Local r/11)) : M.r)
               (((Record
                  ((((type_name M.r) (name M.y) (index 1)) (Var y/12 : bool))) :
                  M.r)
                 (Var (Bound (Local r/11)) : M.r)))
               : M.r))
             : M.r)))
          : M.r)))))
     (recursive false))
    ((members
      (((name get) (args (r/13)) (scheme "M.r -> int")
        (body
         (Field (Var (Bound (Local r/13)) : M.r)
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
           [ ( Pat.poly_record
                 ~rest:(Bind "others")
                 [ "a", Pat.var "a"; "b", Pat.any ]
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
      (((name ops) (args (r/9))
        (scheme
         "{a : 'a; b : 'b; ..'c} -> 'a * {a : 'a; b : 'b; c : int; ..'c} * {a : 'a; ..'c} * {a : bool; b : 'b; ..'c} * {a : int; b : unit}")
        (body
         (Tuple
          ((Poly_field (Var (Bound (Local r/9)) : "{a : 'a; b : 'b; ..'c}") a :
            'a)
           (Poly_extend ((c (Lit (Int 1) : int)))
            (Var (Bound (Local r/9)) : "{a : 'a; b : 'b; ..'c}") :
            "{a : 'a; b : 'b; c : int; ..'c}")
           (Poly_restrict (Var (Bound (Local r/9)) : "{a : 'a; b : 'b; ..'c}") b
            : "{a : 'a; ..'c}")
           (Poly_update (Var (Bound (Local r/9)) : "{a : 'a; b : 'b; ..'c}")
            ((a (Lit (Bool true) : bool))) : "{a : bool; b : 'b; ..'c}")
           (Poly_record ((a (Lit (Int 1) : int)) (b (Lit Unit : unit))) :
            "{a : int; b : unit}"))
          :
          "'a * {a : 'a; b : 'b; c : int; ..'c} * {a : 'a; ..'c} * {a : bool; b : 'b; ..'c} * {a : int; b : unit}")))))
     (recursive false))
    ((members
      (((name rest) (args (r/10))
        (scheme "{a : 'a; b : 'b; ..'c} -> 'a * {..'c}")
        (body
         (Match (Var (Bound (Local r/10)) : "{a : 'a; b : 'b; ..'c}")
          (((Poly_record (fields ((a (Var a/11 : 'a)) (b (Any : 'b))))
             (rest (Bind others/12)) : "{a : 'a; b : 'b; ..'c}")
            (Tuple
             ((Var (Bound (Local a/11)) : 'a)
              (Var (Bound (Local others/12)) : {..'c}))
             : "'a * {..'c}")))
          : "'a * {..'c}")))))
     (recursive false))
    ((members
      (((name open_) (args (r/13)) (scheme "{a : 'a; ..'b} -> 'a")
        (body
         (Match (Var (Bound (Local r/13)) : "{a : 'a; ..'b}")
          (((Poly_record (fields ((a (Var a/14 : 'a)))) (rest Open) :
             "{a : 'a; ..'b}")
            (Var (Bound (Local a/14)) : 'a)))
          : 'a)))))
     (recursive false))
    ((a/11 'a) (a/14 'a) (others/12 {..'a}) (r/9 "{a : 'a; b : 'b; ..'c}")
     (r/10 "{a : 'a; b : 'b; ..'c}") (r/13 "{a : 'a; ..'b}"))
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
      (((name tags) (args ()) (scheme "[`A; ..'a] * [`B of int; ..'b]")
        (body
         (Tuple
          ((Poly_variant A () : "[`A; ..'a]")
           (Poly_variant B ((Lit (Int 1) : int)) : "[`B of int; ..'b]"))
          : "[`A; ..'a] * [`B of int; ..'b]")))))
     (recursive false))
    ((members
      (((name handle) (args (v/9)) (scheme "[`A | `B of int; ..'a] -> int")
        (body
         (Match (Var (Bound (Local v/9)) : "[`A | `B of int; ..'a]")
          (((Poly_variant A () : "[`A | `B of int; ..'a]") (Lit (Int 0) : int))
           ((Poly_variant B ((Var n/10 : int)) : "[`A | `B of int; ..'a]")
            (Var (Bound (Local n/10)) : int))
           ((Var other/11 : "[`A | `B of int; ..'a]") (Lit (Int 2) : int)))
          : int)))))
     (recursive false))
    ((n/10 int) (other/11 [..'a]) (v/9 "[`A | `B of int; ..'a]"))
    |}]
;;

let%expect_test "quotes" =
  print
    [ top "closed" [] (quote (let_ "y" [] (int 1) (var "y")))
    ; top "nested" [] (quote (quote (app (var "add") [ var "x"; int 1 ])))
    ];
  [%expect
    {|
    ((members
      (((name closed) (args ()) (scheme <int>{..'a})
        (body
         (Quote
          (Let (ident y/9) (args ()) (generalized true)
           (body (Lit (Int 1) : int)) (in_ (Var (Bound (Local y/9)) : int)) :
           int)
          : <int>{..'a})))))
     (recursive false))
    ((members
      (((name nested) (args ()) (scheme "<<int>{x : int; ..'a}>{..'b}")
        (body
         (Quote
          (Quote
           (App
            (App (Var (Persistent (Toplevel add)) : "int -> int -> int")
             (Var (Free x) : int) : "int -> int")
            (Lit (Int 1) : int) : int)
           : "<int>{x : int; ..'a}")
          : "<<int>{x : int; ..'a}>{..'b}")))))
     (recursive false))
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
    ((members (((name n) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((name not_persistent) (args ()) (scheme "<'a>{n : 'a; ..'b}")
        (body (Quote (Var (Free n) : 'a) : "<'a>{n : 'a; ..'b}")))))
     (recursive false))
    ((members
      (((name persistent) (args ()) (scheme <int>{..'a})
        (body
         (Quote
          (App (Var (Persistent (Toplevel id)) : "int -> int")
           (Lit (Int 1) : int) : int)
          : <int>{..'a})))))
     (recursive false))
    ((members
      (((name outer_arg) (args (x/9)) (scheme "'a -> <'b>{x : 'b; ..'c}")
        (body (Quote (Var (Free x) : 'b) : "<'b>{x : 'b; ..'c}")))))
     (recursive false))
    ((members
      (((name local_persistent) (args (n/10)) (scheme "int -> <int>{..'a}")
        (body
         (Let (ident g/12) (args (x/11)) (generalized true)
          (body
           (App
            (App (Var (Bound (Toplevel add)) : "int -> int -> int")
             (Var (Bound (Local x/11)) : int) : "int -> int")
            (Var (Bound (Local n/10)) : int) : int))
          (in_
           (Quote
            (App (Var (Persistent (Local g/12)) : "int -> int")
             (Lit (Int 1) : int) : int)
            : <int>{..'a}))
          : <int>{..'a})))))
     (recursive false))
    ((members
      (((name local_not_persistent) (args ()) (scheme "<'a>{k : 'a; ..'b}")
        (body
         (Let (ident k/13) (args ()) (generalized true)
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
      (((name explicit) (args ()) (scheme "<int>{z : int; ..'a}")
        (body
         (Quote
          (Let (ident x/9) (args ()) (generalized true)
           (body (Lit (Int 1) : int))
           (in_
            (Splice
             (code
              (Quote
               (App
                (App (Var (Persistent (Toplevel add)) : "int -> int -> int")
                 (Var (Free x) : int) : "int -> int")
                (Var (Free y) : int) : int)
               : "<int>{x : int; y : int}"))
             (subs
              (Explicit
               ((x (Var (Bound (Local x/9)) : int)) (y (Var (Free z) : int)))))
             : int))
           : int)
          : "<int>{z : int; ..'a}")))))
     (recursive false))
    |}]
;;

let%expect_test "implicit splices" =
  let code = quote (app (var "add") [ var "x"; int 1 ]) in
  print
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
      (((name bound) (args ()) (scheme <int>{..'a})
        (body
         (Quote
          (Let (ident x/9) (args ()) (generalized true)
           (body (Lit (Int 1) : int))
           (in_
            (Splice
             (code
              (Quote
               (App
                (App (Var (Persistent (Toplevel add)) : "int -> int -> int")
                 (Var (Free x) : int) : "int -> int")
                (Lit (Int 1) : int) : int)
               : "<int>{x : int; ..'_b}"))
             (subs (Implicit ((x (Local x/9))))) : int))
           : int)
          : <int>{..'a}))
        (where "..'_b <= {..'a}"))))
     (recursive false))
    ((members
      (((name propagates) (args ()) (scheme "<int>{x : int; ..'a}")
        (body
         (Quote
          (Splice
           (code
            (Quote
             (App
              (App (Var (Persistent (Toplevel add)) : "int -> int -> int")
               (Var (Free x) : int) : "int -> int")
              (Lit (Int 1) : int) : int)
             : "<int>{x : int; ..'_b}"))
           (subs (Implicit ())) : int)
          : "<int>{x : int; ..'a}"))
        (where "..'_b <= {x : int; ..'a}"))))
     (recursive false))
    ((members
      (((name tail) (args (c/10))
        (scheme "<'a>{f?'b : 'c -> 'c; y?'d : int; ..'e} -> <'a>{..'f}")
        (body
         (Let (ident outer/11) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Quote
            (Let (ident y/12) (args ()) (generalized true)
             (body (Lit (Int 1) : int))
             (in_
              (Let (ident f/14) (args (a/13)) (generalized true)
               (body (Var (Bound (Local a/13)) : 'g))
               (in_
                (Splice
                 (code
                  (Var (Bound (Local c/10)) :
                   "<'a>{f?'b : 'c -> 'c; y?'d : int; ..'e}"))
                 (subs (Implicit ((f (Local f/14)) (y (Local y/12))))) : 'a))
               : 'a))
             : 'a)
            : <'a>{..'f}))
          : <'a>{..'f}))
        (where "..'e <= {..'f}"))))
     (recursive false))
    ((members
      (((name later_level) (args (c/15))
        (scheme "<<'a>{y?'b : int; ..'c}>{-y; ..'d} -> <<'a>{..'e}>{..'f}")
        (body
         (Quote
          (Quote
           (Let (ident y/16) (args ()) (generalized true)
            (body (Lit (Int 1) : int))
            (in_
             (Splice
              (code
               (Splice
                (code
                 (Var (Bound (Local c/15)) :
                  "<<'a>{y?'b : int; ..'c}>{-y; ..'d}"))
                (subs (Implicit ())) : "<'a>{y?'b : int; ..'c}"))
              (subs (Implicit ((y (Local y/16))))) : 'a))
            : 'a)
           : <'a>{..'e})
          : <<'a>{..'e}>{..'f}))
        (where "..'c <= {..'e}, ..'d <= {..'f}"))))
     (recursive false))
    |}]
;;

let%expect_test "toplevel splices" =
  print
    [ top "n" [] (int 3)
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
    ((members (((name n) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((name f) (args (x/9)) (scheme "int -> int")
        (body
         (Splice
          (code
           (Quote
            (App
             (App (Var (Persistent (Toplevel add)) : "int -> int -> int")
              (Var (Free x) : int) : "int -> int")
             (App
              (App (Var (Persistent (Toplevel add)) : "int -> int -> int")
               (Var (Free n) : int) : "int -> int")
              (Lit (Int 1) : int) : int)
             : int)
            : "<int>{n : int; x : int}"))
          (subs (Implicit ((n (Toplevel n)) (x (Local x/9))))) : int)))))
     (recursive false))
    ((members
      (((name closed) (args ()) (scheme int)
        (body
         (Splice (code (Quote (Lit (Int 1) : int) : <int>{}))
          (subs (Implicit ())) : int)))))
     (recursive false))
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
      (((name even) (args (n/9)) (scheme "int -> bool")
        (body
         (App
          (App
           (App (Var (Bound (Toplevel cond)) : "bool -> bool -> bool -> bool")
            (App
             (App (Var (Bound (Toplevel eq)) : "int -> int -> bool")
              (Var (Bound (Local n/9)) : int) : "int -> bool")
             (Lit (Int 0) : int) : bool)
            : "bool -> bool -> bool")
           (Lit (Bool true) : bool) : "bool -> bool")
          (App (Var (Bound (Toplevel odd)) : "int -> bool")
           (Var (Bound (Local n/9)) : int) : bool)
          : bool)))
       ((name odd) (args (n/10)) (scheme "int -> bool")
        (body
         (App
          (App
           (App (Var (Bound (Toplevel cond)) : "bool -> bool -> bool -> bool")
            (App
             (App (Var (Bound (Toplevel eq)) : "int -> int -> bool")
              (Var (Bound (Local n/10)) : int) : "int -> bool")
             (Lit (Int 0) : int) : bool)
            : "bool -> bool -> bool")
           (Lit (Bool false) : bool) : "bool -> bool")
          (App (Var (Bound (Toplevel even)) : "int -> bool")
           (Var (Bound (Local n/10)) : int) : bool)
          : bool)))))
     (recursive true))
    ((members
      (((name main) (args ()) (scheme bool)
        (body
         (App (Var (Bound (Toplevel even)) : "int -> bool") (Lit (Int 3) : int) :
          bool)))))
     (recursive false))
    ((members (((name b) (args ()) (scheme int) (body (Lit (Int 1) : int)))))
     (recursive false))
    ((members
      (((name a) (args ()) (scheme int) (body (Var (Bound (Toplevel b)) : int)))))
     (recursive false))
    ((members
      (((name gen) (args (n/11))
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
      (((name gen) (args (n/9))
        (scheme "'a -> <'b>{gen : 'c -> 'b; n : 'c; ..'d}")
        (body
         (Quote (App (Var (Free gen) : "'c -> 'b") (Var (Free n) : 'c) : 'b) :
          "<'b>{gen : 'c -> 'b; n : 'c; ..'d}")))))
     (recursive false))
    ((members
      (((name a) (args ()) (scheme "<'_a>{b : '_a; ..'_b}")
        (body (Quote (Var (Free b) : '_a) : "<'_a>{b : '_a; ..'_b}")))
       ((name b) (args ()) (scheme "<'_a>{a : '_a; ..'_b}")
        (body (Quote (Var (Free a) : '_a) : "<'_a>{a : '_a; ..'_b}")))))
     (recursive false))
    ((members
      (((name loop) (args (x/10)) (scheme "'a -> 'b")
        (body
         (App (Var (Bound (Toplevel loop)) : "'a -> 'b")
          (Var (Bound (Local x/10)) : 'a) : 'b)))))
     (recursive true))
    ((members
      (((name captured) (args (x/11)) (scheme "'a -> 'b")
        (body
         (Splice
          (code
           (Quote
            (App (Var (Free captured) : "'a -> 'b") (Var (Free x) : 'a) : 'b) :
            "<'b>{captured : 'a -> 'b; x : 'a}"))
          (subs (Implicit ((captured (Toplevel captured)) (x (Local x/11))))) :
          'b)))))
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
           [ ( Pattern.Lit (String "\0000")
             , tuple [ Expr.Lit (Char '\000'); Expr.Lit (String "\0003") ] )
           ])
    ];
  [%expect
    {|
    ((members
      (((name lits) (args (s/9)) (scheme "string -> char * string")
        (body
         (Match (Var (Bound (Local s/9)) : string)
          (((Lit (String "\0000") : string)
            (Tuple ((Lit (Char "\000") : char) (Lit (String "\0003") : string)) :
             "char * string")))
          : "char * string")))))
     (recursive false))
    |}]
;;

let%expect_test "level crossing at splices" =
  print
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
    ((members (((name n) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((name persistent_local) (args ()) (scheme <int>{..'_a})
        (body
         (Quote
          (Let (ident f/10) (args (x/9)) (generalized true)
           (body (Var (Bound (Local x/9)) : 'b))
           (in_
            (Splice
             (code
              (App
               (Var (Bound (Toplevel id)) :
                "<int>{f?'_c : '_d -> '_d; ..'_e} -> <int>{f?'_c : '_d -> '_d; ..'_e}")
               (Quote
                (App (Var (Persistent (Local f/10)) : "int -> int")
                 (Lit (Int 1) : int) : int)
                : "<int>{f?'_c : '_d -> '_d; ..'_e}")
               : "<int>{f?'_c : '_d -> '_d; ..'_e}"))
             (subs (Implicit ((f (Local f/10))))) : int))
           : int)
          : <int>{..'_a}))
        (where "..'_e <= {..'_a}"))))
     (recursive false))
    ((members
      (((name outside_binder) (args ()) (scheme "<'a>{x : 'a; ..'b}")
        (body
         (Let (ident x/11) (args ()) (generalized true)
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
      (((name shadowed) (args (c/12))
        (scheme "<'a>{x?'b : int; ..'c} -> <'a>{..'d}")
        (body
         (Quote
          (Let (ident x/13) (args ()) (generalized true)
           (body (Lit (Int 1) : int))
           (in_
            (Let (ident x/14) (args ()) (generalized true)
             (body (Lit (Int 2) : int))
             (in_
              (Splice
               (code (Var (Bound (Local c/12)) : "<'a>{x?'b : int; ..'c}"))
               (subs (Implicit ((x (Local x/14))))) : 'a))
             : 'a))
           : 'a)
          : <'a>{..'d}))
        (where "..'c <= {..'d}"))))
     (recursive false))
    ((members
      (((name n_shadowed) (args ()) (scheme int)
        (body
         (Let (ident n/15) (args ()) (generalized true)
          (body (Lit (Int 1) : int))
          (in_
           (Splice (code (Quote (Var (Free n) : int) : "<int>{n : int}"))
            (subs (Implicit ((n (Local n/15))))) : int))
          : int)))))
     (recursive false))
    |}]
;;

let%expect_test "toplevel splice in a level -1 let" =
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
    ((members (((name n) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((name deep) (args ()) (scheme int)
        (body
         (Splice
          (code
           (Let (ident x/9) (args ()) (generalized true)
            (body (Quote (Lit (Int 1) : int) : <int>{..'a}))
            (in_
             (Splice
              (code
               (Let (ident k/11) (args (c/10)) (generalized true)
                (body
                 (Let (ident _/12) (args ()) (generalized false)
                  (body
                   (Quote
                    (Let (ident n/13) (args ()) (generalized true)
                     (body (Lit (Int 1) : int))
                     (in_
                      (Splice
                       (code
                        (Var (Bound (Local c/10)) : "<'b>{n?'c : int; ..'d}"))
                       (subs (Implicit ((n (Local n/13))))) : 'b))
                     : 'b)
                    : <'b>{..'e}))
                  (in_ (Var (Bound (Local c/10)) : "<'b>{n?'c : int; ..'d}")) :
                  "<'b>{n?'c : int; ..'d}"))
                (in_
                 (App
                  (Var (Bound (Local k/11)) :
                   "<<int>{}>{-n; x : <int>{}} -> <<int>{}>{-n; x : <int>{}}")
                  (Quote (Var (Free x) : <int>{}) : "<<int>{}>{-n; x : <int>{}}")
                  : "<<int>{}>{-n; x : <int>{}}"))
                : "<<int>{}>{-n; x : <int>{}}"))
              (subs (Implicit ((x (Local x/9))))) : <int>{}))
            : <int>{}))
          (subs (Implicit ())) : int))
        (where "..'d <= {..'e}"))))
     (recursive false))
    |}]
;;

let%expect_test "toplevel used at a wrong level by spliced code" =
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
    {| (Variable_used_at_wrong_level (name n) (var_level 0) (used_at -1)) |}]
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
    ((members (((name n) (args ()) (scheme int) (body (Lit (Int 3) : int)))))
     (recursive false))
    ((members
      (((name persistent_shorthand) (args ()) (scheme <int>{..'a})
        (body
         (Let (ident f/10) (args (y/9)) (generalized true)
          (body (Var (Bound (Local y/9)) : 'b))
          (in_
           (Quote
            (Splice
             (code
              (Quote
               (App (Var (Persistent (Toplevel id)) : "int -> int")
                (App (Var (Persistent (Local f/10)) : "int -> int")
                 (Lit (Int 1) : int) : int)
                : int)
               : "<int>{f?'_c : '_d -> '_d; id?'_e : '_f -> '_f}"))
             (subs
              (Explicit
               ((f (Var (Persistent (Local f/10)) : "'_d -> '_d"))
                (id (Var (Persistent (Toplevel id)) : "'_f -> '_f")))))
             : int)
            : <int>{..'a}))
          : <int>{..'a})))))
     (recursive false))
    ((members
      (((name at_toplevel) (args ()) (scheme int)
        (body
         (Splice
          (code
           (Quote
            (App
             (App (Var (Persistent (Toplevel add)) : "int -> int -> int")
              (Var (Free n) : int) : "int -> int")
             (Var (Free y) : int) : int)
            : "<int>{n : int; y : int}"))
          (subs
           (Explicit
            ((n (Var (Bound (Toplevel n)) : int)) (y (Lit (Int 1) : int)))))
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
      (((name direct) (args (y/9)) (scheme "'a -> 'b")
        (body
         (Let (ident f/11) (args (x/10)) (generalized true)
          (body
           (App (Var (Bound (Local f/11)) : "'c -> 'd")
            (Var (Bound (Local x/10)) : 'c) : 'd))
          (in_
           (App (Var (Bound (Local f/11)) : "'a -> 'b")
            (Var (Bound (Local y/9)) : 'a) : 'b))
          : 'b)))))
     (recursive false))
    ((members
      (((name in_quote) (args ())
        (scheme "<'a -> <'b>{f : 'c -> 'b; x : 'c; ..'d}>{..'e}")
        (body
         (Let (ident f/13) (args (x/12)) (generalized true)
          (body
           (Quote (App (Var (Free f) : "'f -> 'g") (Var (Free x) : 'f) : 'g) :
            "<'g>{f : 'f -> 'g; x : 'f; ..'h}"))
          (in_
           (Quote
            (Var (Persistent (Local f/13)) :
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
      (((name first) (args (p/9)) (scheme "int * 'a -> int")
        (body
         (Match (Var (Bound (Local p/9)) : "int * 'a")
          (((Tuple ((Var a/10 : int) (Any : 'a)) : "int * 'a")
            (Var (Bound (Local a/10)) : int)))
          : int)))))
     (recursive false))
    ((a/10 int) (p/9 "int * 'a"))
    |}]
;;
