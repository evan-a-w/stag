open! Import

let%expect_test "registered declarations" =
  List.iter (Ppx_rust_runtime.all ()) ~f:(fun decl ->
    print_s [%sexp (decl : Ppx_rust_runtime.Decl.t)]);
  [%expect
    {|
    ((key (Example id)) (params ()) (kind (Alias Int)) (shared false))
    ((key (Example Pos t)) (params ())
     (kind
      (Record
       (((name line) (ty Int) (mutable_ false) (skip false))
        ((name col) (ty Int) (mutable_ false) (skip false)))))
     (shared false))
    ((key (Example located)) (params (a))
     (kind
      (Record
       (((name txt) (ty (Var a)) (mutable_ false) (skip false))
        ((name pos) (ty (Decl (Example Pos t) ())) (mutable_ false) (skip false)))))
     (shared false))
    ((key (Example Name t)) (params ())
     (kind (Alias (Decl (Example located) (String)))) (shared false))
    ((key (Example Record_type t)) (params ())
     (kind
      (Record
       (((name name) (ty String) (mutable_ false) (skip false))
        ((name fields) (ty (Array String)) (mutable_ false) (skip false))
        ((name ignored) (ty (Tuple ())) (mutable_ false) (skip true))
        ((name arity) (ty Int) (mutable_ false) (skip false)))))
     (shared true))
    ((key (Example var)) (params (a))
     (kind
      (Record
       (((name id) (ty (Decl (Example id) ())) (mutable_ false) (skip false))
        ((name link) (ty (Option (Var a))) (mutable_ true) (skip false)))))
     (shared false))
    ((key (Example ty)) (params ())
     (kind
      (Variant
       (((name Int) (args (Tuple ())))
        ((name Arrow)
         (args (Tuple ((Decl (Example ty) ()) (Decl (Example ty) ())))))
        ((name Var)
         (args (Tuple ((Decl (Example var) ((Decl (Example ty) ())))))))
        ((name Record)
         (args
          (Record
           (((name record) (ty (Decl (Example Record_type t) ()))
             (mutable_ false) (skip false))
            ((name skipped) (ty (Tuple ())) (mutable_ false) (skip true))
            ((name fields) (ty (List (Tuple (String (Decl (Example ty) ())))))
             (mutable_ false) (skip false))))))
        ((name Unit) (args (Tuple ())))
        ((name Tuple)
         (args
          (Tuple ((Tuple ((Decl (Example ty) ()) (Decl (Example Name t) ())))))))
        ((name Chars) (args (Tuple (Char (Array Bool))))))))
     (shared true))
    ((key (Example Expr Desc t)) (params (e))
     (kind
      (Variant
       (((name Lit) (args (Tuple (Int))))
        ((name App) (args (Tuple ((Var e) (Var e)))))
        ((name Let)
         (args
          (Record
           (((name name) (ty (Decl (Example Name t) ())) (mutable_ false)
             (skip false))
            ((name body) (ty (Var e)) (mutable_ false) (skip false))
            ((name in_) (ty (Var e)) (mutable_ false) (skip false)))))))))
     (shared false))
    ((key (Example Expr t)) (params ())
     (kind
      (Record
       (((name desc)
         (ty (Decl (Example Expr Desc t) ((Decl (Example Expr t) ()))))
         (mutable_ false) (skip false))
        ((name ty) (ty (Decl (Example ty) ())) (mutable_ false) (skip false))
        ((name env) (ty (Map String (Decl (Example ty) ()))) (mutable_ false)
         (skip false))
        ((name free) (ty (Set String)) (mutable_ false) (skip false))
        ((name ids) (ty (Map Int Int)) (mutable_ false) (skip false))
        ((name next) (ty (Option (Decl (Example Expr t) ()))) (mutable_ false)
         (skip false)))))
     (shared false))
    ((key (Example Expr ty)) (params ()) (kind (Alias (Decl (Example ty) ())))
     (shared false))
    ((key (Example Defs expr)) (params ())
     (kind (Alias (Decl (Example located) ((Decl (Example Defs expr_desc) ())))))
     (shared false))
    ((key (Example Defs expr_desc)) (params ())
     (kind
      (Variant
       (((name Const) (args (Tuple (Char Bool))))
        ((name Pair)
         (args
          (Tuple
           ((Decl (Example Defs expr) ()) (Option (Decl (Example Defs expr) ()))))))
        ((name Never) (args (Tuple (Nothing))))
        ((name Absurd) (args (Tuple ((List Nothing))))))))
     (shared false))
    ((key (Example Re t)) (params ())
     (kind (Alias (Decl (Example Defs expr) ()))) (shared false))
    ((key (Example Var t)) (params (a))
     (kind (Alias (Decl (Example var) ((Var a))))) (shared false))
    ((key (Example Chain t)) (params (a))
     (kind (Alias (Decl (Example Var t) ((Var a))))) (shared false))
    ((key (Example chained)) (params ())
     (kind
      (Record
       (((name var) (ty (Decl (Example Chain t) ((Decl (Example ty) ()))))
         (mutable_ false) (skip false)))))
     (shared false))
    |}]
;;

let decl ?(module_path = [ "M" ]) ?(params = []) ?(shared = false) name kind
  : Ppx_rust_runtime.Decl.t
  =
  { key = module_path @ [ name ]; params; kind; shared }
;;

let con ?(args = []) key : Ppx_rust_runtime.Type_expr.t = Decl (key, args)

let field ?(mutable_ = false) name ty : Ppx_rust_runtime.Field.t =
  { name; ty; mutable_; skip = false }
;;

let print_error ?(rebuilt = []) decls =
  match Rust_bindgen.generate ~rebuilt decls with
  | Ok _ -> print_endline "ok"
  | Error error -> print_s [%sexp (error : Error.t)]
;;

let%expect_test "errors" =
  print_error [ decl "t" (Alias (con [ "Unknown"; "t" ])) ];
  print_error [ decl "t" ~params:[ "a" ] (Alias Int) ];
  print_error [ decl "t" (Alias (Var "a")) ];
  print_error
    [ decl "t" ~module_path:[ "M"; "N" ] (Alias Int)
    ; decl "n" ~module_path:[ "M" ] (Alias Int)
    ];
  print_error [ decl "t" (Alias Int); decl "t" (Alias Int) ];
  print_error [ decl "arena" ~module_path:[] (Alias Int) ];
  print_error [ decl "t" ~module_path:[ "Raw"; "X" ] (Alias Int) ];
  print_error
    [ decl "r" ~params:[ "a" ] (Record [ field ~mutable_:true "x" (Var "a") ])
    ; decl
        "s"
        ~params:[ "b" ]
        (Record [ field "y" (con [ "M"; "r" ] ~args:[ Var "b" ]) ])
    ];
  print_error
    [ decl
        "v"
        (Variant
           [ { name = "A"; args = Record [ field ~mutable_:true "x" Int ] } ])
    ];
  print_error [ decl "t" (Record [ { (field "x" Int) with skip = true } ]) ];
  print_error ~rebuilt:[ [ "M"; "t" ] ] [ decl "t" (Record [ field "x" Int ]) ];
  print_error
    [ decl "t" (Record [ field "x" (Map (Tuple [ Int; Int ], Int)) ]) ];
  [%expect
    {|
    ("Unknown type (is its library linked?)" (type_ Unknown.t) (in_ M.t))
    ("Unused type parameters" (type_ M.t) (unused (a)))
    ("Unbound type variable" a)
    ("Same Rust name" (rust m::N) (types (M.N.t M.n)))
    ("Registered twice" (type_ M.t))
    ("Rust name clashes with the crate's own" (rust Arena) (type_ arena))
    ("Rust name clashes with the crate's own" (rust raw::X) (type_ Raw.X.t))
    ("Stored types must be instantiated without type parameters" (type_ M.r))
    ("Mutable fields of inline records are unsupported" (constructor A))
    ("Declarations with [@rust.skip] fields need Ppx_rust_runtime.register_rebuild"
     (type_ M.t))
    ("Rebuilt declarations need [@rust.skip] fields" (type_ M.t))
    ("Unsupported map key or set element" (ty (Tuple (Int Int))))
    |}]
;;

let%expect_test "declarations rebuilt by OCaml callbacks" =
  print_s [%sexp (Ppx_rust_runtime.rebuilt () : Ppx_rust_runtime.Key.t list)];
  [%expect {| ((Example Record_type t) (Example ty)) |}]
;;
