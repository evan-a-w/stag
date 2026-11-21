open! Import

(* Types are built by hand: what the checker produces is [Type.ty]s, and the
   prelude's declarations are only one source of them. *)

let fresh = Fresh.create ()
let var () = Fresh.var fresh ~rank:1
let tycon name args = Type.TCon (name, List.map args ~f:(fun ty -> Type.ATy ty))
let int = tycon "int" []
let bool = tycon "bool" []
let decl ?(params = []) ~name kind = { Signature.Type_decl.name; params; kind }
let variant constructors = Signature.Type_decl.Kind.Variant constructors
let record fields = Signature.Type_decl.Kind.Record fields
let alias ty = Signature.Type_decl.Kind.Alias ty
let ctor name args = { Signature.Constructor.name; args }
let field name ty = { Signature.Field.name; ty }

let present label ty rest =
  Type.RExtend (label, Type.Present, Type.Absent, ty, rest)
;;

let absent label rest =
  Type.RExtend (label, Type.Absent, Type.Absent, Type.unit, rest)
;;

let resolver declarations =
  Resolver.of_signatures
    [ ( ""
      , List.map declarations ~f:(fun declaration ->
          Signature.Item.Type declaration) )
    ]
;;

let shape t ty =
  print_s [%sexp (Runtime_types.shape t ty : (Shape.t, Error.t) Result.t)]
;;

let layout t ty =
  match Runtime_types.layout t ty with
  | Error error -> print_s [%sexp (error : Error.t)]
  | Ok layout ->
    print_s
      [%sexp
        (Layout.scalar_bytes layout : int)
        , (Layout.ptr_words layout : int)
        , (Layout.discriminant layout : Layout.Discriminant.t option)]
;;

let%expect_test "builtins" =
  let t = Runtime_types.create Resolver.empty in
  List.iter
    [ int
    ; bool
    ; tycon "char" []
    ; tycon "string" []
    ; tycon "exn" []
    ; Type.unit
    ; tycon "ptr" [ int ]
    ]
    ~f:(shape t);
  [%expect
    {|
    (Ok (Scalar (scalar I64) (valid ())))
    (Ok (Scalar (scalar I8) (valid ((0 1)))))
    (Ok (Scalar (scalar I8) (valid ())))
    (Ok Ptr)
    (Ok Ptr)
    (Ok (Struct (repr Default) (fields ())))
    (Ok Ptr)
    |}]
;;

let%expect_test "compound" =
  let t = Runtime_types.create Resolver.empty in
  shape t (Type.TTuple [ int; bool ]);
  shape t (Type.TArrow (int, bool));
  shape t (Type.TCode { inner = int; context = Type.REmpty });
  layout t (Type.TTuple [ int; bool ]);
  [%expect
    {|
    (Ok
     (Struct (repr Default)
      (fields
       ((Scalar (scalar I64) (valid ())) (Scalar (scalar I8) (valid ((0 1))))))))
    (Ok Ptr)
    (Ok Ptr)
    (16 0 ())
    |}]
;;

let%expect_test "declarations" =
  let t =
    Runtime_types.create
      (resolver
         [ decl ~name:"point" (record [ field "x" int; field "y" int ])
         ; decl
             ~name:"figure"
             (variant
                [ ctor "Dot" []
                ; ctor "Line" [ int; int ]
                ; ctor "Poly" [ tycon "point" [] ]
                ])
         ; decl ~name:"flag" (alias bool)
         ])
  in
  shape t (tycon "point" []);
  layout t (tycon "point" []);
  shape t (tycon "figure" []);
  layout t (tycon "figure" []);
  shape t (tycon "flag" []);
  [%expect
    {|
    (Ok
     (Struct (repr Default)
      (fields
       ((Scalar (scalar I64) (valid ())) (Scalar (scalar I64) (valid ()))))))
    (16 0 ())
    (Ok
     (Union (repr Default)
      (cases
       ((Struct (repr Default) (fields ()))
        (Struct (repr Default)
         (fields
          ((Scalar (scalar I64) (valid ())) (Scalar (scalar I64) (valid ())))))
        (Struct (repr Default)
         (fields
          ((Struct (repr Default)
            (fields
             ((Scalar (scalar I64) (valid ())) (Scalar (scalar I64) (valid ()))))))))))))
    (24 0 ((Tag (Scalar (scalar_offset 0) (scalar I8)))))
    (Ok (Scalar (scalar I8) (valid ((0 1)))))
    |}]
;;

let%expect_test "parameters" =
  let parameter = var () in
  let pair =
    decl
      ~name:"pair"
      ~params:[ Type.ATy (Type.TVar parameter) ]
      (variant [ ctor "P" [ Type.TVar parameter; Type.TVar parameter ] ])
  in
  let ints = decl ~name:"ints" (alias (tycon "pair" [ int ])) in
  let t = Runtime_types.create (resolver [ pair; ints ]) in
  (* The same declaration at different arguments, nested: finite. *)
  shape t (tycon "pair" [ int ]);
  shape t (tycon "pair" [ tycon "pair" [ int ] ]);
  layout t (tycon "pair" [ bool ]);
  shape t (tycon "ints" []);
  [%expect
    {|
    (Ok
     (Union (repr Default)
      (cases
       ((Struct (repr Default)
         (fields
          ((Scalar (scalar I64) (valid ())) (Scalar (scalar I64) (valid ())))))))))
    (Ok
     (Union (repr Default)
      (cases
       ((Struct (repr Default)
         (fields
          ((Union (repr Default)
            (cases
             ((Struct (repr Default)
               (fields
                ((Scalar (scalar I64) (valid ()))
                 (Scalar (scalar I64) (valid ()))))))))
           (Union (repr Default)
            (cases
             ((Struct (repr Default)
               (fields
                ((Scalar (scalar I64) (valid ()))
                 (Scalar (scalar I64) (valid ())))))))))))))))
    (2 0 ((Single (case 0))))
    (Ok
     (Union (repr Default)
      (cases
       ((Struct (repr Default)
         (fields
          ((Scalar (scalar I64) (valid ())) (Scalar (scalar I64) (valid ())))))))))
    |}]
;;

let%expect_test "structural" =
  let t = Runtime_types.create Resolver.empty in
  (* Sorted by label, absent entries dropped. *)
  shape
    t
    (Type.TRecord
       { id = 1
       ; row = present "y" bool (present "x" int (absent "z" Type.REmpty))
       });
  (* The first entry of a label shadows later ones. *)
  shape
    t
    (Type.TRecord
       { id = 2; row = present "x" int (present "x" bool Type.REmpty) });
  (* Tags sorted. *)
  shape
    t
    (Type.TVariant
       { id = 3; row = present "B" int (present "A" Type.unit Type.REmpty) });
  layout
    t
    (Type.TVariant
       { id = 3; row = present "B" int (present "A" Type.unit Type.REmpty) });
  (* Open, or of unknown presence: not concrete. *)
  shape t (Type.TRecord { id = 4; row = present "x" int (Type.RVar (var ())) });
  shape
    t
    (Type.TRecord
       { id = 5
       ; row =
           Type.RExtend ("x", Type.PVar (var ()), Type.Absent, int, Type.REmpty)
       });
  [%expect
    {|
    (Ok
     (Struct (repr Default)
      (fields
       ((Scalar (scalar I64) (valid ())) (Scalar (scalar I8) (valid ((0 1))))))))
    (Ok (Struct (repr Default) (fields ((Scalar (scalar I64) (valid ()))))))
    (Ok
     (Union (repr Default)
      (cases
       ((Struct (repr Default) (fields ((Struct (repr Default) (fields ())))))
        (Struct (repr Default) (fields ((Scalar (scalar I64) (valid ())))))))))
    (16 0 ((Tag (Scalar (scalar_offset 0) (scalar I8)))))
    (Error (Unresolved_row 1))
    (Error (Unresolved_presence 2))
    |}]
;;

let%expect_test "recursion" =
  (* type t = T of t *)
  let self = decl ~name:"t" (variant [ ctor "T" [ tycon "t" [] ] ]) in
  shape (Runtime_types.create (resolver [ self ])) (tycon "t" []);
  (* type t = T of wrap[t], type wrap['a] = W of 'a *)
  let parameter = var () in
  let wrap =
    decl
      ~name:"wrap"
      ~params:[ Type.ATy (Type.TVar parameter) ]
      (variant [ ctor "W" [ Type.TVar parameter ] ])
  in
  let through_parameter =
    decl ~name:"t" (variant [ ctor "T" [ tycon "wrap" [ tycon "t" [] ] ] ])
  in
  shape
    (Runtime_types.create (resolver [ through_parameter; wrap ]))
    (tycon "t" []);
  (* A pointer is a boundary: this is finite. *)
  let through_pointer =
    decl ~name:"t" (variant [ ctor "T" [ tycon "ptr" [ tycon "t" [] ] ] ])
  in
  let t = Runtime_types.create (resolver [ through_pointer ]) in
  shape t (tycon "t" []);
  layout t (tycon "t" []);
  (* A structural record that reaches itself. *)
  let v = var () in
  let self =
    Type.TRecord { id = 7; row = present "next" (Type.TVar v) Type.REmpty }
  in
  v.state <- Type.Link self;
  shape (Runtime_types.create Resolver.empty) self;
  [%expect
    {|
    (Error (Recursive (t t)))
    (Error (Recursive (t wrap t)))
    (Ok (Union (repr Default) (cases ((Struct (repr Default) (fields (Ptr)))))))
    (0 1 ((Single (case 0))))
    (Error (Recursive ("record 7" "record 7")))
    |}]
;;

let%expect_test "errors" =
  let parameter = var () in
  let t =
    Runtime_types.create
      (resolver
         [ decl ~name:"abstract" Signature.Type_decl.Kind.Abstract
         ; decl
             ~name:"one"
             ~params:[ Type.ATy (Type.TVar parameter) ]
             (alias (Type.TVar parameter))
         ])
  in
  shape t (tycon "missing" []);
  shape t (tycon "abstract" []);
  shape t (tycon "one" []);
  shape t (Type.TVar (var ()));
  shape t Type.error;
  [%expect
    {|
    (Error (Unknown_type missing))
    (Error (Abstract_type abstract))
    (Error (Wrong_arity (name one) (expected 1) (got 0)))
    (Error (Unresolved_ty 6))
    (Error Error_type)
    |}]
;;

let%expect_test "modules" =
  let t =
    Runtime_types.create
      (Resolver.of_signatures
         [ ( ""
           , [ Signature.Item.Module
                 { name = "M"
                 ; items = [ Signature.Item.Type (decl ~name:"t" (alias int)) ]
                 }
             ] )
         ])
  in
  shape t (tycon "M.t" []);
  shape t (tycon "t" []);
  [%expect
    {|
    (Ok (Scalar (scalar I64) (valid ())))
    (Error (Unknown_type t))
    |}]
;;

let%expect_test "polymorphic recursion" =
  (* type t['a] = T of t[list['a]]: no finite layout, and no end to expanding it
     either. *)
  let element = var () in
  let list =
    decl
      ~name:"list"
      ~params:[ Type.ATy (Type.TVar element) ]
      (variant [ ctor "Nil" []; ctor "Cons" [ Type.TVar element ] ])
  in
  let parameter = var () in
  let poly =
    decl
      ~name:"t"
      ~params:[ Type.ATy (Type.TVar parameter) ]
      (variant
         [ ctor "T" [ tycon "t" [ tycon "list" [ Type.TVar parameter ] ] ] ])
  in
  shape (Runtime_types.create (resolver [ list; poly ])) (tycon "t" [ int ]);
  [%expect {| (Error (Recursive (t t t))) |}]
;;
