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

let ctor name args =
  { Signature.Constructor.name
  ; args
  ; global_args = List.map args ~f:(fun _ -> false)
  }
;;

let field name ty = { Signature.Field.name; ty; is_global = false }

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
  shape t (Type.arrow int bool);
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
  [%expect {| (Error (Too_deep 1000)) |}]
;;

let%expect_test "instantiations through an alias" =
  (* type u['a] = A of 'a type r['a] = u['a] [r[r[int]]] is finite: A (A 1). The
     inner [u] is instantiated with [r[int]] and the outer with [int], so the
     same declaration recurs with different arguments. *)
  let u_parameter = var () in
  let u =
    decl
      ~name:"u"
      ~params:[ Type.ATy (Type.TVar u_parameter) ]
      (variant [ ctor "A" [ Type.TVar u_parameter ] ])
  in
  let r_parameter = var () in
  let r =
    decl
      ~name:"r"
      ~params:[ Type.ATy (Type.TVar r_parameter) ]
      (alias (tycon "u" [ Type.TVar r_parameter ]))
  in
  let t = Runtime_types.create (resolver [ u; r ]) in
  shape t (tycon "r" [ tycon "r" [ int ] ]);
  shape t (tycon "r" [ tycon "r" [ tycon "r" [ int ] ] ]);
  [%expect
    {|
    (Ok
     (Union (repr Default)
      (cases
       ((Struct (repr Default)
         (fields
          ((Union (repr Default)
            (cases
             ((Struct (repr Default) (fields ((Scalar (scalar I64) (valid ())))))))))))))))
    (Ok
     (Union (repr Default)
      (cases
       ((Struct (repr Default)
         (fields
          ((Union (repr Default)
            (cases
             ((Struct (repr Default)
               (fields
                ((Union (repr Default)
                  (cases
                   ((Struct (repr Default)
                     (fields ((Scalar (scalar I64) (valid ())))))))))))))))))))))
    |}]
;;

let%expect_test "instantiations through a structural body" =
  (* type u['a] = {x : 'a}
     type r['a] = u[{y : 'a}]
     [r[r[int]]] is {x = {y = {x = {y = 1}}}}. *)
  let u_parameter = var () in
  let u =
    decl
      ~name:"u"
      ~params:[ Type.ATy (Type.TVar u_parameter) ]
      (record [ field "x" (Type.TVar u_parameter) ])
  in
  let r_parameter = var () in
  let structural =
    Fresh.record fresh (present "y" (Type.TVar r_parameter) Type.REmpty)
  in
  let r =
    decl
      ~name:"r"
      ~params:[ Type.ATy (Type.TVar r_parameter) ]
      (alias (tycon "u" [ structural ]))
  in
  let t = Runtime_types.create (resolver [ u; r ]) in
  shape t (tycon "r" [ tycon "r" [ int ] ]);
  [%expect
    {|
    (Ok
     (Struct (repr Default)
      (fields
       ((Struct (repr Default)
         (fields
          ((Struct (repr Default)
            (fields
             ((Struct (repr Default) (fields ((Scalar (scalar I64) (valid ())))))))))))))))
    |}]
;;

let%expect_test "a parameter that reaches itself" =
  (* type t['a] = U of 'a | T of t['a] [t[int]] never finishes expanding: the
     recursive case binds the parameter to itself. *)
  let parameter = var () in
  let t =
    decl
      ~name:"t"
      ~params:[ Type.ATy (Type.TVar parameter) ]
      (variant
         [ ctor "U" [ Type.TVar parameter ]
         ; ctor "T" [ tycon "t" [ Type.TVar parameter ] ]
         ])
  in
  shape (Runtime_types.create (resolver [ t ])) (tycon "t" [ int ]);
  [%expect {| (Error (Recursive (t t))) |}]
;;

let%expect_test "parameters that reach each other" =
  (* type a['x] = A of b['x] type b['y] = B of a['y] *)
  let a_parameter = var () in
  let b_parameter = var () in
  let a =
    decl
      ~name:"a"
      ~params:[ Type.ATy (Type.TVar a_parameter) ]
      (variant [ ctor "A" [ tycon "b" [ Type.TVar a_parameter ] ] ])
  in
  let b =
    decl
      ~name:"b"
      ~params:[ Type.ATy (Type.TVar b_parameter) ]
      (variant [ ctor "B" [ tycon "a" [ Type.TVar b_parameter ] ] ])
  in
  shape (Runtime_types.create (resolver [ a; b ])) (tycon "a" [ int ]);
  [%expect {| (Error (Recursive (a b a))) |}]
;;

let%expect_test "row parameters and arity" =
  let t = Runtime_types.create (resolver []) in
  shape t (tycon "int" [ bool ]);
  shape t (tycon "ptr" []);
  shape t (tycon "ptr" [ int; int ]);
  shape t (tycon "ptr" [ int ]);
  let row_parameter = var () in
  let s =
    decl
      ~name:"s"
      ~params:[ Type.ARow (Type.RVar row_parameter) ]
      (alias Type.unit)
  in
  shape (Runtime_types.create (resolver [ s ])) (tycon "s" [ int ]);
  [%expect
    {|
    (Error (Wrong_arity (name int) (expected 0) (got 1)))
    (Error (Wrong_arity (name ptr) (expected 1) (got 0)))
    (Error (Wrong_arity (name ptr) (expected 1) (got 2)))
    (Ok Ptr)
    (Error (Row_expected 17))
    |}]
;;

let%expect_test "shared instantiations are expanded once" =
  (* type t0 = [{x : t1; y : t1}] ... type t9 = int. [t1] is expanded once, and
     its shape shared by both fields. *)
  let n = 10 in
  let names = List.init n ~f:(fun i -> sprintf "t%d" i) in
  let declarations =
    List.mapi names ~f:(fun i name ->
      let ty =
        match List.nth names (i + 1) with
        | None -> int
        | Some next -> tycon next []
      in
      decl ~name (record [ field "x" ty; field "y" ty ]))
  in
  let t = Runtime_types.create (resolver declarations) in
  (match Runtime_types.shape t (tycon "t0" []) with
   | Ok (Struct { fields = [ x; y ]; _ }) ->
     printf "fields shared: %b\n" (phys_equal x y)
   | result -> print_s [%sexp (result : (Shape.t, Error.t) Result.t)]);
  [%expect {| fields shared: true |}]
;;

let%expect_test "nested polymorphic recursion" =
  (* type t['a] = L of 'a | N of t[('a * 'a)]: [t[int]] never repeats an
     instantiation, and the shape doubles at each one. *)
  let parameter = var () in
  let nested =
    decl
      ~name:"t"
      ~params:[ Type.ATy (Type.TVar parameter) ]
      (variant
         [ ctor "L" [ Type.TVar parameter ]
         ; ctor
             "N"
             [ tycon
                 "t"
                 [ Type.TTuple [ Type.TVar parameter; Type.TVar parameter ] ]
             ]
         ])
  in
  let t = Runtime_types.create (resolver [ nested ]) in
  shape t (tycon "t" [ int ]);
  layout t (tycon "t" [ int ]);
  (* Behind a pointer, it is finite. *)
  let parameter = var () in
  let boxed =
    decl
      ~name:"t"
      ~params:[ Type.ATy (Type.TVar parameter) ]
      (variant
         [ ctor "L" [ Type.TVar parameter ]
         ; ctor
             "N"
             [ tycon
                 "ptr"
                 [ tycon
                     "t"
                     [ Type.TTuple [ Type.TVar parameter; Type.TVar parameter ]
                     ]
                 ]
             ]
         ])
  in
  shape (Runtime_types.create (resolver [ boxed ])) (tycon "t" [ int ]);
  [%expect
    {|
    (Error (Too_large 100000))
    (Too_large 100000)
    (Ok
     (Union (repr Default)
      (cases
       ((Struct (repr Default) (fields ((Scalar (scalar I64) (valid ())))))
        (Struct (repr Default) (fields (Ptr)))))))
    |}]
;;

let%expect_test "regular recursion behind pointers" =
  (* type list['a] = Nil | Cons of 'a * ptr[list['a]] type tree['a] = Leaf |
     Node of ptr[tree['a]] * 'a * ptr[tree['a]] type even = E of ptr[odd] | Zero
     and odd = O of ptr[even] *)
  let element = var () in
  let list =
    decl
      ~name:"list"
      ~params:[ Type.ATy (Type.TVar element) ]
      (variant
         [ ctor "Nil" []
         ; ctor
             "Cons"
             [ Type.TVar element
             ; tycon "ptr" [ tycon "list" [ Type.TVar element ] ]
             ]
         ])
  in
  let label = var () in
  let subtree = tycon "ptr" [ tycon "tree" [ Type.TVar label ] ] in
  let tree =
    decl
      ~name:"tree"
      ~params:[ Type.ATy (Type.TVar label) ]
      (variant
         [ ctor "Leaf" []; ctor "Node" [ subtree; Type.TVar label; subtree ] ])
  in
  let even =
    decl
      ~name:"even"
      (variant [ ctor "Zero" []; ctor "E" [ tycon "ptr" [ tycon "odd" [] ] ] ])
  in
  let odd =
    decl ~name:"odd" (variant [ ctor "O" [ tycon "ptr" [ tycon "even" [] ] ] ])
  in
  let t = Runtime_types.create (resolver [ list; tree; even; odd ]) in
  shape t (tycon "list" [ int ]);
  shape t (tycon "tree" [ tycon "list" [ bool ] ]);
  shape t (tycon "even" []);
  layout t (tycon "list" [ int ]);
  [%expect
    {|
    (Ok
     (Union (repr Default)
      (cases
       ((Struct (repr Default) (fields ()))
        (Struct (repr Default) (fields ((Scalar (scalar I64) (valid ())) Ptr)))))))
    (Ok
     (Union (repr Default)
      (cases
       ((Struct (repr Default) (fields ()))
        (Struct (repr Default)
         (fields
          (Ptr
           (Union (repr Default)
            (cases
             ((Struct (repr Default) (fields ()))
              (Struct (repr Default)
               (fields ((Scalar (scalar I8) (valid ((0 1)))) Ptr))))))
           Ptr)))))))
    (Ok
     (Union (repr Default)
      (cases
       ((Struct (repr Default) (fields ()))
        (Struct (repr Default) (fields (Ptr)))))))
    (8 1
     ((Niche (leaf (Ptr (ptr_index 0))) (untagged 1) (first 0) (last 0)
       (start 0))))
    |}]
;;

let%expect_test "substitution copies have their own ids" =
  (* type u['a] = [{x : 'a}] type r['a] = u[{y : 'a}] Expanding [r[int]] copies
     [{y : 'a}] as [{y : int}]. A record of the program with the same id is
     another type: [u] of it is not cached as the copy's. *)
  let u_parameter = var () in
  let u =
    decl
      ~name:"u"
      ~params:[ Type.ATy (Type.TVar u_parameter) ]
      (record [ field "x" (Type.TVar u_parameter) ])
  in
  let r_parameter = var () in
  let structural =
    Fresh.record fresh (present "y" (Type.TVar r_parameter) Type.REmpty)
  in
  let r =
    decl
      ~name:"r"
      ~params:[ Type.ATy (Type.TVar r_parameter) ]
      (alias (tycon "u" [ structural ]))
  in
  let t = Runtime_types.create (resolver [ u; r ]) in
  List.iter [ 0; 1; 2; 3 ] ~f:(fun id ->
    let program_record =
      Type.TRecord { id; row = present "y" bool Type.REmpty }
    in
    match
      Runtime_types.shape
        t
        (Type.TTuple [ tycon "r" [ int ]; tycon "u" [ program_record ] ])
    with
    | Ok (Struct { fields = [ _; program_shape ]; _ }) ->
      print_s [%sexp (id : int), (program_shape : Shape.t)]
    | result -> print_s [%sexp (result : (Shape.t, Error.t) Result.t)]);
  [%expect
    {|
    (0
     (Struct (repr Default)
      (fields
       ((Struct (repr Default) (fields ((Scalar (scalar I8) (valid ((0 1)))))))))))
    (1
     (Struct (repr Default)
      (fields
       ((Struct (repr Default) (fields ((Scalar (scalar I8) (valid ((0 1)))))))))))
    (2
     (Struct (repr Default)
      (fields
       ((Struct (repr Default) (fields ((Scalar (scalar I8) (valid ((0 1)))))))))))
    (3
     (Struct (repr Default)
      (fields
       ((Struct (repr Default) (fields ((Scalar (scalar I8) (valid ((0 1)))))))))))
    |}]
;;

let%expect_test "the cache is kept across calls" =
  (* type wrap['a] = W of 'a type t = T of wrap[t] [wrap[t]] is not cached by
     [wrap[int]], nor [t] by refusing it. *)
  let parameter = var () in
  let wrap =
    decl
      ~name:"wrap"
      ~params:[ Type.ATy (Type.TVar parameter) ]
      (variant [ ctor "W" [ Type.TVar parameter ] ])
  in
  let self =
    decl ~name:"t" (variant [ ctor "T" [ tycon "wrap" [ tycon "t" [] ] ] ])
  in
  let t = Runtime_types.create (resolver [ wrap; self ]) in
  let first = Runtime_types.shape t (tycon "wrap" [ int ]) in
  let second = Runtime_types.shape t (tycon "wrap" [ int ]) in
  let same_shape = Result.equal phys_equal phys_equal first second in
  print_s [%sexp (first : (Shape.t, Error.t) Result.t), ~~(same_shape : bool)];
  shape t (tycon "t" []);
  shape t (tycon "wrap" [ tycon "t" [] ]);
  shape t (tycon "t" []);
  [%expect
    {|
    ((Ok
      (Union (repr Default)
       (cases
        ((Struct (repr Default) (fields ((Scalar (scalar I64) (valid ())))))))))
     (same_shape true))
    (Error (Recursive (t wrap t)))
    (Error (Recursive (wrap t wrap)))
    (Error (Recursive (t wrap t)))
    |}]
;;

let%expect_test "a parameter bound to different rows" =
  (* type u['a] = U of 'a type box['r] = B of u['r] [box[{x : int}]] and
     [box[{x : bool}]] expand [u] at records of different rows. *)
  let declarations () =
    let u_parameter = var () in
    let u =
      decl
        ~name:"u"
        ~params:[ Type.ATy (Type.TVar u_parameter) ]
        (variant [ ctor "U" [ Type.TVar u_parameter ] ])
    in
    let box_parameter = var () in
    let box =
      decl
        ~name:"box"
        ~params:[ Type.ATy (Type.TVar box_parameter) ]
        (variant [ ctor "B" [ tycon "u" [ Type.TVar box_parameter ] ] ])
    in
    [ u; box ]
  in
  let box_of field_ty =
    Type.TCon ("box", [ Type.ARow (present "x" field_ty Type.REmpty) ])
  in
  let field_scalar t ty =
    let rec go (shape : Shape.t) =
      match shape with
      | Scalar { scalar; _ } -> [ scalar ]
      | Ptr -> []
      | Struct { fields = shapes; _ } | Union { cases = shapes; _ } ->
        List.concat_map shapes ~f:go
      | Array { elt; _ } -> go elt
    in
    print_s
      [%sexp
        (Result.map (Runtime_types.shape t ty) ~f:go
         : (Shape.Scalar.t list, Error.t) Result.t)]
  in
  let t = Runtime_types.create (resolver (declarations ())) in
  field_scalar t (box_of int);
  field_scalar t (box_of bool);
  let t = Runtime_types.create (resolver (declarations ())) in
  field_scalar t (Type.TTuple [ box_of bool; box_of int ]);
  [%expect {|
    (Ok (I64))
    (Ok (I8))
    (Ok (I8 I64))
    |}]
;;

let%expect_test "a declaration does not see its user's bindings" =
  (* Ill-formed: type u = 'a type w['a] = u *)
  let parameter = var () in
  let u = decl ~name:"u" (alias (Type.TVar parameter)) in
  let w =
    decl
      ~name:"w"
      ~params:[ Type.ATy (Type.TVar parameter) ]
      (alias (tycon "u" []))
  in
  let t = Runtime_types.create (resolver [ u; w ]) in
  shape t (tycon "w" [ int ]);
  shape t (tycon "w" [ bool ]);
  shape t (tycon "u" []);
  [%expect
    {|
    (Error (Unresolved_ty 30))
    (Error (Unresolved_ty 30))
    (Error (Unresolved_ty 30))
    |}]
;;

let%expect_test "the work bound does not depend on the cache" =
  (* type d0 = int type d(i+1) = d(i) * d(i): [d(i)] has 2^i leaves, each
     reached through the cache. *)
  let n = 60 in
  let d i = tycon (sprintf "d%d" i) [] in
  let declarations =
    List.init n ~f:(fun i ->
      decl
        ~name:(sprintf "d%d" i)
        (alias (if i = 0 then int else Type.TTuple [ d (i - 1); d (i - 1) ])))
  in
  (* Nodes of the shape as a tree, up to a million. *)
  let summary t ty =
    let count = ref 0 in
    let rec visit (shape : Shape.t) =
      incr count;
      if !count <= 1_000_000
      then (
        match shape with
        | Scalar _ | Ptr -> ()
        | Struct { fields = shapes; _ } | Union { cases = shapes; _ } ->
          List.iter shapes ~f:visit
        | Array { elt; _ } -> visit elt)
    in
    print_s
      [%sexp
        (Result.map (Runtime_types.shape t ty) ~f:(fun shape ->
           visit shape;
           if !count > 1_000_000
           then "more than a million"
           else Int.to_string !count)
         : (string, Error.t) Result.t)]
  in
  let pair = Type.TTuple [ d 14; d 14 ] in
  summary (Runtime_types.create (resolver declarations)) (d (n - 1));
  summary (Runtime_types.create (resolver declarations)) pair;
  let t = Runtime_types.create (resolver declarations) in
  summary t (d 14);
  summary t pair;
  summary t (d (n - 1));
  [%expect
    {|
    (Error (Too_large 100000))
    (Error (Too_large 100000))
    (Ok 32767)
    (Error (Too_large 100000))
    (Error (Too_large 100000))
    |}]
;;
