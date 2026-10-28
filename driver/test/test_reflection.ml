open! Import

let check = File_check.check
let run source = Source_helpers.run ~prelude:false source ~eval:false

let%expect_test "the type of a type" =
  check
    {|
type t = A
module M = struct
  type u['a] = {x : 'a}
end
let a = type int
let b = type t
let c = type M.u[t]
let d = type list['a]
let e = type (int * t -> M.u[string])
let f = type {`x : int; ..'r}
let g (x : Stag.Type.t[list[int]]) = x
let h = g (type list[int])
|};
  [%expect
    {|
    module A : sig
      type t = A
      module M : sig
        type u['a] = { x : 'a }
      end
      val a : Stag.Type.t[int]
      val b : Stag.Type.t[t]
      val c : Stag.Type.t[M.u[t]]
      val d : Stag.Type.t[list['a]]
      val e : Stag.Type.t[int * t -> M.u[string]]
      val f : Stag.Type.t[{`x : int; ..'a}]
      val g : Stag.Type.t[list[int]] -> Stag.Type.t[list[int]]
      val h : Stag.Type.t[list[int]]
    end
    |}]
;;

let%expect_test "errors in a type are type errors where it is written" =
  check {|let a = type u|};
  check {|let a = type list|};
  check {|let a = type list[int, int]|};
  check {|let a = type M.t|};
  check
    {|
let g (x : Stag.Type.t[list[int]]) = x
let h = g (type list[string])
|};
  [%expect
    {|
    File "a.stag", line 1, characters 13-14:
    1 | let a = type u
                     ^
    Error: Unbound type u
    File "a.stag", line 1, characters 13-17:
    1 | let a = type list
                     ^^^^
    Error: The type list expects 1 argument(s), but is given 0
    File "a.stag", line 1, characters 13-27:
    1 | let a = type list[int, int]
                     ^^^^^^^^^^^^^^
    Error: The type list expects 1 argument(s), but is given 2
    File "a.stag", line 1, characters 13-14:
    1 | let a = type M.t
                     ^
    Error: Unbound module M
    File "a.stag", line 3, characters 10-29:
    3 | let h = g (type list[string])
                  ^^^^^^^^^^^^^^^^^^^
    Error: This expression has type string but an expression was expected of type int
    |}]
;;

(* [head ty] is the ref of the declared type [ty] is an instance of. *)
let reflect =
  {|
let head ty =
  match Stag.Type.expr ty with
  | Stag.Type_expr.Con (Stag.Path.Ref r, _) -> r
let decl ty = Stag.Type.decl (head ty)
|}
;;

let%expect_test "reflected types and declarations" =
  run
    (reflect
     ^ {|
module M = struct
  type t = A | B of int * t
end
type t = M.t
type point = {x : int; y : M.t}
type tree['a] = Leaf | Node of tree['a] * 'a * tree['a]
type row[{..'r}] = {`x : int; ..'r}
type abstract
let types =
  ( Stag.Type.expr (type int)
  , Stag.Type.expr (type list[M.t])
  , Stag.Type.expr (type (t * (int -> string)))
  , Stag.Type.expr (type tree['a])
  , Stag.Type.expr (type row[{`y : bool}]) )
let decls =
  ( decl (type M.t)
  , decl (type t)
  , decl (type point)
  , decl (type tree[int])
  , decl (type row[{}])
  , decl (type list[int]) )
let abstract = (decl (type abstract), decl (type int), decl (type string))
include .~(let _ = (types, decls, abstract) in Nil)
|}
    );
  [%expect
    {|
    val head : Stag.Type.t['a] -> Stag.Type_ref.t
    val decl : Stag.Type.t['a] -> option[Stag.Type.Decl.t]
    module M : sig
      type t = A | B of int * t
    end
    type t = M.t
    type point = { x : int; y : M.t }
    type tree['a] = Leaf | Node of tree['a] * 'a * tree['a]
    type row[{..'a}] = {`x : int; ..'a}
    type abstract
    val types : Stag.Type_expr.t * Stag.Type_expr.t * Stag.Type_expr.t * Stag.Type_expr.t * Stag.Type_expr.t
    val decls : option[Stag.Type.Decl.t] * option[Stag.Type.Decl.t] * option[Stag.Type.Decl.t] * option[Stag.Type.Decl.t] * option[Stag.Type.Decl.t] * option[Stag.Type.Decl.t]
    val abstract : option[Stag.Type.Decl.t] * option[Stag.Type.Decl.t] * option[Stag.Type.Decl.t]
    -- evaluated at compile time
    (head <fun>)
    (decl <fun>)
    (types
     (Tuple (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type int))) (Nil))
      (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type list)))
       (Cons
        (Stag.Type_expr.Arg.Ty
         (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type M.t))) (Nil)))
        (Nil)))
      (Stag.Type_expr.Tuple
       (Cons (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type t))) (Nil))
        (Cons
         (Stag.Type_expr.Arrow
          (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type int))) (Nil))
          (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type string))) (Nil)))
         (Nil))))
      (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type tree)))
       (Cons (Stag.Type_expr.Arg.Ty (Stag.Type_expr.Var (String a))) (Nil)))
      (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type row)))
       (Cons
        (Stag.Type_expr.Arg.Row
         (Stag.Type_expr.Row.Extend (String y)
          (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type bool))) (Nil))
          (Stag.Type_expr.Row.Empty)))
        (Nil)))))
    (decls
     (Tuple
      (Some
       (Stag.Type.Decl.t (params (Nil))
        (kind
         (Stag.Type.Kind.Variant
          (Cons (Tuple (Ref (constructor M.A)) (Nil))
           (Cons
            (Tuple (Ref (constructor M.B))
             (Cons (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type int))) (Nil))
              (Cons (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type M.t))) (Nil))
               (Nil))))
            (Nil)))))))
      (Some
       (Stag.Type.Decl.t (params (Nil))
        (kind
         (Stag.Type.Kind.Alias
          (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type M.t))) (Nil))))))
      (Some
       (Stag.Type.Decl.t (params (Nil))
        (kind
         (Stag.Type.Kind.Record
          (Cons
           (Tuple (Ref (field x))
            (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type int))) (Nil)))
           (Cons
            (Tuple (Ref (field y))
             (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type M.t))) (Nil)))
            (Nil)))))))
      (Some
       (Stag.Type.Decl.t
        (params (Cons (Stag.Type_decl.Param.Ty (String a)) (Nil)))
        (kind
         (Stag.Type.Kind.Variant
          (Cons (Tuple (Ref (constructor Leaf)) (Nil))
           (Cons
            (Tuple (Ref (constructor Node))
             (Cons
              (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type tree)))
               (Cons (Stag.Type_expr.Arg.Ty (Stag.Type_expr.Var (String a)))
                (Nil)))
              (Cons (Stag.Type_expr.Var (String a))
               (Cons
                (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type tree)))
                 (Cons (Stag.Type_expr.Arg.Ty (Stag.Type_expr.Var (String a)))
                  (Nil)))
                (Nil)))))
            (Nil)))))))
      (Some
       (Stag.Type.Decl.t
        (params (Cons (Stag.Type_decl.Param.Row (String r)) (Nil)))
        (kind
         (Stag.Type.Kind.Alias
          (Stag.Type_expr.Record
           (Stag.Type_expr.Row.Extend (String x)
            (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type int))) (Nil))
            (Stag.Type_expr.Row.Var (String r))))))))
      (Some
       (Stag.Type.Decl.t
        (params (Cons (Stag.Type_decl.Param.Ty (String a)) (Nil)))
        (kind
         (Stag.Type.Kind.Variant
          (Cons (Tuple (Ref (constructor Nil)) (Nil))
           (Cons
            (Tuple (Ref (constructor Cons))
             (Cons (Stag.Type_expr.Var (String a))
              (Cons
               (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type list)))
                (Cons (Stag.Type_expr.Arg.Ty (Stag.Type_expr.Var (String a)))
                 (Nil)))
               (Nil))))
            (Nil)))))))))
    (abstract (Tuple (None) (None) (None)))
    |}]
;;

let%expect_test "reflection values at run time" =
  check
    ~run:true
    (reflect
     ^ {|
type t = A of int
let shown =
  match Stag.Type.expr (type list[t]) with
  | Stag.Type_expr.Con (Stag.Path.Ref r, Cons (Stag.Type_expr.Arg.Ty (Stag.Type_expr.Con (Stag.Path.Ref t, _)), Nil)) ->
    print_endline (Stag.Type_ref.name r);
    print_endline (Stag.Type_ref.name t)
let failed =
  print_endline "before";
  Stag.Type.decl (head (type t))
|}
    );
  [%expect
    {|
    module A : sig
      val head : Stag.Type.t['a] -> Stag.Type_ref.t
      val decl : Stag.Type.t['a] -> option[Stag.Type.Decl.t]
      type t = A of int
      val shown : unit
      val failed : option[Stag.Type.Decl.t]
    end
    File "a.stag", line 15, characters 2-32:
    15 |   Stag.Type.decl (head (type t))
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: Stag.Type.decl is only available at compile time
    --- output ---
    list
    t
    before
    |}]
;;

(* [derive ty] is [to_string] of the variant [ty], showing [int] arguments and
   those of the type itself. *)
let derive =
  reflect
  ^ {|
let append a b = Stag.Expr.apply (Stag.Expr.var "String.append") (Cons (a, Cons (b, Nil)))

let show_arg self ty v =
  match ty with
  | Stag.Type_expr.Con (Stag.Path.Ref r, _) ->
    (match String.equal (Stag.Type_ref.name r) "int" with
     | true -> Stag.Expr.apply (Stag.Expr.var "Int.to_string") (Cons (v, Nil))
     | false ->
       (match String.equal (Stag.Type_ref.name r) self with
        | true -> Stag.Expr.apply (Stag.Expr.var "to_string") (Cons (v, Nil))
        | false -> Stag.Expr.string "?"))
  | _ -> Stag.Expr.string "?"

let vars i args =
  match args with
  | Nil -> Nil
  | Cons (_, rest) -> Cons (String.append "a" (Int.to_string i), vars (Int.add i 1) rest)

let show_args self acc args vars =
  match (args, vars) with
  | (Cons (ty, args), Cons (v, vars)) ->
    show_args self (append acc (append (Stag.Expr.string " ") (show_arg self ty (Stag.Expr.var v)))) args vars
  | _ -> acc

let pattern_var v = Stag.Pattern.Var v

let arm self constructor =
  match constructor with
  | (r, args) ->
    let names = vars 0 args in
    ( Stag.Pattern.Construct (Stag.Path.Ref r, Stag.map pattern_var names)
    , show_args self (Stag.Expr.string (Stag.Constructor_ref.name r)) args names )

let derive ty =
  let r = head ty in
  match decl ty with
  | Some d ->
    (match d.kind with
     | Variant constructors ->
       Cons
         ( Stag.Toplevel.let_ "to_string" (Cons ("x", Nil))
             (Stag.Expr.Match (Stag.Expr.var "x", Stag.map (arm (Stag.Type_ref.name r)) constructors))
         , Nil ))
|}
;;

let%expect_test "deriving a type in the same module" =
  check
    ~run:true
    (derive
     ^ {|
type tree['a] = Leaf | Node of tree['a] * int * tree['a]
include .~(derive (type tree['a]))
let shown = print_endline (to_string (Node (Node (Leaf, 1, Leaf), 2, Leaf)))
|}
    );
  [%expect
    {|
    module A : sig
      val head : Stag.Type.t['a] -> Stag.Type_ref.t
      val decl : Stag.Type.t['a] -> option[Stag.Type.Decl.t]
      val append : Stag.Expr.t -> Stag.Expr.t -> Stag.Expr.t
      val show_arg : string -> Stag.Type_expr.t -> Stag.Expr.t -> Stag.Expr.t
      val vars : int -> list['a] -> list[string]
      val show_args : string -> Stag.Expr.t -> list[Stag.Type_expr.t] -> list[string] -> Stag.Expr.t
      val pattern_var : string -> Stag.Pattern.t
      val arm : string -> Stag.Constructor_ref.t * list[Stag.Type_expr.t] -> Stag.Pattern.t * Stag.Expr.t
      val derive : Stag.Type.t['a] -> list[Stag.Toplevel.t]
      type tree['a] = Leaf | Node of tree['a] * int * tree['a]
      val to_string : tree['a] -> string
      val shown : unit
    end
    --- output ---
    Node Node Leaf 1 Leaf 2 Leaf
    |}]
;;

let%expect_test "reflected names refer to their declarations wherever spliced" =
  check
    ~run:true
    (derive
     ^ {|
type t = A | B of int * t
type r = {x : int}
let t_type = type t
module M = struct
  type t = A | B
  type int = string
  type s = {x : bool}
  let from_t (y : t) = y
  include .~(derive t_type)
  include .~(Cons (Stag.Toplevel.Let ("id", Cons (("y", Some (Stag.Type.expr t_type)), Nil), None, Stag.Expr.var "y"), Nil))
  include .~(
    match decl (type r) with
    | Some d ->
      (match d.kind with
       | Stag.Type.Kind.Record (Cons ((x, _), Nil)) ->
         Cons
           ( Stag.Toplevel.let_ "get_x" (Cons ("v", Nil)) (Stag.Expr.Field (Stag.Expr.var "v", Stag.Path.Ref x))
           , Cons
               ( Stag.Toplevel.let_ "make" Nil
                   (Stag.Expr.Record (Cons ((Stag.Path.Ref x, Stag.Expr.int 1), Nil)))
               , Cons
                   ( Stag.Toplevel.let_ "b" Nil
                       (match decl t_type with
                        | Some d ->
                          (match d.kind with
                           | Stag.Type.Kind.Variant (Cons ((a, _), _)) ->
                             Stag.Expr.Construct (Stag.Path.Ref a, Nil)))
                   , Cons
                       ( Stag.Toplevel.let_ "c" Nil (Stag.Expr.construct "A" Nil)
                       , Nil ) ) ) ) ))
end
let shown =
  print_endline (M.to_string (B (1, B (2, A))));
  print_endline (M.to_string (M.id M.b));
  print_endline (Int.to_string (M.get_x M.make))
|}
    );
  [%expect
    {|
    module A : sig
      val head : Stag.Type.t['a] -> Stag.Type_ref.t
      val decl : Stag.Type.t['a] -> option[Stag.Type.Decl.t]
      val append : Stag.Expr.t -> Stag.Expr.t -> Stag.Expr.t
      val show_arg : string -> Stag.Type_expr.t -> Stag.Expr.t -> Stag.Expr.t
      val vars : int -> list['a] -> list[string]
      val show_args : string -> Stag.Expr.t -> list[Stag.Type_expr.t] -> list[string] -> Stag.Expr.t
      val pattern_var : string -> Stag.Pattern.t
      val arm : string -> Stag.Constructor_ref.t * list[Stag.Type_expr.t] -> Stag.Pattern.t * Stag.Expr.t
      val derive : Stag.Type.t['a] -> list[Stag.Toplevel.t]
      type t = A | B of int * t
      type r = { x : int }
      val t_type : Stag.Type.t[t]
      module M : sig
        type t = A | B
        type int = string
        type s = { x : bool }
        val from_t : t -> t
        val to_string : A.t -> string
        val id : A.t -> A.t
        val get_x : r -> .int
        val make : r
        val b : A.t
        val c : t
      end
      val shown : unit
    end
    --- output ---
    B 1 B 2 A
    A
    1
    |}]
;;

let%expect_test "a declaration that depends on the splice reflecting it" =
  check
    (derive
     ^ {|
type t = A of M.u
module M = .~(derive (type t))
|}
    );
  check
    (derive
     ^ {|
include .~(derive (type t))
type t = A of u
|}
    );
  [%expect
    {|
    File "a.stag", line 51, characters 14-17:
    51 | type t = A of M.u
                       ^^^
    Error: A declaration splice depends on what it generates: M.u needs the declarations of the module A.M, which the splice at a.stag:52:0-30 is generating, as its expression uses this code
    File "a.stag", line 52, characters 0-30:
    52 | module M = .~(derive (type t))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
    File "a.stag", line 52, characters 14-15:
    52 | type t = A of u
                       ^
    Error: Unbound type u
    File "a.stag", line 51, characters 0-27:
    51 | include .~(derive (type t))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: This include, still being generated because its expression uses this code, may define it: move this code before the include or into another module
    |}]
;;

let%expect_test "refs are typed by what they name" =
  check
    (reflect
     ^ {|
type t = A
include .~(Cons (Stag.Toplevel.let_ "a" Nil (Stag.Expr.Construct (Stag.Path.Ref (head (type t)), Nil)), Nil))
|}
    );
  check
    (reflect
     ^ {|
type t = A
type r = {x : int}
let c =
  match decl (type t) with
  | Some d ->
    (match d.kind with
     | Stag.Type.Kind.Variant (Cons ((c, _), _)) -> Stag.Type.decl c)
|}
    );
  check
    (reflect
     ^ {|
type r = {x : int}
let f =
  match decl (type r) with
  | Some d ->
    (match d.kind with
     | Stag.Type.Kind.Record (Cons ((f, _), _)) -> Stag.Type.decl f)
|}
    );
  [%expect
    {|
    File "a.stag", line 8, characters 80-95:
    8 | include .~(Cons (Stag.Toplevel.let_ "a" Nil (Stag.Expr.Construct (Stag.Path.Ref (head (type t)), Nil)), Nil))
                                                                                        ^^^^^^^^^^^^^^^
    Error: This expression has type Stag.Type_ref.t but an expression was expected of type Stag.Constructor_ref.t
    File "a.stag", line 13, characters 67-68:
    13 |      | Stag.Type.Kind.Variant (Cons ((c, _), _)) -> Stag.Type.decl c)
                                                                            ^
    Error: This expression has type Stag.Constructor_ref.t but an expression was expected of type Stag.Type_ref.t
    File "a.stag", line 12, characters 66-67:
    12 |      | Stag.Type.Kind.Record (Cons ((f, _), _)) -> Stag.Type.decl f)
                                                                           ^
    Error: This expression has type Stag.Field_ref.t but an expression was expected of type Stag.Type_ref.t
    |}]
;;

let%expect_test "the type variables of a type" =
  check
    {|
let f (x : 'a) = type 'a
let one = f 1
let l : Stag.Type.t[list[int]] = type list['a]
let b (x : 'b) : Stag.Type.t[int] = type 'b
let pair = type ('a * 'a)
let same (x : Stag.Type.t['a * 'a]) = x
let p = same (type (int * 'a))
|};
  [%expect
    {|
    module A : sig
      val f : 'a -> Stag.Type.t['b]
      val one : Stag.Type.t['_a]
      val l : Stag.Type.t[list[int]]
      val b : 'a -> Stag.Type.t[int]
      val pair : Stag.Type.t['a * 'a]
      val same : Stag.Type.t['a * 'a] -> Stag.Type.t['a * 'a]
      val p : Stag.Type.t[int * int]
    end
    |}]
;;

let%expect_test "refs to the prelude's declarations where they are shadowed" =
  check
    ~run:true
    (reflect
     ^ {|
let some_ref =
  match decl (type option[int]) with
  | Some d ->
    (match d.kind with
     | Stag.Type.Kind.Variant (Cons (_, Cons ((some, _), Nil))) -> some)
let gen =
  Cons
    ( Stag.Toplevel.Let
        ( "x"
        , Nil
        , Some (Stag.Type.expr (type list[option[int]]))
        , Stag.Expr.construct "Cons" (Cons (Stag.Expr.Construct (Stag.Path.Ref some_ref, Cons (Stag.Expr.int 1, Nil)), Cons (Stag.Expr.construct "Nil" Nil, Nil))) )
    , Nil )
module M = struct
  type option = Some | None
  type list = int
  type int = bool
  include .~gen
end
let shown =
  match M.x with
  | Cons (Some n, Nil) -> print_endline (Int.to_string n)
|}
    );
  [%expect
    {|
    module A : sig
      val head : Stag.Type.t['a] -> Stag.Type_ref.t
      val decl : Stag.Type.t['a] -> option[Stag.Type.Decl.t]
      val some_ref : Stag.Constructor_ref.t
      val gen : list[Stag.Toplevel.t]
      module M : sig
        type option = Some | None
        type list = bool
        type int = bool
        val x : .list[.option[.int]]
      end
      val shown : unit
    end
    --- output ---
    1
    |}]
;;

let%expect_test "refs into spliced modules and includes" =
  check
    ~run:true
    (derive
     ^ {|
module M = .~(Cons (Stag.Toplevel.Type { `name = "t"; `params = Nil; `kind = Stag.Type_decl.Kind.Variant (Cons (("C", Cons (Stag.Type_expr.con "int" Nil, Nil)), Nil)) }, Nil))
include .~(Cons (Stag.Toplevel.Type { `name = "u"; `params = Nil; `kind = Stag.Type_decl.Kind.Variant (Cons (("D", Cons (Stag.Type_expr.con "M.t" Nil, Nil)), Nil)) }, Nil))
module N = struct
  include .~(derive (type M.t))
end
module O = struct
  type u = D
  include .~(derive (type A.u))
end
let shown =
  print_endline (N.to_string (M.C 1));
  print_endline (O.to_string (D (M.C 2)))
|}
    );
  [%expect
    {|
    module A : sig
      val head : Stag.Type.t['a] -> Stag.Type_ref.t
      val decl : Stag.Type.t['a] -> option[Stag.Type.Decl.t]
      val append : Stag.Expr.t -> Stag.Expr.t -> Stag.Expr.t
      val show_arg : string -> Stag.Type_expr.t -> Stag.Expr.t -> Stag.Expr.t
      val vars : int -> list['a] -> list[string]
      val show_args : string -> Stag.Expr.t -> list[Stag.Type_expr.t] -> list[string] -> Stag.Expr.t
      val pattern_var : string -> Stag.Pattern.t
      val arm : string -> Stag.Constructor_ref.t * list[Stag.Type_expr.t] -> Stag.Pattern.t * Stag.Expr.t
      val derive : Stag.Type.t['a] -> list[Stag.Toplevel.t]
      module M : sig
        type t = C of int
      end
      type u = D of M.t
      module N : sig
        val to_string : M.t -> string
      end
      module O : sig
        type u = D
        val to_string : A.u -> string
      end
      val shown : unit
    end
    --- output ---
    C 1
    D ?
    |}]
;;

let%expect_test "refs across files" =
  File_check.check_files
    ~run:true
    [ "show.stag", derive
    ; ( "types.stag"
      , {|type t = A | B of int * t
let ty = type t
|}
      )
    ; ( "a.stag"
      , {|type t = string
type int = bool
include .~(Show.derive Types.ty)
let shown = print_endline (to_string (Types.B (1, Types.A)))
|}
      )
    ];
  [%expect
    {|
    module Show : sig
      val head : Stag.Type.t['a] -> Stag.Type_ref.t
      val decl : Stag.Type.t['a] -> option[Stag.Type.Decl.t]
      val append : Stag.Expr.t -> Stag.Expr.t -> Stag.Expr.t
      val show_arg : string -> Stag.Type_expr.t -> Stag.Expr.t -> Stag.Expr.t
      val vars : int -> list['a] -> list[string]
      val show_args : string -> Stag.Expr.t -> list[Stag.Type_expr.t] -> list[string] -> Stag.Expr.t
      val pattern_var : string -> Stag.Pattern.t
      val arm : string -> Stag.Constructor_ref.t * list[Stag.Type_expr.t] -> Stag.Pattern.t * Stag.Expr.t
      val derive : Stag.Type.t['a] -> list[Stag.Toplevel.t]
    end
    module Types : sig
      type t = A | B of int * t
      val ty : Stag.Type.t[t]
    end
    module A : sig
      type t = string
      type int = bool
      val to_string : Types.t -> string
      val shown : unit
    end
    --- output ---
    B 1 A
    |}]
;;

let%expect_test "types in quotes and generated syntax" =
  check
    ~run:true
    (reflect
     ^ {|
type t = A
let code = .<type t>.
let spliced = .~(.<Stag.Type.expr (type list[t])>.)
let gen name = Cons (Stag.Toplevel.let_ name Nil (Stag.Expr.Type (Stag.Type.expr (type t))), Nil)
include .~(gen "generated")
module M = struct
  type t = B
  include .~(gen "same")
  let check_same : Stag.Type.t[A.t] = same
end
let shown = print_endline (Stag.Type_ref.name (head M.same))
|}
    );
  [%expect
    {|
    module A : sig
      val head : Stag.Type.t['a] -> Stag.Type_ref.t
      val decl : Stag.Type.t['a] -> option[Stag.Type.Decl.t]
      type t = A
      val code : <Stag.Type.t[t]>{..'a}
      val spliced : Stag.Type_expr.t
      val gen : string -> list[Stag.Toplevel.t]
      val generated : Stag.Type.t[t]
      module M : sig
        type t = B
        val same : Stag.Type.t[A.t]
        val check_same : Stag.Type.t[A.t]
      end
      val shown : unit
    end
    --- output ---
    t
    |}]
;;

let%expect_test "errors through refs name their declarations" =
  check
    (reflect
     ^ {|
module M = struct
  type t['a] = A
end
type t = int
include .~(Cons (Stag.Toplevel.Let ("x", Nil, Some (Stag.Type_expr.Con (Stag.Path.Ref (head (type list[int])), Nil)), Stag.Expr.construct "Nil" Nil), Nil))
|}
    );
  check
    (reflect
     ^ {|
module M = struct
  type t['a] = A
end
include .~(Cons (Stag.Toplevel.Let ("x", Nil, Some (Stag.Type_expr.Con (Stag.Path.Ref (head (type M.t[int])), Nil)), Stag.Expr.Construct (Stag.Path.Name "M.A", Nil)), Nil))
|}
    );
  [%expect
    {|
    File "a.stag", line 11, characters 0-155:
    11 | include .~(Cons (Stag.Toplevel.Let ("x", Nil, Some (Stag.Type_expr.Con (Stag.Path.Ref (head (type list[int])), Nil)), Stag.Expr.construct "Nil" Nil), Nil))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the declaration `let x` generated by this splice: The type list expects 1 argument(s), but is given 0
    Note: The generated declaration is:
      let x : .list = Nil
    File "a.stag", line 10, characters 0-172:
    10 | include .~(Cons (Stag.Toplevel.Let ("x", Nil, Some (Stag.Type_expr.Con (Stag.Path.Ref (head (type M.t[int])), Nil)), Stag.Expr.Construct (Stag.Path.Name "M.A", Nil)), Nil))
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Error: In the declaration `let x` generated by this splice: The type A.M.t expects 1 argument(s), but is given 0
    Note: The generated declaration is:
      let x : .A.M.t = M.A
    |}]
;;

let%expect_test "decl of a polymorphic variant alias" =
  run
    (reflect
     ^ {|
type pv['a] = [`A of 'a | `B]
let d = decl (type pv[int])
include .~(let _ = d in Nil)
|}
    );
  [%expect
    {|
    val head : Stag.Type.t['a] -> Stag.Type_ref.t
    val decl : Stag.Type.t['a] -> option[Stag.Type.Decl.t]
    type pv['a] = [`A of 'a | `B]
    val d : option[Stag.Type.Decl.t]
    -- evaluated at compile time
    (head <fun>)
    (decl <fun>)
    (d
     (Some
      (Stag.Type.Decl.t
       (params (Cons (Stag.Type_decl.Param.Ty (String a)) (Nil)))
       (kind
        (Stag.Type.Kind.Alias
         (Stag.Type_expr.Variant
          (Stag.Type_expr.Row.Extend (String A) (Stag.Type_expr.Var (String a))
           (Stag.Type_expr.Row.Extend (String B)
            (Stag.Type_expr.Con (Stag.Path.Ref (Ref (type unit))) (Nil))
            (Stag.Type_expr.Row.Empty)))))))))
    |}]
;;

let%expect_test "checking that re-enters through reflection" =
  check
    (reflect
     ^ {|
type t = A of N.u
module M = .~(let _ = decl (type t) in Nil)
type s = B of M.v
module N = .~(let _ = decl (type s) in Nil)
|}
    );
  [%expect
    {|
    File "a.stag", line 7, characters 14-17:
    7 | type t = A of N.u
                      ^^^
    Error: A declaration splice depends on what it generates: N.u needs the declarations of the module A.N, which the splice at a.stag:10:0-43 is generating, as its expression uses this code
    File "a.stag", line 10, characters 0-43:
    10 | module N = .~(let _ = decl (type s) in Nil)
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Note: The declaration splice is here
    |}]
;;

let%expect_test "type refs' root paths" =
  check
    ~run:true
    (reflect
     ^ {|
module M = struct
  type t = A
end
let show ty =
  let names path = Core.String.concat "." path in
  match Stag.Type_ref.path (head ty) with
  | Stag.Type_ref.Path.Program path -> print_endline (Core.String.append "program " (names path))
  | Stag.Type_ref.Path.Prelude path -> print_endline (Core.String.append "prelude " (names path))
let shown =
  show (type M.t);
  show (type int);
  show (type list[int]);
  show (type Core.Sexp.t);
  show (type Stag.Type_expr.t)
|}
    );
  [%expect
    {|
    module A : sig
      val head : Stag.Type.t['a] -> Stag.Type_ref.t
      val decl : Stag.Type.t['a] -> option[Stag.Type.Decl.t]
      module M : sig
        type t = A
      end
      val show : Stag.Type.t['a] -> unit
      val shown : unit
    end
    --- output ---
    program A.M.t
    prelude int
    prelude list
    prelude Core.Sexp.t
    prelude Stag.Type_expr.t
    |}]
;;
