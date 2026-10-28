open! Import
open Source_helpers

(* Each error is at the node it is about: the span printed is that of the node
   named in the comment. *)

let%expect_test "values" =
  (* the variable *)
  check {|let x = unbound|};
  (* the variable *)
  check {|let x = id M.y|};
  (* Unification errors are at the expression checked against a type: [true]. *)
  check {|let x = add true 1|};
  (* the applied [1] *)
  check {|let x = 1 2|};
  (* the body *)
  check {|let x : int = true|};
  (* the annotated variable *)
  check {|let x = (id : int -> bool)|};
  (* the annotation *)
  check {|let x = (id : unbound)|};
  (* the second [x] *)
  check
    {|
let x = 1
let y = 2
let x = 3
|};
  (* the argument's annotation *)
  check {|let x = let f (a : nope) = 1 in 2|};
  (* the body of [f] *)
  check {|let x = let f : bool = 1 in 2|};
  [%expect
    {|
    ((Not_in_scope unbound) @ t.stag:1:8-15)
    ((Unbound_module M) @ t.stag:1:11-12)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:1:12-16)
    ((Unification_error
      (Type_mismatch (TArrow (TVar (Unbound 18 1)) (TVar (Unbound 19 1)))
       (TCon int ()) Structural_mismatch))
     @ t.stag:1:8-9)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:1:14-18)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:1:8-26)
    ((Unbound_type unbound) @ t.stag:1:14-21)
    ((Duplicate_toplevel x) @ t.stag:4:4-5)
    ((Unbound_type nope) @ t.stag:1:19-23)
    ((Unification_error
      (Type_mismatch (TCon bool ()) (TCon int ())
       (Base_types_not_equal bool int)))
     @ t.stag:1:23-24)
    |}]
;;

let nominal_decls =
  {|
type t = A | B of int
type r = {x : int; y : int}
type s = {z : int}
|}
;;

let%expect_test "nominal types" =
  let check_v expr = check (nominal_decls ^ "let v = " ^ expr) in
  (* the constructor *)
  check_v "C";
  (* the constructor's path *)
  check_v "M.C";
  (* the construction *)
  check_v "B";
  (* the argument *)
  check_v "B true";
  (* the field [w] *)
  check_v "{x = 1; w = 2}";
  (* the field [z] *)
  check_v "{x = 1; z = 2}";
  (* the second [x] *)
  check_v "{x = 1; y = 2; x = 3}";
  (* the record *)
  check_v "{x = 1}";
  (* the [true] *)
  check_v "{x = 1; y = true}";
  (* Source can't write a nominal record without fields ([{}] is the empty
     polymorphic record), so its fields are removed after parsing. The error
     is at the record. *)
  check_program
    (List.map
       (parse_exn (nominal_decls ^ "let v = {x = 1}"))
       ~f:(fun (toplevel : Syntax.Toplevel.t) ->
         match toplevel.txt with
         | Let (name, args, annot, ({ txt = Record _; _ } as record)) ->
           { toplevel with
             txt =
               Syntax.Toplevel.Let
                 (name, args, annot, { record with txt = Syntax.Expr.Record [] })
           }
         | _ -> toplevel));
  (* the record [1] *)
  check_v "(1).x";
  (* the field [w] *)
  check_v "(1).w";
  [%expect
    {|
    ((Unbound_constructor C) @ t.stag:5:8-9)
    ((Unbound_module M) @ t.stag:5:8-9)
    ((Constructor_arity_mismatch (constructor B) (expected 1) (got 0)) @
     t.stag:5:8-9)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:5:10-14)
    ((No_such_field (type_name r) (field w)) @ t.stag:5:16-17)
    ((No_such_field (type_name r) (field z)) @ t.stag:5:16-17)
    ((Duplicate_field x) @ t.stag:5:23-24)
    ((Missing_field y) @ t.stag:5:8-15)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:5:20-24)
    (Empty_record @ t.stag:5:8-15)
    ((Unification_error
      (Type_mismatch (TCon r ()) (TCon int ()) (Base_types_not_equal r int)))
     @ t.stag:5:8-11)
    ((Unbound_field w) @ t.stag:5:12-13)
    |}]
;;

let%expect_test "patterns" =
  let check_case cases =
    check ("type t = A | B of int\nlet v = match 1 with " ^ cases)
  in
  (* the pattern *)
  check_case "true -> 1";
  (* the constructor *)
  check_case "C -> 1";
  (* the pattern *)
  check_case "B -> 1";
  (* the field *)
  check_case "{x = _} -> 1";
  (* the second [a] *)
  check_case "(a, a) -> 1";
  (* the second [`x] *)
  check_case "{`x = _; `x = _} -> 1";
  (* the second case's body *)
  check_case "_ -> 1 | _ -> true";
  (* Closing the scrutinee to the tags of the cases: the pattern. *)
  check_case "`A -> 1";
  [%expect
    {|
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:21-25)
    ((Unbound_constructor C) @ t.stag:2:21-22)
    ((Constructor_arity_mismatch (constructor B) (expected 1) (got 0)) @
     t.stag:2:21-22)
    ((Unbound_field x) @ t.stag:2:22-23)
    ((Duplicate_pattern_variable a) @ t.stag:2:25-26)
    ((Duplicate_row_entry x) @ t.stag:2:30-32)
    ((Unification_error
      (Type_mismatch (TCon int ()) (TCon bool ())
       (Base_types_not_equal int bool)))
     @ t.stag:2:35-39)
    ((Unification_error
      (Type_mismatch (TCon int ())
       (TVariant (RExtend A Present (TCon unit ()) (RVar (Unbound 19 1))))
       Structural_mismatch))
     @ t.stag:2:21-23)
    |}]
;;

let%expect_test "polymorphic records and variants" =
  let check_v expr = check ("let v = " ^ expr) in
  (* the second [`x] *)
  check_v "{`x = 1; `y = 1; `x = 1}";
  (* the [1] *)
  check_v "(1).`x";
  (* the record *)
  check_v "{`y = 1}.`x";
  (* the [1] *)
  check_v "(1) \\ `x";
  (* the second [`x] *)
  check_v "{`x = 1; `x = 1 | 1}";
  (* the extended [1] *)
  check_v "{`x = 1 | 1}";
  (* the updated [1] *)
  check_v "{(1) with `x = 1}";
  (* the second [`x] *)
  check_v "{(1) with `x = 1; `x = 1}";
  (* the applied [1] *)
  check_v "1 (`A 1)";
  [%expect
    {|
    ((Duplicate_row_entry x) @ t.stag:1:25-27)
    ((Unification_error
      (Type_mismatch
       (TRecord (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))))
       (TCon int ()) Structural_mismatch))
     @ t.stag:1:8-11)
    ((Unification_error
      (Row_mismatch
       (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1)))
       (RExtend y Present (TCon int ()) REmpty) (Missing_label x)))
     @ t.stag:1:8-16)
    ((Unification_error
      (Type_mismatch
       (TRecord (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))))
       (TCon int ()) Structural_mismatch))
     @ t.stag:1:8-11)
    ((Duplicate_row_entry x) @ t.stag:1:17-19)
    ((Unification_error
      (Type_mismatch (TRecord (RVar (Unbound 18 1))) (TCon int ())
       Structural_mismatch))
     @ t.stag:1:18-19)
    ((Unification_error
      (Type_mismatch
       (TRecord (RExtend x Present (TVar (Unbound 18 1)) (RVar (Unbound 19 1))))
       (TCon int ()) Structural_mismatch))
     @ t.stag:1:9-12)
    ((Duplicate_row_entry x) @ t.stag:1:26-28)
    ((Unification_error
      (Type_mismatch (TArrow (TVar (Unbound 18 1)) (TVar (Unbound 19 1)))
       (TCon int ()) Structural_mismatch))
     @ t.stag:1:8-9)
    |}]
;;

let%expect_test "type expressions" =
  let annotated ?(decls = "") annot =
    check (decls ^ "let x : " ^ annot ^ " = x")
  in
  let list = "type list['a]\n" in
  let rows = "type rows[{..'r}]\n" in
  (* the type *)
  annotated "unbound";
  (* the path *)
  annotated "M.t";
  (* the type *)
  annotated ~decls:list "list";
  annotated ~decls:list "list[{}]";
  (* the argument *)
  annotated ~decls:rows "rows[int]";
  (* the second [`x] *)
  annotated "{`x : int; `x : int}";
  [%expect
    {|
    ((Unbound_type unbound) @ t.stag:1:8-15)
    ((Unbound_module M) @ t.stag:1:8-9)
    ((Type_arity_mismatch (type_name list) (expected 1) (got 0)) @ t.stag:2:8-12)
    type list['a]
    val x : list[{}]
    ((Type_arg_kind_mismatch (type_name rows) (param (Row r))) @ t.stag:2:13-16)
    ((Duplicate_row_entry x) @ t.stag:1:19-21)
    |}]
;;

let%expect_test "declarations" =
  (* the variable *)
  check {|type t = 'a|};
  (* the row variable *)
  check {|type t = {..'r}|};
  (* the row variable *)
  check {|type t['a] = {..'a}|};
  (* the variable *)
  check {|type t[{..'r}] = 'r|};
  (* the second [t] *)
  check
    {|
type t
type u
type t
|};
  (* the second [A] *)
  check
    {|
type t = A
type u = B | A
|};
  (* the second [x] *)
  check {|type t = {x : int; x : int}|};
  (* the second [a] *)
  check {|type t['a, 'b, {..'a}]|};
  (* the argument's type *)
  check {|type t = A of unbound|};
  (* the declaration of [t] *)
  check {|type t = t * int|};
  (* the declaration of [u] *)
  check
    {|
type u = t
type t = u * int
|};
  (* the recursive use *)
  check {|type t['a] = 'a -> t['a * 'a]|};
  (* the second [M] *)
  check
    {|
module M = struct end
module N = struct end
module M = struct end
|};
  (* the second [x] *)
  check
    {|
module M = struct
  let x = 1
  let x = 1
end
|};
  [%expect
    {|
    ((Unbound_type_variable a) @ t.stag:1:9-11)
    ((Unbound_type_variable r) @ t.stag:1:12-14)
    ((Type_variable_kind_mismatch a) @ t.stag:1:16-18)
    ((Type_variable_kind_mismatch r) @ t.stag:1:17-19)
    ((Duplicate_type t) @ t.stag:4:5-6)
    ((Duplicate_constructor A) @ t.stag:3:13-14)
    ((Duplicate_field x) @ t.stag:1:19-20)
    ((Duplicate_type_parameter (type_name t) (param a)) @ t.stag:1:15-21)
    ((Unbound_type unbound) @ t.stag:1:14-21)
    ((Cyclic_type_alias t) @ t.stag:1:5-6)
    ((Cyclic_type_alias u) @ t.stag:2:5-6)
    ((Non_regular_recursive_alias t) @ t.stag:1:19-29)
    ((Duplicate_module M) @ t.stag:4:7-8)
    ((Duplicate_toplevel M.x) @ t.stag:4:6-7)
    |}]
;;

let%expect_test "staging" =
  (* the spliced [y] *)
  check {|let x = .<let y = 1 in .~y>.|};
  (* the [x] *)
  check {|let x = .~x|};
  (* the [f] *)
  check {|let f = .~(id f)|};
  (* the spliced [1] *)
  check {|let x = .~1|};
  (* the splice *)
  check {|let x = .~(.<y>.)|};
  (* the second [a] *)
  check {|let x = .<.~(.<1>.).{a = 1; b; a}>.|};
  (* the [a] *)
  check {|let x = .~(.<1>.).{a}|};
  (* the splice, failing at the match *)
  check {|let x = .~(match 1 with 2 -> .<1>.)|};
  [%expect
    {|
    ((Variable_used_at_wrong_level (name y) (var_level 1) (used_at 0)) @
     t.stag:1:25-26)
    ((Compile_time_use_of_toplevel_being_checked x) @ t.stag:1:10-11)
    ((Compile_time_use_of_toplevel_being_checked f) @ t.stag:1:14-15)
    ((Unification_error
      (Type_mismatch
       (TCode (inner (TVar (Unbound 18 1))) (context (RVar (Unbound 19 1))))
       (TCon int ()) Structural_mismatch))
     @ t.stag:1:10-11)
    ((Not_in_scope y) @ t.stag:1:8-17)
    ((Duplicate_substitution a) @ t.stag:1:31-32)
    ((Not_in_scope a) @ t.stag:1:19-20)
    ((Splice_evaluation_failed
      (code
       (Match (Lit (Int 1) : int)
        (((Lit (Int 2) : int) (Quote (Lit (Int 1) : int) : <int>{}))) : <int>{}))
      (error ("Match failure: no case matches the value 1" @ t.stag:1:10-35)))
     @ t.stag:1:8-35)
    |}]
;;

(* The location of each node of the body of the last toplevel, and of its
   children. *)
let rec loc_tree (expr : Typed.Expr.t) =
  let head =
    match [%sexp (expr.desc : Typed.Expr.t Typed.Expr.Desc.t)] with
    | List (Atom head :: _) | Atom head -> head
    | List _ -> "?"
  in
  Sexp.List
    (Atom head
     :: Atom (Loc.to_string expr.loc)
     :: List.map (Typed.Expr.sub_exprs expr) ~f:loc_tree)
;;

let print_loc_trees source =
  Option.iter (typecheck source) ~f:(fun t ->
    let last = List.last_exn (checked t).program.sccs in
    List.iter last.members ~f:(fun (toplevel : Typed.Toplevel.t) ->
      print_s (loc_tree toplevel.body)))
;;

let%expect_test "typed nodes keep their syntax's locations" =
  print_loc_trees {|let x = let y = 1 in match y with _ -> (y, true)|};
  (* Generated code keeps the locations of the quote it came from; lets binding
     explicit substitutions are at the splice. *)
  print_loc_trees {|let x = .~(.<a 1>.).{a = id}|};
  [%expect
    {|
    (Let t.stag:1:8-48 (Lit t.stag:1:16-17)
     (Match t.stag:1:21-48 (Var t.stag:1:27-28)
      (Tuple t.stag:1:39-48 (Var t.stag:1:40-41) (Lit t.stag:1:43-47))))
    (Toplevel_splice t.stag:1:8-28
     (Quote t.stag:1:10-19
      (App t.stag:1:13-16 (Var t.stag:1:13-14) (Lit t.stag:1:15-16)))
     (Let t.stag:1:8-28 (Var t.stag:1:25-27)
      (App t.stag:1:13-16 (Var t.stag:1:13-14) (Lit t.stag:1:15-16))))
    |}]
;;
