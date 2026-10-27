open! Import

let parse source =
  match Parser.program ~file:"t.stag" source with
  | Ok program -> program
  | Error error -> raise_s [%sexp (error : Parser.Error.t)]
;;

let print_error error = print_s [%sexp (error : Error.t)]

let%expect_test "the prelude's signature" =
  (match Driver.check_files [ Prelude.file ] with
   | Error error -> print_s [%sexp (error : Driver.Error.t)]
   | Ok driver ->
     print_string (Signature.to_string_hum (Driver.checked driver).signature));
  [%expect
    {|
    module Stag : sig
      type list['a] = Nil | Cons of 'a * list['a]
      type option['a] = None | Some of 'a
      module Lit : sig
        type t = Int of int | Bool of bool | String of string | Char of char | Unit
      end
      module Type_expr : sig
        type t = Var of string | Con of string * list[Arg.t] | Arrow of t * t | Tuple of list[t] | Code of {`context : Row.t; `inner : t} | Record of Row.t | Variant of Row.t
        module Row : sig
          type t = Empty | Extend of string * Type_expr.t * t | Var of string
        end
        module Arg : sig
          type t = Ty of Type_expr.t | Row of Row.t
        end
      end
      module Type_decl : sig
        module Param : sig
          type t = Ty of string | Row of string
        end
        module Kind : sig
          type t = Variant of list[string * list[Type_expr.t]] | Record of list[string * Type_expr.t] | Alias of Type_expr.t | Abstract
        end
        type t = {`kind : Kind.t; `name : string; `params : list[Param.t]}
      end
      module Binding : sig
        type t = string * option[Type_expr.t]
      end
      module Pattern : sig
        module Rest : sig
          type t = Closed | Open | Bind of string
        end
        type t = Any | Var of string | Lit of Lit.t | Tuple of list[t] | Construct of string * list[t] | Record of list[string * t] | Poly_variant of string * option[t] | Poly_record of {`fields : list[string * t]; `rest : Rest.t}
      end
      module Expr : sig
        type t = Lit of Lit.t | Var of string * option[Type_expr.t] | App of t * t | Let of string * list[string * option[Type_expr.t]] * option[Type_expr.t] * t * t | Splice of {`expr : t; `subs : option[list[string * option[t]]]} | Quote of t | Tuple of list[t] | Construct of string * list[t] | Record of list[string * t] | Field of t * string | Match of t * list[Pattern.t * t] | Poly_record of list[string * t] | Poly_field of t * string | Poly_extend of list[string * t] * t | Poly_restrict of t * string | Poly_update of t * list[string * t] | Poly_variant of string * option[t] | Expect of string
      end
      module Toplevel : sig
        type t = Let of string * list[string * option[Type_expr.t]] * option[Type_expr.t] * Expr.t | Type of {`kind : Type_decl.Kind.t; `name : string; `params : list[Type_decl.Param.t]} | Module of string * list[t] | Expect_test of {`body : Expr.t; `name : string}
      end
      module Program : sig
        type t = list[Toplevel.t]
      end
    end
    |}]
;;

(* Every constructor of the prelude's variants, by canonical name. *)
let prelude_constructors =
  match Source_file.parse Prelude.file with
  | Error error -> raise_s [%sexp (error : Source_file.Error.t)]
  | Ok toplevel ->
    let rec go path (toplevel : Syntax.Toplevel.t) =
      match toplevel.txt with
      | Module (name, items) ->
        List.concat_map items ~f:(go (path @ [ name.txt ]))
      | Type { name = _; kind = Variant constructors; params = _ } ->
        List.map constructors ~f:(fun (name, _) ->
          String.concat (path @ [ name.txt ]) ~sep:".")
      | Type _ | Let _ | Expect_test _ -> []
    in
    String.Set.of_list (go [] toplevel)
;;

let rec constructors_of (value : Treewalk.Value.t) =
  match value with
  | Construct ({ name; _ }, args) ->
    Set.add (String.Set.union_list (List.map args ~f:constructors_of)) name
  | Tuple values -> String.Set.union_list (List.map values ~f:constructors_of)
  | Poly_record fields ->
    String.Set.union_list (List.map (Map.data fields) ~f:constructors_of)
  | _ -> String.Set.empty
;;

let round_trip_sources =
  [ {|
type nat = Z | S of nat
type list['a] = Nil | Cons of 'a * list['a]
type with_x[{..'r}] = {`x : int; ..'r}
type point = {x : int; y : M.t}
type void = |
type t
type f = int -> bool * string
type tags[{..'r}] = [`A of int | `B; ..'r]
type closed = [`A | `B of {}]
type code['a] = <'a>{x : int; y : bool; ..'r} -> <int>{}
type args = with_x[{`y : int}] * list[{}] * list['a] * with_x[{..'r}]
|}
  ; {|
let f x (y : int) (_ : bool) : t = x
let lits = (1, true, false, 'c', "s\n", ())
let g = M.N.x
let h = (x : list['a])
let app = f x y
let l = let z = 1 in let w (a : int) : int = a in z; w
let c = (C, C 1, C (1, 2), M.C x)
let r = ({x = 1; M.y = 2}, r.x, r.M.x)
let m = match e with | _ -> 1 | x -> 2 | 1 -> 3 | 'c' -> 4 | "s" -> 5 | true -> 6 | () -> 7
let p = match e with (a, b) -> a | C -> 1 | C p -> 2 | M.C (p, q) -> 3 | {x = p; M.y = q} -> 4
let q = match e with `A -> 1 | `A p -> 2 | {`x = p} -> 3 | {`x = p; ..} -> 4 | {`x = p; ..rest} -> 5
let poly = ({`x = 1; `y = true}, r.`x, {`x = v; `y = w | r}, r \ `x, {r with `x = v}, `A, `A e)
let staged = .< .~e >.
let splices = (.~(f x), .~e.{}, .~e.{x = e1; y})
|}
  ; {stag|
module M = struct
  let x = 1
  module N = struct
    type t = A
  end
end
let%expect_test "power" =
  print_int (Int.add 3 2);
  [%expect {| 5 |}];
  [%expect]
|stag}
  ]
;;

let%expect_test "round trips, modulo locations" =
  let constructors =
    List.fold
      round_trip_sources
      ~init:String.Set.empty
      ~f:(fun constructors source ->
        let program = parse source in
        let value = Syntax_value.Program.to_value program in
        (match Syntax_value.Program.of_value value ~loc:Loc.none with
         | Error error -> print_error error
         | Ok program' ->
           print_s
             [%message "" ~equal:(Syntax.Program.equal program program' : bool)]);
        Set.union constructors (constructors_of value))
  in
  [%expect
    {|
    (equal true)
    (equal true)
    (equal true)
    |}];
  print_s
    [%message
      ""
        ~not_covered:(Set.diff prelude_constructors constructors : String.Set.t)];
  [%expect {| (not_covered ()) |}]
;;

let%expect_test "values" =
  let show source =
    print_endline
      (Treewalk.Value.to_string_hum
         (Syntax_value.Program.to_value (parse source)))
  in
  show {|let f (x : int) = `A x|};
  show {|type t['a] = A | B of 'a * t['a]|};
  [%expect
    {|
    Stag.Cons (Stag.Toplevel.Let ("f", Stag.Cons (("x", Stag.Some (Stag.Type_expr.Con ("int", Stag.Nil))), Stag.Nil), Stag.None, Stag.Expr.Poly_variant ("A", Stag.Some (Stag.Expr.Var ("x", Stag.None)))), Stag.Nil)
    Stag.Cons (Stag.Toplevel.Type {`kind = Stag.Type_decl.Kind.Variant (Stag.Cons (("A", Stag.Nil), Stag.Cons (("B", Stag.Cons (Stag.Type_expr.Var "a", Stag.Cons (Stag.Type_expr.Con ("t", Stag.Cons (Stag.Type_expr.Arg.Ty (Stag.Type_expr.Var "a"), Stag.Nil)), Stag.Nil))), Stag.Nil))); `name = "t"; `params = Stag.Cons (Stag.Type_decl.Param.Ty "a", Stag.Nil)}, Stag.Nil)
    |}]
;;

let%expect_test "nodes are located where given" =
  let loc = Loc.start_of_file "splice.stag" in
  (match
     Syntax_value.Expr.of_value
       (Syntax_value.Expr.to_value
          (match Parser.expr ~file:"t.stag" "f (x : int)" with
           | Ok expr -> expr
           | Error error -> raise_s [%sexp (error : Parser.Error.t)]))
       ~loc
   with
   | Ok expr -> print_s [%sexp (expr : Syntax.Expr.t)]
   | Error error -> print_error error);
  [%expect
    {|
    ((App ((Var f ()) @ splice.stag:1:0-0)
      ((Var x (((Con (int @ splice.stag:1:0-0) ()) @ splice.stag:1:0-0))) @
       splice.stag:1:0-0))
     @ splice.stag:1:0-0)
    |}]
;;

(* The prelude and [file], checked and evaluated, and the value of each toplevel
   by canonical name. *)
let eval_values ?(file = "gen.stag") source =
  match
    Driver.check_files
      ~limits:Source_helpers.limits
      [ Prelude.file; { path = file; contents = source } ]
  with
  | Error error -> raise_s [%sexp (error : Driver.Error.t)]
  | Ok driver ->
    Driver.eval ~limits:Source_helpers.limits driver |> ok_exn;
    fun name ->
      let ident =
        List.find_map_exn (Driver.checked driver).program.sccs ~f:(fun scc ->
          List.find_map scc.members ~f:(fun (member : Typed.Toplevel.t) ->
            Option.some_if (String.equal member.ident.name name) member.ident))
      in
      Treewalk.find (Driver.interpreter driver) ~ident |> ok_exn
;;

let generator =
  {|
let map f l =
  match l with
  | Stag.Nil -> Stag.Nil
  | Stag.Cons (x, rest) -> Stag.Cons (f x, map f rest)

let constructor name = (name, Stag.Nil)
let var name = Stag.Expr.Var (name, Stag.None)
let string s = Stag.Expr.Lit (Stag.Lit.String s)

let arm name = (Stag.Pattern.Construct (name, Stag.Nil), string name)

(* type t = <names>  let to_string x = match x with <name> -> "<name>" ... *)
let enum names =
  Stag.Cons
    ( Stag.Toplevel.Type
        {`name = "t"; `params = Stag.Nil; `kind = Stag.Type_decl.Kind.Variant (map constructor names)}
    , Stag.Cons
        ( Stag.Toplevel.Let
            ( "to_string"
            , Stag.Cons (("x", Stag.Some (Stag.Type_expr.Con ("t", Stag.Nil))), Stag.Nil)
            , Stag.None
            , Stag.Expr.Match (var "x", map arm names) )
        , Stag.Nil ) )

let names = Stag.Cons ("Red", Stag.Cons ("Green", Stag.Cons ("Blue", Stag.Nil)))
let decls = enum names
let bad = enum (Stag.Cons ("Red", Stag.Cons ("light blue", Stag.Nil)))
let keyword = Stag.Cons (Stag.Toplevel.Let ("match", Stag.Nil, Stag.None, var "x"), Stag.Nil)
let bad_path = Stag.Cons (Stag.Toplevel.Let ("x", Stag.Nil, Stag.None, var "M..y"), Stag.Nil)
|}
;;

let%expect_test "declarations computed by a stag program" =
  let value = eval_values generator "Gen.decls" in
  let program = Syntax_value.Program.of_value value ~loc:Loc.none |> ok_exn in
  print_s [%sexp (program : Syntax.Program.t)];
  [%expect
    {|
    ((Type
      ((name t) (params ()) (kind (Variant ((Red ()) (Green ()) (Blue ()))))))
     (Let to_string ((x ((Con t ())))) ()
      (Match (Var x ())
       (((Construct Red ()) (Lit (String Red)))
        ((Construct Green ()) (Lit (String Green)))
        ((Construct Blue ()) (Lit (String Blue)))))))
    |}];
  (* The refs the typechecker made agree with those of [to_value]. *)
  print_s
    [%message
      ""
        ~same_value:
          (Sexp.equal
             [%sexp (value : Treewalk.Value.t)]
             [%sexp (Syntax_value.Program.to_value program : Treewalk.Value.t)]
           : bool)];
  [%expect {| (same_value true) |}];
  (* The generated declarations check, and work, as written. *)
  Source_helpers.run_program
    ~prelude:false
    (program
     @ parse
         {|
let print_all = print_endline (to_string Red); print_endline (to_string Blue)
|}
    );
  [%expect
    {|
    type t = Red | Green | Blue
    val to_string : t -> string
    val print_all : unit
    -- evaluated at compile time
    -- evaluated
    Red
    Blue
    (to_string <fun>)
    (print_all "()")
    |}]
;;

let print_program value =
  match Syntax_value.Program.of_value value ~loc:Loc.none with
  | Ok program -> print_s [%sexp (program : Syntax.Program.t)]
  | Error error -> print_error error
;;

let%expect_test "malformed values" =
  let values = eval_values generator in
  List.iter [ "Gen.bad"; "Gen.keyword"; "Gen.bad_path" ] ~f:(fun name ->
    print_program (values name));
  [%expect
    {|
    ("Malformed syntax"
     ((expected "a capitalized identifier") (got "\"light blue\"")
      (within (Stag.Toplevel.Type Stag.Type_decl.Kind.Variant))))
    ("Malformed syntax"
     ((expected "a lowercase identifier") (got "\"match\"")
      (within (Stag.Toplevel.Let))))
    ("Malformed syntax"
     ((expected
       "a path of capitalized identifiers ending in a lowercase identifier")
      (got "\"M..y\"") (within (Stag.Toplevel.Let Stag.Expr.Var))))
    |}];
  (* A value of another type, eg. one with constructors of the same names. *)
  let values =
    eval_values
      ~file:"other.stag"
      {|
type option['a] = None | Some of 'a
let decls = Stag.Cons (Stag.Toplevel.Let ("x", Stag.Nil, Stag.None, Stag.Expr.Lit (Stag.Lit.Int 1)), Stag.Nil)
let other = Some 1
|}
  in
  let other = values "Other.other" in
  let rec replace_none (value : Treewalk.Value.t) : Treewalk.Value.t =
    match value with
    | Construct ({ name = "Stag.None"; _ }, []) -> other
    | Construct (ref, args) -> Construct (ref, List.map args ~f:replace_none)
    | value -> value
  in
  print_program (replace_none (values "Other.decls"));
  [%expect
    {|
    ("Malformed syntax"
     ((expected Stag.option) (got "Other.Some 1") (within (Stag.Toplevel.Let))))
    |}];
  print_program (Int 1);
  [%expect
    {| ("Malformed syntax" ((expected Stag.list) (got 1) (within ()))) |}]
;;
