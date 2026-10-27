open! Import

let parse source =
  match Parser.program ~file:"t.stag" source with
  | Ok program -> program
  | Error error -> raise_s [%sexp (error : Parser.Error.t)]
;;

let print_error error = print_endline (Error.to_string_hum error)

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
      val map : ('a -> 'b) -> list['a] -> list['b]
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
        val con : string -> list[t] -> t
        val tuple : list[t] -> t
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
        val tuple : list[t] -> t
      end
      module Expr : sig
        type t = Lit of Lit.t | Var of string * option[Type_expr.t] | App of t * t | Let of string * list[string * option[Type_expr.t]] * option[Type_expr.t] * t * t | Splice of {`expr : t; `subs : option[list[string * option[t]]]} | Quote of t | Tuple of list[t] | Construct of string * list[t] | Record of list[string * t] | Field of t * string | Match of t * list[Pattern.t * t] | Poly_record of list[string * t] | Poly_field of t * string | Poly_extend of list[string * t] * t | Poly_restrict of t * string | Poly_update of t * list[string * t] | Poly_variant of string * option[t] | Expect of string
        val var : string -> t
        val int : int -> t
        val string : string -> t
        val apply : t -> list[t] -> t
        val tuple : list[t] -> t
        val let_ : string -> list[string] -> t -> t -> t
      end
      module Toplevel : sig
        type t = Let of string * list[string * option[Type_expr.t]] * option[Type_expr.t] * Expr.t | Type of {`kind : Type_decl.Kind.t; `name : string; `params : list[Type_decl.Param.t]} | Module of string * list[t] | Expect_test of {`body : Expr.t; `name : string}
        val let_ : string -> list[string] -> Expr.t -> t
      end
      module Program : sig
        type t = list[Toplevel.t]
      end
    end
    |}]
;;

(* The refs of the constructors in [value], by name. *)
let rec constructors_of (value : Treewalk.Value.t) =
  let union values =
    List.map values ~f:constructors_of
    |> List.fold ~init:String.Map.empty ~f:(fun acc refs ->
      Map.merge_skewed acc refs ~combine:(fun ~key:_ ref _ -> ref))
  in
  match value with
  | Construct (ref, args) -> Map.set (union args) ~key:ref.name ~data:ref
  | Tuple values -> union values
  | Poly_record fields -> union (Map.data fields)
  | _ -> String.Map.empty
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

(* By name, those [to_value] makes from [round_trip_sources]. *)
let round_trip_constructors =
  List.map round_trip_sources ~f:(fun source ->
    constructors_of (Syntax_value.Program.to_value (parse source)))
  |> List.reduce_exn ~f:(Map.merge_skewed ~combine:(fun ~key:_ ref _ -> ref))
;;

let%expect_test "round trips, modulo locations" =
  List.iter round_trip_sources ~f:(fun source ->
    let program = parse source in
    match
      Syntax_value.Program.of_value
        (Syntax_value.Program.to_value program)
        ~loc:Loc.none
    with
    | Error error -> print_error error
    | Ok program' ->
      print_s
        [%message "" ~equal:(Syntax.Program.equal program program' : bool)]);
  [%expect
    {|
    (equal true)
    (equal true)
    (equal true)
    |}]
;;

let%expect_test "constructors are made as the typechecker resolves them" =
  let signature =
    match Driver.check_files [ Prelude.file ] with
    | Error error -> raise_s [%sexp (error : Driver.Error.t)]
    | Ok driver -> (Driver.checked driver).signature
  in
  (* A function matching every constructor of each variant of the prelude. *)
  let rec probes path (items : Signature.t) =
    List.concat_map items ~f:(function
      | Module { name; items } -> probes (path @ [ name ]) items
      | Type { kind = Variant constructors; _ } ->
        [ List.map constructors ~f:(fun { name; args } ->
            let args =
              match List.length args with
              | 0 -> ""
              | 1 -> " _"
              | n ->
                sprintf
                  " (%s)"
                  (String.concat ~sep:", " (List.init n ~f:(Fn.const "_")))
            in
            sprintf "%s%s -> 0" (String.concat ~sep:"." (path @ [ name ])) args)
          |> String.concat ~sep:" | "
        ]
      | Type _ | Value _ -> [])
  in
  let source =
    List.mapi
      (probes [] signature)
      ~f:(sprintf "let probe%d x = match x with %s")
    |> String.concat ~sep:"\n"
  in
  let typechecker =
    match
      Driver.check_files
        [ Prelude.file; { path = "probe.stag"; contents = source } ]
    with
    | Error error -> raise_s [%sexp (error : Driver.Error.t)]
    | Ok driver ->
      List.concat_map (Driver.checked driver).program.sccs ~f:(fun scc ->
        List.concat_map scc.members ~f:(fun (member : Typed.Toplevel.t) ->
          match member.body.desc with
          | Match (_, cases)
            when String.is_prefix member.ident.name ~prefix:"Probe." ->
            List.map cases ~f:(fun ((pattern : Typed.Pattern.t), _) ->
              match pattern.desc with
              | Construct (ref, _) -> ref.name, ref
              | _ -> raise_s [%message "Not a constructor pattern"])
          | _ -> []))
      |> String.Map.of_alist_exn
  in
  let sexp_of_refs = [%sexp_of: Typed.Constructor_ref.t String.Map.t] in
  print_s
    [%message
      ""
        ~constructors:(Map.length typechecker : int)
        ~same_as_to_value:
          (Sexp.equal
             (sexp_of_refs typechecker)
             (sexp_of_refs round_trip_constructors)
           : bool)];
  [%expect {| ((constructors 60) (same_as_to_value true)) |}]
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
let arm name = (Stag.Pattern.Construct (name, Stag.Nil), Stag.Expr.string name)
let constructor name = (name, Stag.Nil)

(* type t = <names>  let to_string x = match x with <name> -> "<name>" ... *)
let enum names =
  Stag.Cons
    ( Stag.Toplevel.Type
        {`name = "t"; `params = Stag.Nil; `kind = Stag.Type_decl.Kind.Variant (Stag.map constructor names)}
    , Stag.Cons
        ( Stag.Toplevel.let_
            "to_string"
            (Stag.Cons ("x", Stag.Nil))
            (Stag.Expr.Match (Stag.Expr.var "x", Stag.map arm names))
        , Stag.Nil ) )

let names = Stag.Cons ("Red", Stag.Cons ("Green", Stag.Cons ("Blue", Stag.Nil)))
let decls = enum names
let bad = enum (Stag.Cons ("Red", Stag.Cons ("light blue", Stag.Nil)))
let keyword = Stag.Cons (Stag.Toplevel.let_ "match" Stag.Nil (Stag.Expr.var "x"), Stag.Nil)
let bad_path = Stag.Cons (Stag.Toplevel.let_ "x" Stag.Nil (Stag.Expr.var "M..y"), Stag.Nil)
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
     (Let to_string ((x ())) ()
      (Match (Var x ())
       (((Construct Red ()) (Lit (String Red)))
        ((Construct Green ()) (Lit (String Green)))
        ((Construct Blue ()) (Lit (String Blue)))))))
    |}];
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
    Malformed syntax: expected a capitalized identifier, got "light blue", within element 0 > Stag.Toplevel.Type > field `kind > Stag.Type_decl.Kind.Variant > element 1 > component 0
    Malformed syntax: expected a lowercase identifier or _, got "match", within element 0 > Stag.Toplevel.Let argument 0
    Malformed syntax: expected a path of capitalized identifiers ending in a lowercase identifier, got "M..y", within element 0 > Stag.Toplevel.Let argument 3 > Stag.Expr.Var argument 0
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
    {| Malformed syntax: expected Stag.option, got Other.Some 1, within element 0 > Stag.Toplevel.Let argument 2 |}];
  print_program (Int 1);
  [%expect {| Malformed syntax: expected Stag.list, got 1 |}]
;;

let print_expr value =
  match Syntax_value.Expr.of_value value ~loc:Loc.none with
  | Ok expr -> print_s [%sexp (expr : Syntax.Expr.t)]
  | Error error -> print_error error
;;

let%expect_test "malformed values made otherwise" =
  let expr source =
    match Parser.expr ~file:"t.stag" source with
    | Ok expr -> Syntax_value.Expr.to_value expr
    | Error error -> raise_s [%sexp (error : Parser.Error.t)]
  in
  (* [f] applied to the first constructor named [name] in [value]. *)
  let rec edit (value : Treewalk.Value.t) ~name ~f : Treewalk.Value.t =
    match value with
    | Construct (ref, args) when String.equal ref.name name -> f ref args
    | Construct (ref, args) -> Construct (ref, List.map args ~f:(edit ~name ~f))
    | Tuple values -> Tuple (List.map values ~f:(edit ~name ~f))
    | Poly_record fields -> Poly_record (Map.map fields ~f:(edit ~name ~f))
    | value -> value
  in
  (* Wrong arity. *)
  print_expr
    (edit (expr "f (g x)") ~name:"Stag.Expr.App" ~f:(fun ref args ->
       Construct (ref, List.take args 1)));
  [%expect
    {| Malformed syntax: expected Stag.Expr.App applied to its declared arguments, got Stag.Expr.App (Stag.Expr.Var ("f", Stag.None)) |}];
  (* A wrong payload. *)
  print_expr
    (edit (expr "(1, 2)") ~name:"Stag.Lit.Int" ~f:(fun ref _ ->
       Construct (ref, [ String "1" ])));
  [%expect
    {| Malformed syntax: expected an int, got "1", within Stag.Expr.Tuple > element 0 > Stag.Expr.Lit > Stag.Lit.Int |}];
  (* A missing record field. *)
  print_expr
    (edit (expr ".< .~e >.") ~name:"Stag.Expr.Splice" ~f:(fun ref args ->
       match args with
       | [ Poly_record fields ] ->
         Construct (ref, [ Poly_record (Map.remove fields "subs") ])
       | _ -> assert false));
  [%expect
    {| Malformed syntax: expected a record with a field `subs, got {`expr = Stag.Expr.Var ("e", Stag.None)}, within Stag.Expr.Quote > Stag.Expr.Splice |}];
  (* Not a pair. *)
  print_expr
    (edit
       (expr "let f x (y : int) = x in f")
       ~name:"Stag.Cons"
       ~f:(fun ref args ->
         match args with
         | [ Tuple [ name; annot ]; tail ] ->
           Construct (ref, [ Tuple [ name; annot; annot ]; tail ])
         | _ -> assert false));
  [%expect
    {| Malformed syntax: expected a pair, got ("x", Stag.None, Stag.None), within Stag.Expr.Let argument 1 > element 0 |}];
  (* A list ending in something else. *)
  print_expr (edit (expr "(a, b, c)") ~name:"Stag.Nil" ~f:(fun _ _ -> Unit));
  [%expect
    {| Malformed syntax: expected Stag.list, got (), within Stag.Expr.Tuple |}]
;;

let%expect_test "names" =
  let at txt = Located.create ~loc:Loc.none txt in
  let expr desc : Syntax.Expr.t = at desc in
  let ty desc : Syntax.Type_expr.t = at desc in
  let unit = expr (Lit Unit) in
  let of_expr f name =
    Syntax_value.Expr.of_value
      (Syntax_value.Expr.to_value (f name))
      ~loc:Loc.none
    |> Result.ignore_m
  in
  let of_type f name =
    Syntax_value.Type_expr.of_value
      (Syntax_value.Type_expr.to_value (f name))
      ~loc:Loc.none
    |> Result.ignore_m
  in
  let of_toplevel f name =
    Syntax_value.Toplevel.of_value
      (Syntax_value.Toplevel.to_value (f name))
      ~loc:Loc.none
    |> Result.ignore_m
  in
  (* Each position, by the syntax with the name there. *)
  let positions =
    [ ( "pattern"
      , of_expr (fun name ->
          expr (Match (unit, [ at (Syntax.Pattern.Var name), unit ]))) )
    ; "let", of_expr (fun name -> expr (Let (at name, [], None, unit, unit)))
    ; ( "param"
      , of_expr (fun name ->
          expr (Let (at "f", [ at name, None ], None, unit, unit))) )
    ; ( "module"
      , of_toplevel (fun name -> at (Syntax.Toplevel.Module (at name, []))) )
    ; ( "type"
      , of_toplevel (fun name ->
          at
            (Syntax.Toplevel.Type
               { name = at name; params = []; kind = Abstract })) )
    ; "label", of_expr (fun name -> expr (Poly_variant (at name, None)))
    ; "tyvar", of_type (fun name -> ty (Var name))
    ; ( "context"
      , of_type (fun name ->
          ty
            (Code
               { inner = ty (Tuple [])
               ; context =
                   at
                     (Syntax.Type_expr.Extend
                        (at name, ty (Tuple []), at Syntax.Type_expr.Empty))
               })) )
    ; ( "subs"
      , of_expr (fun name ->
          expr (Splice { expr = unit; subs = Some [ at name, None ] })) )
    ; ( "var path"
      , of_expr (fun name ->
          expr (Var (Syntax.Path.of_list_exn [ name ], None))) )
    ; ( "con path"
      , of_expr (fun name ->
          expr (Construct (at (Syntax.Path.of_list_exn [ name ]), []))) )
    ]
  in
  let texts =
    [ "x"
    ; "x'"
    ; "_x"
    ; "_"
    ; "match"
    ; "X"
    ; "X'"
    ; "M.x"
    ; "M.X"
    ; "M..x"
    ; "M."
    ; ".x"
    ; ""
    ; " x"
    ; "x "
    ; "x (* c *)"
    ; "x y"
    ; "'a"
    ; "`A"
    ; "_a"
    ; "a'"
    ]
  in
  let width =
    List.fold texts ~init:0 ~f:(fun width text ->
      max width (String.length (sprintf "%S" text)))
  in
  printf "%*s" width "";
  List.iter positions ~f:(fun (position, _) -> printf " %s" position);
  print_endline "";
  List.iter texts ~f:(fun text ->
    printf "%-*s" width (sprintf "%S" text);
    List.iter positions ~f:(fun (position, of_name) ->
      printf
        " %*s"
        (String.length position)
        (if Result.is_ok (of_name text) then "ok" else "-"));
    print_endline "");
  [%expect
    {|
                pattern let param module type label tyvar context subs var path con path
    "x"              ok  ok    ok      -   ok    ok    ok      ok   ok       ok        -
    "x'"             ok  ok    ok      -   ok    ok     -      ok   ok       ok        -
    "_x"             ok  ok    ok      -   ok    ok     -      ok   ok       ok        -
    "_"               -  ok    ok      -    -    ok     -       -    -        -        -
    "match"           -   -     -      -    -    ok    ok       -    -        -        -
    "X"               -   -     -     ok    -    ok    ok       -    -        -       ok
    "X'"              -   -     -     ok    -    ok     -       -    -        -       ok
    "M.x"             -   -     -      -    -     -     -       -    -       ok        -
    "M.X"             -   -     -      -    -     -     -       -    -        -       ok
    "M..x"            -   -     -      -    -     -     -       -    -        -        -
    "M."              -   -     -      -    -     -     -       -    -        -        -
    ".x"              -   -     -      -    -     -     -       -    -        -        -
    ""                -   -     -      -    -     -     -       -    -        -        -
    " x"              -   -     -      -    -     -     -       -    -        -        -
    "x "              -   -     -      -    -     -     -       -    -        -        -
    "x (* c *)"       -   -     -      -    -     -     -       -    -        -        -
    "x y"             -   -     -      -    -     -     -       -    -        -        -
    "'a"              -   -     -      -    -     -     -       -    -        -        -
    "`A"              -   -     -      -    -     -     -       -    -        -        -
    "_a"             ok  ok    ok      -   ok    ok     -      ok   ok       ok        -
    "a'"             ok  ok    ok      -   ok    ok     -      ok   ok       ok        -
    |}];
  (* Each says what it expected. *)
  List.iter positions ~f:(fun (position, of_name) ->
    match of_name "x y" with
    | Ok () -> printf "%s: ok\n" position
    | Error error -> printf "%s: %s\n" position (Error.to_string_hum error));
  [%expect
    {|
    pattern: Malformed syntax: expected a lowercase identifier, got "x y", within Stag.Expr.Match argument 1 > element 0 > component 0 > Stag.Pattern.Var
    let: Malformed syntax: expected a lowercase identifier or _, got "x y", within Stag.Expr.Let argument 0
    param: Malformed syntax: expected a lowercase identifier or _, got "x y", within Stag.Expr.Let argument 1 > element 0 > component 0
    module: Malformed syntax: expected a capitalized identifier, got "x y", within Stag.Toplevel.Module argument 0
    type: Malformed syntax: expected a lowercase identifier, got "x y", within Stag.Toplevel.Type > field `name
    label: Malformed syntax: expected a label, got "x y", within Stag.Expr.Poly_variant argument 0
    tyvar: Malformed syntax: expected a type variable name, got "x y", within Stag.Type_expr.Var
    context: Malformed syntax: expected a lowercase identifier, got "x y", within Stag.Type_expr.Code > field `context > Stag.Type_expr.Row.Extend argument 0
    subs: Malformed syntax: expected a lowercase identifier, got "x y", within Stag.Expr.Splice > field `subs > element 0 > component 0
    var path: Malformed syntax: expected a path of capitalized identifiers ending in a lowercase identifier, got "x y", within Stag.Expr.Var argument 0
    con path: Malformed syntax: expected a path of capitalized identifiers ending in a capitalized identifier, got "x y", within Stag.Expr.Construct argument 0
    |}]
;;

let%expect_test "shapes" =
  let at txt = Located.create ~loc:Loc.none txt in
  let unit : Syntax.Expr.t = at (Syntax.Expr.Lit Unit) in
  let int_ty : Syntax.Type_expr.t =
    at (Syntax.Type_expr.Con (at (Syntax.Path.name "int"), []))
  in
  let any : Syntax.Pattern.t = at Syntax.Pattern.Any in
  let show ~to_value ~of_value sexp_of x =
    match of_value (to_value x) ~loc:Loc.none with
    | Ok x -> print_s (sexp_of x)
    | Error error -> print_error error
  in
  let expr =
    show
      ~to_value:Syntax_value.Expr.to_value
      ~of_value:Syntax_value.Expr.of_value
      Syntax.Expr.sexp_of_t
  in
  let pattern p =
    show
      ~to_value:Syntax_value.Pattern.to_value
      ~of_value:Syntax_value.Pattern.of_value
      Syntax.Pattern.sexp_of_t
      p
  in
  let type_expr =
    show
      ~to_value:Syntax_value.Type_expr.to_value
      ~of_value:Syntax_value.Type_expr.of_value
      Syntax.Type_expr.sexp_of_t
  in
  let type_decl =
    show
      ~to_value:Syntax_value.Type_decl.to_value
      ~of_value:Syntax_value.Type_decl.of_value
      Syntax.Type_decl.sexp_of_t
  in
  List.iter
    [ []; [ unit ]; [ unit; unit ] ]
    ~f:(fun es -> expr (at (Syntax.Expr.Tuple es)));
  [%expect
    {|
    Malformed syntax: expected at least two components, got Stag.Nil, within Stag.Expr.Tuple
    Malformed syntax: expected at least two components, got Stag.Cons (Stag.Expr.Lit Stag.Lit.Unit, Stag.Nil), within Stag.Expr.Tuple
    (Tuple ((Lit Unit) (Lit Unit)))
    |}];
  List.iter
    [ []; [ any ]; [ any; any ] ]
    ~f:(fun ps -> pattern (at (Syntax.Pattern.Tuple ps)));
  [%expect
    {|
    Malformed syntax: expected at least two components, got Stag.Nil, within Stag.Pattern.Tuple
    Malformed syntax: expected at least two components, got Stag.Cons (Stag.Pattern.Any, Stag.Nil), within Stag.Pattern.Tuple
    (Tuple (Any Any))
    |}];
  List.iter
    [ []; [ int_ty ]; [ int_ty; int_ty ] ]
    ~f:(fun ts -> type_expr (at (Syntax.Type_expr.Tuple ts)));
  [%expect
    {|
    (Tuple ())
    Malformed syntax: expected no components or at least two, got Stag.Cons (Stag.Type_expr.Con ("int", Stag.Nil), Stag.Nil), within Stag.Type_expr.Tuple
    (Tuple ((Con int ()) (Con int ())))
    |}];
  expr (at (Syntax.Expr.Record []));
  pattern (at (Syntax.Pattern.Record []));
  type_decl { name = at "t"; params = []; kind = Record [] };
  [%expect
    {|
    Malformed syntax: expected at least one field, got Stag.Nil, within Stag.Expr.Record
    Malformed syntax: expected at least one field, got Stag.Nil, within Stag.Pattern.Record
    Malformed syntax: expected at least one field, got Stag.Nil, within field `kind > Stag.Type_decl.Kind.Record
    |}]
;;

(* Shapes the parser can't produce, which are accepted as meaningful. *)
let%expect_test "shapes left to the typechecker" =
  let values =
    eval_values
      {|
let unit = Stag.Type_expr.con "unit" Stag.Nil
let int = Stag.Type_expr.con "int" Stag.Nil
let decls =
  Stag.Cons (Stag.Toplevel.Type {`name = "void"; `params = Stag.Nil; `kind = Stag.Type_decl.Kind.Variant Stag.Nil},
  Stag.Cons (Stag.Toplevel.Type {`name = "pair"; `params = Stag.Nil;
     `kind = Stag.Type_decl.Kind.Variant (Stag.Cons (("P", Stag.Cons (Stag.Type_expr.Tuple (Stag.Cons (int, Stag.Cons (int, Stag.Nil))), Stag.Nil)), Stag.Nil))},
  Stag.Cons (Stag.Toplevel.Let ("absurd", Stag.Cons (("v", Stag.Some (Stag.Type_expr.con "void" Stag.Nil)), Stag.Nil), Stag.Some int,
     Stag.Expr.Match (Stag.Expr.var "v", Stag.Nil)),
  Stag.Cons (Stag.Toplevel.let_ "p" Stag.Nil
     (Stag.Expr.Construct ("P", Stag.Cons (Stag.Expr.Tuple (Stag.Cons (Stag.Expr.int 1, Stag.Cons (Stag.Expr.int 2, Stag.Nil))), Stag.Nil))),
  Stag.Cons (Stag.Toplevel.let_ "first" (Stag.Cons ("p", Stag.Nil))
     (Stag.Expr.Match (Stag.Expr.var "p", Stag.Cons ((Stag.Pattern.Construct ("P", Stag.Cons (Stag.Pattern.Var "q", Stag.Nil)), Stag.Expr.var "q"), Stag.Nil))),
  Stag.Cons (Stag.Toplevel.let_ "extended" Stag.Nil (Stag.Expr.Poly_extend (Stag.Nil, Stag.Expr.Poly_record (Stag.Cons (("x", Stag.Expr.int 1), Stag.Nil)))),
  Stag.Cons (Stag.Toplevel.let_ "updated" Stag.Nil (Stag.Expr.Poly_update (Stag.Expr.var "extended", Stag.Nil)),
  Stag.Nil)))))))
|}
  in
  let program =
    Syntax_value.Program.of_value (values "Gen.decls") ~loc:Loc.none |> ok_exn
  in
  print_s [%sexp (program : Syntax.Program.t)];
  [%expect
    {|
    ((Type ((name void) (params ()) (kind (Variant ()))))
     (Type
      ((name pair) (params ())
       (kind (Variant ((P ((Tuple ((Con int ()) (Con int ()))))))))))
     (Let absurd ((v ((Con void ())))) ((Con int ())) (Match (Var v ()) ()))
     (Let p () () (Construct P ((Tuple ((Lit (Int 1)) (Lit (Int 2)))))))
     (Let first ((p ())) ()
      (Match (Var p ()) (((Construct P ((Var q))) (Var q ())))))
     (Let extended () () (Poly_extend () (Poly_record ((x (Lit (Int 1)))))))
     (Let updated () () (Poly_update (Var extended ()) ())))
    |}];
  Source_helpers.run_program
    ~prelude:false
    (program
     @ parse
         {|let both = match first p with (a, b) -> print_int (Int.add a b); print_endline ""|}
    );
  [%expect
    {|
    type void = |
    type pair = P of (int * int)
    val absurd : void -> int
    val p : pair
    val first : pair -> int * int
    val extended : {`x : int}
    val updated : {`x : int}
    val both : unit
    -- evaluated at compile time
    -- evaluated
    3
    (absurd <fun>)
    (p (P (Tuple 1 2)))
    (first <fun>)
    (extended ((x 1)))
    (updated ((x 1)))
    (both "()")
    |}];
  (* A match of no arms is only of an empty type. *)
  Source_helpers.run_program
    ~prelude:false
    (program
     @ [ Located.create
           ~loc:Loc.none
           (Syntax.Toplevel.Let
              ( Located.create ~loc:Loc.none "bad"
              , []
              , None
              , Located.create
                  ~loc:Loc.none
                  (Syntax.Expr.Match
                     ( Located.create
                         ~loc:Loc.none
                         (Syntax.Expr.Var (Syntax.Path.name "p", None))
                     , [] )) ))
       ]);
  [%expect
    {|
    (Unification_error
     (Type_mismatch (TVariant REmpty) (TCon pair ()) Structural_mismatch))
    |}]
;;

let%expect_test "negative ints are [Int.neg] applied to a literal" =
  let values =
    eval_values
      {|
let minus n = Stag.Expr.App (Stag.Expr.var "Int.neg", Stag.Expr.int n)
let decls = Stag.Cons (Stag.Toplevel.let_ "minus_one" Stag.Nil (minus 1), Stag.Nil)
let literal = Stag.Cons (Stag.Toplevel.let_ "minus_one" Stag.Nil (Stag.Expr.int (Int.neg 1)), Stag.Nil)
|}
  in
  let program =
    Syntax_value.Program.of_value (values "Gen.decls") ~loc:Loc.none |> ok_exn
  in
  print_s [%sexp (program : Syntax.Program.t)];
  [%expect {| ((Let minus_one () () (App (Var Int.neg ()) (Lit (Int 1))))) |}];
  Source_helpers.run_program
    ~prelude:false
    (program @ parse {|let printed = print_int minus_one; print_endline ""|});
  [%expect
    {|
    val minus_one : int
    val printed : unit
    -- evaluated at compile time
    -- evaluated
    -1
    (minus_one -1)
    (printed "()")
    |}];
  print_program (values "Gen.literal");
  [%expect
    {| Malformed syntax: expected a non-negative int, got -1, within element 0 > Stag.Toplevel.Let argument 3 > Stag.Expr.Lit > Stag.Lit.Int |}]
;;

let%expect_test "prelude helpers" =
  let values =
    eval_values
      {|
let int = Stag.Type_expr.con "int" Stag.Nil
let types =
  Stag.Cons (Stag.Type_expr.tuple Stag.Nil,
  Stag.Cons (Stag.Type_expr.tuple (Stag.Cons (int, Stag.Nil)),
  Stag.Cons (Stag.Type_expr.tuple (Stag.Cons (int, Stag.Cons (int, Stag.Nil))),
  Stag.Cons (Stag.Type_expr.con "list" (Stag.Cons (int, Stag.Nil)), Stag.Nil))))
let patterns =
  Stag.Cons (Stag.Pattern.tuple Stag.Nil,
  Stag.Cons (Stag.Pattern.tuple (Stag.Cons (Stag.Pattern.Any, Stag.Nil)),
  Stag.Cons (Stag.Pattern.tuple (Stag.Cons (Stag.Pattern.Var "a", Stag.Cons (Stag.Pattern.Var "b", Stag.Nil))), Stag.Nil)))
let one = Stag.Expr.int 1
let exprs =
  Stag.Cons (Stag.Expr.tuple Stag.Nil,
  Stag.Cons (Stag.Expr.tuple (Stag.Cons (one, Stag.Nil)),
  Stag.Cons (Stag.Expr.tuple (Stag.Cons (one, Stag.Cons (Stag.Expr.string "s", Stag.Nil))),
  Stag.Cons (Stag.Expr.apply (Stag.Expr.var "f") Stag.Nil,
  Stag.Cons (Stag.Expr.apply (Stag.Expr.var "f") (Stag.Cons (one, Stag.Cons (Stag.Expr.var "x", Stag.Nil))), Stag.Nil)))))
let decls =
  Stag.Cons (Stag.Toplevel.let_ "add" (Stag.Cons ("x", Stag.Cons ("y", Stag.Nil)))
    (Stag.Expr.apply (Stag.Expr.var "Int.add") (Stag.Cons (Stag.Expr.var "x", Stag.Cons (Stag.Expr.var "y", Stag.Nil)))),
  Stag.Cons (Stag.Toplevel.let_ "seven" Stag.Nil
    (Stag.Expr.let_ "double" (Stag.Cons ("n", Stag.Nil))
       (Stag.Expr.apply (Stag.Expr.var "add") (Stag.Cons (Stag.Expr.var "n", Stag.Cons (Stag.Expr.var "n", Stag.Nil))))
       (Stag.Expr.Match (Stag.Expr.tuple (Stag.Cons (Stag.Expr.int 3, Stag.Cons (one, Stag.Nil))),
          Stag.Cons ((Stag.Pattern.tuple (Stag.Cons (Stag.Pattern.Var "a", Stag.Cons (Stag.Pattern.Var "b", Stag.Nil))),
                      Stag.Expr.apply (Stag.Expr.var "add") (Stag.Cons (Stag.Expr.App (Stag.Expr.var "double", Stag.Expr.var "a"), Stag.Cons (Stag.Expr.var "b", Stag.Nil)))),
                     Stag.Nil)))),
  Stag.Nil))
|}
  in
  let each of_value sexp_of name =
    let rec elements (value : Treewalk.Value.t) =
      match value with
      | Construct ({ name = "Stag.Cons"; _ }, [ x; rest ]) -> x :: elements rest
      | _ -> []
    in
    List.iter
      (elements (values name))
      ~f:(fun value ->
        match of_value value ~loc:Loc.none with
        | Ok x -> print_s (sexp_of x)
        | Error error -> print_error error)
  in
  each Syntax_value.Type_expr.of_value Syntax.Type_expr.sexp_of_t "Gen.types";
  [%expect
    {|
    (Con unit ())
    (Con int ())
    (Tuple ((Con int ()) (Con int ())))
    (Con list ((Ty (Con int ()))))
    |}];
  each Syntax_value.Pattern.of_value Syntax.Pattern.sexp_of_t "Gen.patterns";
  [%expect
    {|
    (Lit Unit)
    Any
    (Tuple ((Var a) (Var b)))
    |}];
  each Syntax_value.Expr.of_value Syntax.Expr.sexp_of_t "Gen.exprs";
  [%expect
    {|
    (Lit Unit)
    (Lit (Int 1))
    (Tuple ((Lit (Int 1)) (Lit (String s))))
    (Var f ())
    (App (App (Var f ()) (Lit (Int 1))) (Var x ()))
    |}];
  let program =
    Syntax_value.Program.of_value (values "Gen.decls") ~loc:Loc.none |> ok_exn
  in
  print_s [%sexp (program : Syntax.Program.t)];
  [%expect
    {|
    ((Let add ((x ()) (y ())) ()
      (App (App (Var Int.add ()) (Var x ())) (Var y ())))
     (Let seven () ()
      (Let double ((n ())) () (App (App (Var add ()) (Var n ())) (Var n ()))
       (Match (Tuple ((Lit (Int 3)) (Lit (Int 1))))
        (((Tuple ((Var a) (Var b)))
          (App (App (Var add ()) (App (Var double ()) (Var a ()))) (Var b ()))))))))
    |}];
  Source_helpers.run_program
    ~prelude:false
    (program @ parse {|let printed = print_int seven; print_endline ""|});
  [%expect
    {|
    val add : int -> int -> int
    val seven : int
    val printed : unit
    -- evaluated at compile time
    -- evaluated
    7
    (add <fun>)
    (seven 7)
    (printed "()")
    |}]
;;
