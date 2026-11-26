open! Import
open Syntax
include Lift_intf

let indirect_constructors =
  List.map
    ~f:(fun (name, arity) -> "Stag." ^ name, arity)
    [ "Type_expr.Arrow", 2
    ; "Type_expr.Code", 1
    ; "Type_expr.Mut", 1
    ; "Type_expr.Immut", 1
    ; "Type_expr.Splice", 1
    ; "Type_expr.Moded", 2
    ; "Type_expr.Constrained", 2
    ; "Type_expr.Modality", 2
    ; "Type_expr.Row.Extend", 3
    ; "Mode_expr.Term.Meet", 2
    ; "Mode_expr.Term.Release", 1
    ; "Mode_expr.Term.Static_if_const", 1
    ; "Mode_expr.Term.Const_if_static", 1
    ; "Mode_expr.Constraint.Submode", 2
    ; "Pattern.Deref", 1
    ; "Pattern.Poly_variant", 2
    ; "Pattern.Exception", 1
    ; "Expr.App", 2
    ; "Expr.Let", 5
    ; "Expr.Let_nonrec", 5
    ; "Expr.Seq", 2
    ; "Expr.Let_mut", 4
    ; "Expr.Assign", 2
    ; "Expr.Store", 2
    ; "Expr.Fun", 2
    ; "Expr.Splice", 1
    ; "Expr.Quote", 1
    ; "Expr.Deref", 1
    ; "Expr.Field", 2
    ; "Expr.Record_update", 2
    ; "Expr.Match", 2
    ; "Expr.Poly_field", 2
    ; "Expr.Poly_extend", 2
    ; "Expr.Poly_restrict", 2
    ; "Expr.Poly_update", 2
    ; "Expr.Poly_variant", 2
    ; "Expr.Stack", 1
    ; "Expr.Exclave", 1
    ; "Expr.Region", 1
    ; "Expr.Nontail", 1
    ; "Expr.Annotated", 2
    ; "Expr.Forget", 2
    ]
;;

let indirect_arity =
  let table = String.Table.of_alist_exn indirect_constructors in
  fun name -> Hashtbl.find table name
;;

module Make (B : Builder) = struct
  let con name args =
    match indirect_arity name, args with
    | None, _ -> B.con name args
    | Some 1, [ arg ] -> B.con name [ B.ptr arg ]
    | Some _, _ -> B.con name [ B.ptr (B.tuple args) ]
  ;;

  let cons x xs = con "Cons" [ B.box (B.tuple [ x; xs ]) ]

  let list f xs =
    List.fold_right xs ~init:(con "Nil" []) ~f:(fun x tail -> cons (f x) tail)
  ;;

  let option f = function
    | None -> con "None" []
    | Some x -> con "Some" [ f x ]
  ;;

  let name (name : Name.t) = B.string name.txt
  let pair f g (a, b) = B.tuple [ f a; g b ]

  let decl_path_txt kind : Decl_path.t -> B.t = function
    | Path p -> con "Stag.Path.Name" [ B.string (Path.to_string p) ]
    | Root p -> con "Stag.Path.Name" [ B.string ("." ^ Path.to_string p) ]
    | Resolved name -> con "Stag.Path.Ref" [ B.ref_ kind name ]
  ;;

  let decl_path kind ({ txt; loc = _ } : Decl_path.t Located.t) =
    decl_path_txt kind txt
  ;;

  let lit : Lit.t -> B.t = function
    | Int n -> con "Stag.Lit.Int" [ B.int n ]
    | Bool b -> con "Stag.Lit.Bool" [ B.bool b ]
    | String s -> con "Stag.Lit.String" [ B.string s ]
    | Char c -> con "Stag.Lit.Char" [ B.char c ]
    | Unit -> con "Stag.Lit.Unit" []
  ;;

  let no_antiquote _ _ = None

  let mode_atom atom =
    con
      ("Stag.Mode_expr.Atom." ^ Sexp.to_string (Mode_expr.Atom.sexp_of_t atom))
      []
  ;;

  let mode_expr ({ txt; loc = _ } : Mode_expr.t) =
    match txt with
    | Var var -> con "Stag.Mode_expr.Var" [ B.string var ]
    | Atoms atoms -> con "Stag.Mode_expr.Atoms" [ list mode_atom atoms ]
    | Var_at (locality, var) ->
      con "Stag.Mode_expr.Var_at" [ mode_atom locality; B.string var ]
  ;;

  let mode_axis (axis : Mode_expr.Axis.t) =
    con
      ("Stag.Mode_expr.Axis." ^ Sexp.to_string (Mode_expr.Axis.sexp_of_t axis))
      []
  ;;

  let rec mode_term (term : Mode_expr.Term.t) =
    let con name = con ("Stag.Mode_expr.Term." ^ name) in
    match term with
    | Mode m -> con "Mode" [ mode_expr m ]
    | Axis (axis, m) -> con "Axis" [ mode_axis axis; mode_expr m ]
    | Meet (t, atom) -> con "Meet" [ mode_term t; mode_atom atom ]
    | Release t -> con "Release" [ mode_term t ]
    | Static_if_const t -> con "Static_if_const" [ mode_term t ]
    | Const_if_static t -> con "Const_if_static" [ mode_term t ]
  ;;

  let mode_constraint : Mode_expr.Constraint.t -> _ = function
    | Submode { lower; upper } ->
      con
        "Stag.Mode_expr.Constraint.Submode"
        [ mode_term lower; mode_term upper ]
    | Pointer_free var ->
      con "Stag.Mode_expr.Constraint.Pointer_free" [ B.string var ]
  ;;

  let type_param ({ txt; loc = _ } : Type_decl.Param.t Located.t) =
    match txt with
    | Ty var -> con "Stag.Type_decl.Param.Ty" [ B.string var ]
    | Row var -> con "Stag.Type_decl.Param.Row" [ B.string var ]
    | Mode var -> con "Stag.Type_decl.Param.Mode" [ B.string var ]
  ;;

  let rec pattern ({ txt; loc = _ } : Pattern.t) =
    let con name = con ("Stag.Pattern." ^ name) in
    match txt with
    | Any -> con "Any" []
    | Var var -> con "Var" [ B.string var ]
    | Lit l -> con "Lit" [ lit l ]
    | Tuple ps -> con "Tuple" [ list pattern ps ]
    | Deref p -> con "Deref" [ pattern p ]
    | Construct (p, args) ->
      con "Construct" [ decl_path Constructor p; list pattern args ]
    | Record fields ->
      con "Record" [ list (pair (decl_path Field) pattern) fields ]
    | Poly_variant (tag, arg) ->
      con "Poly_variant" [ name tag; option pattern arg ]
    | Poly_record { fields; rest } ->
      let rest_value : Pattern.Rest.t -> B.t = function
        | Closed -> con "Rest.Closed" []
        | Open -> con "Rest.Open" []
        | Bind var -> con "Rest.Bind" [ name var ]
      in
      con
        "Poly_record"
        [ B.record
            [ "fields", list (pair name pattern) fields
            ; "rest", rest_value rest
            ]
        ]
    | Exception p -> con "Exception" [ pattern p ]
  ;;

  (* [antiquote] replaces splices, the expression of each splice it doesn't
     being lifted as any other. *)
  module Lifter (A : sig
      val antiquote : Antiquote.Kind.t -> Expr.t -> B.t option
    end) =
  struct
    let rec type_expr ({ txt; loc = _ } : Type_expr.t) =
      let con name = con ("Stag.Type_expr." ^ name) in
      match txt with
      | Var var -> con "Var" [ B.string var ]
      | Con (p, args) -> con "Con" [ decl_path Type p; list type_arg args ]
      | Arrow (a, b) -> con "Arrow" [ type_expr a; type_expr b ]
      | Tuple ts -> con "Tuple" [ list type_expr ts ]
      | Code { inner; context } ->
        con
          "Code"
          [ B.record [ "inner", type_expr inner; "context", row context ] ]
      | Record r -> con "Record" [ row r ]
      | Variant r -> con "Variant" [ row r ]
      | Mut t -> con "Mut" [ type_expr t ]
      | Immut t -> con "Immut" [ type_expr t ]
      | Moded (t, m) -> con "Moded" [ type_expr t; mode_expr m ]
      | Modality (t, m) -> con "Modality" [ type_expr t; mode_expr m ]
      | Constrained (t, cs) ->
        con "Constrained" [ type_expr t; list mode_constraint cs ]
      | Splice e ->
        (match A.antiquote Type_expr e with
         | Some lifted -> lifted
         | None -> con "Splice" [ expr e ])

    and row ({ txt; loc = _ } : Type_expr.row) =
      let con name = con ("Stag.Type_expr.Row." ^ name) in
      match txt with
      | Empty -> con "Empty" []
      | Extend (label, ty, rest) ->
        con "Extend" [ name label; type_expr ty; row rest ]
      | Var var -> con "Var" [ name var ]

    and type_arg : Type_expr.arg -> B.t = function
      | Ty ty -> con "Stag.Type_expr.Arg.Ty" [ type_expr ty ]
      | Row r -> con "Stag.Type_expr.Arg.Row" [ row r ]
      | Mode m -> con "Stag.Type_expr.Arg.Mode" [ mode_expr m ]

    and type_decl
      ({ name = decl_name; params; kind; nonrec_ = _ } : Type_decl.t)
      =
      B.record
        [ "name", name decl_name
        ; "params", list type_param params
        ; "kind", type_decl_kind kind
        ]

    and type_decl_kind : Type_decl.Kind.t -> B.t = function
      | Variant constructors ->
        con
          "Stag.Type_decl.Kind.Variant"
          [ list (pair name (list type_expr)) constructors ]
      | Record fields ->
        con "Stag.Type_decl.Kind.Record" [ list (pair name type_expr) fields ]
      | Alias ty -> con "Stag.Type_decl.Kind.Alias" [ type_expr ty ]
      | Abstract -> con "Stag.Type_decl.Kind.Abstract" []
      | Malformed kind -> type_decl_kind kind
      | Splice { txt = e; loc = _ } ->
        (match A.antiquote Type_kind e with
         | Some lifted -> lifted
         | None -> con "Stag.Type_decl.Kind.Splice" [ expr e ])

    and binding b = pair name annot b

    and annot ({ ty; mode } : Annot.t) =
      B.record [ "ty", option type_expr ty; "mode", option mode_expr mode ]

    and expr ({ txt; loc = _ } : Expr.t) =
      let con name = con ("Stag.Expr." ^ name) in
      let fields = list (pair name expr) in
      match txt with
      | Lit l -> con "Lit" [ lit l ]
      | Var (p, a) -> con "Var" [ decl_path_txt Value p; annot a ]
      | App (f, x) -> con "App" [ expr f; expr x ]
      | Let (var, bindings, a, equal, body) ->
        con
          "Let"
          [ name var; list binding bindings; annot a; expr equal; expr body ]
      | Let_nonrec (var, bindings, a, equal, body) ->
        con
          "Let_nonrec"
          [ name var; list binding bindings; annot a; expr equal; expr body ]
      | Seq (first, second) -> con "Seq" [ expr first; expr second ]
      | Let_mut (var, a, equal, body) ->
        con "Let_mut" [ name var; annot a; expr equal; expr body ]
      | Assign (p, value) ->
        con "Assign" [ decl_path_txt Value p.txt; expr value ]
      | Store (target, value) -> con "Store" [ expr target; expr value ]
      | Fun (params, body) -> con "Fun" [ list binding params; expr body ]
      | Splice { expr = spliced; subs } ->
        (match A.antiquote Expr spliced with
         | Some lifted -> lifted
         | None ->
           con
             "Splice"
             [ B.record
                 [ "expr", expr spliced
                 ; "subs", option (list (pair name (option expr))) subs
                 ]
             ])
      | Quote e -> con "Quote" [ expr e ]
      | Struct_quote toplevels -> con "Struct_quote" [ program toplevels ]
      | Tuple es -> con "Tuple" [ list expr es ]
      | Deref e -> con "Deref" [ expr e ]
      | Construct (p, args) ->
        con "Construct" [ decl_path Constructor p; list expr args ]
      | Record record_fields ->
        con "Record" [ list (pair (decl_path Field) expr) record_fields ]
      | Record_update (e, record_fields) ->
        con
          "Record_update"
          [ expr e; list (pair (decl_path Field) expr) record_fields ]
      | Field (e, p) -> con "Field" [ expr e; decl_path Field p ]
      | Type ty -> con "Type" [ type_expr ty ]
      | Match (e, arms) -> con "Match" [ expr e; list (pair pattern expr) arms ]
      | Poly_record poly_fields -> con "Poly_record" [ fields poly_fields ]
      | Poly_field (e, label) -> con "Poly_field" [ expr e; name label ]
      | Poly_extend (extension, e) ->
        con "Poly_extend" [ fields extension; expr e ]
      | Poly_restrict (e, label) -> con "Poly_restrict" [ expr e; name label ]
      | Poly_update (e, updates) -> con "Poly_update" [ expr e; fields updates ]
      | Poly_variant (tag, arg) ->
        con "Poly_variant" [ name tag; option expr arg ]
      | Expect expected -> con "Expect" [ B.string expected.txt ]
      | Stack e -> con "Stack" [ expr e ]
      | Exclave e -> con "Exclave" [ expr e ]
      | Region e -> con "Region" [ expr e ]
      | Nontail e -> con "Nontail" [ expr e ]
      | Annotated (e, a) -> con "Annotated" [ expr e; annot a ]
      | Forget (names, e) -> con "Forget" [ list name names; expr e ]
      | Error -> raise_s [%message "Lift: an expression that did not parse"]

    and toplevel ({ txt; loc = _ } : Toplevel.t) =
      let con name = con ("Stag.Toplevel." ^ name) in
      match txt with
      | Let (var, bindings, a, body) ->
        con "Let" [ name var; list binding bindings; annot a; expr body ]
      | Let_nonrec (var, bindings, a, body) ->
        con "Let_nonrec" [ name var; list binding bindings; annot a; expr body ]
      | Let_mut (var, a, body) -> con "Let_mut" [ name var; annot a; expr body ]
      | Type decl ->
        con (if decl.nonrec_ then "Type_nonrec" else "Type") [ type_decl decl ]
      | Exception (exn_name, args) | Malformed_exception (exn_name, args) ->
        con "Exception" [ name exn_name; list type_expr args ]
      | External { name = ext_name; ty; symbol } ->
        con
          "External"
          [ B.record
              [ "name", name ext_name
              ; "ty", type_expr ty
              ; "symbol", B.string symbol.txt
              ]
          ]
      | Module (module_name, items) ->
        con "Module" [ name module_name; program items ]
      | Module_splice (module_name, e, _) ->
        (match A.antiquote Item e with
         | Some items -> con "Module" [ name module_name; items ]
         | None -> con "Module_splice" [ name module_name; expr e ])
      | Include (e, _) -> con "Include" [ expr e ]
      | Expect_test { name = test_name; body } ->
        con
          "Expect_test"
          [ B.record [ "name", B.string test_name.txt; "body", expr body ] ]
      | Bench { name = bench_name; param; body } ->
        con
          "Bench"
          [ B.record
              [ "name", B.string bench_name.txt
              ; ( "param"
                , option
                    (fun ({ name = param; args } : Bench_param.t) ->
                      pair name (list expr) (param, args))
                    param )
              ; "body", expr body
              ]
          ]

    and program (toplevels : Toplevel.t list) =
      match toplevels with
      | [] -> con "Nil" []
      | item :: rest ->
        let antiquoted =
          match item.txt with
          | Include (e, _) -> A.antiquote Item e
          | _ -> None
        in
        (match antiquoted, rest with
         | Some items, [] -> items
         | Some items, _ :: _ -> B.append items (program rest)
         | None, _ -> cons (toplevel item) (program rest))
    ;;
  end

  include Lifter (struct
      let antiquote = no_antiquote
    end)

  let program ?antiquote t =
    match antiquote with
    | None -> program t
    | Some antiquote ->
      let module L =
        Lifter (struct
          let antiquote = antiquote
        end)
      in
      L.program t
  ;;
end
