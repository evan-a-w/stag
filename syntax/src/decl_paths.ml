open! Import
open Syntax

module Namespace = struct
  type t =
    | Type
    | Constructor
    | Field
    | Value
end

let rec pattern_binders (p : Pattern.t) =
  match p.txt with
  | Any | Lit _ -> []
  | Var name -> [ name ]
  | Tuple ps | Construct (_, ps) -> List.concat_map ps ~f:pattern_binders
  | Record fs -> List.concat_map fs ~f:(fun (_, p) -> pattern_binders p)
  | Poly_variant (_, p) -> Option.value_map p ~default:[] ~f:pattern_binders
  | Poly_record { fields; rest } ->
    List.concat_map fields ~f:(fun (_, p) -> pattern_binders p)
    @
      (match rest with
      | Bind name -> [ name.txt ]
      | Closed | Open -> [])
  | Exception p -> pattern_binders p
;;

let rec map' (t : Toplevel.t) ~modules ~f:f' =
  let f namespace ~locals path = f' namespace ~modules ~locals path in
  let type_path = f Namespace.Type ~locals:String.Set.empty in
  let constructor = f Constructor ~locals:String.Set.empty in
  let field = f Field ~locals:String.Set.empty in
  let bind locals names = Set.union locals (String.Set.of_list names) in
  let binding_names = List.map ~f:(fun ((n : Name.t), _) -> n.txt) in
  let rec type_expr (t : Type_expr.t) : Type_expr.t =
    let txt : Type_expr.desc =
      match t.txt with
      | Var _ -> t.txt
      | Con (p, args) -> Con (type_path p, List.map args ~f:arg)
      | Arrow (a, b) -> Arrow (type_expr a, type_expr b)
      | Tuple ts -> Tuple (List.map ts ~f:type_expr)
      | Code { inner; context } ->
        Code { inner = type_expr inner; context = row context }
      | Record r -> Record (row r)
      | Variant r -> Variant (row r)
    in
    { t with txt }
  and row (r : Type_expr.row) : Type_expr.row =
    match r.txt with
    | Empty | Var _ -> r
    | Extend (n, t, rest) -> { r with txt = Extend (n, type_expr t, row rest) }
  and arg : Type_expr.arg -> Type_expr.arg = function
    | Ty t -> Ty (type_expr t)
    | Row r -> Row (row r)
  in
  let binding (n, t) = n, Option.map t ~f:type_expr in
  let rec pattern (p : Pattern.t) : Pattern.t =
    match List_literal.map_pattern_items p ~f:pattern with
    | Some p -> p
    | None -> pattern_desc p
  and pattern_desc (p : Pattern.t) : Pattern.t =
    let txt : Pattern.desc =
      match p.txt with
      | Any | Var _ | Lit _ -> p.txt
      | Tuple ps -> Tuple (List.map ps ~f:pattern)
      | Construct (c, ps) -> Construct (constructor c, List.map ps ~f:pattern)
      | Record fs ->
        Record (List.map fs ~f:(fun (p, value) -> field p, pattern value))
      | Poly_variant (n, p) -> Poly_variant (n, Option.map p ~f:pattern)
      | Poly_record { fields; rest } ->
        Poly_record
          { fields = List.map fields ~f:(fun (n, p) -> n, pattern p); rest }
      | Exception p -> Exception (pattern p)
    in
    { p with txt }
  in
  let rec expr locals (e : Expr.t) : Expr.t =
    match List_literal.map_expr_items e ~f:(expr locals) with
    | Some e -> e
    | None -> expr_desc locals e
  and expr_desc locals (e : Expr.t) : Expr.t =
    let expr' = expr in
    let expr = expr locals in
    let fields = List.map ~f:(fun (p, value) -> field p, expr value) in
    let poly_fields = List.map ~f:(fun (n, value) -> n, expr value) in
    let txt : Expr.desc =
      match e.txt with
      | Lit _ | Expect _ | Error -> e.txt
      | Var (p, t) ->
        Var
          ( (f Value ~locals { txt = p; loc = e.loc }).txt
          , Option.map t ~f:type_expr )
      | App (a, b) -> App (expr a, expr b)
      | Let (n, bs, t, value, body) ->
        Let
          ( n
          , List.map bs ~f:binding
          , Option.map t ~f:type_expr
          , expr' (bind locals (n.txt :: binding_names bs)) value
          , expr' (bind locals [ n.txt ]) body )
      | Fun (bs, body) ->
        Fun (List.map bs ~f:binding, expr' (bind locals (binding_names bs)) body)
      | Splice { expr = e; subs } ->
        Splice
          { expr = expr e
          ; subs =
              Option.map
                subs
                ~f:(List.map ~f:(fun (n, e) -> n, Option.map e ~f:expr))
          }
      | Quote e -> Quote (expr e)
      | Tuple es -> Tuple (List.map es ~f:expr)
      | Construct (c, es) -> Construct (constructor c, List.map es ~f:expr)
      | Record fs -> Record (fields fs)
      | Field (e, p) -> Field (expr e, field p)
      | Record_update (e, fs) -> Record_update (expr e, fields fs)
      | Type t -> Type (type_expr t)
      | Match (e, cases) ->
        Match
          ( expr e
          , List.map cases ~f:(fun (p, e) ->
              pattern p, expr' (bind locals (pattern_binders p)) e) )
      | Poly_record fs -> Poly_record (poly_fields fs)
      | Poly_field (e, n) -> Poly_field (expr e, n)
      | Poly_extend (fs, e) -> Poly_extend (poly_fields fs, expr e)
      | Poly_restrict (e, n) -> Poly_restrict (expr e, n)
      | Poly_update (e, fs) -> Poly_update (expr e, poly_fields fs)
      | Poly_variant (n, e) -> Poly_variant (n, Option.map e ~f:expr)
    in
    { e with txt }
  in
  let toplevel (t : Toplevel.t) : Toplevel.t =
    let txt : Toplevel.desc =
      match t.txt with
      | Let (n, bs, ty, e) ->
        Let
          ( n
          , List.map bs ~f:binding
          , Option.map ty ~f:type_expr
          , expr (String.Set.of_list (binding_names bs)) e )
      | Type { name; params; kind } ->
        let rec decl_kind : Type_decl.Kind.t -> Type_decl.Kind.t = function
          | Variant cs ->
            Variant
              (List.map cs ~f:(fun (n, ts) -> n, List.map ts ~f:type_expr))
          | Record fs -> Record (List.map fs ~f:(fun (n, t) -> n, type_expr t))
          | Alias t -> Alias (type_expr t)
          | Abstract -> Abstract
          | Malformed kind -> Malformed (decl_kind kind)
        in
        Type { name; params; kind = decl_kind kind }
      | Exception (n, ts) -> Exception (n, List.map ts ~f:type_expr)
      | Malformed_exception (n, ts) ->
        Malformed_exception (n, List.map ts ~f:type_expr)
      | Module (n, ts) ->
        Module (n, List.map ts ~f:(map' ~modules:(modules @ [ n.txt ]) ~f:f'))
      | Module_splice (n, e, expansion) ->
        Module_splice (n, expr String.Set.empty e, expansion)
      | Include (e, expansion) -> Include (expr String.Set.empty e, expansion)
      | Expect_test { name; body } ->
        Expect_test { name; body = expr String.Set.empty body }
      | Bench { name; param; body } ->
        Bench
          { name
          ; param =
              Option.map param ~f:(fun { name; args } ->
                { Bench_param.name
                ; args = List.map args ~f:(expr String.Set.empty)
                })
          ; body =
              expr
                (String.Set.of_list
                   (Option.to_list (Option.map param ~f:(fun p -> p.name.txt))))
                body
          }
    in
    { t with txt }
  in
  toplevel t
;;

let map t ~f = map' t ~modules:[] ~f
