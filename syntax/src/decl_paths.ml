open! Import
open Syntax

module Namespace = struct
  type t =
    | Type
    | Constructor
    | Field
end

let map (t : Toplevel.t) ~f =
  let type_path = f Namespace.Type in
  let constructor = f Constructor in
  let field = f Field in
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
  let rec expr (e : Expr.t) : Expr.t =
    let fields = List.map ~f:(fun (p, value) -> field p, expr value) in
    let poly_fields = List.map ~f:(fun (n, value) -> n, expr value) in
    let txt : Expr.desc =
      match e.txt with
      | Lit _ | Expect _ -> e.txt
      | Var (p, t) -> Var (p, Option.map t ~f:type_expr)
      | App (a, b) -> App (expr a, expr b)
      | Let (n, bs, t, value, body) ->
        Let
          ( n
          , List.map bs ~f:binding
          , Option.map t ~f:type_expr
          , expr value
          , expr body )
      | Fun (bs, body) -> Fun (List.map bs ~f:binding, expr body)
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
        Match (expr e, List.map cases ~f:(fun (p, e) -> pattern p, expr e))
      | Poly_record fs -> Poly_record (poly_fields fs)
      | Poly_field (e, n) -> Poly_field (expr e, n)
      | Poly_extend (fs, e) -> Poly_extend (poly_fields fs, expr e)
      | Poly_restrict (e, n) -> Poly_restrict (expr e, n)
      | Poly_update (e, fs) -> Poly_update (expr e, poly_fields fs)
      | Poly_variant (n, e) -> Poly_variant (n, Option.map e ~f:expr)
    in
    { e with txt }
  in
  let rec toplevel (t : Toplevel.t) : Toplevel.t =
    let txt : Toplevel.desc =
      match t.txt with
      | Let (n, bs, ty, e) ->
        Let (n, List.map bs ~f:binding, Option.map ty ~f:type_expr, expr e)
      | Type { name; params; kind } ->
        let kind : Type_decl.Kind.t =
          match kind with
          | Variant cs ->
            Variant
              (List.map cs ~f:(fun (n, ts) -> n, List.map ts ~f:type_expr))
          | Record fs -> Record (List.map fs ~f:(fun (n, t) -> n, type_expr t))
          | Alias t -> Alias (type_expr t)
          | Abstract -> Abstract
        in
        Type { name; params; kind }
      | Exception (n, ts) -> Exception (n, List.map ts ~f:type_expr)
      | Module (n, ts) -> Module (n, List.map ts ~f:toplevel)
      | Module_splice (n, e, expansion) -> Module_splice (n, expr e, expansion)
      | Include (e, expansion) -> Include (expr e, expansion)
      | Expect_test { name; body } -> Expect_test { name; body = expr body }
      | Bench { name; param; body } ->
        Bench
          { name
          ; param =
              Option.map param ~f:(fun { name; args } ->
                { Bench_param.name; args = List.map args ~f:expr })
          ; body = expr body
          }
    in
    { t with txt }
  in
  toplevel t
;;
