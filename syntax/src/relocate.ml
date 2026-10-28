open! Import
open Syntax

let ( let* ) = Option.( >>= )
let ( let+ ) = Option.( >>| )

let list f gs ws =
  match List.zip gs ws with
  | Unequal_lengths -> None
  | Ok pairs -> Option.all (List.map pairs ~f:(fun (g, w) -> f g w))
;;

let option f g w =
  match g, w with
  | None, None -> Some None
  | Some g, Some w -> Option.map (f g w) ~f:Option.some
  | None, Some _ | Some _, None -> None
;;

let pair f1 f2 (g1, g2) (w1, w2) =
  let* x1 = f1 g1 w1 in
  let+ x2 = f2 g2 w2 in
  x1, x2
;;

let same equal g w = Option.some_if (equal g w) g

let located f (g : _ Located.t) (w : _ Located.t) =
  let+ txt = f g.txt w.txt in
  { Located.txt; loc = w.loc }
;;

let name = located (same String.equal)

let decl_path_desc (g : Decl_path.t) (w : Decl_path.t) =
  match g, w with
  | (Resolved _ | Root _), (Path _ | Root _) -> Some g
  | _ -> same Decl_path.equal g w
;;

let decl_path = located decl_path_desc

let rec type_expr g w = located type_desc g w

and type_desc (g : Type_expr.desc) (w : Type_expr.desc) : Type_expr.desc option =
  match g, w with
  | Var a, Var b -> Option.some_if (String.equal a b) g
  | Con (p, args), Con (p', args') ->
    let* p = decl_path p p' in
    let+ args = list arg args args' in
    Type_expr.Con (p, args)
  | Arrow (a, b), Arrow (a', b') ->
    let* a = type_expr a a' in
    let+ b = type_expr b b' in
    Type_expr.Arrow (a, b)
  | Tuple ts, Tuple ts' ->
    let+ ts = list type_expr ts ts' in
    Type_expr.Tuple ts
  | Code { inner; context }, Code { inner = inner'; context = context' } ->
    let* inner = type_expr inner inner' in
    let+ context = row context context' in
    Type_expr.Code { inner; context }
  | Record r, Record r' ->
    let+ r = row r r' in
    Type_expr.Record r
  | Variant r, Variant r' ->
    let+ r = row r r' in
    Type_expr.Variant r
  | Splice e, Splice e' ->
    let+ e = expr e e' in
    Type_expr.Splice e
  | ( ( Var _
      | Con _
      | Arrow _
      | Tuple _
      | Code _
      | Record _
      | Variant _
      | Splice _ )
    , _ ) -> None

and row g w = located row_desc g w

and row_desc (g : Type_expr.row_desc) (w : Type_expr.row_desc) =
  match g, w with
  | Empty, Empty -> Some g
  | Extend (n, t, r), Extend (n', t', r') ->
    let* n = name n n' in
    let* t = type_expr t t' in
    let+ r = row r r' in
    Type_expr.Extend (n, t, r)
  | Var n, Var n' ->
    let+ n = name n n' in
    Type_expr.Var n
  | (Empty | Extend _ | Var _), _ -> None

and arg (g : Type_expr.arg) (w : Type_expr.arg) =
  match g, w with
  | Ty t, Ty t' ->
    let+ t = type_expr t t' in
    Type_expr.Ty t
  | Row r, Row r' ->
    let+ r = row r r' in
    Type_expr.Row r
  | (Ty _ | Row _), _ -> None

and binding b b' = pair name (option type_expr) b b'
and pattern g w = located pattern_desc g w

and pattern_desc (g : Pattern.desc) (w : Pattern.desc) : Pattern.desc option =
  match g, w with
  | Any, Any -> Some g
  | Var a, Var b -> Option.some_if (String.equal a b) g
  | Lit a, Lit b -> Option.some_if (Lit.equal a b) g
  | Tuple ps, Tuple ps' ->
    let+ ps = list pattern ps ps' in
    Pattern.Tuple ps
  | Construct (p, ps), Construct (p', ps') ->
    let* p = decl_path p p' in
    let+ ps = list pattern ps ps' in
    Pattern.Construct (p, ps)
  | Record fs, Record fs' ->
    let+ fs = list (pair decl_path pattern) fs fs' in
    Pattern.Record fs
  | Poly_variant (n, p), Poly_variant (n', p') ->
    let* n = name n n' in
    let+ p = option pattern p p' in
    Pattern.Poly_variant (n, p)
  | Poly_record { fields; rest }, Poly_record { fields = fields'; rest = rest' }
    ->
    let* fields = list (pair name pattern) fields fields' in
    let+ rest =
      match rest, rest' with
      | Bind n, Bind n' ->
        let+ n = name n n' in
        Pattern.Rest.Bind n
      | (Closed | Open), _ -> same Pattern.Rest.equal rest rest'
      | Bind _, _ -> None
    in
    Pattern.Poly_record { fields; rest }
  | Exception p, Exception p' ->
    let+ p = pattern p p' in
    Pattern.Exception p
  | ( ( Any
      | Var _
      | Lit _
      | Tuple _
      | Construct _
      | Record _
      | Poly_variant _
      | Poly_record _
      | Exception _ )
    , _ ) -> None

and expr g w = located expr_desc g w

and expr_desc (g : Expr.desc) (w : Expr.desc) : Expr.desc option =
  let fields = list (pair decl_path expr) in
  let poly_fields = list (pair name expr) in
  match g, w with
  | Lit a, Lit b -> Option.some_if (Lit.equal a b) g
  | Var (p, t), Var (p', t') ->
    let* p = decl_path_desc p p' in
    let+ t = option type_expr t t' in
    Expr.Var (p, t)
  | App (f, x), App (f', x') ->
    let* f = expr f f' in
    let+ x = expr x x' in
    Expr.App (f, x)
  | Let (n, bs, t, e, in_), Let (n', bs', t', e', in_') ->
    let* n = name n n' in
    let* bs = list binding bs bs' in
    let* t = option type_expr t t' in
    let* e = expr e e' in
    let+ in_ = expr in_ in_' in
    Expr.Let (n, bs, t, e, in_)
  | Fun (bs, e), Fun (bs', e') ->
    let* bs = list binding bs bs' in
    let+ e = expr e e' in
    Expr.Fun (bs, e)
  | Splice { expr = e; subs }, Splice { expr = e'; subs = subs' } ->
    let* e = expr e e' in
    let+ subs = option (list (pair name (option expr))) subs subs' in
    Expr.Splice { expr = e; subs }
  | Quote e, Quote e' ->
    let+ e = expr e e' in
    Expr.Quote e
  | Tuple es, Tuple es' ->
    let+ es = list expr es es' in
    Expr.Tuple es
  | Construct (p, es), Construct (p', es') ->
    let* p = decl_path p p' in
    let+ es = list expr es es' in
    Expr.Construct (p, es)
  | Record fs, Record fs' ->
    let+ fs = fields fs fs' in
    Expr.Record fs
  | Field (e, p), Field (e', p') ->
    let* e = expr e e' in
    let+ p = decl_path p p' in
    Expr.Field (e, p)
  | Record_update (e, fs), Record_update (e', fs') ->
    let* e = expr e e' in
    let+ fs = fields fs fs' in
    Expr.Record_update (e, fs)
  | Type t, Type t' ->
    let+ t = type_expr t t' in
    Expr.Type t
  | Match (e, cases), Match (e', cases') ->
    let* e = expr e e' in
    let+ cases = list (pair pattern expr) cases cases' in
    Expr.Match (e, cases)
  | Poly_record fs, Poly_record fs' ->
    let+ fs = poly_fields fs fs' in
    Expr.Poly_record fs
  | Poly_field (e, n), Poly_field (e', n') ->
    let* e = expr e e' in
    let+ n = name n n' in
    Expr.Poly_field (e, n)
  | Poly_extend (fs, e), Poly_extend (fs', e') ->
    let* fs = poly_fields fs fs' in
    let+ e = expr e e' in
    Expr.Poly_extend (fs, e)
  | Poly_restrict (e, n), Poly_restrict (e', n') ->
    let* e = expr e e' in
    let+ n = name n n' in
    Expr.Poly_restrict (e, n)
  | Poly_update (e, fs), Poly_update (e', fs') ->
    let* e = expr e e' in
    let+ fs = poly_fields fs fs' in
    Expr.Poly_update (e, fs)
  | Poly_variant (n, e), Poly_variant (n', e') ->
    let* n = name n n' in
    let+ e = option expr e e' in
    Expr.Poly_variant (n, e)
  | Expect s, Expect s' ->
    let+ s = located (same String.equal) s s' in
    Expr.Expect s
  | Struct_quote ts, Struct_quote ts' ->
    let+ ts = toplevels ts ts' in
    Expr.Struct_quote ts
  | ( ( Lit _
      | Var _
      | App _
      | Let _
      | Fun _
      | Splice _
      | Quote _
      | Tuple _
      | Construct _
      | Record _
      | Field _
      | Record_update _
      | Type _
      | Match _
      | Poly_record _
      | Poly_field _
      | Poly_extend _
      | Poly_restrict _
      | Poly_update _
      | Poly_variant _
      | Expect _
      | Struct_quote _ )
    , _ ) -> None

and type_decl
      ({ name = n; params; kind } : Type_decl.t)
      ({ name = n'; params = params'; kind = kind' } : Type_decl.t)
  =
  let* n = name n n' in
  let* params = list (located (same Type_decl.Param.equal)) params params' in
  let+ kind =
    match kind, kind' with
    | Variant cs, Variant cs' ->
      let+ cs = list (pair name (list type_expr)) cs cs' in
      Type_decl.Kind.Variant cs
    | Record fs, Record fs' ->
      let+ fs = list (pair name type_expr) fs fs' in
      Type_decl.Kind.Record fs
    | Alias t, Alias t' ->
      let+ t = type_expr t t' in
      Type_decl.Kind.Alias t
    | Abstract, Abstract -> Some kind
    | Splice e, Splice e' ->
      let+ txt = expr e.txt e'.txt in
      Type_decl.Kind.Splice { e' with txt }
    | (Variant _ | Record _ | Alias _ | Abstract | Splice _), _ -> None
  in
  { Type_decl.name = n; params; kind }

and toplevel g w = located toplevel_desc g w

and toplevel_desc (g : Toplevel.desc) (w : Toplevel.desc) =
  match g, w with
  | Let (n, bs, t, e), Let (n', bs', t', e') ->
    let* n = name n n' in
    let* bs = list binding bs bs' in
    let* t = option type_expr t t' in
    let+ e = expr e e' in
    Toplevel.Let (n, bs, t, e)
  | Type d, Type d' ->
    let+ d = type_decl d d' in
    Toplevel.Type d
  | Exception (n, ts), Exception (n', ts') ->
    let* n = name n n' in
    let+ ts = list type_expr ts ts' in
    Toplevel.Exception (n, ts)
  | Module (n, ts), Module (n', ts') ->
    let* n = name n n' in
    let+ ts = toplevels ts ts' in
    Toplevel.Module (n, ts)
  | Module_splice (n, e, x), Module_splice (n', e', x') ->
    let* n = name n n' in
    let* e = expr e e' in
    let+ x = option (located toplevels) x x' in
    Toplevel.Module_splice (n, e, x)
  | Include (e, x), Include (e', x') ->
    let* e = expr e e' in
    let+ x = option (located toplevels) x x' in
    Toplevel.Include (e, x)
  | Expect_test { name = n; body }, Expect_test { name = n'; body = body' } ->
    let* n = located (same String.equal) n n' in
    let+ body = expr body body' in
    Toplevel.Expect_test { name = n; body }
  | ( Bench { name = n; param; body }
    , Bench { name = n'; param = param'; body = body' } ) ->
    let* n = located (same String.equal) n n' in
    let* param =
      option
        (fun ({ name = x; args } : Bench_param.t)
          ({ name = x'; args = args' } : Bench_param.t) ->
           let* x = name x x' in
           let+ args = list expr args args' in
           { Bench_param.name = x; args })
        param
        param'
    in
    let+ body = expr body body' in
    Toplevel.Bench { name = n; param; body }
  | ( ( Let _
      | Type _
      | Exception _
      | Module _
      | Module_splice _
      | Include _
      | Expect_test _
      | Bench _ )
    , _ ) -> None

and toplevels gs ws = list toplevel gs ws

let toplevels ~written generated = toplevels generated written
