open! Import
open Syntax

module Kind = struct
  type t =
    | Item
    | Type_expr
    | Type_kind
    | Expr
  [@@deriving sexp_of, compare, equal]
end

type t =
  { kind : Kind.t
  ; expr : Expr.t
  ; loc : Loc.t
  ; has_subs : bool
  }
[@@deriving sexp_of]

let map ?type_expr:replace_type_expr toplevels ~f =
  let splice ~depth kind ~loc ~has_subs (e : Expr.t) ~inner =
    if depth = 0
    then f { kind; expr = e; loc; has_subs }
    else inner (depth - 1) e
  in
  let rec type_expr depth (t : Type_expr.t) : Type_expr.t =
    let type_expr = type_expr depth in
    let txt : Type_expr.desc =
      match t.txt with
      | Var _ -> t.txt
      | Con (p, args) -> Con (p, List.map args ~f:(arg depth))
      | Arrow (a, b) ->
        let a = type_expr a in
        Arrow (a, type_expr b)
      | Tuple ts -> Tuple (List.map ts ~f:type_expr)
      | Code { inner; context } ->
        let inner = type_expr inner in
        Code { inner; context = row depth context }
      | Record r -> Record (row depth r)
      | Variant r -> Variant (row depth r)
      | Mut t -> Mut (type_expr t)
      | Immut t -> Immut (type_expr t)
      | Moded (t, m) -> Moded (type_expr t, m)
      | Global t -> Global (type_expr t)
      | Splice e ->
        (match replace_type_expr with
         | Some replace when depth = 0 ->
           (replace
              { kind = Type_expr; expr = e; loc = t.loc; has_subs = false }
            : Type_expr.t)
             .txt
         | Some _ | None ->
           Splice
             (splice ~depth Type_expr ~loc:t.loc ~has_subs:false e ~inner:expr))
    in
    { t with txt }
  and row depth (r : Type_expr.row) : Type_expr.row =
    match r.txt with
    | Empty | Var _ -> r
    | Extend (n, t, rest) ->
      let t = type_expr depth t in
      { r with txt = Extend (n, t, row depth rest) }
  and arg depth : Type_expr.arg -> Type_expr.arg = function
    | Ty t -> Ty (type_expr depth t)
    | Row r -> Row (row depth r)
  and binding depth ((n, a) : Binding.t) = n, Annot.map a ~f:(type_expr depth)
  and expr depth (e : Expr.t) : Expr.t =
    let sub = expr depth in
    let fields = List.map ~f:(fun (n, e) -> n, sub e) in
    let annot = Annot.map ~f:(type_expr depth) in
    let txt : Expr.desc =
      match e.txt with
      | Lit _ | Expect _ | Error -> e.txt
      | Deref e -> Deref (sub e)
      | Var (p, a) -> Var (p, annot a)
      | App (a, b) ->
        let a = sub a in
        App (a, sub b)
      | Let (n, bs, a, body, in_) ->
        let bs = List.map bs ~f:(binding depth) in
        let a = annot a in
        let body = sub body in
        Let (n, bs, a, body, sub in_)
      | Let_mut (n, a, body, in_) ->
        let a = annot a in
        let body = sub body in
        Let_mut (n, a, body, sub in_)
      | Assign (p, value) -> Assign (p, sub value)
      | Store (target, value) -> Store (sub target, sub value)
      | Fun (bs, body) ->
        let bs = List.map bs ~f:(binding depth) in
        Fun (bs, sub body)
      | Splice { expr = spliced; subs } ->
        let spliced =
          splice
            ~depth
            Expr
            ~loc:e.loc
            ~has_subs:(Option.is_some subs)
            spliced
            ~inner:expr
        in
        Splice
          { expr = spliced
          ; subs =
              Option.map
                subs
                ~f:(List.map ~f:(fun (n, e) -> n, Option.map e ~f:sub))
          }
      | Quote q -> Quote (expr (depth + 1) q)
      | Struct_quote ts -> Struct_quote (List.map ts ~f:(toplevel (depth + 1)))
      | Tuple es -> Tuple (List.map es ~f:sub)
      | Construct (p, es) -> Construct (p, List.map es ~f:sub)
      | Record fs -> Record (fields fs)
      | Field (e, p) -> Field (sub e, p)
      | Record_update (e, fs) ->
        let e = sub e in
        Record_update (e, fields fs)
      | Type t -> Type (type_expr depth t)
      | Match (e, arms) ->
        let e = sub e in
        Match (e, List.map arms ~f:(fun (p, e) -> p, sub e))
      | Poly_record fs -> Poly_record (fields fs)
      | Poly_field (e, n) -> Poly_field (sub e, n)
      | Poly_extend (fs, e) ->
        let fs = fields fs in
        Poly_extend (fs, sub e)
      | Poly_restrict (e, n) -> Poly_restrict (sub e, n)
      | Poly_update (e, fs) ->
        let e = sub e in
        Poly_update (e, fields fs)
      | Poly_variant (n, e) -> Poly_variant (n, Option.map e ~f:sub)
      | Local e -> Local (sub e)
      | Exclave e -> Exclave (sub e)
      | Nontail e -> Nontail (sub e)
    in
    { e with txt }
  and toplevel depth (t : Toplevel.t) : Toplevel.t =
    let sub = expr depth in
    let type_expr = type_expr depth in
    let txt : Toplevel.desc =
      match t.txt with
      | Let (n, bs, a, body) ->
        let bs = List.map bs ~f:(binding depth) in
        let a = Annot.map a ~f:type_expr in
        Let (n, bs, a, sub body)
      | Let_mut (n, a, body) ->
        let a = Annot.map a ~f:type_expr in
        Let_mut (n, a, sub body)
      | Type { name; params; kind } ->
        let rec decl_kind : Type_decl.Kind.t -> Type_decl.Kind.t = function
          | Malformed kind -> Malformed (decl_kind kind)
          | Variant cs ->
            Variant
              (List.map cs ~f:(fun (n, ts) -> n, List.map ts ~f:type_expr))
          | Record fs -> Record (List.map fs ~f:(fun (n, t) -> n, type_expr t))
          | Alias t -> Alias (type_expr t)
          | Abstract -> Abstract
          | Splice e ->
            Splice
              { e with
                txt =
                  splice
                    ~depth
                    Type_kind
                    ~loc:e.loc
                    ~has_subs:false
                    e.txt
                    ~inner:expr
              }
        in
        Type { name; params; kind = decl_kind kind }
      | Exception (n, ts) -> Exception (n, List.map ts ~f:type_expr)
      | Malformed_exception (n, ts) ->
        Malformed_exception (n, List.map ts ~f:type_expr)
      | Module (n, ts) -> Module (n, List.map ts ~f:(toplevel depth))
      | Module_splice (n, e, expansion) ->
        let e = splice ~depth Item ~loc:t.loc ~has_subs:false e ~inner:expr in
        Module_splice (n, e, expansion)
      | Include (e, expansion) ->
        let e = splice ~depth Item ~loc:t.loc ~has_subs:false e ~inner:expr in
        Include (e, expansion)
      | Expect_test { name; body } -> Expect_test { name; body = sub body }
      | Bench { name; param; body } ->
        let param =
          Option.map param ~f:(fun (p : Bench_param.t) ->
            { p with args = List.map p.args ~f:sub })
        in
        Bench { name; param; body = sub body }
    in
    { t with txt }
  in
  List.map toplevels ~f:(toplevel 0)
;;

let iter toplevels ~f =
  ignore
    (map toplevels ~f:(fun t ->
       f t;
       t.expr)
     : Toplevel.t list)
;;

(* No program can name it: canonical names never start with [%]. *)
let placeholder_prefix = "%antiquote."

let placeholder i =
  Located.create
    ~loc:Loc.none
    (Expr.Var (Resolved (placeholder_prefix ^ Int.to_string i), Annot.none))
;;

let index (e : Expr.t) =
  match e.txt with
  | Var (Resolved name, annot) when Annot.is_none annot ->
    String.chop_prefix name ~prefix:placeholder_prefix
    |> Option.map ~f:Int.of_string
  | _ -> None
;;
