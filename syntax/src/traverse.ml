open! Import
open Syntax

module Splice = struct
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
    ; loc : Loc.t
    ; has_subs : bool
    }
  [@@deriving sexp_of]
end

module Namespace = struct
  type t =
    | Type
    | Constructor
    | Field
    | Value
  [@@deriving sexp_of, compare, equal]
end

module Members = struct
  type t =
    | Module of string
    | Struct_quote
    | Expansion
  [@@deriving sexp_of, compare, equal]
end

module Child = struct
  type t =
    { depth : int
    ; binds : string list
    ; splice : Splice.t option
    ; members : Members.t option
    }
  [@@deriving sexp_of]

  let same = { depth = 0; binds = []; splice = None; members = None }
  let binding binds = { same with binds }

  let splice kind ~loc ~has_subs =
    { same with depth = -1; splice = Some { kind; loc; has_subs } }
  ;;
end

let rec pattern_binders (p : Pattern.t) =
  match p.txt with
  | Any | Lit _ -> []
  | Var name -> [ name ]
  | Tuple ps | Construct (_, ps) -> List.concat_map ps ~f:pattern_binders
  | Deref p | Exception p -> pattern_binders p
  | Record fs -> List.concat_map fs ~f:(fun (_, p) -> pattern_binders p)
  | Poly_variant (_, p) -> Option.value_map p ~default:[] ~f:pattern_binders
  | Poly_record { fields; rest } ->
    List.concat_map fields ~f:(fun (_, p) -> pattern_binders p)
    @
      (match rest with
      | Bind name -> [ name.txt ]
      | Closed | Open -> [])
;;

module Visitor_type = struct
  type 'ctx t =
    { enter : 'ctx -> Child.t -> 'ctx
    ; into_expansions : bool
    ; expr : 'ctx t -> 'ctx -> Child.t -> Expr.t -> Expr.t
    ; pattern : 'ctx t -> 'ctx -> Pattern.t -> Pattern.t
    ; type_expr : 'ctx t -> 'ctx -> Type_expr.t -> Type_expr.t
    ; toplevel : 'ctx t -> 'ctx -> Toplevel.t -> Toplevel.t
    ; decl_path :
        'ctx -> Namespace.t -> Decl_path.t Located.t -> Decl_path.t Located.t
    }
end

(* Children are mapped in source order: where a constructor has several, they
   are mapped by [let]s first, as OCaml evaluates its arguments in no set order. *)
let map = List.map
let map_snd list ~f = map list ~f:(fun (a, b) -> a, f b)

let rec type_expr (v : _ Visitor_type.t) ctx (t : Type_expr.t) : Type_expr.t =
  let sub = v.type_expr v (v.enter ctx Child.same) in
  let txt : Type_expr.desc =
    match t.txt with
    | Var _ -> t.txt
    | Con (p, args) ->
      let p = v.decl_path ctx Type p in
      Con (p, map args ~f:(arg v ctx))
    | Arrow (a, b) ->
      let a = sub a in
      Arrow (a, sub b)
    | Tuple ts -> Tuple (map ts ~f:sub)
    | Code { inner; context } ->
      let inner = sub inner in
      Code { inner; context = row v ctx context }
    | Record r -> Record (row v ctx r)
    | Variant r -> Variant (row v ctx r)
    | Mut t -> Mut (sub t)
    | Immut t -> Immut (sub t)
    | Moded (t, m) -> Moded (sub t, m)
    | Modality (t, m) -> Modality (sub t, m)
    | Constrained (t, cs) -> Constrained (sub t, cs)
    | Splice e ->
      let child = Child.splice Type_expr ~loc:t.loc ~has_subs:false in
      Splice (v.expr v (v.enter ctx child) child e)
  in
  { t with txt }

and row v ctx (r : Type_expr.row) : Type_expr.row =
  match r.txt with
  | Empty | Var _ -> r
  | Extend (n, t, rest) ->
    let t = v.type_expr v (v.enter ctx Child.same) t in
    { r with txt = Extend (n, t, row v ctx rest) }

and arg v ctx : Type_expr.arg -> Type_expr.arg = function
  | Ty t -> Ty (v.type_expr v (v.enter ctx Child.same) t)
  | Row r -> Row (row v ctx r)
  | Mode _ as arg -> arg
;;

let annot (v : _ Visitor_type.t) ctx annot =
  Annot.map annot ~f:(v.type_expr v (v.enter ctx Child.same))
;;

let bindings v ctx bs = map_snd bs ~f:(annot v ctx)
let binding_names bs = List.map bs ~f:(fun ((n : Name.t), _) -> n.txt)

let pattern (v : _ Visitor_type.t) ctx (p : Pattern.t) : Pattern.t =
  let sub = v.pattern v (v.enter ctx Child.same) in
  let txt : Pattern.desc =
    match p.txt with
    | Any | Var _ | Lit _ -> p.txt
    | Tuple ps -> Tuple (map ps ~f:sub)
    | Deref p -> Deref (sub p)
    | Construct (c, ps) ->
      let c = v.decl_path ctx Constructor c in
      Construct (c, map ps ~f:sub)
    | Record fs ->
      Record
        (map fs ~f:(fun (f, p) ->
           let f = v.decl_path ctx Field f in
           f, sub p))
    | Poly_variant (n, p) -> Poly_variant (n, Option.map p ~f:sub)
    | Poly_record { fields; rest } ->
      Poly_record { fields = map_snd fields ~f:sub; rest }
    | Exception p -> Exception (sub p)
  in
  { p with txt }
;;

let rec expr (v : _ Visitor_type.t) ctx (e : Expr.t) : Expr.t =
  let child child e = v.expr v (v.enter ctx child) child e in
  let sub = child Child.same in
  let annot = annot v ctx in
  let decl_path = v.decl_path ctx in
  let fields =
    map ~f:(fun (p, e) ->
      let p = decl_path Field p in
      p, sub e)
  in
  let poly_fields = map_snd ~f:sub in
  let txt : Expr.desc =
    match e.txt with
    | Lit _ | Expect _ | Error -> e.txt
    | Var (p, a) ->
      let p = decl_path Value { txt = p; loc = e.loc } in
      Var (p.txt, annot a)
    | App (a, b) ->
      let a = sub a in
      App (a, sub b)
    | Let (n, bs, a, value, body) ->
      let bs' = bindings v ctx bs in
      let a = annot a in
      let value = child (Child.binding (n.txt :: binding_names bs)) value in
      Let (n, bs', a, value, child (Child.binding [ n.txt ]) body)
    | Let_mut (n, a, value, body) ->
      let a = annot a in
      let value = sub value in
      Let_mut (n, a, value, child (Child.binding [ n.txt ]) body)
    | Seq (first, second) ->
      let first = sub first in
      Seq (first, sub second)
    | Assign (p, value) ->
      let p = decl_path Value p in
      Assign (p, sub value)
    | Store (target, value) ->
      let target = sub target in
      Store (target, sub value)
    | Fun (bs, body) ->
      let bs' = bindings v ctx bs in
      Fun (bs', child (Child.binding (binding_names bs)) body)
    | Splice { expr = spliced; subs } ->
      let spliced =
        child
          (Child.splice Expr ~loc:e.loc ~has_subs:(Option.is_some subs))
          spliced
      in
      Splice
        { expr = spliced
        ; subs = Option.map subs ~f:(map_snd ~f:(fun e -> Option.map e ~f:sub))
        }
    | Quote q -> Quote (child { Child.same with depth = 1 } q)
    | Struct_quote ts ->
      Struct_quote
        (members
           v
           ctx
           ts
           { Child.same with depth = 1; members = Some Struct_quote })
    | Tuple es -> Tuple (map es ~f:sub)
    | Deref e -> Deref (sub e)
    | Construct (c, es) ->
      let c = decl_path Constructor c in
      Construct (c, map es ~f:sub)
    | Record fs -> Record (fields fs)
    | Field (e, p) ->
      let e = sub e in
      Field (e, decl_path Field p)
    | Record_update (e, fs) ->
      let e = sub e in
      Record_update (e, fields fs)
    | Type t -> Type (v.type_expr v (v.enter ctx Child.same) t)
    | Match (e, cases) ->
      let e = sub e in
      Match
        ( e
        , map cases ~f:(fun (p, body) ->
            let p = v.pattern v (v.enter ctx Child.same) p in
            p, child (Child.binding (pattern_binders p)) body) )
    | Poly_record fs -> Poly_record (poly_fields fs)
    | Poly_field (e, n) -> Poly_field (sub e, n)
    | Poly_extend (fs, e) ->
      let fs = poly_fields fs in
      Poly_extend (fs, sub e)
    | Poly_restrict (e, n) -> Poly_restrict (sub e, n)
    | Poly_update (e, fs) ->
      let e = sub e in
      Poly_update (e, poly_fields fs)
    | Poly_variant (n, e) -> Poly_variant (n, Option.map e ~f:sub)
    | Stack e -> Stack (sub e)
    | Exclave e -> Exclave (sub e)
    | Region e -> Region (sub e)
    | Nontail e -> Nontail (sub e)
    | Annotated (e, a) ->
      let e = sub e in
      Annotated (e, annot a)
    | Forget (names, e) ->
      let hidden = List.map names ~f:(fun (n : Name.t) -> n.txt) in
      Forget (names, child (Child.binding hidden) e)
  in
  { e with txt }

and members v ctx ts (child : Child.t) =
  map ts ~f:(v.toplevel v (v.enter ctx child))

and toplevel (v : _ Visitor_type.t) ctx (t : Toplevel.t) : Toplevel.t =
  let child child e = v.expr v (v.enter ctx child) child e in
  let sub = child Child.same in
  let type_expr = v.type_expr v (v.enter ctx Child.same) in
  let annot = annot v ctx in
  let item e = child (Child.splice Item ~loc:t.loc ~has_subs:false) e in
  let expansion =
    Option.map ~f:(fun (expansion : _ Located.t) ->
      if v.into_expansions
      then
        { expansion with
          txt =
            members
              v
              ctx
              expansion.txt
              { Child.same with members = Some Expansion }
        }
      else expansion)
  in
  let txt : Toplevel.desc =
    match t.txt with
    | Let (n, bs, a, body) ->
      let bs' = bindings v ctx bs in
      let a = annot a in
      Let (n, bs', a, child (Child.binding (binding_names bs)) body)
    | Let_mut (n, a, body) ->
      let a = annot a in
      Let_mut (n, a, sub body)
    | Type { name; params; kind } ->
      let rec decl_kind : Type_decl.Kind.t -> Type_decl.Kind.t = function
        | Malformed kind -> Malformed (decl_kind kind)
        | Variant cs -> Variant (map_snd cs ~f:(map ~f:type_expr))
        | Record fs -> Record (map_snd fs ~f:type_expr)
        | Alias t -> Alias (type_expr t)
        | Abstract -> Abstract
        | Splice e ->
          Splice
            { e with
              txt =
                child (Child.splice Type_kind ~loc:e.loc ~has_subs:false) e.txt
            }
      in
      Type { name; params; kind = decl_kind kind }
    | Exception (n, ts) -> Exception (n, map ts ~f:type_expr)
    | External { name; ty; symbol } ->
      External { name; ty = type_expr ty; symbol }
    | Malformed_exception (n, ts) -> Malformed_exception (n, map ts ~f:type_expr)
    | Module (n, ts) ->
      Module
        (n, members v ctx ts { Child.same with members = Some (Module n.txt) })
    | Module_splice (n, e, x) ->
      let e = item e in
      Module_splice (n, e, expansion x)
    | Include (e, x) ->
      let e = item e in
      Include (e, expansion x)
    | Expect_test { name; body } -> Expect_test { name; body = sub body }
    | Bench { name; param; body } ->
      let param =
        Option.map param ~f:(fun (p : Bench_param.t) ->
          { p with args = map p.args ~f:sub })
      in
      let binds =
        Option.value_map param ~default:[] ~f:(fun p -> [ p.name.txt ])
      in
      Bench { name; param; body = child (Child.binding binds) body }
  in
  { t with txt }
;;

module Visitor = struct
  include Visitor_type

  let default ~enter ~into_expansions : _ t =
    { enter
    ; into_expansions
    ; expr = (fun v ctx _ e -> expr v ctx e)
    ; pattern
    ; type_expr
    ; toplevel
    ; decl_path = (fun _ _ p -> p)
    }
  ;;
end
