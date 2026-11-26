open! Import
open Syntax

let none = Located.create
let name : string -> Name.t = none
let located_path name = none (Decl_path.Path (Path.of_string name))
let labelled = List.map ~f:(fun (label, x) -> name label, x)

module Ty = struct
  let con ?(args = []) name : Type_expr.t =
    none (Type_expr.Con (located_path name, args))
  ;;

  let int = con "int"
  let var name : Type_expr.t = none (Type_expr.Var name : Type_expr.desc)
end

let expr desc : Expr.t = none desc
let int n = expr (Lit (Int n))
let string s = expr (Lit (String s))
let unit = expr (Lit Unit)
let var name = expr (Var (Path (Path.of_string name), Annot.none))
let app f args = List.fold args ~init:f ~f:(fun f x -> expr (App (f, x)))
let bindings = List.map ~f:(fun (arg, annot) -> name arg, annot)
let args = List.map ~f:(fun name -> name, Annot.none)
let typed name ty = name, { Annot.ty = Some ty; mode = None }

let expr_let (name', binds, annot, equal, in_) =
  expr
    (Let (name name', bindings binds, { ty = annot; mode = None }, equal, in_))
;;

let let_ name params equal in_ = expr_let (name, args params, None, equal, in_)
let seq first second = let_ "_" [] first second
let quote e = expr (Quote e)

let splice ?subs e =
  let subs = Option.map subs ~f:(List.map ~f:(fun (sub, e) -> name sub, e)) in
  expr (Splice { expr = e; subs })
;;

let tuple exprs = expr (Tuple exprs)
let construct ?(args = []) name = expr (Construct (located_path name, args))

let record fields =
  expr (Record (List.map fields ~f:(fun (name, e) -> located_path name, e)))
;;

let match_ e cases = expr (Match (e, cases))
let tag ?arg tag = expr (Poly_variant (name tag, arg))
let toplevel desc : Toplevel.t = none desc

let top_let (name', binds, annot, e) =
  toplevel (Let (name name', bindings binds, { ty = annot; mode = None }, e))
;;

let top ?annot name params e = top_let (name, args params, annot, e)
let module_ name' toplevels = toplevel (Module (name name', toplevels))

module Pat = struct
  let pattern desc : Pattern.t = none desc
  let var name = pattern (Var name)
  let deref p = pattern (Deref p)

  let construct ?(args = []) name =
    pattern (Construct (located_path name, args))
  ;;

  let record fields =
    pattern
      (Record (List.map fields ~f:(fun (name, p) -> located_path name, p)))
  ;;

  let tag ?arg tag = pattern (Poly_variant (name tag, arg))
end

module Decl = struct
  let ty name = none (Type_decl.Param.Ty name)

  let type_ ?(params = []) name' kind =
    toplevel
      (Type { Type_decl.name = name name'; params; kind; nonrec_ = false })
  ;;

  let variant ?params name constructors =
    type_ ?params name (Variant (labelled constructors))
  ;;

  let record name fields = type_ name (Record (labelled fields))
  let alias name ty = type_ name (Alias ty)
  let abstract name = type_ name Abstract
end
