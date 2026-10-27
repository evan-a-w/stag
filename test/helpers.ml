open! Core
open Stag
open Syntax

let path name = Path.of_string name
let none = Located.create
let name : string -> Name.t = none
let located_path name = none (path name)

module Ty = struct
  let con ?(args = []) name : Type_expr.t =
    none (Type_expr.Con (located_path name, args))
  ;;

  let int = con "int"
  let bool = con "bool"
  let unit = con "unit"
  let var name : Type_expr.t = none (Type_expr.Var name : Type_expr.desc)
  let arrow a b : Type_expr.t = none (Type_expr.Arrow (a, b))
  let tuple tys : Type_expr.t = none (Type_expr.Tuple tys)

  (** Present [entries], in order, before [tail] (closed if [None]). *)
  let row ?(entries = []) ?tail () =
    let tail : Type_expr.row_desc =
      match tail with
      | None -> Empty
      | Some tail -> Var (name tail)
    in
    List.fold_right entries ~init:(none tail) ~f:(fun (label, ty) rest ->
      none (Type_expr.Extend (name label, ty, rest)))
  ;;

  let code ?entries ?tail inner : Type_expr.t =
    none (Type_expr.Code { inner; context = row ?entries ?tail () })
  ;;

  let record ?tail entries : Type_expr.t =
    none (Type_expr.Record (row ~entries ?tail ()))
  ;;

  let variant ?tail entries : Type_expr.t =
    none (Type_expr.Variant (row ~entries ?tail ()))
  ;;
end

let expr desc : Expr.t = none desc
let int n = expr (Lit (Int n))
let bool b = expr (Lit (Bool b))
let string s = expr (Lit (String s))
let char c = expr (Lit (Char c))
let var ?annot name = expr (Var (path name, annot))
let app f args = List.fold args ~init:f ~f:(fun f x -> expr (App (f, x)))
let bindings = List.map ~f:(fun (arg, annot) -> name arg, annot)
let args = List.map ~f:(fun name -> name, None)
let typed name ty = name, Some ty

(** [Expr.Let], with unlocated names. *)
let expr_let (name', binds, annot, equal, in_) =
  expr (Let (name name', bindings binds, annot, equal, in_))
;;

let let_ ?annot name params equal in_ =
  expr_let (name, args params, annot, equal, in_)
;;

let seq first second = let_ "_" [] first second
let quote e = expr (Quote e)

let splice ?subs e =
  let subs = Option.map subs ~f:(List.map ~f:(fun (sub, e) -> name sub, e)) in
  expr (Splice { expr = e; subs })
;;

let toplevel desc : Toplevel.t = none desc

(** [Toplevel.Let], with unlocated names. *)
let top_let (name', binds, annot, e) =
  toplevel (Let (name name', bindings binds, annot, e))
;;

let top ?annot name params e = top_let (name, args params, annot, e)
let module_ name' toplevels = toplevel (Module (name name', toplevels))

(** The name of a [Let] toplevel. *)
let let_name (toplevel : Toplevel.t) =
  match toplevel.txt with
  | Let (name, _, _, _) -> Some name.txt
  | Type _ | Module _ | Expect_test _ -> None
;;

let prims =
  let int_arg name = name, Some Ty.int in
  [ top_let ("add", [ int_arg "x"; int_arg "y" ], Some Ty.int, var "x")
  ; top_let ("eq", [ int_arg "x"; int_arg "y" ], Some Ty.bool, bool true)
  ; top_let
      ( "cond"
      , [ "c", Some Ty.bool; "x", None; "y", None ]
      , None
      , (* The recursive call makes both branches share a type. *)
        seq (app (var "cond") [ var "c"; var "y"; var "x" ]) (var "x") )
  ; top_let ("id", [ "x", None ], None, var "x")
  ]
;;

let print_signature signature = print_string (Signature.to_string_hum signature)

(* So looping tests fail fast. *)
let limits =
  { Limits.fuel = Some 1_000_000; max_depth = Some 10_000; time_limit = None }
;;

let typechecker () =
  let interpreter = Treewalk.create ~stamps:(Stamp_counter.create ()) () in
  Treewalk.set_limits interpreter limits;
  Typecheck.create ~interpreter ()
;;

let check program =
  let typecheck = typechecker () in
  match Typecheck.typecheck typecheck ~program:(prims @ program) with
  | Ok { signature; _ } ->
    print_signature (List.drop signature (List.length prims))
  | Error error -> print_s [%sexp (error : Typecheck_error.t)]
;;

let unit = expr (Lit Unit)
let tuple exprs = expr (Tuple exprs)
let construct ?(args = []) name = expr (Construct (located_path name, args))

let record fields =
  expr (Record (List.map fields ~f:(fun (name, e) -> located_path name, e)))
;;

let labelled = List.map ~f:(fun (label, x) -> name label, x)
let field e name = expr (Field (e, located_path name))
let match_ e cases = expr (Match (e, cases))
let poly_record fields = expr (Poly_record (labelled fields))
let dot e label = expr (Poly_field (e, name label))
let extend fields e = expr (Poly_extend (labelled fields, e))
let restrict e label = expr (Poly_restrict (e, name label))
let update e fields = expr (Poly_update (e, labelled fields))
let tag ?arg tag = expr (Poly_variant (name tag, arg))

module Pat = struct
  let pattern desc : Pattern.t = none desc
  let any = pattern Any
  let var name = pattern (Var name)
  let int n = pattern (Lit (Int n))
  let string s = pattern (Lit (String s))
  let unit = pattern (Lit Unit)
  let tuple patterns = pattern (Tuple patterns)

  let construct ?(args = []) name =
    pattern (Construct (located_path name, args))
  ;;

  let record fields =
    pattern
      (Record (List.map fields ~f:(fun (name, p) -> located_path name, p)))
  ;;

  let tag ?arg tag = pattern (Poly_variant (name tag, arg))

  (** [Bind] the rest to [bind], if given. *)
  let poly_record ?(rest = Pattern.Rest.Closed) ?bind fields =
    let rest : Pattern.Rest.t =
      match bind with
      | Some bind -> Bind (name bind)
      | None -> rest
    in
    pattern (Poly_record { fields = labelled fields; rest })
  ;;
end

module Decl = struct
  let ty name = none (Type_decl.Param.Ty name)
  let row name = none (Type_decl.Param.Row name)

  let type_ ?(params = []) name' kind =
    toplevel (Type { Type_decl.name = name name'; params; kind })
  ;;

  let variant ?params name constructors =
    type_ ?params name (Variant (labelled constructors))
  ;;

  let record ?params name fields = type_ ?params name (Record (labelled fields))
  let alias ?params name ty = type_ ?params name (Alias ty)
  let abstract ?params name = type_ ?params name Abstract

  (** ['a list = Nil | Cons of 'a * 'a list] *)
  let list =
    variant
      "list"
      ~params:[ ty "a" ]
      [ "Nil", []
      ; "Cons", [ Ty.var "a"; Ty.con "list" ~args:[ Ty (Ty.var "a") ] ]
      ]
  ;;
end
