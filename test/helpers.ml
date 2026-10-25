open! Core
open Stag
open Syntax

let path name = Path.of_string name

module Ty = struct
  let con ?(args = []) name = Type_expr.Con (path name, args)
  let int = con "int"
  let bool = con "bool"
  let unit = con "unit"
  let var name : Type_expr.t = Var name
  let arrow a b = Type_expr.Arrow (a, b)
  let tuple tys = Type_expr.Tuple tys

  let code ?(entries = []) ?tail inner =
    let tail : Type_expr.context =
      match tail with
      | None -> Empty
      | Some name -> Var name
    in
    Type_expr.Code
      { inner
      ; context =
          List.fold_right entries ~init:tail ~f:(fun (name, ty) rest ->
            Type_expr.Extend (name, ty, rest))
      }
  ;;
end

let int n = Expr.Lit (Int (Int.to_string n))
let bool b = Expr.Lit (Bool b)
let var ?annot name = Expr.Var (path name, annot)
let app f args = List.fold args ~init:f ~f:(fun f x -> Expr.App (f, x))
let args = List.map ~f:(fun name -> name, None)

let let_ ?annot name params equal in_ =
  Expr.Let (name, args params, annot, equal, in_)
;;

let seq first second = let_ "_" [] first second
let quote expr = Expr.Quote expr
let splice ?subs expr = Expr.Splice { expr; subs }
let top ?annot name params expr = Toplevel.Let (name, args params, annot, expr)
let module_ name toplevels = Toplevel.Module (name, toplevels)

let prims =
  let int_arg name = name, Some Ty.int in
  [ Toplevel.Let ("add", [ int_arg "x"; int_arg "y" ], Some Ty.int, var "x")
  ; Let ("eq", [ int_arg "x"; int_arg "y" ], Some Ty.bool, bool true)
  ; Let
      ( "cond"
      , [ "c", Some Ty.bool; "x", None; "y", None ]
      , None
      , (* The recursive call makes both branches share a type. *)
        seq (app (var "cond") [ var "c"; var "y"; var "x" ]) (var "x") )
  ; Let ("id", [ "x", None ], None, var "x")
  ]
;;

let print_types typecheck types =
  List.iter types ~f:(fun (name, ty) ->
    printf "%s : %s\n" name (Typecheck.to_string_hum typecheck ty))
;;

let check program =
  let typecheck = Typecheck.create () in
  match Typecheck.typecheck typecheck ~program:(prims @ program) with
  | Ok types -> print_types typecheck (List.drop types (List.length prims))
  | Error error -> print_s [%sexp (error : Typecheck.Typecheck_error.t)]
;;

let unit = Expr.Lit Unit
let tuple exprs = Expr.Tuple exprs
let construct ?(args = []) name = Expr.Construct (path name, args)

let record fields =
  Expr.Record (List.map fields ~f:(fun (name, e) -> path name, e))
;;

let field expr name = Expr.Field (expr, path name)
let match_ expr cases = Expr.Match (expr, cases)

module Pat = struct
  let any = Pattern.Any
  let var name = Pattern.Var name
  let int n = Pattern.Lit (Int (Int.to_string n))
  let tuple patterns = Pattern.Tuple patterns
  let construct ?(args = []) name = Pattern.Construct (path name, args)

  let record fields =
    Pattern.Record (List.map fields ~f:(fun (name, p) -> path name, p))
  ;;
end

module Decl = struct
  let ty name = Type_decl.Param.Ty name
  let context name = Type_decl.Param.Context name

  let type_ ?(params = []) name kind =
    Toplevel.Type { Type_decl.name; params; kind }
  ;;

  let variant ?params name constructors =
    type_ ?params name (Variant constructors)
  ;;

  let record ?params name fields = type_ ?params name (Record fields)
  let alias ?params name ty = type_ ?params name (Alias ty)

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
