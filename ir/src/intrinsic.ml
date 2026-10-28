open! Import

type t =
  | Type_expr
  | Type_ref_name
  | Constructor_ref_name
  | Field_ref_name
  | Type_decl
[@@deriving sexp_of, equal, enumerate]

let name = function
  | Type_expr -> "type_expr"
  | Type_ref_name -> "type_ref_name"
  | Constructor_ref_name -> "constructor_ref_name"
  | Field_ref_name -> "field_ref_name"
  | Type_decl -> "type_decl"
;;

let public_name = function
  | Type_expr -> "Stag.Type.expr"
  | Type_ref_name -> "Stag.Type_ref.name"
  | Constructor_ref_name -> "Stag.Constructor_ref.name"
  | Field_ref_name -> "Stag.Field_ref.name"
  | Type_decl -> "Stag.Type.decl"
;;

let prelude ?(args = []) name = Type.TCon (Prelude_path.canonical name, args)

let ty t ~var =
  let name ref_ = Type.TArrow (prelude ref_, Type.string) in
  match t with
  | Type_expr ->
    Type.TArrow
      (prelude "Stag.Type.t" ~args:[ ATy (var ()) ], prelude "Stag.Type_expr.t")
  | Type_ref_name -> name "Stag.Type_ref.t"
  | Constructor_ref_name -> name "Stag.Constructor_ref.t"
  | Field_ref_name -> name "Stag.Field_ref.t"
  | Type_decl ->
    TArrow
      ( prelude "Stag.Type_ref.t"
      , prelude "option" ~args:[ ATy (prelude "Stag.Type.Decl.t") ] )
;;

let find path =
  match (path : Syntax.Path.t :> string list) with
  | [ "Intrinsic"; member ] ->
    List.find all ~f:(fun t -> String.equal (name t) member)
  | _ -> None
;;
