open! Import

type t =
  | Type_expr
  | Type_ref_name
  | Type_ref_path
  | Constructor_ref_name
  | Field_ref_name
  | Type_decl
  | Value_ref_name
  | Erase
  | Raise
  | Ptr_make
  | Assume_global
[@@deriving sexp_of, equal, enumerate]

let name = function
  | Type_expr -> "type_expr"
  | Type_ref_name -> "type_ref_name"
  | Type_ref_path -> "type_ref_path"
  | Constructor_ref_name -> "constructor_ref_name"
  | Field_ref_name -> "field_ref_name"
  | Type_decl -> "type_decl"
  | Value_ref_name -> "value_ref_name"
  | Erase -> "erase"
  | Raise -> "raise"
  | Ptr_make -> "ptr_make"
  | Assume_global -> "assume_global"
;;

let public_name = function
  | Type_expr -> "Stag.Type.expr"
  | Type_ref_name -> "Stag.Type_ref.name"
  | Type_ref_path -> "Stag.Type_ref.path"
  | Constructor_ref_name -> "Stag.Constructor_ref.name"
  | Field_ref_name -> "Stag.Field_ref.name"
  | Type_decl -> "Stag.Type.decl"
  | Value_ref_name -> "Stag.Value_ref.name"
  | Erase -> "Stag.Code.erase"
  | Raise -> "raise"
  | Ptr_make -> "Ptr.make"
  | Assume_global -> "Unsafe.assume_global"
;;

let prelude ?(args = []) name = Type.TCon (Prelude_path.canonical name, args)

let ty t ~var ~row =
  let name ref_ = Type.arrow (prelude ref_) Type.string in
  match t with
  | Type_expr ->
    Type.arrow
      (prelude "Stag.Type.t" ~args:[ ATy (var ()) ])
      (prelude "Stag.Type_expr.t")
  | Type_ref_name -> name "Stag.Type_ref.t"
  | Type_ref_path ->
    Type.arrow (prelude "Stag.Type_ref.t") (prelude "Stag.Type_ref.Path.t")
  | Constructor_ref_name -> name "Stag.Constructor_ref.t"
  | Field_ref_name -> name "Stag.Field_ref.t"
  | Type_decl ->
    Type.arrow
      (prelude "Stag.Type_ref.t")
      (prelude "option" ~args:[ ATy (prelude "Stag.Type.Decl.t") ])
  | Value_ref_name -> name "Stag.Value_ref.t"
  | Erase ->
    Type.arrow
      (TCode { inner = var (); context = row () })
      (prelude "Stag.Expr.t")
  | Raise -> Type.arrow Type.exn (var ())
  | Ptr_make ->
    let pointee = var () in
    Type.arrow pointee (Type.ptr pointee)
  | Assume_global ->
    let ty = var () in
    TArrow
      ( ty
      , ty
      , { arg = Fixed { Arrow_modes.default_const with locality = Local }
        ; res = Fixed Arrow_modes.default_const
        ; curry = Arrow_modes.default_curry
        } )
;;

let find path =
  match (path : Syntax.Path.t :> string list) with
  | [ "Intrinsic"; member ] ->
    List.find all ~f:(fun t -> String.equal (name t) member)
  | _ -> None
;;
