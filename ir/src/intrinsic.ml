open! Import

type t =
  | Type_expr
  | Type_decl
  | Ref_name
[@@deriving sexp_of, equal, enumerate]

let name = function
  | Type_expr -> "type_expr"
  | Type_decl -> "type_decl"
  | Ref_name -> "ref_name"
;;

let public_name = function
  | Type_expr -> "Stag.Type.expr"
  | Type_decl -> "Stag.Type.decl"
  | Ref_name -> "Stag.Ref.name"
;;

let prelude name = Type.TCon (Prelude_path.canonical name, [])

let ty t ~var ~record =
  let ref_ = prelude "Stag.Ref.t" in
  match t with
  | Type_expr ->
    Type.TArrow
      ( TCon (Prelude_path.canonical "Stag.Type.t", [ ATy (var ()) ])
      , prelude "Stag.Type_expr.t" )
  | Type_decl ->
    TArrow
      ( ref_
      , TCon
          ( Prelude_path.canonical "option"
          , [ ATy
                (record
                   (Type.RExtend
                      ( "params"
                      , Present
                      , TCon
                          ( Prelude_path.canonical "list"
                          , [ ATy (prelude "Stag.Type_decl.Param.t") ] )
                      , RExtend
                          ("kind", Present, prelude "Stag.Type.Kind.t", REmpty)
                      )))
            ] ) )
  | Ref_name -> TArrow (ref_, Type.string)
;;

let find path =
  match (path : Syntax.Path.t :> string list) with
  | [ "Intrinsic"; member ] ->
    List.find all ~f:(fun t -> String.equal (name t) member)
  | _ -> None
;;
