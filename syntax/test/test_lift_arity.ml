open! Import
open Syntax
module Lift = Stag_syntax.Lift

(* The constructors of the prelude's [Stag] whose one argument is a [ptr], with
   how many components it points to, as [Lift.indirect_arity] has them. *)
let indirect_constructors program =
  let rec toplevels prefix (items : Toplevel.t list) =
    List.concat_map items ~f:(fun item ->
      match item.txt with
      | Module (name, items) -> toplevels (prefix @ [ name.txt ]) items
      | Type { name = type_name; kind = Variant constructors; _ } ->
        List.filter_map constructors ~f:(fun (name, args) ->
          match args with
          | [ { txt = Con ({ txt = Path path; _ }, [ pointee ]); _ } ]
            when [%equal: string list] (path :> string list) [ "ptr" ] ->
            let arity =
              (* [ptr[{`x : t}]] parses as a row argument. *)
              match pointee with
              | Ty { txt = Tuple components; _ } -> List.length components
              | Ty _ | Row _ | Mode _ -> 1
            in
            Some (String.concat ~sep:"." (prefix @ [ name.txt ]), arity)
          | _ -> None)
      | _ -> [])
  in
  toplevels [] program
  |> List.filter ~f:(fun (name, _) -> String.is_prefix name ~prefix:"Stag.")
;;

let%expect_test "Lift.indirect_arity matches the prelude's declarations" =
  let path = "../../prelude/prelude.stag" in
  let program =
    match Parser.program ~file:path (In_channel.read_all path) with
    | Ok program -> program
    | Error _ -> failwith "the prelude doesn't parse"
  in
  let declared = indirect_constructors program in
  let sort = List.sort ~compare:[%compare: string * int] in
  print_s [%sexp (declared : (string * int) list)];
  print_s
    [%message
      "equal"
        ~_:
          ([%equal: (string * int) list]
             (sort declared)
             (sort Lift.indirect_constructors)
           : bool)];
  [%expect
    {|
    ((Stag.Mode_expr.Term.Meet 2) (Stag.Mode_expr.Term.Release 1)
     (Stag.Mode_expr.Term.Static_if_const 1)
     (Stag.Mode_expr.Term.Const_if_static 1)
     (Stag.Mode_expr.Constraint.Submode 2) (Stag.Type_expr.Arrow 2)
     (Stag.Type_expr.Code 1) (Stag.Type_expr.Mut 1) (Stag.Type_expr.Immut 1)
     (Stag.Type_expr.Splice 1) (Stag.Type_expr.Moded 2)
     (Stag.Type_expr.Constrained 2) (Stag.Type_expr.Modality 2)
     (Stag.Type_expr.Row.Extend 3) (Stag.Pattern.Deref 1)
     (Stag.Pattern.Poly_variant 2) (Stag.Pattern.Exception 1) (Stag.Pattern.Or 2)
     (Stag.Expr.App 2) (Stag.Expr.Let 5) (Stag.Expr.Seq 2) (Stag.Expr.Let_mut 4)
     (Stag.Expr.Assign 2) (Stag.Expr.Store 2) (Stag.Expr.Fun 2)
     (Stag.Expr.Splice 1) (Stag.Expr.Quote 1) (Stag.Expr.Deref 1)
     (Stag.Expr.Field 2) (Stag.Expr.Record_update 2) (Stag.Expr.Match 2)
     (Stag.Expr.Poly_field 2) (Stag.Expr.Poly_extend 2)
     (Stag.Expr.Poly_restrict 2) (Stag.Expr.Poly_update 2)
     (Stag.Expr.Poly_variant 2) (Stag.Expr.Stack 1) (Stag.Expr.Exclave 1)
     (Stag.Expr.Region 1) (Stag.Expr.Nontail 1) (Stag.Expr.Annotated 2)
     (Stag.Expr.Forget 2) (Stag.Expr.Attributed 2) (Stag.Toplevel.Attributed 2))
    (equal true)
    |}]
;;
