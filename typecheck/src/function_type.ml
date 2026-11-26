open! Import

module Level = struct
  type t =
    { args : (Typed.Ident.t * Type.ty) list
    ; annots : (Typed.Ident.t * Syntax.Annot.t) list
    ; ty : Type.ty
    ; loc : Loc.t
    }
end

type t =
  { args : (Typed.Ident.t * Type.ty) list
  ; annots : (Typed.Ident.t * Syntax.Annot.t) list
  ; inner : Level.t list
  ; body : Source.Expr.t
  ; result_annot : Syntax.Type_expr.t option
  ; res_ty : Type.ty
  ; ty : Type.ty
  ; arg_modes : Mode.Var.t list
  ; res_mode : Mode.Var.t
  ; partial_modes : Mode.Var.t list
  }

let params t = t.args @ List.concat_map t.inner ~f:(fun level -> level.args)

(* [fun]s directly nested in [body], each with its arguments and location, and
   the innermost body. *)
let rec nested_funs (body : Source.Expr.t) =
  match Source.Expr.view body with
  | Fun { args; body = inner } ->
    let levels, innermost = nested_funs inner in
    (args, Source.Expr.loc body) :: levels, innermost
  | _ -> [], body
;;

let create
  ?(merge = true)
  t
  (scope : Scope.t)
  ~(args : (Source.Binder.t * Syntax.Annot.t) list)
  ~annot
  ~res_mode
  ~(body : Source.Expr.t)
  ~loc
  =
  let levels, body =
    match annot, res_mode with
    | None, None when merge && not (List.is_empty args) -> nested_funs body
    | _ -> [], body
  in
  List.iter (args :: List.map levels ~f:fst) ~f:(fun args ->
    Duplicates.report
      scope
      (List.filter_map args ~f:(fun ((binder : Source.Binder.t), _) ->
         match binder with
         | Name name when not (String.equal name.txt "_") -> Some name
         | Name _ | Ident _ -> None))
      ~error:(fun name -> Duplicate_parameter name));
  let arity = List.length args in
  let level_arities = List.map levels ~f:(fun (args, _) -> List.length args) in
  let args = args @ List.concat_map levels ~f:fst in
  let annot_type = function
    | None -> Scope.fresh_ty t scope, None
    | Some annot ->
      (match Scope.elaborate_resolved scope annot with
       | Ok (ty, resolved) -> ty, Some resolved
       | Error error ->
         Scope.report scope error;
         Scope.fresh_ty t scope, None)
  in
  let moded annot =
    Mode_rules.annotated (Scope.modes scope) annot ~var:(Scope.mode_var scope)
  in
  let args =
    List.map args ~f:(fun (binder, (annot : Syntax.Annot.t)) ->
      let ty, resolved = annot_type annot.ty in
      let mode = moded annot.mode in
      Hashtbl.set
        scope.program.parameter_modes
        ~key:(Mode.Var.id mode)
        ~data:mode;
      binder, ty, { annot with ty = resolved }, mode)
  in
  let res_ty, result_annot = annot_type annot in
  let res_mode = moded res_mode in
  let partial_modes =
    List.drop_last args
    |> Option.value ~default:[]
    |> List.map ~f:(fun _ -> Scope.arrow_mode scope)
  in
  (* The type after each number of arguments, the full type first. *)
  let tys =
    let results =
      if List.is_empty args
      then []
      else
        List.map partial_modes ~f:(fun partial ->
          partial, Scope.arrow_mode scope)
        @ [ res_mode, Mode_rules.Function.result_curry (Scope.modes scope) ~loc
          ]
    in
    List.fold_right
      (List.zip_exn args results)
      ~init:[ res_ty ]
      ~f:(fun ((_, arg_ty, _, arg_mode), (res_mode, curry)) tys ->
        Type.TArrow
          ( arg_ty
          , List.hd_exn tys
          , { arg = Var arg_mode; res = Var res_mode; curry = Var curry } )
        :: tys)
  in
  let arg_modes = List.map args ~f:(fun (_, _, _, mode) -> mode) in
  let bound =
    List.map args ~f:(fun (binder, ty, resolved, _) ->
      let ident = Program_env.bind scope.program binder ty ~level:scope.level in
      ( (ident, ty)
      , Option.some_if (not (Syntax.Annot.is_none resolved)) (ident, resolved) ))
  in
  let level bound =
    let args, annots = List.unzip bound in
    args, List.filter_opt annots
  in
  let args, annots = level (List.take bound arity) in
  let inner =
    List.folding_map
      (List.zip_exn level_arities (List.map levels ~f:snd))
      ~init:arity
      ~f:(fun start (count, loc) ->
        let args, annots = level (List.sub bound ~pos:start ~len:count) in
        start + count, { Level.args; annots; ty = List.nth_exn tys start; loc })
  in
  { args
  ; annots
  ; inner
  ; body
  ; result_annot
  ; res_ty
  ; ty = List.hd_exn tys
  ; arg_modes
  ; res_mode
  ; partial_modes
  }
;;
