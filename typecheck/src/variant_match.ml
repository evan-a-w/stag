open! Import

type t = string list

let empty = []

let pattern_type
  t
  (scope : Scope.t)
  handled
  ~(pattern : Source.Pattern.t)
  ~scrutinee
  =
  match Source.Pattern.view pattern, handled with
  | Var binder, _ :: _ ->
    let row, rest =
      Poly_row.open_
        t
        scope
        (List.map handled ~f:(fun tag -> tag, Scope.fresh_ty t scope))
    in
    let rest = Fresh.variant t rest in
    let ident = Program_env.bind scope.program binder rest ~level:scope.level in
    ( { Typed.Pattern.desc = Var ident
      ; ty = Fresh.variant t row
      ; loc = Source.Pattern.loc pattern
      }
    , [ Pattern_var.bound
          ident
          rest
          ~loc:
            (Source.Binder.loc binder ~enclosing:(Source.Pattern.loc pattern))
      ] )
  | _ -> Pattern_check.pattern_type t scope ~pattern ~expected:scrutinee
;;

(* The alternatives of an or-pattern, each matching what it does. *)
let rec alternatives ({ desc; _ } as pattern : Typed.Pattern.t) =
  match desc with
  | Or (left, right) -> alternatives left @ alternatives right
  | _ -> [ pattern ]
;;

let add_case handled pattern =
  List.fold (alternatives pattern) ~init:handled ~f:(fun handled pattern ->
    match pattern.desc with
    | Poly_variant (tag, arg)
      when Option.for_all arg ~f:Typed.Pattern.is_irrefutable
           && not (List.mem handled tag ~equal:String.equal) ->
      handled @ [ tag ]
    | _ -> handled)
;;

let is_empty_nominal (scope : Scope.t) scrutinee =
  match Type.canonical_ty scrutinee with
  | TCon (name, _) -> Type_env.is_empty_variant scope.program.type_env name
  | _ -> Ok false
;;

let is_closed_variant scrutinee =
  match Type.canonical_ty scrutinee with
  | TVariant { row; _ } ->
    let rec loop seen row =
      match Type.canonical_row row with
      | Type.REmpty -> true
      | RVar _ -> false
      | RExtend (tag, presence, _, _, rest) ->
        let shadowed = Set.mem seen tag in
        let seen = Set.add seen tag in
        (match shadowed, Type.canonical_presence presence with
         | _, PVar _ | true, Present -> false
         | _, Absent | false, Present -> loop seen rest)
    in
    loop String.Set.empty row
  | _ -> false
;;

let close t (scope : Scope.t) ~scrutinee ~loc (patterns : Typed.Pattern.t list) =
  let patterns =
    List.concat_map patterns ~f:alternatives
    |> List.map ~f:(fun (pattern : Typed.Pattern.t) -> pattern.desc)
  in
  let tags =
    List.filter_map patterns ~f:(function
      | Poly_variant (tag, _) -> Some tag
      | _ -> None)
    |> List.stable_dedup ~compare:String.compare
  in
  let has_catch_all =
    List.exists patterns ~f:(function
      | Var _ | Any -> true
      | _ -> false)
  in
  let%bind.Result needs_close =
    match patterns with
    | [] -> Result.map (is_empty_nominal scope scrutinee) ~f:not
    | _ :: _ ->
      Ok
        ((not has_catch_all)
         && (not (List.is_empty tags))
         && not (is_closed_variant scrutinee))
  in
  if not needs_close
  then Ok ()
  else (
    let closed =
      Poly_row.present
        (List.map tags ~f:(fun tag -> tag, Scope.fresh_ty t scope))
        ~tail:REmpty
    in
    Unify.unify_ty t ~ty1:(Fresh.variant t closed) ~ty2:scrutinee
    |> Typecheck_error.lift ~loc)
;;
