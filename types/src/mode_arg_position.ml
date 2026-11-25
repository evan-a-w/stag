open! Import

type t =
  | Held
  | Covariant
  | Contravariant
[@@deriving equal, sexp_of]

let iter ~con start ~f =
  (* [variance] is relative to the innermost invariant position: one restarts it
     at [Covariant]. *)
  let relative ~outer (inner : Variance.t) : Variance.t =
    match inner with
    | Invariant -> Covariant
    | Covariant | Contravariant | Bivariant -> Variance.compose ~outer inner
  in
  let visited = Int.Table.create () in
  let first_visit id variance =
    let before =
      Option.value (Hashtbl.find visited id) ~default:Variance.Bivariant
    in
    let after = Variance.join before variance in
    Hashtbl.set visited ~key:id ~data:after;
    not (Variance.equal before after)
  in
  let rec ty (variance : Variance.t) (ty' : Type.ty) =
    match ty' with
    | TVar { id; state = Link target; _ } ->
      if first_visit id variance then ty variance target
    | TVar _ -> ()
    | TCon (name, args) ->
      let declared = con name in
      List.iteri args ~f:(fun i arg ->
        let inner =
          match declared with
          | Some declared ->
            Option.value (List.nth declared i) ~default:Variance.Invariant
          | None -> Invariant
        in
        match (arg : Type.arg) with
        | ATy t -> ty (relative ~outer:variance inner) t
        | ARow r -> row r
        | AMode m ->
          let position : t =
            match Variance.compose ~outer:variance inner with
            | Covariant -> Covariant
            | Contravariant -> Contravariant
            | Invariant | Bivariant -> Held
          in
          Mode_arg.iter_result m ~f:(fun var -> Ok (f var position))
          |> Result.ok_exn)
    | TArrow (arg, res, _) ->
      ty (Variance.compose ~outer:variance Contravariant) arg;
      ty variance res
    | TTuple tys -> List.iter tys ~f:(ty variance)
    | TCode { inner; context } ->
      ty variance inner;
      row context
    | TRecord { id; row = r } | TVariant { id; row = r } ->
      if first_visit id Invariant then row r
  and row (row' : Type.row) =
    match row' with
    | REmpty -> ()
    | RExtend (_, _, _, t, rest) ->
      ty Covariant t;
      row rest
    | RVar { id; state = Link target; _ } ->
      if first_visit id Invariant then row target
    | RVar _ -> ()
  in
  ty Covariant start
;;
