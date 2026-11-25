open! Import

let compute ~con ~params (kind : Signature.Type_decl.Kind.t) =
  let index =
    List.filter_mapi params ~f:(fun index (arg : Type.arg) ->
      match arg with
      | ATy (TVar var) -> Some (var.id, index)
      | ARow (RVar var) -> Some (var.id, index)
      | AMode (Var var) -> Some (Mode.Var.id var, index)
      | ATy _ | ARow _ | AMode (Fixed _) -> None)
    |> Int.Map.of_alist_reduce ~f:(fun first _ -> first)
  in
  let variances = Array.create ~len:(List.length params) Variance.Bivariant in
  let occurs id variance =
    Option.iter (Map.find index id) ~f:(fun i ->
      variances.(i) <- Variance.join variances.(i) variance)
  in
  let mode_var variance var = occurs (Mode.Var.id var) variance in
  (* A cyclic type reaches the same linked node again, maybe at another
     variance: it is walked again whenever that adds to the variances it has
     been walked at. *)
  let visited = Int.Table.create () in
  let first_visit id variance =
    let before =
      Option.value (Hashtbl.find visited id) ~default:Variance.Bivariant
    in
    let after = Variance.join before variance in
    Hashtbl.set visited ~key:id ~data:after;
    not (Variance.equal before after)
  in
  let rec ty variance (ty' : Type.ty) =
    match ty' with
    | TVar { id; state = Link target; _ } ->
      if first_visit id variance then ty variance target
    | TVar var -> occurs var.id variance
    | TCon (name, args) ->
      let declared = con name in
      List.iteri args ~f:(fun i arg ->
        let inner =
          match declared with
          | Some declared ->
            Option.value (List.nth declared i) ~default:Variance.Invariant
          | None -> Invariant
        in
        let variance = Variance.compose ~outer:variance inner in
        match (arg : Type.arg) with
        | ATy t -> ty variance t
        | ARow r -> row variance r
        | AMode m ->
          Mode_arg.iter_result m ~f:(fun var -> Ok (mode_var variance var))
          |> Result.ok_exn)
    | TArrow (arg, res, modes) ->
      let contra = Variance.compose ~outer:variance Contravariant in
      ty contra arg;
      ty variance res;
      let side variance (side : Arrow_modes.Side.t) =
        match side with
        | Var var -> mode_var variance var
        | Fixed _ -> ()
      in
      side contra modes.arg;
      side variance modes.res;
      side (Variance.compose ~outer:variance Invariant) modes.curry
    | TTuple tys -> List.iter tys ~f:(ty variance)
    | TCode { inner; context } ->
      ty variance inner;
      row (Variance.compose ~outer:variance Invariant) context
    | TRecord { id; row = r } | TVariant { id; row = r } ->
      let variance = Variance.compose ~outer:variance Invariant in
      if first_visit id variance then row variance r
  and row variance (row' : Type.row) =
    match row' with
    | REmpty -> ()
    | RExtend (_, _, _, t, rest) ->
      ty variance t;
      row variance rest
    | RVar { id; state = Link target; _ } ->
      if first_visit id variance then row variance target
    | RVar var -> occurs var.id variance
  in
  let modality (modality : Member_modality.t) =
    match modality with
    | Param i -> variances.(i) <- Variance.join variances.(i) Covariant
    | Id | Meet _ -> ()
  in
  (match kind with
   | Abstract ->
     Array.fill variances ~pos:0 ~len:(Array.length variances) Invariant
   | Alias t -> ty Covariant t
   | Record fields ->
     List.iter fields ~f:(fun (field : Signature.Field.t) ->
       ty Covariant field.ty;
       modality field.modality)
   | Variant constructors ->
     List.iter constructors ~f:(fun (ctor : Signature.Constructor.t) ->
       List.iter ctor.args ~f:(ty Covariant);
       List.iter ctor.modalities ~f:modality));
  Array.to_list variances
;;
