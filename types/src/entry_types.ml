open! Import

module Presence_var = struct
  type t = (Type.presence, Type.presence_bound) Type.Var.t
end

module Occurrence = struct
  type t =
    | Entry_of of Presence_var.t
    | Other
end

module Candidate = struct
  type t =
    { var : (Type.ty, Nothing.t) Type.Var.t
    ; presence : Presence_var.t
    ; other : Type.ty
    }
end

let same_equality (ty1, ty2) (ty1', ty2') =
  (Type.same_ty ty1 ty1' && Type.same_ty ty2 ty2')
  || (Type.same_ty ty1 ty2' && Type.same_ty ty2 ty1')
;;

let dedup_presence_bounds (var : Presence_var.t) =
  var.bounds
  <- List.fold var.bounds ~init:[] ~f:(fun acc (bound : Type.presence_bound) ->
       match bound with
       | If_present (ty1, ty2)
         when Type.same_ty ty1 ty2
              || List.exists acc ~f:(function
                | Type.If_present (ty1', ty2') ->
                  same_equality (ty1, ty2) (ty1', ty2')
                | At_most _ | At_least _ -> false) -> acc
       | If_present _ | At_most _ | At_least _ -> bound :: acc)
     |> List.rev
;;

let dedup_row_bounds (var : (Type.row, Type.bound) Type.Var.t) =
  var.bounds
  <- List.fold var.bounds ~init:[] ~f:(fun acc bound ->
       if List.exists acc ~f:(Type.same_bound bound) then acc else bound :: acc)
     |> List.rev
;;

let same_kept (kept1 : Type.kept) (kept2 : Type.kept) =
  match kept1, kept2 with
  | Row_bound r1, Row_bound r2 ->
    phys_equal r1.lower r2.lower && Type.same_bound r1.bound r2.bound
  | If_present_kept e1, If_present_kept e2 ->
    Type.same_presence (PVar e1.presence) (PVar e2.presence)
    && same_equality (e1.ty1, e1.ty2) (e2.ty1, e2.ty2)
  | (Row_bound _ | If_present_kept _), _ -> false
;;

let dedup_kept kept =
  List.fold kept ~init:[] ~f:(fun acc (kept : Type.kept) ->
    match kept with
    | If_present_kept { ty1; ty2; _ } when Type.same_ty ty1 ty2 -> acc
    | _ when List.exists acc ~f:(same_kept kept) -> acc
    | _ -> kept :: acc)
  |> List.rev
;;

(* Whether [lower] being [Present] makes [upper] so. *)
let at_most (lower : Presence_var.t) (upper : Presence_var.t) =
  let visited = Int.Hash_set.create () in
  let rec go (var : Presence_var.t) =
    var.id = upper.id
    || (Hash_set.strict_add visited var.id |> Hash_set.Ok_or_duplicate.is_ok
        && List.exists var.bounds ~f:(function
          | At_most bound ->
            (match Type.canonical_presence bound with
             | PVar var -> go var
             | Present | Absent -> false)
          | At_least _ | If_present _ -> false))
  in
  go lower
;;

let simplify ~is_generalized ~tys ~presences ~rows ~kept =
  let occurrences = Int.Table.create () in
  let nodes = Int.Hash_set.create () in
  let links = Int.Hash_set.create () in
  let occurs (var : _ Type.Var.t) occurrence =
    if is_generalized var.id
    then Hashtbl.add_multi occurrences ~key:var.id ~data:occurrence
  in
  let rec walk_ty (ty : Type.ty) =
    match ty with
    | TVar ({ state = Link target; _ } as var) ->
      if Hash_set.strict_add links var.id |> Hash_set.Ok_or_duplicate.is_ok
      then walk_ty target
    | TVar var -> occurs var Occurrence.Other
    | TCon (_, args) ->
      List.iter args ~f:(function
        | ATy ty -> walk_ty ty
        | ARow row -> walk_row row)
    | TArrow (ty1, ty2) ->
      walk_ty ty1;
      walk_ty ty2
    | TTuple tys -> List.iter tys ~f:walk_ty
    | TCode { inner; context } ->
      walk_ty inner;
      walk_row context
    | TRecord { id; row } | TVariant { id; row } ->
      if Hash_set.strict_add nodes id |> Hash_set.Ok_or_duplicate.is_ok
      then walk_row row
  and walk_row (row : Type.row) =
    match row with
    | REmpty -> ()
    | RVar ({ state = Link target; _ } as var) ->
      if Hash_set.strict_add links var.id |> Hash_set.Ok_or_duplicate.is_ok
      then walk_row target
    | RVar _ -> ()
    | RExtend (_, presence, _, ty, rest) ->
      entry presence ty;
      walk_row rest
  and entry presence ty =
    match Type.canonical_presence presence, Type.canonical_ty ty with
    | PVar presence, TVar var -> occurs var (Entry_of presence)
    | (Present | Absent | PVar _), _ -> walk_ty ty
  in
  let candidates = Queue.create () in
  let equality presence ty1 ty2 =
    entry (PVar presence) ty1;
    entry (PVar presence) ty2;
    let candidate ty other =
      match Type.canonical_ty ty with
      | TVar var when is_generalized var.id ->
        Queue.enqueue candidates { Candidate.var; presence; other }
      | _ -> ()
    in
    candidate ty1 ty2;
    candidate ty2 ty1
  in
  List.iter tys ~f:walk_ty;
  List.iter rows ~f:(fun (var : (Type.row, Type.bound) Type.Var.t) ->
    List.iter var.bounds ~f:(fun (bound : Type.bound) -> walk_row bound.row));
  List.iter presences ~f:(fun (var : Presence_var.t) ->
    List.iter var.bounds ~f:(function
      | Type.If_present (ty1, ty2) -> equality var ty1 ty2
      | At_most _ | At_least _ -> ()));
  List.iter kept ~f:(function
    | Type.Row_bound { bound; _ } -> walk_row bound.row
    | If_present_kept { presence; ty1; ty2; _ } -> equality presence ty1 ty2);
  let frozen = Int.Hash_set.create () in
  Queue.iter candidates ~f:(fun { Candidate.var; presence; other } ->
    let qualifies () =
      List.for_all (Hashtbl.find_multi occurrences var.id) ~f:(function
        | Occurrence.Entry_of under -> at_most under presence
        | Other -> false)
    in
    match var.state with
    | Generic when (not (Hash_set.mem frozen var.id)) && qualifies () ->
      let other_vars = ref [] in
      let collect ~guarded:_ (var : _ Type.Var.t) : (unit, Nothing.t) Result.t =
        other_vars := var.id :: !other_vars;
        Ok ()
      in
      (match
         Type.iter_ty { ty = collect; row = collect; presence = collect } other
       with
       | Ok () -> ()
       | Error _ -> .);
      if not (List.exists !other_vars ~f:(Hash_set.mem frozen))
      then (
        List.iter !other_vars ~f:(Hash_set.add frozen);
        Hash_set.add frozen var.id;
        var.state <- Link other)
    | Generic | Unbound _ | Link _ -> ())
;;

let dedup ~presences ~rows =
  List.iter presences ~f:dedup_presence_bounds;
  List.iter rows ~f:dedup_row_bounds
;;
