open! Import

module Generalized = struct
  type t =
    { scheme : Type.ty
    ; templates : Quoted_use.t list
    }
end

module Code = struct
  type t =
    { rank : Type.Rank.t
    ; dispatch : Quoted_use.t -> unit
    ; templates : Quoted_use.t list Typed.Ident.Table.t
    ; mutable generalized : Generalized.t list (** Latest first. *)
    }
end

type t =
  { code : Code.t
  ; enclosing : t option
  ; mutable uses : Quoted_use.t list (** Latest first. *)
  }

let create ~rank ~dispatch =
  { code =
      { rank
      ; dispatch
      ; templates = Typed.Ident.Table.create ()
      ; generalized = []
      }
  ; enclosing = None
  ; uses = []
  }
;;

let enter t = { code = t.code; enclosing = Some t; uses = [] }

let add t (use : Quoted_use.t) =
  match t.enclosing with
  | None ->
    Unify.lower_ty ~rank:t.code.rank use.ty;
    t.code.dispatch use
  | Some _ -> t.uses <- use :: t.uses
;;

let leave t ~rank ~ident ~ty ~generalize =
  let enclosing =
    match t.enclosing with
    | Some enclosing -> enclosing
    | None -> raise_s [%message "Local_uses.leave: not of a let"]
  in
  let uses = List.rev t.uses in
  t.uses <- [];
  let lower (use : Quoted_use.t) = Unify.lower_ty ~rank use.ty in
  let templates =
    match generalize with
    | None ->
      List.iter uses ~f:lower;
      []
    | Some generalize ->
      let templates, others =
        List.partition_tf uses ~f:(fun (use : Quoted_use.t) ->
          Scheme.generalizes ~rank use.ty)
      in
      List.iter others ~f:lower;
      generalize
        (ty :: List.map templates ~f:(fun (use : Quoted_use.t) -> use.ty));
      templates
  in
  if not (List.is_empty templates)
  then (
    Hashtbl.set t.code.templates ~key:ident ~data:templates;
    t.code.generalized
    <- { Generalized.scheme = ty; templates } :: t.code.generalized);
  List.iter uses ~f:(add enclosing)
;;

let templates t ident =
  Hashtbl.find t.code.templates ident |> Option.value ~default:[]
;;

let take_generalized t =
  let generalized = t.code.generalized in
  t.code.generalized <- [];
  Hashtbl.clear t.code.templates;
  List.rev generalized
;;
