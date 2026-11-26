open! Import

module Code = struct
  type t =
    { rank : Type.Rank.t
    ; dispatch : Quoted_use.t -> unit
    ; generalized : Quoted_use.t list Typed.Ident.Table.t
    ; mutable schemes : (Type.ty * Quoted_use.t list) list
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
      ; generalized = Typed.Ident.Table.create ()
      ; schemes = []
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

let uses t = List.rev t.uses

let leave t ~rank ~ident ~scheme ~templates =
  let enclosing =
    match t.enclosing with
    | Some enclosing -> enclosing
    | None -> raise_s [%message "Local_uses.leave: not of a let"]
  in
  List.iter (uses t) ~f:(fun (use : Quoted_use.t) ->
    if List.mem templates use ~equal:phys_equal
    then t.code.dispatch use
    else (
      Unify.lower_ty ~rank use.ty;
      add enclosing use));
  if not (List.is_empty templates)
  then (
    Hashtbl.set t.code.generalized ~key:ident ~data:templates;
    t.code.schemes <- (scheme, templates) :: t.code.schemes);
  t.uses <- []
;;

let generalized t ident =
  Hashtbl.find t.code.generalized ident |> Option.value ~default:[]
;;

let take_schemes t =
  let schemes = t.code.schemes in
  t.code.schemes <- [];
  Hashtbl.clear t.code.generalized;
  List.rev schemes
;;
