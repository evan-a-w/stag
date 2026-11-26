open! Import

module Frame = struct
  type t =
    { id : Typed.Allocation.Id.t
    ; name : string
    ; loc : Loc.t
    ; ty : Type.ty
    ; parent : t option
    ; mutable generalized : bool
    }

  let id t = t.id
  let set_generalized t = t.generalized <- true
end

type t =
  { toplevel : Typed.Ident.t
  ; fresh : unit -> Typed.Allocation.Id.t
  ; mutable sites :
      (Typed.Allocation.Id.t * Mode_rules.Site.t * Frame.t option) list
  (** Latest first. *)
  ; mutable frames : Frame.t list (** Latest first. *)
  }

let create toplevel ~fresh = { toplevel; fresh; sites = []; frames = [] }

let add t site ~frame =
  let id = t.fresh () in
  t.sites <- (id, site, frame) :: t.sites;
  id
;;

let frame t ~parent ~name ~loc ~ty =
  let frame =
    { Frame.id = t.fresh (); name; loc; ty; parent; generalized = false }
  in
  t.frames <- frame :: t.frames;
  frame
;;

(* The locality nodes of the sides of the arrows in [ty] that aren't fixed, with
   where each first is; and every mode variable in [ty]. *)
let sides start =
  let vars = Queue.create () in
  let var v =
    Queue.enqueue vars (Mode.Var.node v Locality);
    false
  in
  let seen_types = Int.Hash_set.create () in
  let seen_modes = Int.Hash_set.create () in
  let found = Queue.create () in
  let side ~steps (side : Arrow_modes.Side.t) which =
    match side with
    | Fixed _ | Var_at _ -> ()
    | Var var ->
      let lower = Mode.Var.lower var
      and upper = Mode.Var.upper var in
      if (not (Hash_set.mem seen_modes (Mode.Var.id var)))
         && not (Mode.Locality.equal lower.locality upper.locality)
      then (
        Hash_set.add seen_modes (Mode.Var.id var);
        Queue.enqueue
          found
          ( { Typed.Allocation.Side.steps = List.rev steps; side = which }
          , Mode.Var.node var Locality ))
  in
  let first id =
    if Hash_set.mem seen_types id
    then false
    else (
      Hash_set.add seen_types id;
      true)
  in
  let rec ty (ty' : Type.ty) ~(steps : Typed.Allocation.Side.Step.t list) =
    let step ty' s = ty ty' ~steps:(s :: steps) in
    match ty' with
    | TVar { id; state = Link target; _ } -> if first id then ty target ~steps
    | TVar _ -> ()
    | TArrow (arg, res, modes) ->
      ignore (Arrow_modes.exists modes ~f:var : bool);
      side ~steps modes.arg `Arg;
      side ~steps modes.res `Res;
      step arg Arg;
      step res Res
    | TTuple tys -> List.iteri tys ~f:(fun i ty' -> step ty' (Component i))
    | TCon (_, args) ->
      List.iteri args ~f:(fun i (arg : Type.arg) ->
        match arg with
        | ATy ty' -> step ty' (Type_arg i)
        | ARow r -> row r ~steps:(Typed.Allocation.Side.Step.Type_arg i :: steps)
        | AMode mode -> ignore (Mode_arg.exists mode ~f:var : bool))
    | TCode { inner; context } ->
      step inner Code;
      row context ~steps
    | TRecord { id; row = r } | TVariant { id; row = r } ->
      if first (-id - 1) then row r ~steps
  and row (row' : Type.row) ~(steps : Typed.Allocation.Side.Step.t list) =
    match row' with
    | REmpty -> ()
    | RExtend (name, _, _, ty', rest) ->
      ty ty' ~steps:(Field name :: steps);
      row rest ~steps
    | RVar { id; state = Link target; _ } -> if first id then row target ~steps
    | RVar _ -> ()
  in
  ty start ~steps:[];
  Queue.to_list found, Queue.to_list vars
;;

module Owner = struct
  type t =
    { owner : Typed.Allocation.Owner.t
    ; depth : int
    ; sides : (Typed.Allocation.Side.t * Mode.Var.Node.t) list
    ; vars : Mode.Var.Node.t list
    }
end

let id = Mode.Var.Node.var_id

(* Fixes [nodes] at their least, and has [also] hold, then reads. [None] if they
   can't. *)
let assuming nodes ~also ~read =
  let least = Mode.Reason.create "an instance has it at its least" in
  Mode.Var.assuming
    (fun () ->
      Result.bind (also ()) ~f:(fun () ->
        List.map nodes ~f:(fun node ->
          Mode.Var.Node.at_most node (Mode.Var.Node.lower node) least)
        |> Result.all_unit))
    ~read
;;

let place ~rank members ~awaiting =
  let awaiting = List.concat_map awaiting ~f:(fun ty -> snd (sides ty)) in
  let owners = Queue.create () in
  let owner toplevel (lets : Frame.t list) ~ty =
    let sides, vars = sides ty in
    let owner =
      { Owner.owner =
          { toplevel
          ; lets =
              List.rev_map lets ~f:(fun (frame : Frame.t) ->
                { Typed.Allocation.Owner.Let.id = frame.id
                ; name = frame.name
                ; loc = frame.loc
                })
          }
      ; depth = List.length lets
      ; sides
      ; vars
      }
    in
    Queue.enqueue owners owner;
    owner
  in
  (* Each site, with its id and the owners it is in, innermost first. *)
  let sites =
    List.concat_map members ~f:(fun (t, ty) ->
      let toplevel = owner t.toplevel [] ~ty in
      (* Of each frame, the owners down to it, innermost first, and the
         generalized frames they are of. A frame is made after its parent. *)
      let within = Typed.Allocation.Id.Table.create () in
      List.iter (List.rev t.frames) ~f:(fun (frame : Frame.t) ->
        let outer =
          Option.value_map
            frame.parent
            ~default:([ toplevel ], [])
            ~f:(fun parent -> Hashtbl.find_exn within parent.id)
        in
        Hashtbl.add_exn
          within
          ~key:frame.id
          ~data:
            (if frame.generalized
             then (
               let owners, lets = outer in
               let lets = frame :: lets in
               owner t.toplevel lets ~ty:frame.ty :: owners, lets)
             else outer));
      List.rev_map t.sites ~f:(fun (id, site, frame) ->
        ( (id, site)
        , Option.value_map frame ~default:[ toplevel ] ~f:(fun frame ->
            fst (Hashtbl.find_exn within frame.id)) )))
    |> Array.of_list
  in
  let owners = Queue.to_list owners in
  let sides = List.concat_map owners ~f:(fun owner -> owner.sides) in
  let in_scheme =
    let ids =
      Int.Hash_set.of_list
        (List.concat_map owners ~f:(fun owner ->
           List.map (List.map owner.sides ~f:snd @ owner.vars) ~f:id)
         @ List.map awaiting ~f:id)
    in
    fun node -> Hash_set.mem ids (id node)
  in
  (* A site is made as locally as its mode's upper bound allows, which the nodes
     above it bound. Generalizing the owners' types decides each: one of their
     schemes keeps it, chosen by the instance; or it is left to code outside the
     component, which may yet constrain it; or is of another scheme; or is
     summarized away, having every constraint it will have. All but those
     summarized away are pinned at their least, as an instance may have them, so
     that the upper bound read is the least any instance gives. So a site is
     only made in a region when every node above it is pinned or summarized
     away: no instance, and no code yet to be checked, can make it any less
     local. *)
  let above =
    Array.map sites ~f:(fun ((_, (site : Mode_rules.Site.t)), _) ->
      Mode.Var.Node.above (Mode.Var.node site.mode Locality))
  in
  let internal node =
    match Mode.Var.Node.fate node ~rank ~kept:in_scheme with
    | Eliminated -> true
    | Outer | Kept | Generic -> false
  in
  let pinned =
    let seen = Int.Hash_set.create () in
    let pin node =
      if Hash_set.mem seen (id node)
      then None
      else (
        Hash_set.add seen (id node);
        Some node)
    in
    List.filter_map (List.map sides ~f:snd) ~f:pin
    @ List.filter_map
        (List.concat (Array.to_list above))
        ~f:(fun node -> if internal node then None else pin node)
  in
  let read () =
    Array.map sites ~f:(fun ((_, site), _) -> Mode_rules.placement site)
  in
  let least =
    match assuming pinned ~also:(fun () -> Ok ()) ~read with
    | Some least -> least
    | None ->
      (* Fixing nodes at their lower bounds can't fail; were it to, the heap is
         safe whatever the instance. *)
      Array.map sites ~f:(fun _ -> Typed.Allocation.Placement.Heap)
  in
  let above =
    Array.map above ~f:(fun nodes -> Int.Set.of_list (List.map nodes ~f:id))
  in
  let follows = Array.map sites ~f:(fun _ -> None) in
  (* A side of an owner a site is in, innermost owners' first, may make it more
     local in the instances where it is [local]. *)
  List.stable_sort owners ~compare:(fun (a : Owner.t) b ->
    Int.compare b.depth a.depth)
  |> List.iter ~f:(fun (owner : Owner.t) ->
    List.iter owner.sides ~f:(fun (side, node) ->
      let applies i =
        Option.is_none follows.(i)
        && Set.mem above.(i) (id node)
        && List.exists (snd sites.(i)) ~f:(phys_equal owner)
      in
      (* Only the nodes above the sites it applies to bound what is read of
         them, and fixing the others at their least can't fail. *)
      let relevant = Int.Hash_set.create () in
      Array.iteri sites ~f:(fun i _ ->
        if applies i then Set.iter above.(i) ~f:(Hash_set.add relevant));
      if not (Hash_set.is_empty relevant)
      then
        Option.iter
          (assuming
             (List.filter pinned ~f:(fun pinned ->
                id pinned <> id node && Hash_set.mem relevant (id pinned)))
             ~also:(fun () ->
               Mode.Var.Node.at_least
                 node
                 (Mode.Locality.index Local)
                 (Mode.Reason.create "an instance has it local"))
             ~read)
          ~f:(fun if_local ->
            Array.iteri if_local ~f:(fun i if_local ->
              if applies i
                 && not (Typed.Allocation.Placement.equal if_local least.(i))
              then
                follows.(i)
                <- Some
                     { Typed.Allocation.Follows.owner = owner.owner
                     ; side
                     ; if_local
                     }))));
  Array.to_list
    (Array.mapi sites ~f:(fun i ((id, (site : Mode_rules.Site.t)), chain) ->
       { Typed.Allocation.id
       ; owner = (List.hd_exn chain : Owner.t).owner
       ; loc = site.loc
       ; kind = site.kind
       ; placement = least.(i)
       ; follows = follows.(i)
       }))
  |> List.stable_sort ~compare:(fun (a : Typed.Allocation.t) b ->
    Loc.compare a.loc b.loc)
;;
