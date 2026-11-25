open! Import

type t =
  { id : int
  ; stage : Graph.t
  ; provenance : Graph.t
  ; locality : Graph.t
  }

let nodes t = [ t.stage; t.provenance; t.locality ]
let sexp_of_t t = [%sexp (nodes t : Graph.t list)]

let node (type a) t (axis : a Axis.t) =
  match axis with
  | Stage -> t.stage
  | Provenance -> t.provenance
  | Locality -> t.locality
;;

let index (type a) (axis : a Axis.t) (point : a) =
  let module C = (val Axis.chain axis) in
  C.index point
;;

let const_is_static = Reason.create "const implies static"

let of_nodes ~id ~stage ~provenance ~locality =
  let t = { id; stage; provenance; locality } in
  Graph.atomically (fun () ->
    Graph.add_edge t.provenance Morphism.const_is_static t.stage const_is_static)
  |> Mode_error.ok_exn;
  t
;;

let create ids ~rank =
  let id = Id_source.next ids in
  of_nodes
    ~id
    ~stage:(Graph.create ~var_id:id (T Stage) ~rank)
    ~provenance:(Graph.create ~var_id:id (T Provenance) ~rank)
    ~locality:(Graph.create ~var_id:id (T Locality) ~rank)
;;

let id t = t.id

let get t =
  let point (type a) (axis : a Axis.t) f : a =
    let module C = (val Axis.chain axis) in
    C.of_index_exn (f (node t axis))
  in
  fun f : Const.t ->
    { stage = point Stage f
    ; provenance = point Provenance f
    ; locality = point Locality f
    }
;;

let lower t = get t Graph.lower
let upper t = get t Graph.upper

let assuming f ~read =
  match
    Undo.atomically (fun () ->
      match f () with
      | Ok () -> Error (`Read (read ()))
      | Error (_ : Mode_error.t) -> Error `Failed)
  with
  | Ok (_ : Nothing.t) -> .
  | Error (`Read read) -> Some read
  | Error `Failed -> None
;;

let is_generic t = Graph.is_generic t.stage
let lower_rank t ~rank = List.iter (nodes t) ~f:(Graph.lower_rank ~rank)

let all_axes f =
  List.fold_result Axis.Any.all ~init:() ~f:(fun () axis -> f axis)
;;

let crosses (type a) (crossing : Crossing.t) (axis : a Axis.t) =
  match axis with
  | Stage -> crossing.stage
  | Provenance -> crossing.provenance
  | Locality -> crossing.locality
;;

(* What [crossed] (a value that crosses) and [full] (one that doesn't) add, with
   the crossing of [axis]. *)
let with_crossing ~crossing axis ~crossed ~full ~guard =
  match (crosses crossing axis : Crossing.When.t) with
  | Never -> full ()
  | Always -> crossed ()
  | While_pointer_free layout ->
    let%bind.Result () = crossed () in
    guard layout
;;

let submode_axis (type a) ~crossing ~locality t1 t2 (axis : a Axis.t) reason =
  let full =
    match axis with
    | Locality ->
      Option.map (Locality_map.morphism locality) ~f:(fun morphism ->
        t1.locality, morphism, t2.locality)
    | Stage | Provenance ->
      Some (node t1 axis, Morphism.identity (T axis), node t2 axis)
  in
  with_crossing
    ~crossing
    axis
    ~crossed:(fun () ->
      match axis with
      | Stage -> Graph.add_edge t1.stage Morphism.cross_portable t2.stage reason
      | Provenance | Locality -> Ok ())
    ~full:(fun () ->
      match full with
      | None -> Ok ()
      | Some (lo, morphism, hi) -> Graph.add_edge lo morphism hi reason)
    ~guard:(fun layout ->
      match full with
      | None -> Ok ()
      | Some (lo, morphism, hi) ->
        Graph.Layout.guard_edge lo morphism hi ~guards:[ layout ] reason)
;;

let submode ?(crossing = Crossing.none) ?(locality = Locality_map.id) a b reason
  =
  Graph.atomically (fun () ->
    all_axes (fun (T axis) -> submode_axis ~crossing ~locality a b axis reason))
;;

let allocated t reason =
  Graph.atomically (fun () ->
    Graph.add_edge t.stage Morphism.runtime_is_gc t.provenance reason)
;;

let submode_on ?(crossing = Crossing.none) a b axis reason =
  Graph.atomically (fun () ->
    submode_axis ~crossing ~locality:Locality_map.id a b axis reason)
;;

let meet_on (type a) a (axis : a Axis.t) (point : a) b reason =
  let point = index axis point in
  if point = 0
  then Ok ()
  else if point = Axis.Any.size (T axis) - 1
  then
    Graph.add_edge
      (node a axis)
      (Morphism.identity (T axis))
      (node b axis)
      reason
  else
    Graph.add_edge
      (node a axis)
      (Morphism.meet_with (T axis) point)
      (node b axis)
      reason
;;

let submode_meet a (point : Const.t) b reason =
  Graph.atomically (fun () ->
    all_axes (fun (T axis) -> meet_on a axis (Const.get point axis) b reason))
;;

let submode_meet_on a axis point b reason =
  Graph.atomically (fun () -> meet_on a axis point b reason)
;;

let equate a b reason =
  Graph.atomically (fun () ->
    let%bind.Result () = submode a b reason in
    submode b a reason)
;;

let at_least' t axis point reason =
  Graph.at_least (node t axis) (index axis point) reason
;;

let at_most' (type a) ~crossing t (axis : a Axis.t) (point : a) reason =
  let point = index axis point in
  with_crossing
    ~crossing
    axis
    ~crossed:(fun () ->
      match axis with
      | Stage ->
        Graph.at_most
          t.stage
          (Morphism.right Morphism.cross_portable point)
          reason
      | Provenance | Locality -> Ok ())
    ~full:(fun () -> Graph.at_most (node t axis) point reason)
    ~guard:(fun layout ->
      Graph.Layout.guard_upper (node t axis) point ~guards:[ layout ] reason)
;;

let at_least t axis point reason =
  Graph.atomically (fun () -> at_least' t axis point reason)
;;

let at_most ?(crossing = Crossing.none) t axis point reason =
  Graph.atomically (fun () -> at_most' ~crossing t axis point reason)
;;

let at_least_const t const reason =
  Graph.atomically (fun () ->
    all_axes (fun (T axis) -> at_least' t axis (Const.get const axis) reason))
;;

let at_most_const ?(crossing = Crossing.none) t const reason =
  Graph.atomically (fun () ->
    all_axes (fun (T axis) ->
      at_most' ~crossing t axis (Const.get const axis) reason))
;;

let of_const ids ~rank const =
  let t = create ids ~rank in
  let reason = Reason.create "constant" in
  let%map.Result () =
    Graph.atomically (fun () ->
      let%bind.Result () = at_least_const t const reason in
      at_most_const t const reason)
  in
  t
;;

let default_to_lower t = List.iter (nodes t) ~f:Graph.default_to_lower
let default_to_upper t = List.iter (nodes t) ~f:Graph.default_to_upper
let generalize ts ~rank = Graph.generalize (List.concat_map ts ~f:nodes) ~rank
let reopen t ~rank = List.iter (nodes t) ~f:(Graph.reopen ~rank)

(* A type's parameters are made when first needed, which may be under
   [atomically]. *)
let create_generic ?at_most ids =
  let id = Id_source.next ids in
  let uppers =
    match at_most with
    | None -> []
    | Some (const, reason) ->
      List.filter_map Axis.Any.all ~f:(fun (T axis as any) ->
        let point = index axis (Const.get const axis) in
        Option.some_if (point < Axis.Any.size any - 1) (any, point, reason))
  in
  let node =
    Graph.create_generic
      ~uppers
      ~var_id:id
      [ T Stage; T Provenance; T Locality ]
      ~edges:(fun node ->
        [ ( node (T Provenance)
          , Morphism.const_is_static
          , node (T Stage)
          , const_is_static )
        ])
  in
  { id
  ; stage = node (T Stage)
  ; provenance = node (T Provenance)
  ; locality = node (T Locality)
  }
;;

module Instance = struct
  type var = t

  type t =
    { graph : Graph.Instance.t
    ; vars : var Int.Table.t
    }

  let create ids ~rank =
    { graph = Graph.Instance.create ids ~rank; vars = Int.Table.create () }
  ;;

  let instantiate t (var : var) =
    if not (is_generic var)
    then var
    else (
      match Hashtbl.find t.vars var.id with
      | Some copy -> copy
      | None ->
        let id = Graph.Instance.var_id t.graph var.id in
        let instantiate = Graph.Instance.instantiate t.graph in
        let stage = instantiate var.stage in
        let provenance = instantiate var.provenance in
        let locality = instantiate var.locality in
        let copy = { id; stage; provenance; locality } in
        Graph.atomically (fun () ->
          Hashtbl.set t.vars ~key:var.id ~data:copy;
          Undo.record (fun () -> Hashtbl.remove t.vars var.id);
          Ok ())
        |> Mode_error.ok_exn;
        copy)
  ;;
end

let rank t = Graph.rank t.stage

module Node = struct
  type t = Graph.t

  let var_id = Graph.var_id
  let axis = Graph.axis
  let lower = Graph.lower
  let upper = Graph.upper
  let raised = Graph.raised

  module Fate = Graph.Fate

  let fate = Graph.fate
  let above = Graph.above
  let at_most = Graph.at_most
  let at_least = Graph.at_least

  module Guarded = struct
    type kind = Graph.Guarded.kind =
      | Edge of t * Morphism.t * t
      | Upper of t * int
      | Lower of t * int

    type t = Graph.Guarded.t =
      { kind : kind
      ; guards : Graph.Layout.t list
      ; holds : bool
      }

    let among = Graph.Guarded.among
  end
end

let node_any t (Axis.Any.T axis) = node t axis

let relate a morphism b reason =
  Graph.atomically (fun () ->
    Graph.add_edge
      (node_any a (Morphism.dst morphism))
      morphism
      (node_any b (Morphism.src morphism))
      reason)
;;

let lower_assuming (type a) t (axis : a Axis.t) ~(raised : int -> a option) : a =
  let module C = (val Axis.chain axis) in
  C.of_index_exn
    (Graph.lower_assuming (node t axis) ~raised:(fun node ->
       Option.map (raised (Graph.var_id node)) ~f:C.index))
;;

let waits_on t axis = snd (Graph.lower_sources (node t axis))
