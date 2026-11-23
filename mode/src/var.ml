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

let submode_axis (type a) ~crossing t1 t2 (axis : a Axis.t) reason =
  if not (crosses crossing axis)
  then
    Graph.add_edge
      (node t1 axis)
      (Morphism.identity (T axis))
      (node t2 axis)
      reason
  else (
    match axis with
    | Stage -> Graph.add_edge t1.stage Morphism.cross_portable t2.stage reason
    | Provenance | Locality -> Ok ())
;;

let submode ?(crossing = Crossing.none) a b reason =
  Graph.atomically (fun () ->
    all_axes (fun (T axis) -> submode_axis ~crossing a b axis reason))
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
  if not (crosses crossing axis)
  then Graph.at_most (node t axis) point reason
  else (
    match axis with
    | Stage ->
      Graph.at_most
        t.stage
        (Morphism.right Morphism.cross_portable point)
        reason
    | Provenance | Locality -> Ok ())
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

let generic_relations (type a) t (axis : a Axis.t) =
  Graph.generic_relations (node t axis)
;;

let lower_assuming t ~raised =
  get t (Graph.lower_assuming ~raised:(fun id -> raised id))
;;
