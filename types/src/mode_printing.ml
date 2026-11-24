open! Import

(* An axis, with its points as indices from [0], its least. *)
module Axis = struct
  type t = Mode.Axis.Any.t

  let all = Mode.Axis.Any.all

  let name : t -> string = function
    | T Stage -> "stage"
    | T Provenance -> "provenance"
    | T Locality -> "locality"
  ;;

  let index (T axis : t) const =
    let module C = (val Mode.Axis.chain axis) in
    C.index (Mode.Const.get const axis)
  ;;

  let max (T axis : t) =
    let module C = (val Mode.Axis.chain axis) in
    C.index C.max
  ;;

  let atom (T axis : t) index =
    let module C = (val Mode.Axis.chain axis) in
    Sexp.to_string (C.sexp_of_t (C.of_index_exn index)) |> String.lowercase
  ;;

  let node (T axis : t) var = Mode.Var.node var axis
  let is_stage : t -> bool = function
    | T Stage -> true
    | T (Provenance | Locality) -> false
  ;;

  let is_provenance : t -> bool = function
    | T Provenance -> true
    | T (Stage | Locality) -> false
  ;;
end

module Printed = struct
  type t =
    | Lower of (Axis.t * int) list * int (** Atoms, at most one per axis. *)
    | Upper of int * (Axis.t * int) list
    | Relation of int * int * Axis.t list (** On the axes, every one if all. *)
end

type t =
  { vars : Mode.Var.t Int.Table.t
  ; order : int Int.Table.t
  ; counts : int Int.Table.t
  ; results : Int.Hash_set.t
  ; args : Int.Hash_set.t
  ; named : Int.Hash_set.t
  ; cells : Int.Hash_set.t
  ; held : Int.Hash_set.t
  ; rep : int Int.Table.t
  ; atoms : (Axis.t * int) list Int.Table.t
  ; names : string Int.Table.t
  ; mutable printed : Printed.t list
  }

let create () =
  { vars = Int.Table.create ()
  ; order = Int.Table.create ()
  ; counts = Int.Table.create ()
  ; results = Int.Hash_set.create ()
  ; args = Int.Hash_set.create ()
  ; named = Int.Hash_set.create ()
  ; cells = Int.Hash_set.create ()
  ; held = Int.Hash_set.create ()
  ; rep = Int.Table.create ()
  ; atoms = Int.Table.create ()
  ; names = Int.Table.create ()
  ; printed = []
  }
;;

let count t var ~(side : [ `Arg | `Res | `Held | `Cell ]) =
  let id = Mode.Var.id var in
  Hashtbl.set t.vars ~key:id ~data:var;
  (match side with
   | `Arg -> Hash_set.add t.args id
   | `Res -> Hash_set.add t.results id
   | `Held ->
     Hash_set.add t.named id;
     Hash_set.add t.held id
   | `Cell ->
     Hash_set.add t.named id;
     Hash_set.add t.cells id);
  if not (Hashtbl.mem t.order id)
  then Hashtbl.set t.order ~key:id ~data:(Hashtbl.length t.order);
  Hashtbl.incr t.counts id
;;

(* Whether each counted variable is below each other on the axis, through any
   others: those of the other members of a recursive group, or a weak variable's
   neighbours. *)
let reach t axis =
  let edges = Int.Table.create () in
  let visited = Int.Hash_set.create () in
  let queue = Queue.create () in
  let visit node =
    let id = Mode.Var.Node.var_id node in
    if not (Hash_set.mem visited id)
    then (
      Hash_set.add visited id;
      Queue.enqueue queue node)
  in
  Hashtbl.iter t.vars ~f:(fun var -> visit (Axis.node axis var));
  while not (Queue.is_empty queue) do
    let node = Queue.dequeue_exn queue in
    let id = Mode.Var.Node.var_id node in
    List.iter (Mode.Var.Node.relations node) ~f:(fun (direction, other) ->
      visit other;
      let other = Mode.Var.Node.var_id other in
      match direction with
      | `Le -> Hashtbl.add_multi edges ~key:id ~data:other
      | `Ge -> Hashtbl.add_multi edges ~key:other ~data:id)
  done;
  let reach = Int.Table.create () in
  Hashtbl.iter_keys t.vars ~f:(fun start ->
    let seen = Int.Hash_set.create () in
    let rec go id =
      if not (Hash_set.mem seen id)
      then (
        Hash_set.add seen id;
        List.iter (Hashtbl.find_multi edges id) ~f:go)
    in
    go start;
    Hashtbl.set reach ~key:start ~data:seen);
  fun a b -> Hash_set.mem (Hashtbl.find_exn reach a) b
;;

(* The variables are grouped into classes equal on every axis, each printed as
   one. Then, on each axis: classes equal on it (a cycle) are related in a ring,
   others by the edges not implied through a third or by their bounds; a bound
   is printed where neither the edges into the class nor what an annotation
   gives every variable (a result's [portable]) imply it. A class related to no
   other, named once (or fixed on every axis) and with each axis free or fixed
   prints as the atoms of its fixed axes, which read back as it. *)
let settle t =
  let reaches =
    let by_axis = List.map Axis.all ~f:(fun axis -> axis, reach t axis) in
    fun axis -> List.Assoc.find_exn by_axis axis ~equal:Mode.Axis.Any.equal
  in
  let by_position =
    Hashtbl.keys t.vars
    |> List.sort ~compare:(fun a b ->
      Int.compare (Hashtbl.find_exn t.order a) (Hashtbl.find_exn t.order b))
  in
  let var_bounds axis id =
    let var = Hashtbl.find_exn t.vars id in
    Axis.index axis (Mode.Var.lower var), Axis.index axis (Mode.Var.upper var)
  in
  (* A cell's locality means nothing: a variable only on cells is left out of
     the locality relations and bounds. *)
  let cell_only id =
    Hash_set.mem t.cells id
    && not
         (Hash_set.mem t.args id
          || Hash_set.mem t.results id
          || Hash_set.mem t.held id)
  in
  let irrelevant (axis : Axis.t) id =
    match axis with
    | T Locality -> cell_only id
    | T (Stage | Provenance) -> false
  in
  (* Equal on the axis: by their constraints, or fixed to the same point. *)
  let equal_on axis a b =
    (irrelevant axis a && irrelevant axis b)
    || (reaches axis a b && reaches axis b a)

    ||
    let lower, upper = var_bounds axis a in
    lower = upper && [%equal: int * int] (lower, upper) (var_bounds axis b)
  in
  Hashtbl.clear t.rep;
  List.iter by_position ~f:(fun id ->
    let rep =
      List.find_exn by_position ~f:(fun other ->
        List.for_all Axis.all ~f:(fun axis -> equal_on axis id other))
    in
    Hashtbl.set t.rep ~key:id ~data:rep);
  let reps =
    List.filter by_position ~f:(fun id -> Hashtbl.find_exn t.rep id = id)
  in
  let members rep =
    List.filter by_position ~f:(fun id -> Hashtbl.find_exn t.rep id = rep)
  in
  let lower axis rep =
    List.fold (members rep) ~init:0 ~f:(fun acc id ->
      Int.max acc (fst (var_bounds axis id)))
  in
  let upper axis rep =
    List.fold (members rep) ~init:(Axis.max axis) ~f:(fun acc id ->
      Int.min acc (snd (var_bounds axis id)))
  in
  let ignored axis rep = List.for_all (members rep) ~f:(irrelevant axis) in
  (* What an annotation's variable is at least: as an arrow's result is never
     [const], one only on results is [portable]. *)
  let floor axis rep =
    let members = members rep in
    if Axis.is_stage axis
       && List.exists members ~f:(Hash_set.mem t.results)
       && not (List.exists members ~f:(Hash_set.mem t.args))
    then Axis.index axis { Mode.Const.min with stage = Portable }
    else 0
  in
  (* What a variable is at most by its other axes: [const] is [static]. *)
  let ceiling axis rep =
    if Axis.is_provenance axis
       && upper (T Stage) rep = Axis.index (T Stage) Mode.Const.min
    then Axis.index axis Mode.Const.min
    else Axis.max axis
  in
  (* Implied by the bounds, once they read back. *)
  let by_bounds axis a b = upper axis a <= lower axis b in
  (* On each axis, the classes equal on it are related in a ring, others by the
     edges not implied through a third. *)
  let edges axis =
    let reaches = reaches axis in
    let cycle rep =
      List.filter reps ~f:(fun other -> reaches rep other && reaches other rep)
    in
    let leaders =
      List.filter reps ~f:(fun rep -> List.hd_exn (cycle rep) = rep)
    in
    let rings =
      List.concat_map leaders ~f:(fun leader ->
        match cycle leader with
        | [ _ ] -> []
        | cycle ->
          List.zip_exn cycle (List.tl_exn cycle @ [ List.hd_exn cycle ]))
    in
    let above a b = a <> b && reaches a b && not (reaches b a) in
    let between =
      List.concat_map leaders ~f:(fun a ->
        List.filter_map leaders ~f:(fun b ->
          Option.some_if
            (above a b
             && not (List.exists leaders ~f:(fun c -> above a c && above c b))
            )
            (a, b)))
    in
    rings @ between
    |> List.filter ~f:(fun (a, b) ->
      not (by_bounds axis a b || ignored axis a || ignored axis b))
  in
  let edges = List.map Axis.all ~f:(fun axis -> axis, edges axis) in
  let edge axis pair =
    List.Assoc.find_exn edges axis ~equal:Mode.Axis.Any.equal
    |> fun edges -> List.mem edges pair ~equal:[%equal: int * int]
  in
  (* A relation on the axes of its edges, or on every axis where that is
     implied on the others. *)
  let relations =
    List.concat_map reps ~f:(fun a ->
      List.filter_map reps ~f:(fun b ->
        match List.filter Axis.all ~f:(fun axis -> edge axis (a, b)) with
        | [] -> None
        | axes ->
          if List.for_all Axis.all ~f:(fun axis ->
               reaches axis a b
               || by_bounds axis a b
               || (ignored axis a && ignored axis b))
          then Some (a, b, Axis.all)
          else Some (a, b, axes)))
  in
  let related rep =
    List.exists relations ~f:(fun (a, b, _) -> a = rep || b = rep)
  in
  (* A class's bounds on the axis read back from atoms alone if it is free there
     or fixed to a point. *)
  let atom axis rep =
    let lower = lower axis rep
    and upper = upper axis rep in
    if lower = upper
    then (
      (* [const] says [static]. *)
      let implied = ceiling axis rep < Axis.max axis && lower = ceiling axis rep in
      Some (Option.some_if (not implied) lower))
    else if lower <= floor axis rep && upper >= ceiling axis rep
    then Some None
    else None
  in
  Hashtbl.clear t.atoms;
  List.iter reps ~f:(fun rep ->
    let members = members rep in
    match
      List.map Axis.all ~f:(fun axis ->
        Option.map (atom axis rep) ~f:(Option.map ~f:(fun point -> axis, point)))
      |> Option.all
    with
    | Some atoms
      when (not (related rep))
           && (not (List.exists members ~f:(Hash_set.mem t.named)))
           && (List.for_all Axis.all ~f:(fun axis ->
                 lower axis rep = upper axis rep)
               ||
               match members with
               | [ id ] ->
                 [%equal: int option] (Hashtbl.find t.counts id) (Some 1)
                 && Mode.Var.is_generic (Hashtbl.find_exn t.vars id)
               | _ -> false) ->
      Hashtbl.set t.atoms ~key:rep ~data:(List.filter_opt atoms)
    | _ -> ());
  (* A bound, unless implied where read back by the printed relations into and
     out of the class, or by what every annotation's variable is. A cycle's is
     printed once, on its first member. *)
  let bounds axis =
    let printed (a, b) =
      List.exists relations ~f:(fun (a', b', axes) ->
        a = a' && b = b' && List.mem axes axis ~equal:Mode.Axis.Any.equal)
    in
    let closure ~step start =
      let seen = Int.Hash_set.create () in
      let rec go id =
        List.iter reps ~f:(fun next ->
          if step id next && not (Hash_set.mem seen next)
          then (
            Hash_set.add seen next;
            go next))
      in
      go start;
      seen
    in
    let done_lower = Int.Hash_set.create () in
    let done_upper = Int.Hash_set.create () in
    List.filter reps ~f:(fun rep ->
      not (Hashtbl.mem t.atoms rep || ignored axis rep))
    |> List.concat_map ~f:(fun rep ->
      let below = closure rep ~step:(fun id next -> printed (next, id)) in
      let above = closure rep ~step:(fun id next -> printed (id, next)) in
      let cycle = Hash_set.filter below ~f:(Hash_set.mem above) in
      Hash_set.add cycle rep;
      let outside set = Hash_set.filter set ~f:(Fn.non (Hash_set.mem cycle)) in
      let implied_lower =
        Hash_set.fold
          (outside below)
          ~init:(Hash_set.fold cycle ~init:0 ~f:(fun acc id -> Int.max acc (floor axis id)))
          ~f:(fun acc id -> Int.max acc (Int.max (lower axis id) (floor axis id)))
      in
      let implied_upper =
        Hash_set.fold
          (outside above)
          ~init:(Hash_set.fold cycle ~init:(Axis.max axis) ~f:(fun acc id -> Int.min acc (ceiling axis id)))
          ~f:(fun acc id -> Int.min acc (upper axis id))
      in
      let bound done_ ~implied printed =
        if implied || Hash_set.exists cycle ~f:(Hash_set.mem done_)
        then []
        else (
          Hash_set.add done_ rep;
          [ printed ])
      in
      bound
        done_lower
        ~implied:(lower axis rep <= implied_lower)
        (`Lower, rep, lower axis rep)
      @ bound
          done_upper
          ~implied:(upper axis rep >= implied_upper)
          (`Upper, rep, upper axis rep))
  in
  let bounds =
    List.concat_map Axis.all ~f:(fun axis ->
      List.map (bounds axis) ~f:(fun bound -> axis, bound))
  in
  (* A class's bounds on one side, every axis's together. *)
  let bounds_of rep side =
    List.filter_map bounds ~f:(fun (axis, (side', rep', point)) ->
      Option.some_if (rep = rep' && Poly.equal side side') (axis, point))
  in
  t.printed
  <- List.concat_map reps ~f:(fun rep ->
       (match bounds_of rep `Lower with
        | [] -> []
        | atoms -> [ Printed.Lower (atoms, rep) ])
       @ List.filter_map relations ~f:(fun (a, b, axes) ->
         Option.some_if (a = rep) (Printed.Relation (a, b, axes)))
       @
       match bounds_of rep `Upper with
       | [] -> []
       | atoms -> [ Printed.Upper (rep, atoms) ])
;;

let name t rep =
  Hashtbl.find_or_add t.names rep ~default:(fun () ->
    let weak =
      Hashtbl.find t.vars rep
      |> Option.exists ~f:(fun var -> not (Mode.Var.is_generic var))
    in
    sprintf "'%sm%d" (if weak then "_" else "") (Hashtbl.length t.names))
;;

let rep t var =
  let id = Mode.Var.id var in
  Hashtbl.find t.rep id |> Option.value ~default:id
;;

let side t var =
  let rep = rep t var in
  match Hashtbl.find t.atoms rep with
  | Some [] -> None
  | Some atoms ->
    Some
      (" @ "
       ^ String.concat
           ~sep:" "
           (List.map atoms ~f:(fun (axis, point) -> Axis.atom axis point)))
  | None -> Some (" @ " ^ name t rep)
;;

let held t var = name t (rep t var)

let constraints t =
  let atoms points =
    String.concat
      ~sep:" "
      (List.map points ~f:(fun (axis, point) -> Axis.atom axis point))
  in
  List.concat_map t.printed ~f:(function
    | Lower (points, rep) -> [ atoms points ^ " <= " ^ name t rep ]
    | Upper (rep, points) -> [ name t rep ^ " <= " ^ atoms points ]
    | Relation (lo, hi, axes) ->
      if List.length axes = List.length Axis.all
      then [ name t lo ^ " <= " ^ name t hi ]
      else
        List.map axes ~f:(fun axis ->
          sprintf
            "%s(%s) <= %s(%s)"
            (Axis.name axis)
            (name t lo)
            (Axis.name axis)
            (name t hi)))
;;
