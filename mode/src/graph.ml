open! Import

module Why = struct
  type t =
    | Initial
    | Bound of Reason.t
    | Step of
        { reasons : Reason.t list
        ; prev : t
        }

  let to_list t =
    let rec go acc = function
      | Initial -> acc
      | Bound reason -> reason :: acc
      | Step { reasons; prev } -> go (reasons @ acc) prev
    in
    go [] t
  ;;
end

type t =
  { id : int
  ; var_id : int
  ; axis : Axis.Any.t
  ; mutable lower : int
  ; mutable lower_why : Why.t
  ; mutable upper : int
  ; mutable upper_why : Why.t
  ; mutable succ : edge list
  ; mutable pred : edge list
  ; mutable dormant : dormant list
  (** The dormant constraints on it, fired or not: [generalize] follows them. *)
  ; mutable state : state
  }

and edge =
  { lo : t
  ; morphism : Morphism.t
  ; hi : t
  ; reasons : Reason.t list
  }

and state =
  | Rank of int
  | Generic of
      { above : implied list (** [this <= m(other)] *)
      ; below : implied list (** [other <= m(this)] *)
      ; guarded : dormant list
      (** What its scheme adds once a layout of a variable not generalized has
          pointers: each instance gets a copy, on copies of the generic nodes. *)
      }

and implied =
  { other : t
  ; implied_morphism : Morphism.t
  ; path : Reason.t list
  }

(* A constraint added once any of [guards] has pointers. *)
and dormant =
  { kind : kind
  ; guards : layout list
  ; dormant_reasons : Reason.t list
  ; mutable fired : bool
  }

and kind =
  | Edge of t * Morphism.t * t
  | Upper of t * int
  | Lower of t * int

and layout =
  { layout_id : int
  ; mutable layout_state : layout_state
  ; mutable bound : Mode_error.Bound.t option (** Annotated as pointer-free. *)
  ; mutable layout_above : layout list
  ; mutable layout_below : layout list
  (** It has pointers if any of these do: the layouts of the variables of the
      type its own variable was bound to. *)
  ; mutable waiting : dormant list
  ; mutable conjuncts : (layout * layout) list
  (** [(other, both)]: [both] has pointers once it and [other] do. *)
  }

and layout_state =
  | Unknown
  | Pointer_free
  | Has_pointers of Reason.t list

let name t = sprintf !"m%d.%{Sexp}" t.var_id (Axis.Any.sexp_of_t t.axis)

let sexp_of_bounds t =
  let point = Axis.Any.sexp_of_point t.axis in
  if t.lower = t.upper
  then point t.lower
  else Sexp.Atom (sprintf !"%{Sexp}..%{Sexp}" (point t.lower) (point t.upper))
;;

let sexp_of_applied morphism t =
  if Morphism.is_identity morphism
  then Sexp.Atom (name t)
  else List [ Morphism.sexp_of_t morphism; Atom (name t) ]
;;

let sexp_of_kind = function
  | Edge (lo, morphism, hi) ->
    [ Sexp.Atom (name lo); Atom "<="; sexp_of_applied morphism hi ]
  | Upper (t, point) ->
    [ Atom (name t); Atom "<="; Axis.Any.sexp_of_point t.axis point ]
  | Lower (t, point) ->
    [ Axis.Any.sexp_of_point t.axis point; Atom "<="; Atom (name t) ]
;;

let sexp_of_guards guards =
  Sexp.List
    (List.map guards ~f:(fun layout ->
       Sexp.Atom (sprintf "l%d" layout.layout_id)))
;;

let sexp_of_t t =
  match t.state with
  | Rank _ -> Sexp.List [ Atom (name t); sexp_of_bounds t ]
  | Generic { above; below; guarded } ->
    let constraints =
      List.map above ~f:(fun { other; implied_morphism; _ } ->
        Sexp.List
          [ Atom (name t); Atom "<="; sexp_of_applied implied_morphism other ])
      @ List.map below ~f:(fun { other; implied_morphism; _ } ->
        Sexp.List
          [ Atom (name other); Atom "<="; sexp_of_applied implied_morphism t ])
      @ List.map guarded ~f:(fun dormant ->
        Sexp.List
          (sexp_of_kind dormant.kind
           @ [ Atom "unless"; sexp_of_guards dormant.guards ]))
    in
    List
      [ Atom (name t); sexp_of_bounds t; List (Atom "generic" :: constraints) ]
;;

let create ~var_id axis ~rank =
  { id = (var_id * List.length Axis.Any.all) + Axis.Any.index axis
  ; var_id
  ; axis
  ; lower = 0
  ; lower_why = Initial
  ; upper = Axis.Any.size axis - 1
  ; upper_why = Initial
  ; succ = []
  ; pred = []
  ; dormant = []
  ; state = Rank rank
  }
;;

let lower t = t.lower
let upper t = t.upper

let is_generic t =
  match t.state with
  | Generic _ -> true
  | Rank _ -> false
;;

let lower_rank t ~rank =
  match t.state with
  | Rank r when r > rank -> t.state <- Rank rank
  | Rank _ | Generic _ -> ()
;;

let log = Undo.record
let atomically = Undo.atomically

let check t =
  if t.lower <= t.upper
  then Ok ()
  else (
    let (T axis) = t.axis in
    let module C = (val Axis.chain axis) in
    Error
      (Mode_error.Mismatch
         { var = t.var_id
         ; axis
         ; lower = C.of_index_exn t.lower
         ; upper = C.of_index_exn t.upper
         ; lower_because = Why.to_list t.lower_why
         ; upper_because = Why.to_list t.upper_why
         }))
;;

let rec raise_lower t point why =
  if point <= t.lower
  then Ok ()
  else (
    let lower = t.lower
    and lower_why = t.lower_why in
    log (fun () ->
      t.lower <- lower;
      t.lower_why <- lower_why);
    t.lower <- point;
    t.lower_why <- why;
    let%bind.Result () = check t in
    List.fold_result t.succ ~init:() ~f:(fun () edge ->
      raise_lower
        edge.hi
        (Morphism.left edge.morphism t.lower)
        (Step { reasons = edge.reasons; prev = t.lower_why })))
;;

let rec lower_upper t point why =
  if point >= t.upper
  then Ok ()
  else (
    let upper = t.upper
    and upper_why = t.upper_why in
    log (fun () ->
      t.upper <- upper;
      t.upper_why <- upper_why);
    t.upper <- point;
    t.upper_why <- why;
    let%bind.Result () = check t in
    List.fold_result t.pred ~init:() ~f:(fun () edge ->
      lower_upper
        edge.lo
        (Morphism.right edge.morphism t.upper)
        (Step { reasons = List.rev edge.reasons; prev = t.upper_why })))
;;

let add_edge' lo morphism hi reasons =
  assert (Axis.Any.equal (Morphism.dst morphism) lo.axis);
  assert (Axis.Any.equal (Morphism.src morphism) hi.axis);
  let edge = { lo; morphism; hi; reasons } in
  let succ = lo.succ
  and pred = hi.pred in
  log (fun () ->
    lo.succ <- succ;
    hi.pred <- pred);
  lo.succ <- edge :: lo.succ;
  hi.pred <- edge :: hi.pred;
  let%bind.Result () =
    raise_lower
      hi
      (Morphism.left morphism lo.lower)
      (Step { reasons; prev = lo.lower_why })
  in
  lower_upper
    lo
    (Morphism.right morphism hi.upper)
    (Step { reasons = List.rev reasons; prev = hi.upper_why })
;;

let defaulted = Reason.create "defaulted"

let default_to_lower t =
  atomically (fun () -> lower_upper t t.lower (Bound defaulted))
  |> Mode_error.ok_exn
;;

let default_to_upper t =
  atomically (fun () -> raise_lower t t.upper (Bound defaulted))
  |> Mode_error.ok_exn
;;

let why_of_reasons = function
  | [] -> Why.Initial
  | first :: rest -> Step { reasons = rest; prev = Bound first }
;;

let add_kind kind reasons =
  match kind with
  | Edge (lo, morphism, hi) -> add_edge' lo morphism hi reasons
  | Upper (t, point) -> lower_upper t point (why_of_reasons reasons)
  | Lower (t, point) -> raise_lower t point (why_of_reasons reasons)
;;

let kind_nodes = function
  | Edge (lo, _, hi) -> [ lo; hi ]
  | Upper (t, _) | Lower (t, _) -> [ t ]
;;

(* In the innermost [attempt]: the dormant constraints that failed to be added,
   and the layouts raised. *)
module Attempted = struct
  type t =
    { mutable failed : dormant list
    ; mutable raised : layout list
    }
end

let attempted : Attempted.t option ref = ref None

let fire dormant ~because =
  if dormant.fired
  then Ok ()
  else (
    log (fun () -> dormant.fired <- false);
    dormant.fired <- true;
    let result = add_kind dormant.kind (dormant.dormant_reasons @ because) in
    if Result.is_error result
    then
      Option.iter !attempted ~f:(fun attempted ->
        attempted.failed <- dormant :: attempted.failed);
    result)
;;

module Layout_ = struct
  type t = layout

  let create ids =
    { layout_id = Id_source.next ids
    ; layout_state = Unknown
    ; bound = None
    ; layout_above = []
    ; layout_below = []
    ; waiting = []
    ; conjuncts = []
    }
  ;;

  let id t = t.layout_id

  let has_pointers t =
    match t.layout_state with
    | Has_pointers _ -> true
    | Unknown | Pointer_free -> false
  ;;

  let bound t = Option.map t.bound ~f:(fun bound -> bound.reason)

  let is_unknown t =
    match t.layout_state, t.bound with
    | Unknown, None -> true
    | Unknown, Some _ | Pointer_free, _ | Has_pointers _, _ -> false
  ;;

  let set_state t state =
    let previous = t.layout_state in
    log (fun () -> t.layout_state <- previous);
    t.layout_state <- state
  ;;

  let rec raise t ~because =
    match t.layout_state with
    | Has_pointers _ | Pointer_free -> Ok ()
    | Unknown ->
      (match t.bound with
       | Some bound -> Error (Mode_error.Has_pointers { because; bound })
       | None ->
         set_state t (Has_pointers because);
         Option.iter !attempted ~f:(fun attempted ->
           attempted.raised <- t :: attempted.raised);
         let%bind.Result () =
           List.fold_result (List.rev t.waiting) ~init:() ~f:(fun () dormant ->
             fire dormant ~because)
         in
         let%bind.Result () =
           List.fold_result t.layout_above ~init:() ~f:(fun () above ->
             raise above ~because)
         in
         List.fold_result t.conjuncts ~init:() ~f:(fun () (other, both) ->
           match other.layout_state with
           | Has_pointers because' -> raise both ~because:(because @ because')
           | Unknown | Pointer_free -> Ok ()))
  ;;

  let rec pointer_free t =
    match t.layout_state with
    | Unknown ->
      set_state t Pointer_free;
      List.iter t.conjuncts ~f:(fun (_, both) -> pointer_free both)
    | Pointer_free | Has_pointers _ -> ()
  ;;

  let both ids a b =
    let both = create ids in
    (match a.layout_state, b.layout_state with
     | Pointer_free, _ | _, Pointer_free -> pointer_free both
     | Has_pointers because, Has_pointers because' ->
       set_state both (Has_pointers (because @ because'))
     | _ ->
       List.iter
         [ a, b; b, a ]
         ~f:(fun (layout, other) ->
           let conjuncts = layout.conjuncts in
           log (fun () -> layout.conjuncts <- conjuncts);
           layout.conjuncts <- (other, both) :: conjuncts));
    both
  ;;

  let rec bound_by t bound =
    match t.layout_state, t.bound with
    | Has_pointers because, _ ->
      Error (Mode_error.Has_pointers { because; bound })
    | Pointer_free, _ | Unknown, Some _ -> Ok ()
    | Unknown, None ->
      log (fun () -> t.bound <- None);
      t.bound <- Some bound;
      List.fold_result t.layout_below ~init:() ~f:(fun () below ->
        bound_by below bound)
  ;;

  let add_below ~lower t =
    match lower.layout_state, t.layout_state with
    | _, (Has_pointers _ | Pointer_free) | Pointer_free, Unknown -> Ok ()
    | Has_pointers because, Unknown -> raise t ~because
    | Unknown, Unknown ->
      let above = lower.layout_above
      and below = t.layout_below in
      log (fun () ->
        lower.layout_above <- above;
        t.layout_below <- below);
      lower.layout_above <- t :: lower.layout_above;
      t.layout_below <- lower :: t.layout_below;
      (match t.bound with
       | None -> Ok ()
       | Some reason -> bound_by lower reason)
  ;;

  (* The layouts without [layout_below] that decide whether [t] has pointers:
     those of unbound variables. *)
  let roots ts =
    let seen = Int.Hash_set.create () in
    let found = Queue.create () in
    let rec visit t =
      if Hash_set.strict_add seen t.layout_id |> Hash_set.Ok_or_duplicate.is_ok
      then (
        match t.layout_state with
        | Pointer_free -> ()
        | Has_pointers _ -> Queue.enqueue found t
        | Unknown ->
          if List.is_empty t.layout_below
          then Queue.enqueue found t
          else List.iter t.layout_below ~f:visit)
    in
    List.iter ts ~f:visit;
    Queue.to_list found
    |> List.sort ~compare:(fun a b -> Int.compare a.layout_id b.layout_id)
  ;;
end

(* Adds [kind] now if a guard has pointers, else once one does. *)
let guard kind ~guards reasons =
  match
    List.find_map guards ~f:(fun layout ->
      match layout.layout_state with
      | Has_pointers because -> Some because
      | Unknown | Pointer_free -> None)
  with
  | Some because -> add_kind kind (reasons @ because)
  | None ->
    let guards =
      List.filter guards ~f:(fun layout ->
        match layout.layout_state with
        | Unknown -> true
        | Pointer_free | Has_pointers _ -> false)
    in
    if not (List.is_empty guards)
    then (
      let dormant =
        { kind; guards; dormant_reasons = reasons; fired = false }
      in
      List.iter guards ~f:(fun layout ->
        let waiting = layout.waiting in
        log (fun () -> layout.waiting <- waiting);
        layout.waiting <- dormant :: waiting);
      List.iter (kind_nodes kind) ~f:(fun node ->
        let previous = node.dormant in
        log (fun () -> node.dormant <- previous);
        node.dormant <- dormant :: previous));
    Ok ()
;;

module Layout = struct
  include Layout_

  let raise t ~because = atomically (fun () -> raise t ~because)

  let bound_by ?instance t reason =
    atomically (fun () -> bound_by t { reason; instance })
  ;;

  let add_below ~lower t = atomically (fun () -> add_below ~lower t)

  let attempt f =
    let outer = !attempted in
    let mine = { Attempted.failed = []; raised = [] } in
    attempted := Some mine;
    let result =
      Exn.protect
        ~f:(fun () -> atomically f)
        ~finally:(fun () -> attempted := outer)
    in
    match result with
    | Ok _ ->
      Option.iter outer ~f:(fun outer ->
        outer.failed <- mine.failed @ outer.failed;
        outer.raised <- mine.raised @ outer.raised);
      result
    | Error _ ->
      (* Undone, what the layouts raised guard would fire again: it all follows
         from the one binding that failed. *)
      let dropped =
        mine.failed
        @ List.concat_map mine.raised ~f:(fun layout -> layout.waiting)
      in
      atomically (fun () ->
        List.iter dropped ~f:(fun dormant ->
          if not dormant.fired
          then (
            log (fun () -> dormant.fired <- false);
            dormant.fired <- true));
        Ok ())
      |> Mode_error.ok_exn;
      result
  ;;

  let pointer_free t =
    atomically (fun () ->
      pointer_free t;
      Ok ())
    |> Mode_error.ok_exn
  ;;

  let both ids a b =
    let result = ref None in
    atomically (fun () ->
      result := Some (both ids a b);
      Ok ())
    |> Mode_error.ok_exn;
    Option.value_exn !result
  ;;

  let guard_edge lo morphism hi ~guards reason =
    guard (Edge (lo, morphism, hi)) ~guards [ reason ]
  ;;

  let guard_upper node point ~guards reason =
    guard (Upper (node, point)) ~guards [ reason ]
  ;;
end

let add_edge lo morphism hi reason =
  add_kind (Edge (lo, morphism, hi)) [ reason ]
;;

let at_least t point reason = add_kind (Lower (t, point)) [ reason ]
let at_most t point reason = add_kind (Upper (t, point)) [ reason ]

(* The constraints between [start] and the nodes [stop] accepts, through paths
   in nodes [through] accepts: [start <= m(other)] when [forward],
   [other <= m(start)] otherwise. *)
let implied start ~forward ~stop ~through =
  let visited = Int.Table.create () in
  let found = ref [] in
  let seen node morphism =
    let morphisms = Hashtbl.find_multi visited node.id in
    List.mem morphisms morphism ~equal:(fun a b -> Morphism.compare a b = 0)
  in
  let rec visit node morphism path =
    let edges = if forward then node.succ else node.pred in
    List.iter edges ~f:(fun edge ->
      let other, morphism, path =
        if forward
        then
          edge.hi, Morphism.compose morphism edge.morphism, path @ edge.reasons
        else
          edge.lo, Morphism.compose edge.morphism morphism, edge.reasons @ path
      in
      if stop other
      then (
        if not
             (List.exists !found ~f:(fun implied ->
                phys_equal implied.other other
                && Morphism.compare implied.implied_morphism morphism = 0))
        then found := { other; implied_morphism = morphism; path } :: !found)
      else if through other && not (seen other morphism)
      then (
        Hashtbl.add_multi visited ~key:other.id ~data:morphism;
        visit other morphism path))
  in
  visit start (Morphism.identity start.axis) [];
  List.rev !found
  |> List.filter ~f:(fun implied ->
    not
      (phys_equal implied.other start
       && Morphism.is_inflationary implied.implied_morphism))
;;

module Fate = struct
  type t =
    | Outer
    | Kept
    | Generic
    | Eliminated
  [@@deriving equal]
end

let fate node ~rank ~kept : Fate.t =
  match node.state with
  | Generic _ -> Generic
  | Rank r when r <= rank -> Outer
  | Rank _ -> if kept node then Kept else Eliminated
;;

(* The dormant constraints of [x]'s scheme: those between it and the nodes kept
   or outer, through paths in internal nodes with dormant constraints, guarded
   by the layouts any of which has pointers once they hold. (Requiring each of a
   path's layouts, rather than any, would be more precise.) Those [above] and
   [below] already imply, unguarded, are left out.

   So read, the constraints of one kind and ends found through several paths are
   one, guarded by the union of the paths' layouts: a node is visited with the
   union of those of the paths to it, again only when that grows, so at most
   once per layout. *)
let guarded_implied x ~is_kept ~is_outer ~internal ~above ~below ~above_of =
  let by_id a b = Int.compare a.layout_id b.layout_id in
  let union a b =
    List.merge a b ~compare:by_id
    |> List.remove_consecutive_duplicates ~equal:phys_equal
  in
  let subset a b =
    List.for_all a ~f:(fun layout -> List.mem b layout ~equal:phys_equal)
  in
  let kind_key = function
    | Edge (lo, morphism, hi) -> `Edge (lo.id, Morphism.code morphism, hi.id)
    | Upper (_, point) -> `Upper point
    | Lower (_, point) -> `Lower point
  in
  (* By kind: the guards so far, and the reasons of the first path found. *)
  let found = Hashtbl.Poly.create () in
  let add kind guards path =
    if not (List.is_empty guards)
    then
      Hashtbl.update found (kind_key kind) ~f:(function
        | None -> kind, guards, path
        | Some (kind, guards', path) -> kind, union guards guards', path)
  in
  (* An annotated layout never has pointers. *)
  let guards_of dormant =
    if dormant.fired
    then None
    else (
      match
        List.filter (Layout.roots dormant.guards) ~f:(fun layout ->
          Option.is_none layout.bound)
      with
      | [] -> None
      | roots -> Some (List.sort roots ~compare:by_id))
  in
  let search ~forward =
    (* By node and morphism: the guards it was last visited with, [[]] once
       reached by a path not guarded. *)
    let visited = Hashtbl.Poly.create () in
    let unguarded = Queue.create ()
    and guarded = Queue.create () in
    let reach node morphism guards path =
      let key = node.id, Morphism.code morphism in
      let visit guards =
        Hashtbl.set visited ~key ~data:guards;
        Queue.enqueue
          (if List.is_empty guards then unguarded else guarded)
          (node, morphism, guards, path)
      in
      match Hashtbl.find visited key with
      | Some [] -> ()
      | None -> visit guards
      | Some current ->
        if List.is_empty guards
        then visit []
        else if not (subset guards current)
        then visit (union guards current)
    in
    let step other morphism guards path =
      if (forward && is_kept other) || is_outer other
      then
        add
          (if forward
           then Edge (x, morphism, other)
           else Edge (other, morphism, x))
          guards
          path
      else if internal other
      then reach other morphism guards path
    in
    let visit (node, morphism, guards, path) =
      (* Unless visited again since, with more guards. *)
      if List.equal
           phys_equal
           guards
           (Hashtbl.find_exn visited (node.id, Morphism.code morphism))
      then (
        if (not (List.is_empty guards)) && not (phys_equal node x)
        then
          add
            (if forward
             then Upper (x, Morphism.right morphism node.upper)
             else Lower (x, Morphism.left morphism node.lower))
            guards
            path;
        List.iter node.dormant ~f:(fun dormant ->
          match guards_of dormant with
          | None -> ()
          | Some dormant_guards ->
            let guards = union guards dormant_guards in
            (match dormant.kind, forward with
             | Upper (n, point), true when phys_equal n node ->
               add
                 (Upper (x, Morphism.right morphism point))
                 guards
                 (path @ dormant.dormant_reasons)
             | Lower (n, point), false when phys_equal n node ->
               add
                 (Lower (x, Morphism.left morphism point))
                 guards
                 (dormant.dormant_reasons @ path)
             | Edge (lo, edge_morphism, hi), true when phys_equal lo node ->
               step
                 hi
                 (Morphism.compose morphism edge_morphism)
                 guards
                 (path @ dormant.dormant_reasons)
             | Edge (lo, edge_morphism, hi), false when phys_equal hi node ->
               step
                 lo
                 (Morphism.compose edge_morphism morphism)
                 guards
                 (dormant.dormant_reasons @ path)
             | (Edge _ | Upper _ | Lower _), _ -> ()));
        if forward
        then
          List.iter node.succ ~f:(fun edge ->
            step
              edge.hi
              (Morphism.compose morphism edge.morphism)
              guards
              (path @ edge.reasons))
        else
          List.iter node.pred ~f:(fun edge ->
            step
              edge.lo
              (Morphism.compose edge.morphism morphism)
              guards
              (edge.reasons @ path)))
    in
    reach x (Morphism.identity x.axis) [] [];
    (* What paths not guarded reach first, so that guarded ones skip it. *)
    while not (Queue.is_empty unguarded) do
      visit (Queue.dequeue_exn unguarded)
    done;
    while not (Queue.is_empty guarded) do
      visit (Queue.dequeue_exn guarded)
    done
  in
  search ~forward:true;
  search ~forward:false;
  let found =
    Hashtbl.data found
    |> List.map ~f:(fun (kind, guards, path) ->
      { kind; guards; dormant_reasons = path; fired = false })
  in
  let implied_by (implied : implied list) other morphism =
    List.exists implied ~f:(fun implied ->
      phys_equal implied.other other
      && Morphism.le implied.implied_morphism morphism)
  in
  (* By [hi]: the [x <= m(hi)] another, to a kept node, implies with a
     constraint of that node's scheme. *)
  let through = Int.Table.create () in
  List.iter found ~f:(fun dormant ->
    match dormant.kind with
    | Edge (lo, morphism, mid) when phys_equal lo x && is_kept mid ->
      List.iter (above_of mid) ~f:(fun implied ->
        Hashtbl.add_multi
          through
          ~key:implied.other.id
          ~data:(dormant, Morphism.compose morphism implied.implied_morphism))
    | Edge _ | Upper _ | Lower _ -> ());
  let implied_through dormant =
    match dormant.kind with
    | Edge (lo, morphism, hi) when phys_equal lo x ->
      List.exists (Hashtbl.find_multi through hi.id) ~f:(fun (other, via) ->
        (not (phys_equal other dormant))
        && subset other.guards dormant.guards
        && Morphism.le via morphism)
    | Edge _ | Upper _ | Lower _ -> false
  in
  List.filter found ~f:(fun dormant ->
    let redundant =
      match dormant.kind with
      | Upper (_, point) -> point >= x.upper
      | Lower (_, point) -> point <= x.lower
      | Edge (lo, morphism, hi) ->
        if phys_equal lo x
        then
          (phys_equal hi x && Morphism.is_inflationary morphism)
          || implied_by above hi morphism
        else implied_by below lo morphism
    in
    not (redundant || implied_through dormant))
  |> List.sort ~compare:(fun a b ->
    Poly.compare (kind_key a.kind) (kind_key b.kind))
;;

let generalize nodes ~rank =
  Undo.assert_not_atomic "generalize";
  let kept =
    List.filter nodes ~f:(fun node ->
      match node.state with
      | Rank r -> r > rank
      | Generic _ -> false)
    |> List.dedup_and_sort ~compare:(fun a b -> Int.compare a.id b.id)
  in
  let kept_ids =
    Int.Hash_set.of_list (List.map kept ~f:(fun node -> node.id))
  in
  let fate node =
    fate node ~rank ~kept:(fun node -> Hash_set.mem kept_ids node.id)
  in
  let is_kept node = Fate.equal (fate node) Kept in
  let is_outer node = Fate.equal (fate node) Outer in
  (* Nodes of other schemes are passed through, as eliminated ones are. *)
  let internal node =
    match fate node with
    | Eliminated | Generic -> true
    | Outer | Kept -> false
  in
  let above = Int.Table.create ()
  and below = Int.Table.create () in
  List.iter kept ~f:(fun node ->
    Hashtbl.set
      above
      ~key:node.id
      ~data:
        (implied
           node
           ~forward:true
           ~stop:(fun other -> is_kept other || is_outer other)
           ~through:internal);
    Hashtbl.set
      below
      ~key:node.id
      ~data:(implied node ~forward:false ~stop:is_outer ~through:internal));
  let find table node =
    Hashtbl.find table node.id |> Option.value ~default:[]
  in
  (* The constraints [n <= m(y)] of the kept [y] by the outer [n]. *)
  let outer_below = Int.Table.create () in
  List.iter kept ~f:(fun y ->
    List.iter (find below y) ~f:(fun c ->
      if is_outer c.other
      then Hashtbl.add_multi outer_below ~key:c.other.id ~data:(y, c)));
  let remove_outer_below c =
    if is_outer c.other
    then
      Hashtbl.change outer_below c.other.id ~f:(fun current ->
        Option.map current ~f:(fun current ->
          List.filter current ~f:(fun (_, c') -> not (phys_equal c c'))))
  in
  (* The constraints [n <= m(other)] from [n] in the scheme, except [excluding].
     Edges between outer nodes are not used: a later generalization may make one
     end generic and the other not, so an instance would lose them. *)
  let steps n ~excluding =
    let keep c = not (phys_equal c excluding) in
    if is_kept n
    then
      List.filter_map (find above n) ~f:(fun c ->
        Option.some_if (keep c) (c.other, c.implied_morphism))
    else if is_outer n
    then
      List.filter_map (Hashtbl.find_multi outer_below n.id) ~f:(fun (y, c) ->
        Option.some_if (keep c) (y, c.implied_morphism))
    else []
  in
  (* Whether [src <= m(dst)] follows from the constraints other than
     [excluding]. *)
  let derivable ~excluding ~src ~dst m =
    let visited = Int.Table.create () in
    let rec go n acc =
      (phys_equal n dst && Morphism.le acc m)
      || ((not
             (List.mem (Hashtbl.find_multi visited n.id) acc ~equal:(fun a b ->
                Morphism.compare a b = 0)))
          &&
          (Hashtbl.add_multi visited ~key:n.id ~data:acc;
           List.exists (steps n ~excluding) ~f:(fun (n', e) ->
             go n' (Morphism.compose acc e))))
    in
    go src (Morphism.identity src.axis)
  in
  (* Drops, one at a time, the constraints the others imply. Each dropped one is
     implied by those remaining when it is dropped, so by the final ones. *)
  let reduce table node ~src ~dst =
    List.iter (find table node) ~f:(fun c ->
      if derivable ~excluding:c ~src:(src c) ~dst:(dst c) c.implied_morphism
      then (
        if phys_equal table below then remove_outer_below c;
        Hashtbl.update table node.id ~f:(fun current ->
          List.filter (Option.value current ~default:[]) ~f:(fun c' ->
            not (phys_equal c c')))))
  in
  List.iter kept ~f:(fun x ->
    reduce above x ~src:(fun _ -> x) ~dst:(fun c -> c.other);
    reduce below x ~src:(fun c -> c.other) ~dst:(fun _ -> x));
  let guarded =
    List.map kept ~f:(fun x ->
      ( x
      , guarded_implied
          x
          ~is_kept
          ~is_outer
          ~internal
          ~above:(find above x)
          ~below:(find below x)
          ~above_of:(find above) ))
  in
  List.iter guarded ~f:(fun (node, guarded) ->
    node.state
    <- Generic { above = find above node; below = find below node; guarded });
  (* A scheme is fixed once made: its instances add what it guards. *)
  List.iter kept ~f:(fun node ->
    List.iter node.dormant ~f:(fun dormant ->
      if not dormant.fired
      then (
        dormant.fired <- true;
        List.iter dormant.guards ~f:(fun layout ->
          layout.waiting
          <- List.filter layout.waiting ~f:(fun d -> not (phys_equal d dormant)))));
    node.dormant <- [])
;;

let reopen node ~rank =
  Undo.assert_not_atomic "reopen";
  match node.state with
  | Generic _ -> node.state <- Rank rank
  | Rank _ -> ()
;;

let create_generic ~var_id axes ~edges =
  let nodes = List.map axes ~f:(fun axis -> create ~var_id axis ~rank:0) in
  let node axis =
    List.find_exn nodes ~f:(fun node -> Axis.Any.equal node.axis axis)
  in
  let edges = edges node in
  List.iter edges ~f:(fun (lo, morphism, hi, reason) ->
    if Morphism.left morphism lo.lower > hi.lower
       || Morphism.right morphism hi.upper < lo.upper
    then raise_s [%message "Graph.create_generic: an edge tightens a bound"];
    let edge = { lo; morphism; hi; reasons = [ reason ] } in
    lo.succ <- edge :: lo.succ;
    hi.pred <- edge :: hi.pred);
  let implied ~f node = List.filter_map edges ~f:(fun edge -> f node edge) in
  List.iter nodes ~f:(fun node ->
    let above =
      implied node ~f:(fun node (lo, implied_morphism, hi, reason) ->
        Option.some_if
          (phys_equal lo node)
          { other = hi; implied_morphism; path = [ reason ] })
    and below =
      implied node ~f:(fun node (lo, implied_morphism, hi, reason) ->
        Option.some_if
          (phys_equal hi node)
          { other = lo; implied_morphism; path = [ reason ] })
    in
    node.state <- Generic { above; below; guarded = [] });
  node
;;

module Instance = struct
  type node = t

  let create_node = create

  type t =
    { ids : Id_source.t
    ; rank : int
    ; var_ids : int Int.Table.t
    ; nodes : node Int.Table.t
    }

  let create ids ~rank =
    { ids; rank; var_ids = Int.Table.create (); nodes = Int.Table.create () }
  ;;

  let var_id t var_id =
    Hashtbl.find_or_add t.var_ids var_id ~default:(fun () ->
      Id_source.next t.ids)
  ;;

  let rec instantiate t (node : node) =
    match node.state with
    | Rank _ -> node
    | Generic { above; below; guarded } ->
      (match Hashtbl.find t.nodes node.id with
       | Some copy -> copy
       | None ->
         let copy =
           { (create_node ~var_id:(var_id t node.var_id) node.axis ~rank:t.rank) with
             lower = node.lower
           ; lower_why = node.lower_why
           ; upper = node.upper
           ; upper_why = node.upper_why
           }
         in
         atomically (fun () ->
           Hashtbl.set t.nodes ~key:node.id ~data:copy;
           log (fun () -> Hashtbl.remove t.nodes node.id);
           let%bind.Result () =
             List.fold_result above ~init:() ~f:(fun () implied ->
               add_edge'
                 copy
                 implied.implied_morphism
                 (instantiate t implied.other)
                 implied.path)
           in
           let%bind.Result () =
             List.fold_result below ~init:() ~f:(fun () implied ->
               add_edge'
                 (instantiate t implied.other)
                 implied.implied_morphism
                 copy
                 implied.path)
           in
           List.fold_result guarded ~init:() ~f:(fun () dormant ->
             let kind =
               match dormant.kind with
               | Edge (lo, morphism, hi) ->
                 Edge (instantiate t lo, morphism, instantiate t hi)
               | Upper (_, point) -> Upper (copy, point)
               | Lower (_, point) -> Lower (copy, point)
             in
             guard kind ~guards:dormant.guards dormant.dormant_reasons))
         |> Mode_error.ok_exn;
         copy)
  ;;
end

let rank t =
  match t.state with
  | Rank rank -> Some rank
  | Generic _ -> None
;;

let var_id t = t.var_id

let above t =
  let seen = Int.Hash_set.create () in
  let found = Queue.create () in
  let todo = Stack.singleton t in
  while not (Stack.is_empty todo) do
    let node = Stack.pop_exn todo in
    if not (Hash_set.mem seen node.id)
    then (
      Hash_set.add seen node.id;
      Queue.enqueue found node;
      List.iter node.succ ~f:(fun edge -> Stack.push todo edge.hi))
  done;
  Queue.to_list found
;;

(* What a generic node's scheme adds to every instance, as a layout guarding it
   has pointers. *)
let holding t =
  match t.state with
  | Rank _ -> []
  | Generic { guarded; _ } ->
    List.filter_map guarded ~f:(fun dormant ->
      Option.some_if
        (List.exists (Layout.roots dormant.guards) ~f:Layout.has_pointers)
        dormant.kind)
;;

(* A generic node's constraints are those its scheme keeps, which imply the
   rest. *)
let scheme_lower t =
  List.fold (holding t) ~init:t.lower ~f:(fun lower -> function
    | Lower (_, point) -> Int.max lower point
    | Edge _ | Upper _ -> lower)
;;

let scheme_upper t =
  List.fold (holding t) ~init:t.upper ~f:(fun upper -> function
    | Upper (_, point) -> Int.min upper point
    | Edge _ | Lower _ -> upper)
;;

let forward t =
  match t.state with
  | Rank _ -> List.map t.succ ~f:(fun edge -> edge.morphism, edge.hi)
  | Generic { above; _ } ->
    List.map above ~f:(fun { other; implied_morphism; _ } ->
      implied_morphism, other)
    @ List.filter_map (holding t) ~f:(function
      | Edge (lo, morphism, hi) when phys_equal lo t -> Some (morphism, hi)
      | Edge _ | Upper _ | Lower _ -> None)
;;

let raised seeds =
  let estimates = Int.Table.create () in
  let raised = Int.Table.create () in
  let estimate node =
    Hashtbl.find estimates node.id |> Option.value ~default:node.lower
  in
  let nodes = Queue.create () in
  let raise_to node point =
    if point <= estimate node
    then Ok ()
    else if point > scheme_upper node
    then Error ()
    else (
      Hashtbl.set estimates ~key:node.id ~data:point;
      Hashtbl.set raised ~key:node.id ~data:node;
      Queue.enqueue nodes node;
      Ok ())
  in
  let rec propagate () =
    match Queue.dequeue nodes with
    | None -> Ok ()
    | Some node ->
      let%bind.Result () =
        List.fold_result (forward node) ~init:() ~f:(fun () (morphism, other) ->
          raise_to other (Morphism.left morphism (estimate node)))
      in
      propagate ()
  in
  match
    let%bind.Result () =
      List.fold_result seeds ~init:() ~f:(fun () (node, point) ->
        raise_to node point)
    in
    propagate ()
  with
  | Error () -> None
  | Ok () ->
    Some (Hashtbl.data raised |> List.map ~f:(fun node -> node, estimate node))
;;

(* The nodes [t]'s lower bound may come from, through edges and dormant edges
   not fired, and the layouts deciding whether those are. *)
let lower_sources t =
  let seen = Int.Hash_set.create () in
  let nodes = Queue.create () in
  let guards = Queue.create () in
  let todo = Stack.singleton t in
  while not (Stack.is_empty todo) do
    let node = Stack.pop_exn todo in
    if Hash_set.strict_add seen node.id |> Hash_set.Ok_or_duplicate.is_ok
    then (
      Queue.enqueue nodes node;
      List.iter node.pred ~f:(fun edge -> Stack.push todo edge.lo);
      List.iter node.dormant ~f:(fun dormant ->
        match dormant.kind with
        | _ when dormant.fired -> ()
        | Edge (lo, _, hi) when phys_equal hi node ->
          Queue.enqueue_all guards dormant.guards;
          Stack.push todo lo
        | Lower _ -> Queue.enqueue_all guards dormant.guards
        | Edge _ | Upper _ -> ()))
  done;
  ( Queue.to_list nodes
  , Layout.roots (Queue.to_list guards)
    |> List.filter ~f:(fun layout ->
      Option.is_none layout.bound
      &&
      match layout.layout_state with
      | Unknown -> true
      | Pointer_free | Has_pointers _ -> false) )
;;

let lower_assuming t ~raised =
  let estimates = Int.Table.create () in
  let nodes = Queue.create () in
  let todo = Stack.singleton t in
  while not (Stack.is_empty todo) do
    let node = Stack.pop_exn todo in
    if not (Hashtbl.mem estimates node.id)
    then (
      Hashtbl.set
        estimates
        ~key:node.id
        ~data:
          (match raised node with
           | Some point when not (is_generic node) -> Int.max node.lower point
           | Some _ | None -> node.lower);
      Queue.enqueue nodes node;
      List.iter node.pred ~f:(fun edge -> Stack.push todo edge.lo))
  done;
  while not (Queue.is_empty nodes) do
    let node = Queue.dequeue_exn nodes in
    let estimate = Hashtbl.find_exn estimates node.id in
    List.iter node.succ ~f:(fun edge ->
      match Hashtbl.find estimates edge.hi.id with
      | None -> ()
      | Some current ->
        let from = Morphism.left edge.morphism estimate in
        if from > current
        then (
          Hashtbl.set estimates ~key:edge.hi.id ~data:from;
          Queue.enqueue nodes edge.hi))
  done;
  Hashtbl.find_exn estimates t.id
;;

let axis t = t.axis

module Guarded = struct
  type node = t

  type nonrec kind = kind =
    | Edge of node * Morphism.t * node
    | Upper of node * int
    | Lower of node * int

  type t =
    { kind : kind
    ; guards : Layout.t list
    ; holds : bool
    }

  let among nodes =
    let ids = Int.Hash_set.of_list (List.map nodes ~f:(fun node -> node.id)) in
    let is_in node = Hash_set.mem ids node.id in
    let of_dormant (dormant : dormant) =
      let roots = Layout.roots dormant.guards in
      let holds = List.exists roots ~f:Layout.has_pointers in
      let guards =
        List.filter roots ~f:(fun layout ->
          Option.is_none layout.bound && Layout.is_unknown layout)
      in
      if holds || not (List.is_empty guards)
      then Some { kind = dormant.kind; guards; holds }
      else None
    in
    List.dedup_and_sort nodes ~compare:(fun a b -> Int.compare a.id b.id)
    |> List.concat_map ~f:(fun x ->
      let dormants =
        match x.state with
        | Generic { guarded; _ } -> guarded
        | Rank _ ->
          guarded_implied
            x
            ~is_kept:is_in
            ~is_outer:(fun _ -> false)
            ~internal:(fun node -> not (is_in node || is_generic node))
            ~above:[]
            ~below:[]
            ~above_of:(fun _ -> [])
      in
      List.filter_map dormants ~f:(fun (dormant : dormant) ->
        if List.for_all (kind_nodes dormant.kind) ~f:is_in
        then of_dormant dormant
        else None))
  ;;
end
