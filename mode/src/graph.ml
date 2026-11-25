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
      }

and implied =
  { other : t
  ; implied_morphism : Morphism.t
  ; path : Reason.t list
  }

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

let sexp_of_t t =
  match t.state with
  | Rank _ -> Sexp.List [ Atom (name t); sexp_of_bounds t ]
  | Generic { above; below } ->
    let constraints =
      List.map above ~f:(fun { other; implied_morphism; _ } ->
        Sexp.List
          [ Atom (name t); Atom "<="; sexp_of_applied implied_morphism other ])
      @ List.map below ~f:(fun { other; implied_morphism; _ } ->
        Sexp.List
          [ Atom (name other); Atom "<="; sexp_of_applied implied_morphism t ])
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

let add_edge lo morphism hi reason = add_edge' lo morphism hi [ reason ]
let at_least t point reason = raise_lower t point (Bound reason)
let at_most t point reason = lower_upper t point (Bound reason)
let defaulted = Reason.create "defaulted"

let default_to_lower t =
  atomically (fun () -> lower_upper t t.lower (Bound defaulted))
  |> Mode_error.ok_exn
;;

let default_to_upper t =
  atomically (fun () -> raise_lower t t.upper (Bound defaulted))
  |> Mode_error.ok_exn
;;

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

let generalize nodes ~rank =
  Undo.assert_not_atomic "generalize";
  let is_outer node =
    match node.state with
    | Rank r -> r <= rank
    | Generic _ -> false
  in
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
  let is_kept node = Hash_set.mem kept_ids node.id in
  let internal node = not (is_kept node || is_outer node) in
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
  List.iter kept ~f:(fun node ->
    node.state <- Generic { above = find above node; below = find below node })
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
    | Generic { above; below } ->
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
           List.fold_result below ~init:() ~f:(fun () implied ->
             add_edge'
               (instantiate t implied.other)
               implied.implied_morphism
               copy
               implied.path))
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

(* A generic node's constraints are those its scheme keeps, which imply the
   rest. *)
let forward t =
  match t.state with
  | Rank _ -> List.map t.succ ~f:(fun edge -> edge.morphism, edge.hi)
  | Generic { above; _ } ->
    List.map above ~f:(fun { other; implied_morphism; _ } ->
      implied_morphism, other)
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
    else if point > node.upper
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

let lower_assuming t ~raised =
  let estimates = Int.Table.create () in
  let nodes = Queue.create () in
  let rec collect node =
    if not (Hashtbl.mem estimates node.id)
    then (
      Hashtbl.set
        estimates
        ~key:node.id
        ~data:
          (if raised node.var_id && not (is_generic node)
           then node.upper
           else node.lower);
      Queue.enqueue nodes node;
      List.iter node.pred ~f:(fun edge -> collect edge.lo))
  in
  collect t;
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
