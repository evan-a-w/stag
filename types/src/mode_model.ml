open! Import

module Edge = struct
  type t =
    { lo : Mode_node.t
    ; morphism : Mode.Morphism.t
    ; hi : Mode_node.t
    }
end

module Item = struct
  type t =
    | Edges of Edge.t list
    | Lower of Mode_node.t * int
    | Upper of Mode_node.t * int
end

(* The nodes by index, and each item's edges in adjacency lists, with whether
   the item is kept. A raise is propagated over [base] levels sparsely: a node's
   level is [over.(i)] if [stamp.(i) = generation], else [base.(i)]. *)
type t =
  { closure : Mode_closure.t
  ; nodes : Mode_node.t array
  ; index : Mode_node.t -> int
  ; find : Mode_node.t -> int option
  ; real_lows : int array
  ; base_lower : int array
  ; kept : bool array (* By item; the implicit edges' item is the last. *)
  ; forward : (int * Mode.Morphism.t * int) list array
  ; backward : (int * Mode.Morphism.t * int) list array
  ; lower : (int * int) list array (* Bounds, as (item, point), by node. *)
  ; upper : (int * int) list array
  ; over : int array
  ; stamp : int array
  ; mutable generation : int
  ; queue : int Queue.t
  }

let create closure ~nodes ~base_lower ~implicit items =
  let nodes = Array.of_list nodes in
  let table = Mode_node.Table.create () in
  Array.iteri nodes ~f:(fun i n -> Hashtbl.set table ~key:n ~data:i);
  let index = Hashtbl.find_exn table in
  let size = Array.length nodes in
  let items = Array.of_list (items @ [ Item.Edges implicit ]) in
  let t =
    { closure
    ; nodes
    ; index
    ; find = Hashtbl.find table
    ; real_lows = Array.map nodes ~f:(Mode_closure.lower closure)
    ; base_lower = Array.map nodes ~f:base_lower
    ; kept = Array.create ~len:(Array.length items) true
    ; forward = Array.create ~len:size []
    ; backward = Array.create ~len:size []
    ; lower = Array.create ~len:size []
    ; upper = Array.create ~len:size []
    ; over = Array.create ~len:size 0
    ; stamp = Array.create ~len:size 0
    ; generation = 0
    ; queue = Queue.create ()
    }
  in
  Array.iteri items ~f:(fun item -> function
    | Edges edges ->
      List.iter edges ~f:(fun { Edge.lo; morphism; hi } ->
        let lo = index lo
        and hi = index hi in
        t.forward.(lo) <- (item, morphism, hi) :: t.forward.(lo);
        t.backward.(hi) <- (item, morphism, lo) :: t.backward.(hi))
    | Lower (n, p) -> t.lower.(index n) <- (item, p) :: t.lower.(index n)
    | Upper (n, p) -> t.upper.(index n) <- (item, p) :: t.upper.(index n));
  t, items
;;

let bound t bounds ~init ~f =
  List.fold bounds ~init ~f:(fun acc (item, p) ->
    if t.kept.(item) then f acc p else acc)
;;

let size t = Array.length t.nodes

(* The least levels the kept items give, from [init], raised along the kept
   edges until nothing changes. *)
let fixpoint t ~init ~edges ~step ~better =
  let levels = Array.init (size t) ~f:init in
  let queue = Queue.of_list (List.init (size t) ~f:Fn.id) in
  while not (Queue.is_empty queue) do
    let from = Queue.dequeue_exn queue in
    List.iter (edges from) ~f:(fun (item, m, other) ->
      if t.kept.(item)
      then (
        let point = step m levels.(from) in
        if better point levels.(other)
        then (
          levels.(other) <- point;
          Queue.enqueue queue other)))
  done;
  levels
;;

let lows t =
  fixpoint
    t
    ~init:(fun i -> bound t t.lower.(i) ~init:t.base_lower.(i) ~f:Int.max)
    ~edges:(fun i -> t.forward.(i))
    ~step:Mode.Morphism.left
    ~better:( > )
;;

let ups t =
  fixpoint
    t
    ~init:(fun i ->
      bound t t.upper.(i) ~init:(Mode_node.max t.nodes.(i).axis) ~f:Int.min)
    ~edges:(fun i -> t.backward.(i))
    ~step:Mode.Morphism.right
    ~better:( < )
;;

(* Raises [x] to [p] over [base], until [stop] holds; then [level] reads the
   levels raised, and [touched] the nodes raised, until the next raise. *)
let raise_ t base x p ~stop =
  t.generation <- t.generation + 1;
  let level i = if t.stamp.(i) = t.generation then t.over.(i) else base.(i) in
  let set i point =
    t.over.(i) <- point;
    t.stamp.(i) <- t.generation
  in
  let touched = ref [] in
  if p > level x
  then (
    set x p;
    touched := [ x ];
    Queue.clear t.queue;
    Queue.enqueue t.queue x;
    while (not (Queue.is_empty t.queue)) && not (stop level) do
      let lo = Queue.dequeue_exn t.queue in
      List.iter t.forward.(lo) ~f:(fun (item, m, hi) ->
        if t.kept.(item)
        then (
          let point = Mode.Morphism.left m (level lo) in
          if point > level hi
          then (
            if t.stamp.(hi) <> t.generation then touched := hi :: !touched;
            set hi point;
            Queue.enqueue t.queue hi)))
    done);
  level, !touched
;;

let real_lower t i = t.real_lows.(i)
let real_upper t i = Mode_closure.upper t.closure t.nodes.(i)
let domain t i = Mode_closure.domain t.closure t.nodes.(i)

let implied t (item : Item.t) =
  match item with
  | Edges edges ->
    (* While the rest are exact, an edge raising nothing above [0] at its
       source's lower bound adds nothing to the lower bounds. *)
    let lows =
      if List.exists edges ~f:(fun { lo; morphism; hi = _ } ->
           Mode.Morphism.left morphism (real_lower t (t.index lo)) > 0)
      then lows t
      else t.real_lows
    in
    List.for_all edges ~f:(fun { lo; morphism; hi } ->
      let lo = t.index lo
      and hi = t.index hi in
      List.for_all (domain t lo) ~f:(fun p ->
        let need = Mode.Morphism.left morphism p in
        let level, _ =
          raise_ t lows lo p ~stop:(fun level -> level hi >= need)
        in
        level hi >= need))
  | Lower _ -> Array.for_alli (lows t) ~f:(fun i p -> p = real_lower t i)
  | Upper _ -> Array.for_alli (ups t) ~f:(fun i p -> p = real_upper t i)
;;

let check_exact t =
  let fail what =
    raise_s
      [%message
        "Mode_model: the printed constraints are not exact" (what : Sexp.t)]
  in
  let lows = lows t in
  let ups = ups t in
  Array.iteri t.nodes ~f:(fun i n ->
    if lows.(i) <> real_lower t i || ups.(i) <> real_upper t i
    then
      fail
        [%message
          "bounds"
            (n : Mode_node.t)
            (lows.(i) : int)
            (ups.(i) : int)
            (real_lower t i : int)
            (real_upper t i : int)]);
  (* The levels being the real ones, a raise needs comparing only where either
     raises. *)
  Array.iteri t.nodes ~f:(fun i x ->
    List.iter (domain t i) ~f:(fun p ->
      let real = Int.Table.create () in
      List.iter (Mode_closure.raised_alist t.closure x p) ~f:(fun (y, point) ->
        Option.iter (t.find y) ~f:(fun j ->
          if point > lows.(j)
          then
            Hashtbl.update real j ~f:(fun current ->
              Int.max point (Option.value current ~default:point))));
      let level, touched = raise_ t lows i p ~stop:(fun _ -> false) in
      List.iter
        (touched @ Hashtbl.keys real)
        ~f:(fun j ->
          let real = Hashtbl.find real j |> Option.value ~default:lows.(j) in
          if level j <> real
          then
            fail
              [%message
                "raise"
                  (x : Mode_node.t)
                  (p : int)
                  ~y:(t.nodes.(j) : Mode_node.t)
                  ~level:(level j : int)
                  (real : int)])))
;;

let reduce closure ~nodes ~base_lower ~implicit items ~item =
  let t, kinds =
    create closure ~nodes ~base_lower ~implicit (List.map items ~f:item)
  in
  List.iteri items ~f:(fun i _ ->
    t.kept.(i) <- false;
    if not (implied t kinds.(i)) then t.kept.(i) <- true);
  check_exact t;
  List.filteri items ~f:(fun i _ -> t.kept.(i))
;;
