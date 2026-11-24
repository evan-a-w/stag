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
   the item is kept. *)
type t =
  { closure : Mode_closure.t
  ; nodes : Mode_node.t array
  ; index : Mode_node.t -> int
  ; dead : bool array
  ; base_lower : int array
  ; kept : bool array (* By item; the implicit edges' item is the last. *)
  ; forward : (int * Mode.Morphism.t * int) list array
  ; backward : (int * Mode.Morphism.t * int) list array
  ; lower : (int * int) list array (* Bounds, as (item, point), by node. *)
  ; upper : (int * int) list array
  }

let create closure ~nodes ~dead ~base_lower ~implicit items =
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
    ; dead = Array.map nodes ~f:dead
    ; base_lower = Array.map nodes ~f:base_lower
    ; kept = Array.create ~len:(Array.length items) true
    ; forward = Array.create ~len:size []
    ; backward = Array.create ~len:size []
    ; lower = Array.create ~len:size []
    ; upper = Array.create ~len:size []
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

(* Raises [levels] along the kept edges from [start] until nothing changes. *)
let propagate t levels ~start =
  let queue = Queue.of_list start in
  while not (Queue.is_empty queue) do
    let lo = Queue.dequeue_exn queue in
    List.iter t.forward.(lo) ~f:(fun (item, m, hi) ->
      if t.kept.(item)
      then (
        let point = Mode.Morphism.left m levels.(lo) in
        if point > levels.(hi)
        then (
          levels.(hi) <- point;
          Queue.enqueue queue hi)))
  done
;;

let all t = List.init (Array.length t.nodes) ~f:Fn.id

let lows t =
  let levels =
    Array.mapi t.nodes ~f:(fun i _ ->
      bound t t.lower.(i) ~init:t.base_lower.(i) ~f:Int.max)
  in
  propagate t levels ~start:(all t);
  levels
;;

let ups t =
  let levels =
    Array.mapi t.nodes ~f:(fun i n ->
      bound t t.upper.(i) ~init:(Mode_node.max n.axis) ~f:Int.min)
  in
  let queue = Queue.of_list (all t) in
  while not (Queue.is_empty queue) do
    let hi = Queue.dequeue_exn queue in
    List.iter t.backward.(hi) ~f:(fun (item, m, lo) ->
      if t.kept.(item)
      then (
        let point = Mode.Morphism.right m levels.(hi) in
        if point < levels.(lo)
        then (
          levels.(lo) <- point;
          Queue.enqueue queue lo)))
  done;
  levels
;;

let raised t lows x p =
  let levels = Array.copy lows in
  levels.(x) <- Int.max levels.(x) p;
  propagate t levels ~start:[ x ];
  levels
;;

let real_lower t i = Mode_closure.lower t.closure t.nodes.(i)
let real_upper t i = Mode_closure.upper t.closure t.nodes.(i)

(* What a node may be raised to from outside: nothing, if dead. A dead node's
   own level is never compared either: only what its edges raise. *)
let domain t i =
  if t.dead.(i) then [] else Mode_closure.domain t.closure t.nodes.(i)
;;

let implied t (item : Item.t) =
  match item with
  | Edges edges ->
    let lows = lows t in
    List.for_all edges ~f:(fun { lo; morphism; hi } ->
      let lo = t.index lo
      and hi = t.index hi in
      t.dead.(hi)
      || List.for_all (domain t lo) ~f:(fun p ->
        (raised t lows lo p).(hi) >= Mode.Morphism.left morphism p))
  | Lower _ ->
    Array.for_alli (lows t) ~f:(fun i p -> t.dead.(i) || p = real_lower t i)
  | Upper _ ->
    Array.for_alli (ups t) ~f:(fun i p -> t.dead.(i) || p = real_upper t i)
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
    if (not t.dead.(i))
       && (lows.(i) <> real_lower t i || ups.(i) <> real_upper t i)
    then
      fail
        [%message
          "bounds"
            (n : Mode_node.t)
            (lows.(i) : int)
            (ups.(i) : int)
            (real_lower t i : int)
            (real_upper t i : int)]);
  Array.iteri t.nodes ~f:(fun i x ->
    List.iter (domain t i) ~f:(fun p ->
      let levels = raised t lows i p in
      Array.iteri t.nodes ~f:(fun j y ->
        let real = Mode_closure.raise t.closure x p y in
        if (not t.dead.(j)) && levels.(j) <> real
        then
          fail
            [%message
              "raise"
                (x : Mode_node.t)
                (p : int)
                (y : Mode_node.t)
                (levels.(j) : int)
                (real : int)])))
;;

let reduce closure ~nodes ~dead ~base_lower ~implicit items ~item =
  let t, kinds =
    create closure ~nodes ~dead ~base_lower ~implicit (List.map items ~f:item)
  in
  List.iteri items ~f:(fun i _ ->
    t.kept.(i) <- false;
    if not (implied t kinds.(i)) then t.kept.(i) <- true);
  check_exact t;
  List.filteri items ~f:(fun i _ -> t.kept.(i))
;;
