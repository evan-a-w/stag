open! Import

module Verdict = struct
  type t =
    | Implied
    | Not_implied
    | Unknown
  [@@deriving sexp_of]
end

type t = Verdict.t array array

(* The nodes no cycle is reachable from, by Tarjan's algorithm: it finds each
   strongly connected component after those it reaches. *)
let acyclic succ =
  let n = Array.length succ in
  let index = Array.create ~len:n (-1)
  and low = Array.create ~len:n 0
  and on_stack = Array.create ~len:n false
  and acyclic = Array.create ~len:n false in
  let next = ref 0 in
  let stack = Stack.create () in
  let rec visit v =
    index.(v) <- !next;
    low.(v) <- !next;
    incr next;
    Stack.push stack v;
    on_stack.(v) <- true;
    Array.iter succ.(v) ~f:(fun (w, _) ->
      if index.(w) < 0
      then (
        visit w;
        low.(v) <- Int.min low.(v) low.(w))
      else if on_stack.(w)
      then low.(v) <- Int.min low.(v) index.(w));
    if low.(v) = index.(v)
    then (
      let rec pop members =
        let w = Stack.pop_exn stack in
        on_stack.(w) <- false;
        let members = w :: members in
        if w = v then members else pop members
      in
      let members = pop [] in
      let is_acyclic =
        match members with
        | [ _ ] ->
          Array.for_all succ.(v) ~f:(fun (w, _) -> w <> v && acyclic.(w))
        | _ -> false
      in
      List.iter members ~f:(fun w -> acyclic.(w) <- is_acyclic))
  in
  for v = 0 to n - 1 do
    if index.(v) < 0 then visit v
  done;
  acyclic
;;

let work = Work.create "mode.graph"

(* A search from each node, recording for each node and morphism it reaches
   through which of the node's constraints it does: [many] when through several.
   [src <= m(dst)] is implied by the others when they reach [dst], at a morphism
   at most [m]. From a node no cycle is reachable from, a constraint implied is
   implied through constraints strictly nested in it (between nodes it reaches,
   or one parallel to it at a smaller morphism), which can replace it in any
   derivation of another, never nested in it in turn: so it is implied whichever
   of the others are dropped first. *)
let create ~succ =
  let acyclic = acyclic succ in
  let many = -1 in
  let reached : (Morphism.t * int ref) list array =
    Array.create ~len:(Array.length succ) []
  in
  let touched = Stack.create () in
  let todo = Queue.create () in
  let reach w morphism first =
    Work.incr work;
    match
      List.find reached.(w) ~f:(fun (morphism', _) ->
        Morphism.code morphism' = Morphism.code morphism)
    with
    | None ->
      if List.is_empty reached.(w) then Stack.push touched w;
      let label = ref first in
      reached.(w) <- (morphism, label) :: reached.(w);
      Queue.enqueue todo (w, morphism, label)
    | Some (_, label) ->
      if !label <> many && !label <> first
      then (
        label := many;
        Queue.enqueue todo (w, morphism, label))
  in
  Array.mapi succ ~f:(fun src constraints ->
    Array.iteri constraints ~f:(fun i (w, morphism) -> reach w morphism i);
    while not (Queue.is_empty todo) do
      let v, morphism, label = Queue.dequeue_exn todo in
      let first = !label in
      Array.iter succ.(v) ~f:(fun (w, e) ->
        reach w (Morphism.compose morphism e) first)
    done;
    let verdicts =
      Array.mapi constraints ~f:(fun i (dst, morphism) : Verdict.t ->
        if dst = src && Morphism.is_inflationary morphism
        then Implied
        else if List.exists reached.(dst) ~f:(fun (morphism', label) ->
                  !label <> i && Morphism.le morphism' morphism)
        then if acyclic.(src) then Implied else Unknown
        else Not_implied)
    in
    Stack.until_empty touched (fun w -> reached.(w) <- []);
    verdicts)
;;

let verdict t ~src i = t.(src).(i)
