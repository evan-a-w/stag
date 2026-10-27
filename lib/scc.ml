open! Core

let tarjan ~successors =
  let n = Array.length successors in
  let component = Array.create ~len:n (-1) in
  let lowlink = Array.create ~len:n 0 in
  let order = Array.create ~len:n (-1) in
  let stack = Stack.create () in
  let next = ref 0 in
  let components = ref 0 in
  let rec visit v =
    order.(v) <- !next;
    lowlink.(v) <- !next;
    incr next;
    Stack.push stack v;
    List.iter successors.(v) ~f:(fun w ->
      if order.(w) < 0
      then (
        visit w;
        lowlink.(v) <- Int.min lowlink.(v) lowlink.(w))
      else if component.(w) < 0
      then lowlink.(v) <- Int.min lowlink.(v) order.(w));
    if lowlink.(v) = order.(v)
    then (
      let rec pop () =
        let w = Stack.pop_exn stack in
        component.(w) <- !components;
        if w <> v then pop ()
      in
      pop ();
      incr components)
  in
  for v = 0 to n - 1 do
    if order.(v) < 0 then visit v
  done;
  component, !components
;;

(* Kahn's algorithm on the components, taking the ready one of smallest vertex
   first. *)
let topological_components ~successors =
  let n = Array.length successors in
  let component, count = tarjan ~successors in
  let members = Array.create ~len:count [] in
  for v = n - 1 downto 0 do
    members.(component.(v)) <- v :: members.(component.(v))
  done;
  let dependents = Array.create ~len:count [] in
  let pending = Array.create ~len:count 0 in
  Array.iteri successors ~f:(fun v ws ->
    List.iter ws ~f:(fun w ->
      let c = component.(v) in
      let d = component.(w) in
      if c <> d
      then (
        dependents.(d) <- c :: dependents.(d);
        pending.(c) <- pending.(c) + 1)));
  let smallest c = List.hd_exn members.(c) in
  let ready = ref Int.Set.empty in
  for c = 0 to count - 1 do
    if pending.(c) = 0 then ready := Set.add !ready (smallest c)
  done;
  let rec loop acc =
    match Set.min_elt !ready with
    | None -> List.rev acc
    | Some v ->
      ready := Set.remove !ready v;
      let c = component.(v) in
      List.iter dependents.(c) ~f:(fun d ->
        pending.(d) <- pending.(d) - 1;
        if pending.(d) = 0 then ready := Set.add !ready (smallest d));
      loop (members.(c) :: acc)
  in
  loop []
;;
