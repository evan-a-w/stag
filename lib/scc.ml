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

let topological_components ~successors =
  let n = Array.length successors in
  let component, count = tarjan ~successors in
  let members = Array.create ~len:count [] in
  for v = n - 1 downto 0 do
    members.(component.(v)) <- v :: members.(component.(v))
  done;
  let component_deps =
    Array.map members ~f:(fun vs ->
      List.concat_map vs ~f:(fun v ->
        List.map successors.(v) ~f:(fun w -> component.(w))))
  in
  let done_ = Array.create ~len:count false in
  let rec loop acc =
    let ready =
      List.find (List.init n ~f:Fn.id) ~f:(fun v ->
        let c = component.(v) in
        (not done_.(c))
        && List.for_all component_deps.(c) ~f:(fun d -> d = c || done_.(d)))
    in
    match ready with
    | None -> List.rev acc
    | Some v ->
      let c = component.(v) in
      done_.(c) <- true;
      loop (members.(c) :: acc)
  in
  loop []
;;
