open! Import

let print successors =
  Scc.topological_components ~successors:(Array.of_list successors)
  |> [%sexp_of: int list list]
  |> print_s
;;

let%expect_test "empty" =
  print [];
  [%expect {| () |}]
;;

let%expect_test "self loops and singletons" =
  print [ [ 0 ]; []; [ 2 ] ];
  [%expect {| ((0) (1) (2)) |}]
;;

let%expect_test "cycles" =
  print [ [ 1 ]; [ 2 ]; [ 0 ] ];
  print [ [ 3 ]; [ 2 ]; [ 1; 0 ]; [ 0 ] ];
  print [ [ 1 ]; [ 0; 2 ]; [ 3 ]; [ 2; 4 ]; [] ];
  [%expect
    {|
    ((0 1 2))
    ((0 3) (1 2))
    ((4) (2 3) (0 1))
    |}]
;;

let%expect_test "dependencies come first, otherwise smallest vertex first" =
  print [ []; []; []; [] ];
  print [ [ 3 ]; []; [ 1 ]; [] ];
  print [ [ 2 ]; [ 2 ]; [] ];
  print [ [ 1; 2 ]; [ 3 ]; [ 3 ]; [] ];
  [%expect
    {|
    ((0) (1) (2) (3))
    ((1) (2) (3) (0))
    ((2) (0) (1))
    ((3) (1) (2) (0))
    |}]
;;

let%expect_test "disconnected" =
  print [ [ 1 ]; [ 0 ]; [ 3 ]; [ 2 ]; [] ];
  print [ [ 4 ]; [ 3 ]; [ 0 ]; [ 1 ]; [ 2 ] ];
  [%expect
    {|
    ((0 1) (2 3) (4))
    ((0 2 4) (1 3))
    |}]
;;

(* The specification, quadratically: components by reachability, then
   repeatedly the ready one of smallest vertex. *)
let reference successors =
  let n = Array.length successors in
  let reaches =
    Array.init n ~f:(fun v ->
      let seen = Array.create ~len:n false in
      let rec visit v =
        List.iter successors.(v) ~f:(fun w ->
          if not seen.(w)
          then (
            seen.(w) <- true;
            visit w))
      in
      visit v;
      seen)
  in
  let same v w = v = w || (reaches.(v).(w) && reaches.(w).(v)) in
  let component v = List.filter (List.init n ~f:Fn.id) ~f:(same v) in
  let rec loop ~done_ acc =
    match
      List.find (List.init n ~f:Fn.id) ~f:(fun v ->
        (not (Set.mem done_ v))
        && List.for_all (component v) ~f:(fun u ->
          List.for_all successors.(u) ~f:(fun w -> same v w || Set.mem done_ w)))
    with
    | None -> List.rev acc
    | Some v ->
      loop
        ~done_:(Set.union done_ (Int.Set.of_list (component v)))
        (component v :: acc)
  in
  loop ~done_:Int.Set.empty []
;;

let%expect_test "agrees with the specification on random graphs" =
  let random = Random.State.make [| 0 |] in
  let disagreements =
    List.init 500 ~f:(fun _ ->
      let n = Random.State.int random 9 in
      let successors =
        Array.init n ~f:(fun _ ->
          List.init (Random.State.int random 3) ~f:(fun _ ->
            Random.State.int random n))
      in
      successors)
    |> List.filter ~f:(fun successors ->
      not
        ([%equal: int list list]
           (Scc.topological_components ~successors)
           (reference successors)))
  in
  print_s [%sexp (disagreements : int list array list)];
  [%expect {| () |}]
;;

let%expect_test "large graphs" =
  let n = 200_000 in
  let chain = Array.init n ~f:(fun v -> if v = 0 then [] else [ v - 1 ]) in
  let components = Scc.topological_components ~successors:chain in
  print_s
    [%sexp
      (List.length components : int), (List.hd components : int list option)];
  let cycle = Array.init n ~f:(fun v -> [ (v + 1) % n ]) in
  let components = Scc.topological_components ~successors:cycle in
  print_s [%sexp (List.map components ~f:List.length : int list)];
  [%expect
    {|
    (200000 ((0)))
    (200000)
    |}]
;;
