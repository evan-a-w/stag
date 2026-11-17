open! Import

let%expect_test "pairs are unordered, and pack both ids" =
  let show id1 id2 =
    let pair = Id_pair.create id1 id2 in
    print_s
      [%message
        (id1 : int)
          (id2 : int)
          (pair : Id_pair.t)
          ~same_swapped:
            ([%equal: int] (pair :> int) (Id_pair.create id2 id1 :> int) : bool)]
  in
  show 0 0;
  show 3 7;
  show 7 3;
  show 0 Id_pair.max_id;
  show Id_pair.max_id Id_pair.max_id;
  [%expect
    {|
    ((id1 0) (id2 0) (pair (0 0)) (same_swapped true))
    ((id1 3) (id2 7) (pair (3 7)) (same_swapped true))
    ((id1 7) (id2 3) (pair (3 7)) (same_swapped true))
    ((id1 0) (id2 2147483647) (pair (0 2147483647)) (same_swapped true))
    ((id1 2147483647) (id2 2147483647) (pair (2147483647 2147483647))
     (same_swapped true))
    |}]
;;

let%expect_test "distinct pairs pack to distinct ints" =
  let ids = [ 0; 1; 2; 1 lsl 16; Id_pair.max_id - 1; Id_pair.max_id ] in
  let pairs =
    List.concat_map ids ~f:(fun id1 ->
      List.filter_map ids ~f:(fun id2 ->
        Option.some_if (id1 <= id2) (Id_pair.create id1 id2 :> int)))
  in
  print_s
    [%message
      (List.length pairs : int)
        ~distinct:
          (List.length (List.dedup_and_sort pairs ~compare:Int.compare) : int)];
  [%expect {| (("List.length pairs" 21) (distinct 21)) |}]
;;

let%expect_test "ids out of range are rejected" =
  let try_ id1 id2 =
    print_s
      [%sexp
        (Or_error.try_with (fun () -> Id_pair.create id1 id2)
         : Id_pair.t Or_error.t)]
  in
  try_ (-1) 0;
  try_ 0 (-1);
  try_ (Id_pair.max_id + 1) 0;
  try_ 0 (Id_pair.max_id + 1);
  [%expect
    {|
    (Error ("Id_pair.create: id out of range" (id1 -1) (id2 0)))
    (Error ("Id_pair.create: id out of range" (id1 0) (id2 -1)))
    (Error ("Id_pair.create: id out of range" (id1 2147483648) (id2 0)))
    (Error ("Id_pair.create: id out of range" (id1 0) (id2 2147483648)))
    |}]
;;

let%expect_test "the hash set adds each pair once, in either order" =
  let set = Id_pair.Hash_set.create () in
  List.iter
    [ 1, 2; 2, 1; 1, 3; 3, 3; 3, 1; 2, 1 ]
    ~f:(fun (id1, id2) ->
      let present = Id_pair.Hash_set.mem_or_add set (Id_pair.create id1 id2) in
      print_s [%message (id1 : int) (id2 : int) (present : bool)]);
  [%expect
    {|
    ((id1 1) (id2 2) (present false))
    ((id1 2) (id2 1) (present true))
    ((id1 1) (id2 3) (present false))
    ((id1 3) (id2 3) (present false))
    ((id1 3) (id2 1) (present true))
    ((id1 2) (id2 1) (present true))
    |}]
;;
