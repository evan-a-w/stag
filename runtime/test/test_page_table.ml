open! Import
module Addr = Memory.Addr

let page_bytes = 4096

(* Pages are never accessed, so any aligned word will do. Owners are pages too,
   apart from the ones they own. *)
let page n = Addr.of_word (Nativeint_u.of_int (n * page_bytes))
let owner n = page (n + 1_000_000)

let owner_of t addr =
  let found = Page_table.find t addr in
  if Addr.is_null found
  then None
  else Some ((Nativeint_u.to_int (Addr.to_word found) / page_bytes) - 1_000_000)
;;

let print_table t pages =
  print_s
    [%message
      ""
        ~length:(Page_table.length t : int)
        ~capacity:(Page_table.capacity t : int)
        ~pages:
          (List.map pages ~f:(fun n ->
             ( n
             , Page_table.For_testing.home t ~page:(page n)
             , Page_table.For_testing.slot t ~page:(page n)
             , owner_of t (page n) ))
           : (int * int * int option * int option) list)]
;;

(* The first [count] pages whose home is [home] in a fresh table. *)
let colliding ~home ~count =
  let t = Page_table.create ~page_bytes in
  Sequence.unfold ~init:1 ~f:(fun n -> Some (n, n + 1))
  |> Sequence.filter ~f:(fun n ->
    Page_table.For_testing.home t ~page:(page n) = home)
  |> Fn.flip Sequence.take count
  |> Sequence.to_list
;;

let%expect_test "add, find, remove" =
  let t = Page_table.create ~page_bytes in
  List.iter [ 1; 2; 3 ] ~f:(fun n ->
    Page_table.add t ~page:(page n) ~owner:(owner n));
  print_table t [ 1; 2; 3; 4 ];
  (* Any address in the page finds it. *)
  print_s
    [%sexp
      (List.map [ 0; 1; 8; 4095; 4096 ] ~f:(fun offset ->
         offset, owner_of t (Addr.add (page 2) offset))
       : (int * int option) list)];
  Page_table.remove t ~page:(page 2);
  print_table t [ 1; 2; 3 ];
  [%expect
    {|
    ((length 3) (capacity 16)
     (pages ((1 9 (9) (1)) (2 3 (3) (2)) (3 13 (13) (3)) (4 7 () ()))))
    ((0 (2)) (1 (2)) (8 (2)) (4095 (2)) (4096 (3)))
    ((length 2) (capacity 16)
     (pages ((1 9 (9) (1)) (2 3 () ()) (3 13 (13) (3)))))
    |}]
;;

let%expect_test "errors" =
  let t = Page_table.create ~page_bytes in
  Page_table.add t ~page:(page 1) ~owner:(owner 1);
  let show f = print_s [%sexp (Or_error.try_with f : unit Or_error.t)] in
  show (fun () -> Page_table.add t ~page:(page 1) ~owner:(owner 2));
  show (fun () -> Page_table.add t ~page:(Addr.add (page 3) 8) ~owner:(owner 3));
  show (fun () -> Page_table.add t ~page:(page 4) ~owner:(Addr.add (owner 4) 8));
  show (fun () -> Page_table.remove t ~page:(page 2));
  show (fun () -> Page_table.remove t ~page:(Addr.add (page 1) 1));
  List.iter [ 0; 4; 12; 4097 ] ~f:(fun page_bytes ->
    show (fun () -> ignore (Page_table.create ~page_bytes : Page_table.t)));
  [%expect
    {|
    (Error "Page_table.add: page already has an owner")
    (Error ("Page_table: unaligned" page))
    (Error ("Page_table: unaligned" owner))
    (Error "Page_table.remove: page has no owner")
    (Error ("Page_table: unaligned" page))
    (Error (Page_table.create (page_bytes 0)))
    (Error (Page_table.create (page_bytes 4)))
    (Error (Page_table.create (page_bytes 12)))
    (Error (Page_table.create (page_bytes 4097)))
    |}]
;;

let%expect_test "collisions and deletion chains" =
  let t = Page_table.create ~page_bytes in
  let home = 15 in
  (* Wrapping around the end of the table. *)
  let pages = colliding ~home ~count:3 in
  List.iter pages ~f:(fun n -> Page_table.add t ~page:(page n) ~owner:(owner n));
  print_table t pages;
  (* Removing the first shifts the others back; removing the middle one of the
     remaining shifts the last. *)
  Page_table.remove t ~page:(page (List.nth_exn pages 0));
  print_table t pages;
  Page_table.remove t ~page:(page (List.nth_exn pages 1));
  print_table t pages;
  [%expect
    {|
    ((length 3) (capacity 16)
     (pages ((8 15 (15) (8)) (21 15 (0) (21)) (42 15 (1) (42)))))
    ((length 2) (capacity 16)
     (pages ((8 15 () ()) (21 15 (15) (21)) (42 15 (0) (42)))))
    ((length 1) (capacity 16)
     (pages ((8 15 () ()) (21 15 () ()) (42 15 (15) (42)))))
    |}]
;;

let%expect_test "an entry whose home is past the hole stays" =
  let t = Page_table.create ~page_bytes in
  let a = colliding ~home:3 ~count:2 in
  let b = colliding ~home:4 ~count:1 in
  (* [a0] at 3, [a1] at 4, [b0] at 5. Removing [a0]: [a1] moves to 3, [b0] to 4.
     Then removing [a1]: [b0] can't go to 3, before its home. *)
  let pages = a @ b in
  List.iter pages ~f:(fun n -> Page_table.add t ~page:(page n) ~owner:(owner n));
  print_table t pages;
  Page_table.remove t ~page:(page (List.nth_exn a 0));
  print_table t pages;
  Page_table.remove t ~page:(page (List.nth_exn a 1));
  print_table t pages;
  [%expect
    {|
    ((length 3) (capacity 16)
     (pages ((2 3 (3) (2)) (23 3 (4) (23)) (15 4 (5) (15)))))
    ((length 2) (capacity 16)
     (pages ((2 3 () ()) (23 3 (3) (23)) (15 4 (4) (15)))))
    ((length 1) (capacity 16) (pages ((2 3 () ()) (23 3 () ()) (15 4 (4) (15)))))
    |}]
;;

let%expect_test "growth" =
  let t = Page_table.create ~page_bytes in
  for n = 1 to 40 do
    Page_table.add t ~page:(page n) ~owner:(owner n);
    if n = 8 || n = 9 || n = 17 || n = 33
    then
      print_s
        [%message
          ""
            ~length:(Page_table.length t : int)
            ~capacity:(Page_table.capacity t : int)]
  done;
  print_s
    [%sexp
      (List.for_all (List.range 1 41) ~f:(fun n ->
         [%equal: int option] (owner_of t (page n)) (Some n))
       : bool)];
  [%expect
    {|
    ((length 8) (capacity 16))
    ((length 9) (capacity 32))
    ((length 17) (capacity 64))
    ((length 33) (capacity 128))
    true
    |}]
;;

let%expect_test "addresses far from any page" =
  let t = Page_table.create ~page_bytes in
  Page_table.add t ~page:(page 0) ~owner:(owner 0);
  Page_table.add t ~page:(page 1) ~owner:(owner 1);
  let show name addr =
    print_s [%message name ~owner:(owner_of t addr : int option)]
  in
  show "2^62" (Addr.of_word (Nativeint_u.shift_left #1n 62));
  show "2^63" (Addr.of_word (Nativeint_u.shift_left #1n 63));
  show
    "2^63 + 4096"
    (Addr.add (Addr.of_word (Nativeint_u.shift_left #1n 63)) 4096);
  show "-1" (Addr.of_word (Nativeint_u.of_int (-1)));
  [%expect
    {|
    (2^62 (owner ()))
    (2^63 (owner ()))
    ("2^63 + 4096" (owner ()))
    (-1 (owner ()))
    |}]
;;

let%expect_test "pages and owners at high addresses" =
  let t = Page_table.create ~page_bytes in
  (* The [i]th page below 2^64, and its owner, 100 pages further down. *)
  let high i = Addr.of_word (Nativeint_u.of_int (-(i * page_bytes))) in
  let pages = List.range 1 101 in
  List.iter pages ~f:(fun i ->
    Page_table.add t ~page:(high i) ~owner:(high (i + 100)));
  let owned_correctly i =
    Addr.equal (Page_table.find t (Addr.add (high i) 4095)) (high (i + 100))
  in
  print_s
    [%message
      ""
        ~length:(Page_table.length t : int)
        ~all_found:(List.for_all pages ~f:owned_correctly : bool)
        ~top_word:
          (Addr.equal
             (Page_table.find t (Addr.of_word (Nativeint_u.of_int (-1))))
             (high 101)
           : bool)];
  List.iter pages ~f:(fun i ->
    if i % 2 = 0 then Page_table.remove t ~page:(high i));
  print_s
    [%message
      ""
        ~length:(Page_table.length t : int)
        ~odd_found:
          (List.for_all pages ~f:(fun i ->
             if i % 2 = 0
             then Addr.is_null (Page_table.find t (high i))
             else owned_correctly i)
           : bool)];
  [%expect
    {|
    ((length 100) (all_found true) (top_word true))
    ((length 50) (odd_found true))
    |}]
;;

let%expect_test "random operations agree with a map" =
  let t = Page_table.create ~page_bytes in
  let model = ref Int.Map.empty in
  let random = Random.State.make [| 42 |] in
  let max_capacity = ref 0 in
  for _ = 1 to 20_000 do
    (* Few distinct pages, so removals and re-adds of the same page are common,
       and clusters form. *)
    let n = Random.State.int random 300 in
    (match Map.mem !model n with
     | true ->
       Page_table.remove t ~page:(page n);
       model := Map.remove !model n
     | false ->
       Page_table.add t ~page:(page n) ~owner:(owner n);
       model := Map.set !model ~key:n ~data:());
    max_capacity := Int.max !max_capacity (Page_table.capacity t);
    let probe = Random.State.int random 300 in
    let expected = Option.some_if (Map.mem !model probe) probe in
    let found =
      owner_of t (Addr.add (page probe) (Random.State.int random page_bytes))
    in
    if not ([%equal: int option] expected found)
    then
      raise_s
        [%message
          "mismatch" (probe : int) (expected : int option) (found : int option)]
  done;
  let all_agree =
    List.for_all (List.range 0 300) ~f:(fun n ->
      [%equal: int option]
        (owner_of t (page n))
        (Option.some_if (Map.mem !model n) n))
  in
  print_s
    [%message
      ""
        (all_agree : bool)
        ~length:(Page_table.length t : int)
        ~model:(Map.length !model : int)
        ~max_capacity:(!max_capacity : int)];
  [%expect {| ((all_agree true) (length 150) (model 150) (max_capacity 512)) |}]
;;
