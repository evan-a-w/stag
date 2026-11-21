open! Import
module Addr = Memory.Addr

let word n = Addr.of_word (Nativeint_u.of_int n)
let int_of addr = Nativeint_u.to_int (Addr.to_word addr)

(* The slots' values, as ints, by [iter_slots]. *)
let slot_values roots =
  let values = ref [] in
  Roots.iter_slots roots ~f:(fun slot ->
    values := int_of (Memory.load_addr slot) :: !values);
  List.rev !values
;;

let print_roots roots =
  print_s
    [%message
      "" ~depth:(Roots.depth roots : int) ~slots:(slot_values roots : int list)]
;;

(* Pushes a frame of [slots] holding [first], [first + 1], ... *)
let push roots ~slots ~first =
  let slot = Roots.push_frame roots ~slots in
  for i = 0 to slots - 1 do
    Memory.store_addr (Addr.add slot (i * 8)) (word (first + i))
  done;
  slot
;;

let%expect_test "frames" =
  let roots = Roots.create () in
  print_roots roots;
  let first = Roots.push_frame roots ~slots:2 in
  print_s
    [%message
      "a fresh frame's slots are null" ~slots:(slot_values roots : int list)];
  Memory.store_addr first (word 1);
  Memory.store_addr (Addr.add first 8) (word 2);
  let _ = push roots ~slots:0 ~first:0 in
  let _ = push roots ~slots:3 ~first:10 in
  print_roots roots;
  Roots.pop_frame roots;
  print_roots roots;
  Roots.pop_frame roots;
  Roots.pop_frame roots;
  print_roots roots;
  print_s
    [%sexp
      (Or_error.try_with (fun () -> Roots.pop_frame roots) : unit Or_error.t)];
  [%expect
    {|
    ((depth 0) (slots ()))
    ("a fresh frame's slots are null" (slots (0 0)))
    ((depth 3) (slots (10 11 12 1 2)))
    ((depth 2) (slots (1 2)))
    ((depth 0) (slots ()))
    (Error "Roots.pop_frame: no frame")
    |}]
;;

let%expect_test "the frame format" =
  let roots = Roots.create () in
  let a = push roots ~slots:2 ~first:1 in
  let b = push roots ~slots:1 ~first:3 in
  let latest = Roots.latest_frame roots in
  let previous = Roots.Frame.next latest in
  print_s
    [%message
      ""
        ~latest_is_b:(Addr.equal (Roots.Frame.slot latest 0) b : bool)
        ~previous_is_a:(Addr.equal (Roots.Frame.slot previous 0) a : bool)
        ~first_is_last:(Addr.is_null (Roots.Frame.next previous) : bool)
        ~slots:
          ((Roots.Frame.slots latest, Roots.Frame.slots previous) : int * int)
        ~map_word_is_map:
          (Addr.equal
             (Memory.load_addr (Addr.add previous 8))
             (Roots.Frame.map previous)
           : bool)
        ~map_of_a:
          (( Roots.Frame_map.num_roots (Roots.Frame.map previous)
           , Roots.Frame_map.num_meta (Roots.Frame.map previous) )
           : int * int)
        ~roots_of_a:
          (List.init 2 ~f:(fun i ->
             Int64_u.to_int (Memory.load_64 (Addr.add previous (16 + (i * 8)))))
           : int list)];
  (* [next; map; roots...], the map [{ num_roots; num_meta }] as two [int32]s. *)
  let c = push roots ~slots:2 ~first:5 in
  ignore c;
  let a_map = Roots.Frame.map previous in
  let c_map = Roots.Frame.map (Roots.latest_frame roots) in
  print_s
    [%message
      "frames of a slot count share a map"
        ~shared:(Addr.equal a_map c_map : bool)
        ~map_bytes:
          (List.init 8 ~f:(fun i ->
             Int8_u.to_int (Memory.load_8 (Addr.add a_map i)) land 0xff)
           : int list)];
  [%expect
    {|
    ((latest_is_b true) (previous_is_a true) (first_is_last true) (slots (1 2))
     (map_word_is_map true) (map_of_a (2 0)) (roots_of_a (1 2)))
    ("frames of a slot count share a map" (shared true)
     (map_bytes (2 0 0 0 0 0 0 0)))
    |}]
;;

let%expect_test "frames don't move as the stack grows" =
  (* 64-byte segments: two frames of two slots each. *)
  let roots = Roots.For_testing.create ~segment_bytes:64 in
  let slots = Array.create ~len:7 (Addr.null ()) in
  for i = 0 to 6 do
    slots.(i) <- push roots ~slots:2 ~first:(i * 10)
  done;
  let segments_used () =
    Roots.For_testing.segments roots, Roots.For_testing.segment roots
  in
  print_s [%message "" ~segments:(segments_used () : int * int)];
  let still_there =
    List.for_all (List.range 0 7) ~f:(fun i ->
      let slot = slots.(i) in
      int_of (Memory.load_addr slot) = i * 10
      && int_of (Memory.load_addr (Addr.add slot 8)) = (i * 10) + 1)
  in
  print_s [%message "" (still_there : bool)];
  for _ = 1 to 4 do
    Roots.pop_frame roots
  done;
  print_roots roots;
  print_s [%message "" ~segments:(segments_used () : int * int)];
  (* Pushing again reuses the segments. *)
  for i = 3 to 6 do
    slots.(i) <- push roots ~slots:2 ~first:(i * 100)
  done;
  print_roots roots;
  print_s [%message "" ~segments:(segments_used () : int * int)];
  [%expect
    {|
    (segments (4 3))
    (still_there true)
    ((depth 3) (slots (20 21 10 11 0 1)))
    (segments (4 1))
    ((depth 7) (slots (600 601 500 501 400 401 300 301 20 21 10 11 0 1)))
    (segments (4 3))
    |}]
;;

let%expect_test "frames bigger than a segment" =
  let roots = Roots.For_testing.create ~segment_bytes:64 in
  (* Too big for the first segment, which is left empty. *)
  let _ = push roots ~slots:10 ~first:0 in
  let _ = push roots ~slots:1 ~first:100 in
  let _ = push roots ~slots:20 ~first:200 in
  print_s
    [%message
      ""
        ~depth:(Roots.depth roots : int)
        ~slot_count:(List.length (slot_values roots) : int)
        ~segments:
          ((Roots.For_testing.segments roots, Roots.For_testing.segment roots)
           : int * int)];
  for _ = 1 to 3 do
    Roots.pop_frame roots
  done;
  print_roots roots;
  (* The first segment is used again. *)
  let _ = push roots ~slots:1 ~first:7 in
  print_roots roots;
  print_s [%message "" ~segment:(Roots.For_testing.segment roots : int)];
  [%expect
    {|
    ((depth 3) (slot_count 31) (segments (4 3)))
    ((depth 0) (slots ()))
    ((depth 1) (slots (7)))
    (segment 0)
    |}]
;;

let%expect_test "a deep stack" =
  let roots = Roots.For_testing.create ~segment_bytes:256 in
  let depth = 10_000 in
  for i = 0 to depth - 1 do
    ignore (push roots ~slots:(i % 5) ~first:i : Addr.t)
  done;
  let count = List.length (slot_values roots) in
  for _ = 1 to depth do
    Roots.pop_frame roots
  done;
  print_s
    [%message
      ""
        (count : int)
        ~segments:(Roots.For_testing.segments roots : int)
        ~segment:(Roots.For_testing.segment roots : int)
        ~depth:(Roots.depth roots : int)];
  [%expect {| ((count 20000) (segments 1334) (segment 0) (depth 0)) |}]
;;

let%expect_test "globals" =
  let roots = Roots.create () in
  let chunk = Memory.Chunk.create ~bytes:80 ~align:8 in
  let global i = Addr.add (Memory.Chunk.base chunk) (i * 8) in
  for i = 0 to 9 do
    Memory.store_addr (global i) (word (1000 + i));
    Roots.add_global roots (global i)
  done;
  let _ = push roots ~slots:1 ~first:1 in
  print_roots roots;
  Roots.remove_global roots (global 3);
  Roots.remove_global roots (global 0);
  print_s
    [%message
      ""
        ~globals:(Roots.globals roots : int)
        ~values:
          (List.sort ~compare (List.tl_exn (slot_values roots)) : int list)];
  let show f = print_s [%sexp (Or_error.try_with f : unit Or_error.t)] in
  show (fun () -> Roots.add_global roots (global 1));
  show (fun () -> Roots.remove_global roots (global 3));
  [%expect
    {|
    ((depth 1) (slots (1 1000 1001 1002 1003 1004 1005 1006 1007 1008 1009)))
    ((globals 8) (values (1001 1002 1004 1005 1006 1007 1008 1009)))
    (Error "Roots.add_global: already added")
    (Error "Roots.remove_global: not added")
    |}]
;;

let%expect_test "a used segment too small for a frame is replaced" =
  let roots = Roots.For_testing.create ~segment_bytes:64 in
  let segments () =
    Roots.For_testing.segments roots, Roots.For_testing.segment roots
  in
  (* Two frames fill the first segment, a third makes the second. *)
  let _ = push roots ~slots:2 ~first:0 in
  let _ = push roots ~slots:2 ~first:10 in
  let _ = push roots ~slots:1 ~first:20 in
  print_s [%message "" ~segments:(segments () : int * int)];
  Roots.pop_frame roots;
  (* The second segment (64 bytes) is too small for this frame: replaced. *)
  let big = push roots ~slots:20 ~first:100 in
  print_s [%message "" ~segments:(segments () : int * int)];
  print_roots roots;
  print_s
    [%message
      ""
        ~big_last_slot:(int_of (Memory.load_addr (Addr.add big (19 * 8))) : int)];
  [%expect
    {|
    (segments (2 1))
    (segments (2 1))
    ((depth 3)
     (slots
      (100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117
       118 119 10 11 0 1)))
    (big_last_slot 119)
    |}]
;;

let%expect_test "destroy" =
  let roots = Roots.For_testing.create ~segment_bytes:64 in
  for i = 0 to 4 do
    ignore (push roots ~slots:2 ~first:(i * 10) : Addr.t)
  done;
  Roots.destroy roots;
  print_roots roots;
  let try_ f = print_s [%sexp (Or_error.try_with f : unit Or_error.t)] in
  try_ (fun () -> ignore (Roots.push_frame roots ~slots:1 : Addr.t));
  try_ (fun () -> Roots.destroy roots);
  [%expect
    {|
    ((depth 0) (slots ()))
    (Error "Roots.push_frame: destroyed")
    (Error "Roots.destroy: destroyed")
    |}]
;;

let%expect_test "frame maps of many slot counts" =
  let roots = Roots.create () in
  let maps = Array.create ~len:6 (Addr.null ()) in
  List.iteri [ 0; 1; 1023; 1024; 5000; 1024 ] ~f:(fun i slots ->
    let _ = Roots.push_frame roots ~slots in
    maps.(i) <- Roots.Frame.map (Roots.latest_frame roots));
  print_s
    [%message
      ""
        ~num_roots:
          (List.init 6 ~f:(fun i -> Roots.Frame_map.num_roots maps.(i))
           : int list)
        ~big_maps_shared:(Addr.equal maps.(3) maps.(5) : bool)
        ~distinct:(not (Addr.equal maps.(3) maps.(4)) : bool)];
  print_s
    [%sexp
      (Or_error.try_with (fun () ->
         ignore (Roots.push_frame roots ~slots:(1 lsl 32)))
       : unit Or_error.t)];
  Roots.destroy roots;
  [%expect
    {|
    ((num_roots (0 1 1023 1024 5000 1024)) (big_maps_shared true)
     (distinct true))
    (Error (Roots.push_frame (slots 4294967296)))
    |}]
;;
