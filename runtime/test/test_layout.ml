open! Import
open Layout_helpers

let%expect_test "scalars and pointers" =
  List.iter [ i8; i16; i32; i64; f32; f64; code; ptr ] ~f:show;
  [%expect
    {|
    scalar 1 (align 1), 0 ptrs, no niche
      .  I8 @0
    scalar 2 (align 2), 0 ptrs, no niche
      .  I16 @0
    scalar 4 (align 4), 0 ptrs, no niche
      .  I32 @0
    scalar 8 (align 8), 0 ptrs, no niche
      .  I64 @0
    scalar 4 (align 4), 0 ptrs, no niche
      .  F32 @0
    scalar 8 (align 8), 0 ptrs, no niche
      .  F64 @0
    scalar 8 (align 8), 0 ptrs, niche Code @0 from 0 (4096)
      .  Code @0
    scalar 0 (align 1), 1 ptrs, niche ptr #0 from 0 (4096)
      .  ptr #0
    |}]
;;

let%expect_test "point, meta, node: scalars before pointers, nested split" =
  let point = struct_ [ i64; i64 ] in
  let meta = struct_ [ i64; ptr ] in
  let node = struct_ [ i64; point; meta; ptr ] in
  show node;
  List.iter
    [ [ Layout.Path.Field 1 ]; [ Field 2 ]; [ Field 2; Field 0 ]; [ Field 3 ] ]
    ~f:(fun path ->
      printf
        "%s addressable: %b\n"
        (path_to_string path)
        (Layout.addressable (layout node) path));
  [%expect
    {|
    scalar 32 (align 8), 2 ptrs, niche ptr #0 from 0 (4096)
      .0    I64 @0
      .1.0  I64 @8
      .1.1  I64 @16
      .2.0  I64 @24
      .2.1  ptr #0
      .3    ptr #1
    .1 addressable: true
    .2 addressable: false
    .2.0 addressable: true
    .3 addressable: true
    |}]
;;

let%expect_test "fields sorted by alignment, stably; repr C keeps order" =
  let fields = [ i8; i64; i8; i16; i32; i8 ] in
  show (struct_ fields);
  show (c_struct fields);
  [%expect
    {|
    scalar 24 (align 8), 0 ptrs, no niche
      .0  I8 @14
      .1  I64 @0
      .2  I8 @15
      .3  I16 @12
      .4  I32 @8
      .5  I8 @16
    scalar 32 (align 8), 0 ptrs, no niche
      .0  I8 @0
      .1  I64 @8
      .2  I8 @16
      .3  I16 @18
      .4  I32 @20
      .5  I8 @24
    |}]
;;

let%expect_test "a nested repr C struct is one unit" =
  show (struct_ [ i8; c_struct [ i8; i32; i8 ]; i64; i8 ]);
  [%expect
    {|
    scalar 24 (align 8), 0 ptrs, no niche
      .0    I8 @20
      .1.0  I8 @8
      .1.1  I32 @12
      .1.2  I8 @16
      .2    I64 @0
      .3    I8 @21
    |}]
;;

let%expect_test "repr C with pointers is an error, also nested" =
  show (c_struct [ i64; ptr ]);
  show (c_struct [ i64; struct_ [ i8; option ptr ] ]);
  show (c_struct [ i64; array ptr ~length:0 ]);
  [%expect
    {|
    (Layout.of_shape "repr C with pointers")
    (Layout.of_shape "repr C with pointers")
    scalar 8 (align 8), 0 ptrs, no niche
      .0  I64 @0
    |}]
;;

let%expect_test "invalid shapes" =
  show (scalar I8 ~valid:(0, 256));
  show (scalar I8 ~valid:(5, 4));
  show (scalar I16 ~valid:(-1, 4));
  show (array i8 ~length:(-1));
  [%expect
    {|
    (Layout.of_shape ("invalid range" (scalar I8) (min 0) (max 256)))
    (Layout.of_shape ("invalid range" (scalar I8) (min 5) (max 4)))
    (Layout.of_shape ("invalid range" (scalar I16) (min -1) (max 4)))
    (Layout.of_shape ("negative array length" (length -1)))
    |}]
;;

let%expect_test "arrays of mixed elements" =
  let elt = struct_ [ i32; ptr; i8 ] in
  let shape = struct_ [ i8; array elt ~length:3; ptr ] in
  show shape;
  let layout = layout shape in
  print_s
    [%sexp
      (Layout.element (Layout.place layout [ Field 1 ]).layout
       |> Option.map ~f:(fun (e : Layout.Element.t) ->
         e.scalar_stride, e.ptr_stride)
       : (int * int) option)];
  printf
    "element addressable: %b, element's i32: %b\n"
    (Layout.addressable layout [ Field 1; Index 1 ])
    (Layout.addressable layout [ Field 1; Index 1; Field 0 ]);
  show (array i64 ~length:0);
  show (array bool ~length:2);
  [%expect
    {|
    scalar 28 (align 4), 4 ptrs, niche ptr #0 from 0 (4096)
      .0       I8 @24
      .1[0].0  I32 @0
      .1[0].1  ptr #0
      .1[0].2  I8 @4
      .1[1].0  I32 @8
      .1[1].1  ptr #1
      .1[1].2  I8 @12
      .1[2].0  I32 @16
      .1[2].1  ptr #2
      .1[2].2  I8 @20
      .2       ptr #3
    ((8 1))
    element addressable: false, element's i32: true
    scalar 0 (align 8), 0 ptrs, no niche
    scalar 2 (align 1), 0 ptrs, niche I8 @0 from 2 (254)
      [0]  tag I8 @0
      [1]  tag I8 @1
    |}]
;;

let%expect_test "option[ptr]: null is None" =
  show (option ptr);
  show (option code);
  [%expect
    {|
    scalar 0 (align 1), 1 ptrs, niche ptr #0 from 1 (4095)
      .   niche ptr #0: |1 untagged, |0..|0 from 0
      |1  ptr #0
    scalar 8 (align 8), 0 ptrs, niche Code @0 from 1 (4095)
      .   niche Code @0: |1 untagged, |0..|0 from 0
      |1  Code @0
    |}]
;;

let%expect_test "option[bool], option[option[bool]]" =
  show bool;
  show (option bool);
  show (option (option bool));
  [%expect
    {|
    scalar 1 (align 1), 0 ptrs, niche I8 @0 from 2 (254)
      .  tag I8 @0
    scalar 1 (align 1), 0 ptrs, niche I8 @0 from 3 (253)
      .   niche I8 @0: |1 untagged, |0..|0 from 2
      |1  tag I8 @0
    scalar 1 (align 1), 0 ptrs, niche I8 @0 from 4 (252)
      .     niche I8 @0: |1 untagged, |0..|0 from 3
      |1    niche I8 @0: |1 untagged, |0..|0 from 2
      |1|1  tag I8 @0
    |}]
;;

let%expect_test "Nothing | Just of ptr * int" =
  show (Union [ unit; struct_ [ ptr; i64 ] ]);
  [%expect
    {|
    scalar 8 (align 8), 1 ptrs, niche ptr #0 from 1 (4095)
      .     niche ptr #0: |1 untagged, |0..|0 from 0
      |1.0  ptr #0
      |1.1  I64 @0
    |}]
;;

let%expect_test "option[small enum], option of a ranged scalar" =
  show (option (enum 5));
  show (option (scalar I32 ~valid:(0, 0x10FFFF)));
  show (option (scalar I8 ~valid:(0, 255)));
  show (option (scalar I64 ~valid:(0, 10)));
  [%expect
    {|
    scalar 1 (align 1), 0 ptrs, niche I8 @0 from 6 (250)
      .   niche I8 @0: |1 untagged, |0..|0 from 5
      |1  tag I8 @0
    scalar 4 (align 4), 0 ptrs, niche I32 @0 from 1114113 (4293853183)
      .   niche I32 @0: |1 untagged, |0..|0 from 1114112
      |1  I32 @0
    scalar 2 (align 1), 0 ptrs, niche I8 @0 from 2 (254)
      .   tag I8 @0
      |1  I8 @1
    scalar 8 (align 8), 0 ptrs, niche I64 @0 from 12 (4611686018427387902)
      .   niche I64 @0: |1 untagged, |0..|0 from 11
      |1  I64 @0
    |}]
;;

let%expect_test "big enums" =
  show (enum 256);
  show (enum 257);
  show (option (enum 256));
  show (Union (List.init 300 ~f:(fun i -> if i = 0 then i64 else unit)));
  [%expect
    {|
    scalar 1 (align 1), 0 ptrs, no niche
      .  tag I8 @0
    scalar 2 (align 2), 0 ptrs, niche I16 @0 from 257 (65279)
      .  tag I16 @0
    scalar 2 (align 1), 0 ptrs, niche I8 @0 from 2 (254)
      .   tag I8 @0
      |1  tag I8 @1
    scalar 16 (align 8), 0 ptrs, niche I16 @0 from 300 (65236)
      .   tag I16 @0
      |0  I64 @8
    |}]
;;

let%expect_test "tagged beats niche-filling" =
  (* The others can't be placed before the untagged case's niche, at 8. *)
  show (Union [ struct_ [ i64; bool ]; struct_ [ i64; i64 ] ]);
  [%expect
    {|
    scalar 24 (align 8), 0 ptrs, niche I8 @0 from 2 (254)
      .     tag I8 @0
      |0.0  I64 @8
      |0.1  tag I8 @16
      |1.0  I64 @8
      |1.1  I64 @16
    |}]
;;

let%expect_test "niche-filling places others around the niche" =
  show (Union [ struct_ [ i64; bool; i8 ]; i16; i8 ]);
  show (Union [ struct_ [ ptr; ptr; i8 ]; struct_ [ ptr ]; i32 ]);
  show (Union [ struct_ [ i64; ptr ]; ptr; struct_ [ ptr; ptr ] ]);
  [%expect
    {|
    scalar 16 (align 8), 0 ptrs, niche I8 @8 from 4 (252)
      .     niche I8 @8: |0 untagged, |1..|2 from 2
      |0.0  I64 @0
      |0.1  tag I8 @8
      |0.2  I8 @9
      |1    I16 @0
      |2    I8 @0
    scalar 4 (align 4), 2 ptrs, niche ptr #0 from 2 (4094)
      .     niche ptr #0: |0 untagged, |1..|2 from 0
      |0.0  ptr #0
      |0.1  ptr #1
      |0.2  I8 @0
      |1.0  ptr #1
      |2    I32 @0
    scalar 8 (align 8), 3 ptrs, niche ptr #0 from 2 (4094)
      .     niche ptr #0: |0 untagged, |1..|2 from 0
      |0.0  I64 @0
      |0.1  ptr #0
      |1    ptr #1
      |2.0  ptr #1
      |2.1  ptr #2
    |}]
;;

let%expect_test "untagged in the middle of the cases" =
  show (Union [ unit; struct_ [ i32; bool ]; unit; unit ]);
  [%expect
    {|
    scalar 8 (align 4), 0 ptrs, niche I8 @4 from 6 (250)
      .     niche I8 @4: |1 untagged, |0..|3 from 2
      |1.0  I32 @0
      |1.1  tag I8 @4
    |}]
;;

let%expect_test "a struct exposes its fields' largest niche" =
  show (struct_ [ bool; ptr; scalar I8 ~valid:(0, 9) ]);
  show (option (struct_ [ i64; bool ]));
  [%expect
    {|
    scalar 2 (align 1), 1 ptrs, niche ptr #0 from 0 (4096)
      .0  tag I8 @0
      .1  ptr #0
      .2  I8 @1
    scalar 16 (align 8), 0 ptrs, niche I8 @8 from 3 (253)
      .     niche I8 @8: |1 untagged, |0..|0 from 2
      |1.0  I64 @0
      |1.1  tag I8 @8
    |}]
;;

let%expect_test "uninhabited" =
  show (Union []);
  show (option (Union []));
  show (struct_ [ i64; Union [] ]);
  [%expect
    {|
    scalar 0 (align 1), 0 ptrs, no niche
      .  uninhabited
    scalar 1 (align 1), 0 ptrs, niche I8 @0 from 2 (254)
      .   tag I8 @0
      |1  uninhabited
    scalar 8 (align 8), 0 ptrs, no niche
      .0  I64 @0
      .1  uninhabited
    |}]
;;

let%expect_test "single case" =
  show (Union [ struct_ [ i64; ptr ] ]);
  show (option (Union [ bool ]));
  [%expect
    {|
    scalar 8 (align 8), 1 ptrs, niche ptr #0 from 0 (4096)
      .     single
      |0.0  I64 @0
      |0.1  ptr #0
    scalar 1 (align 1), 0 ptrs, niche I8 @0 from 3 (253)
      .     niche I8 @0: |1 untagged, |0..|0 from 2
      |1    single
      |1|0  tag I8 @0
    |}]
;;

let%expect_test "place errors" =
  let layout = layout (struct_ [ i64 ]) in
  List.iter
    [ [ Layout.Path.Field 1 ]; [ Case 0 ]; [ Field 0; Field 0 ] ]
    ~f:(fun path ->
      match Layout.place layout path with
      | (_ : Layout.Place.t) -> print_endline "ok"
      | exception exn -> print_s [%sexp (exn : exn)]);
  [%expect
    {|
    ("Layout.place: no such step" (step (Field 1))
     (t
      ((scalar_bytes 8) (scalar_align 8) (ptr_words 0)
       (desc
        (Struct
         (((scalar_offset 0) (ptr_index 0)
           (layout
            ((scalar_bytes 8) (scalar_align 8) (ptr_words 0) (desc (Scalar I64)))))))))))
    ("Layout.place: no such step" (step (Case 0))
     (t
      ((scalar_bytes 8) (scalar_align 8) (ptr_words 0)
       (desc
        (Struct
         (((scalar_offset 0) (ptr_index 0)
           (layout
            ((scalar_bytes 8) (scalar_align 8) (ptr_words 0) (desc (Scalar I64)))))))))))
    ("Layout.place: no such step" (step (Field 0))
     (t ((scalar_bytes 8) (scalar_align 8) (ptr_words 0) (desc (Scalar I64)))))
    |}]
;;
