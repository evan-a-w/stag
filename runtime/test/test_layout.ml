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
    [ [ Layout.Path.Step.Field 1 ]
    ; [ Field 2 ]
    ; [ Field 2; Field 0 ]
    ; [ Field 3 ]
    ]
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
  show (scalar Code ~valid:(4096, 5000));
  show (array i8 ~length:(-1));
  [%expect
    {|
    (Layout.of_shape ("invalid range" (scalar I8) (min 0) (max 256)))
    (Layout.of_shape ("invalid range" (scalar I8) (min 5) (max 4)))
    (Layout.of_shape ("invalid range" (scalar I16) (min -1) (max 4)))
    (Layout.of_shape "a valid range on Code")
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
  show (union [ unit; struct_ [ ptr; i64 ] ]);
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
  show (union (List.init 300 ~f:(fun i -> if i = 0 then i64 else unit)));
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
  show (union [ struct_ [ i64; bool ]; struct_ [ i64; i64 ] ]);
  [%expect
    {|
    scalar 24 (align 8), 0 ptrs, niche I8 @0 from 2 (254)
      .     tag I8 @0
      |0.0  I64 @8
      |0.1  tag I8 @1
      |1.0  I64 @8
      |1.1  I64 @16
    |}]
;;

let%expect_test "niche-filling places others around the niche" =
  show (union [ struct_ [ i64; bool; i8 ]; i16; i8 ]);
  show (union [ struct_ [ ptr; ptr; i8 ]; struct_ [ ptr ]; i32 ]);
  show (union [ struct_ [ i64; ptr ]; ptr; struct_ [ ptr; ptr ] ]);
  [%expect
    {|
    scalar 16 (align 8), 0 ptrs, niche I8 @0 from 3 (253)
      .     tag I8 @0
      |0.0  I64 @8
      |0.1  tag I8 @1
      |0.2  I8 @2
      |1    I16 @2
      |2    I8 @1
    scalar 4 (align 4), 2 ptrs, niche ptr #0 from 2 (4094)
      .     niche ptr #0: |0 untagged, |1..|2 from 0
      |0.0  ptr #0
      |0.1  ptr #1
      |0.2  I8 @0
      |1.0  ptr #1
      |2    I32 @0
    scalar 8 (align 8), 2 ptrs, niche ptr #0 from 2 (4094)
      .     niche ptr #0: |2 untagged, |0..|1 from 0
      |0.0  I64 @0
      |0.1  ptr #1
      |1    ptr #1
      |2.0  ptr #0
      |2.1  ptr #1
    |}]
;;

let%expect_test "untagged in the middle of the cases" =
  show (union [ unit; struct_ [ ptr; i32 ]; unit; unit ]);
  [%expect
    {|
    scalar 4 (align 4), 1 ptrs, niche ptr #0 from 4 (4092)
      .     niche ptr #0: |1 untagged, |0..|3 from 0
      |1.0  ptr #0
      |1.1  I32 @0
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
    scalar 16 (align 8), 0 ptrs, niche I8 @0 from 2 (254)
      .     tag I8 @0
      |1.0  I64 @8
      |1.1  tag I8 @1
    |}]
;;

let%expect_test "uninhabited" =
  show (union []);
  show (option (union []));
  show (struct_ [ i64; union [] ]);
  [%expect
    {|
    scalar 0 (align 1), 0 ptrs, no niche
      .  uninhabited
    scalar 0 (align 1), 0 ptrs, no niche
      .   single |0
      |1  uninhabited
    scalar 8 (align 8), 0 ptrs, no niche
      .0  I64 @0
      .1  uninhabited
    |}]
;;

let%expect_test "single case" =
  show (union [ struct_ [ i64; ptr ] ]);
  show (option (union [ bool ]));
  [%expect
    {|
    scalar 8 (align 8), 1 ptrs, niche ptr #0 from 0 (4096)
      .     single |0
      |0.0  I64 @0
      |0.1  ptr #0
    scalar 1 (align 1), 0 ptrs, niche I8 @0 from 3 (253)
      .     niche I8 @0: |1 untagged, |0..|0 from 2
      |1    single |0
      |1|0  tag I8 @0
    |}]
;;

let%expect_test "place errors" =
  let layout = layout (struct_ [ i64 ]) in
  List.iter
    [ [ Layout.Path.Step.Field 1 ]; [ Case 0 ]; [ Field 0; Field 0 ] ]
    ~f:(fun path ->
      match Layout.place layout path with
      | (_ : Layout.Place.t) -> print_endline "ok"
      | exception exn -> print_s [%sexp (exn : exn)]);
  [%expect
    {|
    ("Layout.place: no such step" (step (Field 1))
     (t
      ((scalar_bytes 8) (scalar_align 8) (ptr_words 0) (inhabited true)
       (desc
        (Struct (repr Default)
         (places
          (((scalar_offset 0) (ptr_index 0)
            (layout
             ((scalar_bytes 8) (scalar_align 8) (ptr_words 0) (inhabited true)
              (desc (Scalar I64)))))))
         (reserved 0))))))
    ("Layout.place: no such step" (step (Case 0))
     (t
      ((scalar_bytes 8) (scalar_align 8) (ptr_words 0) (inhabited true)
       (desc
        (Struct (repr Default)
         (places
          (((scalar_offset 0) (ptr_index 0)
            (layout
             ((scalar_bytes 8) (scalar_align 8) (ptr_words 0) (inhabited true)
              (desc (Scalar I64)))))))
         (reserved 0))))))
    ("Layout.place: no such step" (step (Field 0))
     (t
      ((scalar_bytes 8) (scalar_align 8) (ptr_words 0) (inhabited true)
       (desc (Scalar I64)))))
    |}]
;;

let%expect_test "a case with a pointer niche needn't be the largest" =
  show (union [ struct_ [ i64; i64; i64 ]; ptr ]);
  [%expect
    {|
    scalar 24 (align 8), 1 ptrs, niche ptr #0 from 1 (4095)
      .     niche ptr #0: |1 untagged, |0..|0 from 0
      |0.0  I64 @0
      |0.1  I64 @8
      |0.2  I64 @16
      |1    ptr #0
    |}]
;;

let%expect_test "fields in the padding after the tag" =
  show (union [ struct_ [ i64; i8 ]; unit ]);
  show (union [ struct_ [ i64; i16; i8 ]; struct_ [ i32 ]; unit ]);
  (* Placed around the tag, a case isn't addressable as a whole. *)
  let l = layout (union [ struct_ [ i64; i8 ]; unit ]) in
  print_s
    [%message
      (Layout.addressable l [ Case 0 ] : bool)
        (Layout.addressable l [ Case 0; Field 1 ] : bool)];
  [%expect
    {|
    scalar 16 (align 8), 0 ptrs, niche I8 @0 from 2 (254)
      .     tag I8 @0
      |0.0  I64 @8
      |0.1  I8 @1
    scalar 16 (align 8), 0 ptrs, niche I8 @0 from 3 (253)
      .     tag I8 @0
      |0.0  I64 @8
      |0.1  I16 @2
      |0.2  I8 @1
      |1.0  I32 @4
    (("Layout.addressable l [Case 0]" false)
     ("Layout.addressable l [Case 0; Field 1]" true))
    |}]
;;

let%expect_test "uninhabited cases take no room" =
  show (union [ union []; i64 ]);
  show (union [ union []; union [] ]);
  show (union [ unit; struct_ [ i64; union [] ]; i8 ]);
  show (array (union []) ~length:0);
  show (array (union []) ~length:2);
  [%expect
    {|
    scalar 8 (align 8), 0 ptrs, no niche
      .   single |1
      |0  uninhabited
      |1  I64 @0
    scalar 0 (align 1), 0 ptrs, no niche
      .   uninhabited
      |0  uninhabited
      |1  uninhabited
    scalar 16 (align 8), 0 ptrs, niche I8 @0 from 3 (253)
      .     tag I8 @0
      |1.0  I64 @8
      |1.1  uninhabited
      |2    I8 @1
    scalar 0 (align 1), 0 ptrs, no niche
    scalar 0 (align 1), 0 ptrs, no niche
      [0]  uninhabited
      [1]  uninhabited
    |}]
;;

let%expect_test "a wrapping niche" =
  (* 59 cases besides the untagged one, from 200, wrap round to 2. *)
  show (union (scalar I8 ~valid:(100, 199) :: List.init 59 ~f:(fun _ -> unit)));
  [%expect
    {|
    scalar 1 (align 1), 0 ptrs, niche I8 @0 from 3 (97)
      .   niche I8 @0: |0 untagged, |1..|59 from 200
      |0  I8 @0
    |}]
;;

let%expect_test "repr C unions: tagged, but their tag's unused values are \
                 niches"
  =
  show (c_union [ unit; i8 ]);
  show (c_union [ i64; c_struct [ i8; i32 ] ]);
  show (option (c_union [ unit; i8 ]));
  show (c_union [ ptr ]);
  show (c_union []);
  [%expect
    {|
    scalar 8 (align 4), 0 ptrs, niche I32 @0 from 2 (4294967294)
      .   tag I32 @0
      |1  I8 @4
    scalar 16 (align 8), 0 ptrs, niche I32 @0 from 2 (4294967294)
      .     tag I32 @0
      |0    I64 @8
      |1.0  I8 @8
      |1.1  I32 @12
    scalar 8 (align 4), 0 ptrs, niche I32 @0 from 3 (4294967293)
      .     niche I32 @0: |1 untagged, |0..|0 from 2
      |1    tag I32 @0
      |1|1  I8 @4
    (Layout.of_shape "repr C with pointers")
    (Layout.of_shape "repr C union with no cases")
    |}]
;;

let%expect_test "repr C fixes only its own fields: nested layouts and niches \
                 stay"
  =
  show (c_struct [ i8; struct_ [ i8; i64 ]; option bool ]);
  show (option (c_struct [ i32; bool ]));
  [%expect
    {|
    scalar 32 (align 8), 0 ptrs, niche I8 @24 from 3 (253)
      .0    I8 @0
      .1.0  I8 @16
      .1.1  I64 @8
      .2    niche I8 @24: |1 untagged, |0..|0 from 2
      .2|1  tag I8 @24
    scalar 8 (align 4), 0 ptrs, niche I8 @4 from 3 (253)
      .     niche I8 @4: |1 untagged, |0..|0 from 2
      |1.0  I32 @0
      |1.1  tag I8 @4
    |}]
;;

let%expect_test "addressable within union cases" =
  let l = layout (union [ struct_ [ ptr; i64 ]; struct_ [ i32; i32 ] ]) in
  List.iter
    [ []
    ; [ Layout.Path.Step.Case 0 ]
    ; [ Case 0; Field 0 ]
    ; [ Case 0; Field 1 ]
    ; [ Case 1 ]
    ]
    ~f:(fun path ->
      printf "%-6s %b\n" (path_to_string path) (Layout.addressable l path));
  [%expect
    {|
    .      false
    |0     false
    |0.0   true
    |0.1   true
    |1     true
    |}]
;;

let%expect_test "write plans" =
  let plans shape path =
    let l = layout shape in
    let n =
      match (shape : Shape.t) with
      | Union { cases; _ } -> List.length cases
      | _ -> 1
    in
    List.iter (List.range 0 n) ~f:(fun case ->
      match Layout.write_plan l path ~case with
      | plan -> print_s [%message (case : int) (plan : Layout.Write_plan.t)]
      | exception exn -> print_s [%message (case : int) (exn : exn)])
  in
  plans (option ptr) [];
  plans (union [ unit; struct_ [ ptr; i64; ptr ]; ptr ]) [];
  plans (union [ i8; i64 ]) [];
  plans (union [ union []; i64 ]) [];
  (* At a path: shifted by the union's place. *)
  let l = layout (struct_ [ ptr; i8; option ptr ]) in
  print_s
    [%sexp (Layout.write_plan l [ Field 2 ] ~case:0 : Layout.Write_plan.t)];
  [%expect
    {|
    ((case 0)
     (plan
      ((ptr_index 0) (ptr_words 1) (discriminant (((Ptr (ptr_index 0)) 0))))))
    ((case 1) (plan ((ptr_index 0) (ptr_words 1) (discriminant ()))))
    ((case 0)
     (plan
      ((ptr_index 0) (ptr_words 2) (discriminant (((Ptr (ptr_index 0)) 0))))))
    ((case 1) (plan ((ptr_index 0) (ptr_words 2) (discriminant ()))))
    ((case 2)
     (plan
      ((ptr_index 0) (ptr_words 2) (discriminant (((Ptr (ptr_index 0)) 2))))))
    ((case 0)
     (plan
      ((ptr_index 0) (ptr_words 0)
       (discriminant (((Scalar (scalar_offset 0) (scalar I8)) 0))))))
    ((case 1)
     (plan
      ((ptr_index 0) (ptr_words 0)
       (discriminant (((Scalar (scalar_offset 0) (scalar I8)) 1))))))
    ((case 0)
     (exn
      (Layout.write_plan "a case that is never stored" (path ()) (case 0)
       (t
        ((scalar_bytes 8) (scalar_align 8) (ptr_words 0) (inhabited true)
         (desc
          (Union
           (cases
            (((scalar_offset 0) (ptr_index 0)
              (layout
               ((scalar_bytes 0) (scalar_align 1) (ptr_words 0) (inhabited false)
                (desc (Union (cases ()) (discriminant Uninhabited))))))
             ((scalar_offset 0) (ptr_index 0)
              (layout
               ((scalar_bytes 8) (scalar_align 8) (ptr_words 0) (inhabited true)
                (desc (Scalar I64)))))))
           (discriminant (Single (case 1))))))))))
    ((case 1) (plan ((ptr_index 0) (ptr_words 0) (discriminant ()))))
    ((ptr_index 1) (ptr_words 1) (discriminant (((Ptr (ptr_index 1)) 0))))
    |}]
;;
