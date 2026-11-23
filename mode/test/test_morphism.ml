open! Import
module Morphism = Private.Morphism

let table axis m ~f =
  let (Axis.Any.T axis) = axis in
  let module C = (val Axis.chain axis) in
  List.map C.all ~f:(fun x -> [%sexp (x : C.t), (f m (C.index x) : int)])
;;

let print m =
  print_s
    [%message
      ""
        ~_:(m : Morphism.t)
        ~right:(table (Morphism.src m) m ~f:Morphism.right : Sexp.t list)
        ~left:(table (Morphism.dst m) m ~f:Morphism.left : Sexp.t list)
        ~inflationary:
          (Axis.Any.equal (Morphism.src m) (Morphism.dst m)
           && Morphism.is_inflationary m
           : bool)]
;;

let%expect_test "the morphisms" =
  print (Morphism.identity (T Stage));
  print Morphism.const_is_static;
  print Morphism.cross_portable;
  [%expect
    {|
    (id (right ((Const 0) (Portable 1) (Nonportable 2)))
     (left ((Const 0) (Portable 1) (Nonportable 2))) (inflationary true))
    (const_is_static (right ((Const 0) (Portable 2) (Nonportable 2)))
     (left ((Static 0) (Gc 1) (Unknown 1))) (inflationary false))
    (cross_portable (right ((Const 0) (Portable 2) (Nonportable 2)))
     (left ((Const 0) (Portable 1) (Nonportable 1))) (inflationary true))
    |}]
;;

let%expect_test "composition and order" =
  let id = Morphism.identity (T Stage) in
  print (Morphism.compose Morphism.const_is_static Morphism.cross_portable);
  print (Morphism.compose Morphism.cross_portable Morphism.cross_portable);
  print (Morphism.compose Morphism.cross_portable id);
  [%expect
    {|
    (const_is_static (right ((Const 0) (Portable 2) (Nonportable 2)))
     (left ((Static 0) (Gc 1) (Unknown 1))) (inflationary false))
    (cross_portable (right ((Const 0) (Portable 2) (Nonportable 2)))
     (left ((Const 0) (Portable 1) (Nonportable 1))) (inflationary true))
    (cross_portable (right ((Const 0) (Portable 2) (Nonportable 2)))
     (left ((Const 0) (Portable 1) (Nonportable 1))) (inflationary true))
    |}];
  print_s
    [%message
      ""
        ~id_le_cross:(Morphism.le id Morphism.cross_portable : bool)
        ~cross_le_id:(Morphism.le Morphism.cross_portable id : bool)
        ~is_identity:
          (List.map
             [ id; Morphism.cross_portable; Morphism.const_is_static ]
             ~f:Morphism.is_identity
           : bool list)];
  [%expect
    {| ((id_le_cross true) (cross_le_id false) (is_identity (true false false))) |}]
;;

let%expect_test "locality morphisms" =
  print Morphism.borrow;
  print Morphism.release;
  print (Morphism.compose Morphism.borrow Morphism.release);
  print (Morphism.compose Morphism.release Morphism.borrow);
  print (Morphism.compose Morphism.borrow Morphism.borrow);
  [%expect
    {|
    (borrow (right ((Global 0) (Regional 2) (Local 2)))
     (left ((Global 0) (Regional 1) (Local 1))) (inflationary true))
    (release (right ((Global 0) (Regional 0) (Local 2)))
     (left ((Global 0) (Regional 2) (Local 2))) (inflationary false))
    (release (right ((Global 0) (Regional 0) (Local 2)))
     (left ((Global 0) (Regional 2) (Local 2))) (inflationary false))
    (borrow (right ((Global 0) (Regional 2) (Local 2)))
     (left ((Global 0) (Regional 1) (Local 1))) (inflationary true))
    (borrow (right ((Global 0) (Regional 2) (Local 2)))
     (left ((Global 0) (Regional 1) (Local 1))) (inflationary true))
    |}]
;;

let%expect_test "locality maps" =
  let module L = Stag_mode.Locality_map in
  List.iter
    [ "borrow . release", L.compose L.borrow L.release
    ; "release . borrow", L.compose L.release L.borrow
    ; "id . borrow", L.compose L.id L.borrow
    ; "ignore . borrow", L.compose L.ignore L.borrow
    ; "borrow . ignore", L.compose L.borrow L.ignore
    ]
    ~f:(fun (name, map) -> print_s [%message name ~_:(map : L.t)]);
  [%expect
    {|
    ("borrow . release" release)
    ("release . borrow" borrow)
    ("id . borrow" borrow)
    ("ignore . borrow" ignore)
    ("borrow . ignore" ignore)
    |}]
;;
