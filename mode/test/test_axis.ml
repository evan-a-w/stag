open! Import

let%expect_test "axes are chains" =
  let show (type a) (axis : a Axis.t) =
    let module C = (val Axis.chain axis) in
    print_s
      [%message
        ""
          ~axis:(Axis.Any.T axis : Axis.Any.t)
          ~all:(C.all : C.t list)
          ~min:(C.min : C.t)
          ~max:(C.max : C.t)
          ~indices:(List.map C.all ~f:C.index : int list)
          ~joins:
            (List.concat_map C.all ~f:(fun a ->
               List.map C.all ~f:(fun b -> C.join a b))
             : C.t list)
          ~meets:
            (List.concat_map C.all ~f:(fun a ->
               List.map C.all ~f:(fun b -> C.meet a b))
             : C.t list)]
  in
  show Stage;
  show Provenance;
  show Locality;
  [%expect
    {|
    ((axis stage) (all (Const Portable Nonportable)) (min Const)
     (max Nonportable) (indices (0 1 2))
     (joins
      (Const Portable Nonportable Portable Portable Nonportable Nonportable
       Nonportable Nonportable))
     (meets
      (Const Const Const Const Portable Portable Const Portable Nonportable)))
    ((axis provenance) (all (Static Gc External)) (min Static) (max External)
     (indices (0 1 2))
     (joins (Static Gc External Gc Gc External External External External))
     (meets (Static Static Static Static Gc Gc Static Gc External)))
    ((axis locality) (all (Global Regional Local)) (min Global) (max Local)
     (indices (0 1 2))
     (joins (Global Regional Local Regional Regional Local Local Local Local))
     (meets
      (Global Global Global Global Regional Regional Global Regional Local)))
    |}]
;;

let%expect_test "constant modes are ordered axis by axis" =
  let a : Const.t = { stage = Const; provenance = Gc; locality = Local }
  and b : Const.t =
    { stage = Portable; provenance = Static; locality = Global }
  in
  print_s
    [%message
      ""
        ~min:(Const.min : Const.t)
        ~max:(Const.max : Const.t)
        ~a_le_b:(Const.le a b : bool)
        ~b_le_a:(Const.le b a : bool)
        ~a_le_join:(Const.le a (Const.join a b) : bool)
        ~meet_le_b:(Const.le (Const.meet a b) b : bool)
        ~join:(Const.join a b : Const.t)
        ~meet:(Const.meet a b : Const.t)
        ~locality_of_a:(Const.get a Locality : Locality.t)];
  [%expect
    {|
    ((min ((stage Const) (provenance Static) (locality Global)))
     (max ((stage Nonportable) (provenance External) (locality Local)))
     (a_le_b false) (b_le_a false) (a_le_join true) (meet_le_b true)
     (join ((stage Portable) (provenance Gc) (locality Local)))
     (meet ((stage Const) (provenance Static) (locality Global)))
     (locality_of_a Local))
    |}]
;;
