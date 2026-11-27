open! Import

let reason = Reason.create
let layouts = Id_source.create ()

let ok result =
  match result with
  | Ok () -> ()
  | Error error -> print_s [%message "unexpected" (error : Error.t)]
;;

let print_result result = print_s [%sexp (result : (unit, Error.t) Result.t)]
let show vars = List.iter vars ~f:(fun var -> print_s [%sexp (var : Var.t)])

let%expect_test "constraints through internal variables are kept, instances \
                 are independent"
  =
  let ids = Id_source.create () in
  (* [let f x = let y = x in y]: [x <= y <= result]. *)
  let x = Var.create ids ~rank:1
  and y = Var.create ids ~rank:1
  and result = Var.create ids ~rank:1 in
  ok (Var.submode x y (reason "x is bound to y"));
  ok (Var.submode y result (reason "y is returned"));
  ok (Var.at_most x Provenance Gc (reason "x is gc"));
  Var.generalize [ x; result ] ~rank:0;
  show [ x; y; result ];
  [%expect
    {|
    ((m0.stage Const..Nonportable (generic (m0.stage <= m2.stage)))
     (m0.provenance Static..Gc
      (generic (m0.provenance <= m2.provenance)
       (m0.provenance <= (const_is_static m0.stage))))
     (m0.locality Global..Local (generic (m0.locality <= m2.locality))))
    ((m1.stage Const..Nonportable) (m1.provenance Static..External)
     (m1.locality Global..Local))
    ((m2.stage Const..Nonportable (generic))
     (m2.provenance Static..External
      (generic (m2.provenance <= (const_is_static m2.stage))))
     (m2.locality Global..Local (generic)))
    |}];
  let instance () =
    let instance = Var.Instance.create ids ~rank:0 in
    let x = Var.Instance.instantiate instance x in
    x, Var.Instance.instantiate instance result
  in
  let x1, result1 = instance () in
  let x2, result2 = instance () in
  show [ x1; result1; x2; result2 ];
  [%expect
    {|
    ((m3.stage Const..Nonportable) (m3.provenance Static..Gc)
     (m3.locality Global..Local))
    ((m4.stage Const..Nonportable) (m4.provenance Static..External)
     (m4.locality Global..Local))
    ((m5.stage Const..Nonportable) (m5.provenance Static..Gc)
     (m5.locality Global..Local))
    ((m6.stage Const..Nonportable) (m6.provenance Static..External)
     (m6.locality Global..Local))
    |}];
  ok (Var.at_least x1 Locality Local (reason "local argument"));
  ok (Var.at_most result2 Stage Portable (reason "result in a quote"));
  show [ x1; result1; x2; result2; x; y; result ];
  [%expect
    {|
    ((m3.stage Const..Nonportable) (m3.provenance Static..Gc)
     (m3.locality Local))
    ((m4.stage Const..Nonportable) (m4.provenance Static..External)
     (m4.locality Local))
    ((m5.stage Const..Portable) (m5.provenance Static..Gc)
     (m5.locality Global..Local))
    ((m6.stage Const..Portable) (m6.provenance Static..External)
     (m6.locality Global..Local))
    ((m0.stage Const..Nonportable (generic (m0.stage <= m2.stage)))
     (m0.provenance Static..Gc
      (generic (m0.provenance <= m2.provenance)
       (m0.provenance <= (const_is_static m0.stage))))
     (m0.locality Global..Local (generic (m0.locality <= m2.locality))))
    ((m1.stage Const..Nonportable) (m1.provenance Static..External)
     (m1.locality Global..Local))
    ((m2.stage Const..Nonportable (generic))
     (m2.provenance Static..External
      (generic (m2.provenance <= (const_is_static m2.stage))))
     (m2.locality Global..Local (generic)))
    |}];
  print_result (Var.at_most result1 Locality Global (reason "result returned"));
  [%expect
    {|
    (Error
     ("Mode mismatch" (var m4) (axis locality) (lower Local) (upper Global)
      (lower_because ("local argument" "x is bound to y" "y is returned"))
      (upper_because ("result returned"))))
    |}]
;;

let%expect_test "constraints with outer variables" =
  let ids = Id_source.create () in
  let outer_in = Var.create ids ~rank:0
  and outer_out = Var.create ids ~rank:0 in
  let k = Var.create ids ~rank:1
  and internal = Var.create ids ~rank:1 in
  ok (Var.submode outer_in internal (reason "captured"));
  ok (Var.submode internal k (reason "returned"));
  ok (Var.submode k outer_out (reason "stored"));
  (* Outer variables in the list are not generalized. *)
  Var.generalize [ k; outer_in ] ~rank:0;
  show [ k; outer_in; outer_out ];
  [%expect
    {|
    ((m2.stage Const..Nonportable
      (generic (m2.stage <= m1.stage) (m0.stage <= m2.stage)))
     (m2.provenance Static..External
      (generic (m2.provenance <= m1.provenance)
       (m2.provenance <= (const_is_static m2.stage))
       (m0.provenance <= m2.provenance)))
     (m2.locality Global..Local
      (generic (m2.locality <= m1.locality) (m0.locality <= m2.locality))))
    ((m0.stage Const..Nonportable) (m0.provenance Static..External)
     (m0.locality Global..Local))
    ((m1.stage Const..Nonportable) (m1.provenance Static..External)
     (m1.locality Global..Local))
    |}];
  let k1 = Var.Instance.instantiate (Var.Instance.create ids ~rank:0) k in
  ok (Var.at_least outer_in Locality Regional (reason "outer_in is regional"));
  show [ k1 ];
  ok (Var.at_least k1 Stage Nonportable (reason "k1 is nonportable"));
  show [ outer_out ];
  [%expect
    {|
    ((m4.stage Const..Nonportable) (m4.provenance Static..External)
     (m4.locality Regional..Local))
    ((m1.stage Nonportable) (m1.provenance Static..External)
     (m1.locality Regional..Local))
    |}];
  print_result (Var.at_most k1 Locality Global (reason "k1 is returned"));
  [%expect
    {|
    (Error
     ("Mode mismatch" (var m4) (axis locality) (lower Regional) (upper Global)
      (lower_because ("outer_in is regional" captured returned))
      (upper_because ("k1 is returned"))))
    |}]
;;

let%expect_test "morphisms compose along paths" =
  let ids = Id_source.create () in
  let a = Var.create ids ~rank:1
  and internal = Var.create ids ~rank:1
  and b = Var.create ids ~rank:1 in
  ok (Var.submode ~crossing:Crossing.immediate a internal (reason "an int"));
  ok (Var.submode internal b (reason "returned"));
  Var.generalize [ a; b ] ~rank:0;
  show [ a; b ];
  [%expect
    {|
    ((m0.stage Const..Nonportable
      (generic (m0.stage <= (cross_portable m2.stage))))
     (m0.provenance Static..External
      (generic (m0.provenance <= (const_is_static m0.stage))))
     (m0.locality Global..Local (generic)))
    ((m2.stage Const..Nonportable (generic))
     (m2.provenance Static..External
      (generic (m2.provenance <= (const_is_static m2.stage))))
     (m2.locality Global..Local (generic)))
    |}];
  let instance = Var.Instance.create ids ~rank:0 in
  let a1 = Var.Instance.instantiate instance a
  and b1 = Var.Instance.instantiate instance b in
  ok (Var.at_least_const a1 Const.max (reason "a1 is anything"));
  show [ a1; b1 ];
  [%expect
    {|
    ((m3.stage Nonportable) (m3.provenance External) (m3.locality Local))
    ((m4.stage Portable..Nonportable) (m4.provenance Static..External)
     (m4.locality Global..Local))
    |}]
;;

let%expect_test "cycles, sharing within an instance, and non-generic variables" =
  let ids = Id_source.create () in
  let a = Var.create ids ~rank:1
  and b = Var.create ids ~rank:1
  and c = Var.create ids ~rank:1 in
  ok (Var.equate a b (reason "unified"));
  Var.lower_rank c ~rank:0;
  Var.generalize [ a; b; c ] ~rank:0;
  show [ a; b; c ];
  print_s [%message "" ~c_is_generic:(Var.is_generic c : bool)];
  [%expect
    {|
    ((m0.stage Const..Nonportable (generic (m0.stage <= m1.stage)))
     (m0.provenance Static..External (generic (m0.provenance <= m1.provenance)))
     (m0.locality Global..Local (generic (m0.locality <= m1.locality))))
    ((m1.stage Const..Nonportable (generic (m1.stage <= m0.stage)))
     (m1.provenance Static..External
      (generic (m1.provenance <= m0.provenance)
       (m1.provenance <= (const_is_static m1.stage))))
     (m1.locality Global..Local (generic (m1.locality <= m0.locality))))
    ((m2.stage Const..Nonportable) (m2.provenance Static..External)
     (m2.locality Global..Local))
    (c_is_generic false)
    |}];
  let instance = Var.Instance.create ids ~rank:0 in
  let a1 = Var.Instance.instantiate instance a
  and b1 = Var.Instance.instantiate instance b
  and a1' = Var.Instance.instantiate instance a
  and c1 = Var.Instance.instantiate instance c in
  print_s
    [%message
      ""
        ~same_copy:(phys_equal a1 a1' : bool)
        ~c_unchanged:(phys_equal c c1 : bool)
        ~a1_is_generic:(Var.is_generic a1 : bool)];
  ok (Var.at_least a1 Locality Local (reason "a1 is local"));
  show [ a1; b1 ];
  [%expect
    {|
    ((same_copy true) (c_unchanged true) (a1_is_generic false))
    ((m3.stage Const..Nonportable) (m3.provenance Static..External)
     (m3.locality Local))
    ((m4.stage Const..Nonportable) (m4.provenance Static..External)
     (m4.locality Local))
    |}]
;;

let%expect_test "nested generalization: an older scheme's variables are \
                 internal"
  =
  let ids = Id_source.create () in
  (* An inner scheme at rank 2 relating [p] and [q] of rank 1 through its
     generic variable. *)
  let p = Var.create ids ~rank:1
  and q = Var.create ids ~rank:1
  and inner = Var.create ids ~rank:2 in
  ok (Var.submode p inner (reason "inner captures p"));
  ok (Var.submode inner q (reason "inner flows to q"));
  Var.generalize [ inner ] ~rank:1;
  show [ inner ];
  [%expect
    {|
    ((m2.stage Const..Nonportable
      (generic (m2.stage <= m1.stage) (m0.stage <= m2.stage)))
     (m2.provenance Static..External
      (generic (m2.provenance <= m1.provenance)
       (m2.provenance <= (const_is_static m2.stage))
       (m0.provenance <= m2.provenance)))
     (m2.locality Global..Local
      (generic (m2.locality <= m1.locality) (m0.locality <= m2.locality))))
    |}];
  Var.generalize [ p; q ] ~rank:0;
  show [ p; q ];
  [%expect
    {|
    ((m0.stage Const..Nonportable (generic (m0.stage <= m1.stage)))
     (m0.provenance Static..External
      (generic (m0.provenance <= m1.provenance)
       (m0.provenance <= (const_is_static m0.stage))))
     (m0.locality Global..Local (generic (m0.locality <= m1.locality))))
    ((m1.stage Const..Nonportable (generic))
     (m1.provenance Static..External
      (generic (m1.provenance <= (const_is_static m1.stage))))
     (m1.locality Global..Local (generic)))
    |}]
;;

let%expect_test "instances of the template carry later raised bounds of outer \
                 variables"
  =
  let ids = Id_source.create () in
  let outer = Var.create ids ~rank:0
  and k = Var.create ids ~rank:1 in
  ok (Var.submode outer k (reason "captured"));
  Var.generalize [ k ] ~rank:0;
  ok (Var.at_least outer Stage Portable (reason "outer is computed"));
  let k1 = Var.Instance.instantiate (Var.Instance.create ids ~rank:0) k in
  show [ k; k1 ];
  print_result (Var.at_most k1 Stage Const (reason "k1 is const"));
  [%expect
    {|
    ((m1.stage Portable..Nonportable (generic (m0.stage <= m1.stage)))
     (m1.provenance Static..External
      (generic (m1.provenance <= (const_is_static m1.stage))
       (m0.provenance <= m1.provenance)))
     (m1.locality Global..Local (generic (m0.locality <= m1.locality))))
    ((m2.stage Portable..Nonportable) (m2.provenance Static..External)
     (m2.locality Global..Local))
    (Error
     ("Mode mismatch" (var m2) (axis stage) (lower Portable) (upper Const)
      (lower_because ("outer is computed" captured))
      (upper_because ("k1 is const"))))
    |}]
;;

let%expect_test "instantiating nested schemes together links the copies" =
  let ids = Id_source.create () in
  let p = Var.create ids ~rank:1
  and q = Var.create ids ~rank:1
  and inner = Var.create ids ~rank:2 in
  ok (Var.submode p inner (reason "inner captures p"));
  ok (Var.submode inner q (reason "inner flows to q"));
  Var.generalize [ inner ] ~rank:1;
  Var.generalize [ p; q ] ~rank:0;
  let instance = Var.Instance.create ids ~rank:0 in
  let p1 = Var.Instance.instantiate instance p
  and q1 = Var.Instance.instantiate instance q
  and inner1 = Var.Instance.instantiate instance inner in
  ok (Var.at_least p1 Locality Local (reason "p1 is local"));
  show [ p1; inner1; q1 ];
  (* The schemes are unchanged. *)
  show [ p; inner ];
  [%expect
    {|
    ((m3.stage Const..Nonportable) (m3.provenance Static..External)
     (m3.locality Local))
    ((m5.stage Const..Nonportable) (m5.provenance Static..External)
     (m5.locality Local))
    ((m4.stage Const..Nonportable) (m4.provenance Static..External)
     (m4.locality Local))
    ((m0.stage Const..Nonportable (generic (m0.stage <= m1.stage)))
     (m0.provenance Static..External
      (generic (m0.provenance <= m1.provenance)
       (m0.provenance <= (const_is_static m0.stage))))
     (m0.locality Global..Local (generic (m0.locality <= m1.locality))))
    ((m2.stage Const..Nonportable
      (generic (m2.stage <= m1.stage) (m0.stage <= m2.stage)))
     (m2.provenance Static..External
      (generic (m2.provenance <= m1.provenance)
       (m2.provenance <= (const_is_static m2.stage))
       (m0.provenance <= m2.provenance)))
     (m2.locality Global..Local
      (generic (m2.locality <= m1.locality) (m0.locality <= m2.locality))))
    |}]
;;

let%expect_test "constraints from outer variables compose morphisms" =
  let ids = Id_source.create () in
  let outer = Var.create ids ~rank:0
  and internal = Var.create ids ~rank:1
  and k = Var.create ids ~rank:1 in
  ok (Var.submode outer internal (reason "captured"));
  ok (Var.submode ~crossing:Crossing.immediate internal k (reason "an int"));
  Var.generalize [ k ] ~rank:0;
  show [ k ];
  let k1 = Var.Instance.instantiate (Var.Instance.create ids ~rank:0) k in
  ok (Var.at_least outer Stage Nonportable (reason "outer is computed"));
  ok (Var.at_least outer Locality Local (reason "outer is local"));
  show [ k1 ];
  [%expect
    {|
    ((m2.stage Const..Nonportable
      (generic (m0.stage <= (cross_portable m2.stage))
       (m0.provenance <= (const_is_static m2.stage))))
     (m2.provenance Static..External
      (generic (m2.provenance <= (const_is_static m2.stage))))
     (m2.locality Global..Local (generic)))
    ((m3.stage Portable..Nonportable) (m3.provenance Static..External)
     (m3.locality Global..Local))
    |}]
;;

let%expect_test "cycles through internal variables leave no self constraint" =
  let ids = Id_source.create () in
  let x = Var.create ids ~rank:1
  and internal = Var.create ids ~rank:1 in
  ok (Var.equate x internal (reason "unified"));
  Var.generalize [ x ] ~rank:0;
  show [ x ];
  [%expect
    {|
    ((m0.stage Const..Nonportable (generic))
     (m0.provenance Static..External
      (generic (m0.provenance <= (const_is_static m0.stage))))
     (m0.locality Global..Local (generic)))
    |}]
;;

let%expect_test "implied constraints are dropped" =
  let ids = Id_source.create () in
  let x = Var.create ids ~rank:1
  and y = Var.create ids ~rank:1
  and outer = Var.create ids ~rank:0
  and i1 = Var.create ids ~rank:1
  and i2 = Var.create ids ~rank:1 in
  ok (Var.submode x y (reason "x flows to y"));
  ok (Var.submode y outer (reason "y is stored"));
  (* [x <= outer] through internal variables, implied by the above. *)
  ok (Var.submode x i1 (reason "x flows to i1"));
  ok (Var.submode i1 outer (reason "i1 is stored"));
  (* [x <= y] and the weaker [x <= cross_portable(y)]. *)
  ok (Var.submode ~crossing:Crossing.immediate x i2 (reason "an int"));
  ok (Var.submode i2 y (reason "i2 flows to y"));
  Var.generalize [ x; y ] ~rank:0;
  show [ x; y ];
  [%expect
    {|
    ((m0.stage Const..Nonportable (generic (m0.stage <= m1.stage)))
     (m0.provenance Static..External
      (generic (m0.provenance <= m1.provenance)
       (m0.provenance <= (const_is_static m0.stage))))
     (m0.locality Global..Local (generic (m0.locality <= m1.locality))))
    ((m1.stage Const..Nonportable (generic (m1.stage <= m2.stage)))
     (m1.provenance Static..External
      (generic (m1.provenance <= m2.provenance)
       (m1.provenance <= (const_is_static m1.stage))))
     (m1.locality Global..Local (generic (m1.locality <= m2.locality))))
    |}]
;;

let%expect_test "defaulting an outer variable after generalization" =
  let ids = Id_source.create () in
  let lo = Var.create ids ~rank:0
  and hi = Var.create ids ~rank:0
  and k = Var.create ids ~rank:1 in
  ok (Var.submode lo k (reason "captured"));
  ok (Var.submode k hi (reason "stored"));
  ok (Var.at_least lo Locality Regional (reason "lo is regional"));
  Var.generalize [ k ] ~rank:0;
  Var.default_to_lower lo;
  Var.default_to_upper hi;
  show [ lo; hi ];
  let k1 = Var.Instance.instantiate (Var.Instance.create ids ~rank:0) k in
  show [ k1 ];
  [%expect
    {|
    ((m0.stage Const) (m0.provenance Static) (m0.locality Regional))
    ((m1.stage Nonportable) (m1.provenance External) (m1.locality Local))
    ((m3.stage Const..Nonportable) (m3.provenance Static..External)
     (m3.locality Regional..Local))
    |}]
;;

let%expect_test "constraints implied only through outer variables are kept, as \
                 a later generalization may separate them"
  =
  let ids = Id_source.create () in
  let p = Var.create ids ~rank:1
  and q = Var.create ids ~rank:1
  and inner = Var.create ids ~rank:2 in
  ok (Var.submode inner p (reason "inner flows to p"));
  ok (Var.submode p q (reason "p flows to q"));
  ok (Var.submode inner q (reason "inner flows to q"));
  Var.generalize [ inner ] ~rank:1;
  Var.generalize [ q ] ~rank:0;
  let instance = Var.Instance.create ids ~rank:0 in
  let q1 = Var.Instance.instantiate instance q
  and inner1 = Var.Instance.instantiate instance inner in
  ok (Var.at_least inner1 Locality Local (reason "inner1 is local"));
  show [ inner1; q1 ];
  [%expect
    {|
    ((m4.stage Const..Nonportable) (m4.provenance Static..External)
     (m4.locality Local))
    ((m3.stage Const..Nonportable) (m3.provenance Static..External)
     (m3.locality Local))
    |}]
;;

let%expect_test "generalize is not allowed under atomically" =
  let ids = Id_source.create () in
  let x = Var.create ids ~rank:1 in
  Expect_test_helpers_core.require_does_raise (fun () ->
    Private.Undo.atomically (fun () ->
      Var.generalize [ x ] ~rank:0;
      Ok ()));
  show [ x ];
  [%expect
    {|
    ("cannot be undone, so not allowed under atomically" generalize)
    ((m0.stage Const..Nonportable) (m0.provenance Static..External)
     (m0.locality Global..Local))
    |}]
;;

let%expect_test "instances made under a failed atomically are forgotten" =
  let ids = Id_source.create () in
  let x = Var.create ids ~rank:1 in
  Var.generalize [ x ] ~rank:0;
  let instance = Var.Instance.create ids ~rank:0 in
  let result =
    Private.Undo.atomically (fun () ->
      let x1 = Var.Instance.instantiate instance x in
      let%bind.Result () =
        Var.at_least x1 Locality Local (reason "x1 is local")
      in
      Var.at_most x1 Locality Global (reason "x1 is global"))
  in
  print_result result;
  let x1 = Var.Instance.instantiate instance x in
  ok (Var.at_most x1 Locality Global (reason "x1 is global"));
  show [ x1 ];
  [%expect
    {|
    (Error
     ("Mode mismatch" (var m1) (axis locality) (lower Local) (upper Global)
      (lower_because ("x1 is local")) (upper_because ("x1 is global"))))
    ((m1.stage Const..Nonportable) (m1.provenance Static..External)
     (m1.locality Global))
    |}]
;;

let%expect_test "guarded constraints are added once their layout has pointers, \
                 in either order"
  =
  let ids = Id_source.create () in
  let run ~raise_first =
    let layout = Layout.create layouts in
    let x = Var.create ids ~rank:1
    and y = Var.create ids ~rank:1 in
    let raise () =
      ok (Layout.raise layout ~because:[ reason "'a turned out to be string" ])
    in
    if raise_first then raise ();
    ok
      (Var.submode
         ~crossing:(Crossing.while_pointer_free layout)
         x
         y
         (reason "x is y"));
    ok (Var.at_least x Locality Local (reason "x is local"));
    print_s [%message "before" ~y:(Var.lower y : Const.t)];
    if not raise_first then raise ();
    print_s [%message "after" ~y:(Var.lower y : Const.t)]
  in
  run ~raise_first:false;
  [%expect
    {|
    (before (y ((stage Const) (provenance Static) (locality Global))))
    (after (y ((stage Const) (provenance Static) (locality Local))))
    |}];
  run ~raise_first:true;
  [%expect
    {|
    (before (y ((stage Const) (provenance Static) (locality Local))))
    (after (y ((stage Const) (provenance Static) (locality Local))))
    |}];
  (* A pointer-free layout never adds them; a layout above another has pointers
     once it does. *)
  let lower = Layout.create layouts
  and upper = Layout.create layouts in
  ok (Layout.add_below ~lower upper);
  let x = Var.create ids ~rank:1
  and y = Var.create ids ~rank:1 in
  ok
    (Var.submode
       ~crossing:(Crossing.while_pointer_free upper)
       x
       y
       (reason "x is y"));
  ok (Var.at_least x Locality Local (reason "x is local"));
  ok (Layout.raise lower ~because:[ reason "'b turned out to be string" ]);
  print_s
    [%message (Layout.has_pointers upper : bool) ~y:(Var.lower y : Const.t)];
  [%expect
    {|
    (("Layout.has_pointers upper" true)
     (y ((stage Const) (provenance Static) (locality Local))))
    |}];
  let annotated = Layout.create layouts in
  ok (Layout.bound_by annotated (reason "annotated pointer_free"));
  print_result (Layout.raise annotated ~because:[ reason "it is a string" ]);
  [%expect
    {|
    (Error
     ("Has pointers" (because ("it is a string"))
      (bound ((reason "annotated pointer_free") (instance ())))))
    |}]
;;

let%expect_test "a scheme keeps constraints guarded by a layout not \
                 generalized, through internal variables, for each instance"
  =
  let ids = Id_source.create () in
  let layout = Layout.create layouts in
  (* [let id y = let z = y in z], of a type ['a] of an enclosing scope, not
     known. *)
  let y = Var.create ids ~rank:1
  and z = Var.create ids ~rank:1
  and result = Var.create ids ~rank:1 in
  let crossing = Crossing.while_pointer_free layout in
  ok (Var.submode ~crossing y z (reason "y is bound to z"));
  ok (Var.submode ~crossing z result (reason "z is returned"));
  Var.generalize [ y; result ] ~rank:0;
  show [ y; result ];
  [%expect
    {|
    ((m0.stage Const..Nonportable
      (generic (m0.stage <= (cross_portable m2.stage))
       (m0.stage <= m2.stage unless (l5))))
     (m0.provenance Static..External
      (generic (m0.provenance <= (const_is_static m0.stage))
       (m0.provenance <= m2.provenance unless (l5))))
     (m0.locality Global..Local
      (generic (m0.locality <= m2.locality unless (l5)))))
    ((m2.stage Const..Nonportable (generic))
     (m2.provenance Static..External
      (generic (m2.provenance <= (const_is_static m2.stage))))
     (m2.locality Global..Local (generic)))
    |}];
  let instance () =
    let instance = Var.Instance.create ids ~rank:0 in
    let y = Var.Instance.instantiate instance y in
    let result = Var.Instance.instantiate instance result in
    ok (Var.at_least y Locality Local (reason "a local argument"));
    result
  in
  let before = instance () in
  print_s [%message (Var.lower before : Const.t)];
  [%expect
    {| ("Var.lower before" ((stage Const) (provenance Static) (locality Global))) |}];
  ok (Layout.raise layout ~because:[ reason "'a is string" ]);
  let after = instance () in
  print_s [%message (Var.lower before : Const.t) (Var.lower after : Const.t)];
  [%expect
    {|
    (("Var.lower before" ((stage Const) (provenance Static) (locality Local)))
     ("Var.lower after" ((stage Const) (provenance Static) (locality Local))))
    |}]
;;

let%expect_test "a guarded constraint that failed in an attempt is dropped" =
  let ids = Id_source.create () in
  let first = Layout.create layouts
  and second = Layout.create layouts
  and both = Layout.create layouts in
  ok (Layout.add_below ~lower:first both);
  ok (Layout.add_below ~lower:second both);
  let x = Var.create ids ~rank:1
  and y = Var.create ids ~rank:1 in
  ok (Var.at_least x Locality Local (reason "x is local"));
  ok (Var.at_most y Locality Global (reason "y is global"));
  ok
    (Var.submode
       ~crossing:(Crossing.while_pointer_free both)
       x
       y
       (reason "x is y"));
  let raise layout what =
    Layout.attempt (fun () -> Layout.raise layout ~because:[ reason what ])
  in
  print_result (raise first "'a turned out to be string");
  print_s [%message (Layout.has_pointers first : bool)];
  [%expect
    {|
    (Error
     ("Mode mismatch" (var m1) (axis locality) (lower Local) (upper Global)
      (lower_because ("x is local" "x is y" "'a turned out to be string"))
      (upper_because ("y is global"))))
    ("Layout.has_pointers first" false)
    |}];
  (* Another way it has pointers doesn't fail again. *)
  print_result (raise second "'b turned out to be string");
  print_s
    [%message (Layout.has_pointers both : bool) ~y:(Var.lower y : Const.t)];
  [%expect
    {|
    (Ok ())
    (("Layout.has_pointers both" true)
     (y ((stage Const) (provenance Static) (locality Global))))
    |}];
  (* Under a failed atomically, the drop is undone with the rest. *)
  let third = Layout.create layouts
  and guarded = Layout.create layouts in
  ok (Layout.add_below ~lower:third guarded);
  let z = Var.create ids ~rank:1 in
  ok (Var.at_most z Locality Global (reason "z is global"));
  ok
    (Var.submode
       ~crossing:(Crossing.while_pointer_free guarded)
       x
       z
       (reason "x is z"));
  print_result
    (Private.Undo.atomically (fun () ->
       let%bind.Result () = raise third "'c turned out to be string" in
       Error
         (Error.Has_pointers
            { because = []
            ; bound = { reason = reason "abandoned"; instance = None }
            })));
  print_result (raise guarded "'d turned out to be string");
  [%expect
    {|
    (Error
     ("Mode mismatch" (var m2) (axis locality) (lower Local) (upper Global)
      (lower_because ("x is local" "x is z" "'c turned out to be string"))
      (upper_because ("z is global"))))
    (Error
     ("Mode mismatch" (var m2) (axis locality) (lower Local) (upper Global)
      (lower_because ("x is local" "x is z" "'d turned out to be string"))
      (upper_because ("z is global"))))
    |}]
;;

(* One binding giving a layout pointers is one error: the rest it guards follow
   from it, so are dropped with the one that failed. *)
let%expect_test "what a failed layout guards is dropped together" =
  let ids = Id_source.create () in
  let layout = Layout.create layouts in
  let x = Var.create ids ~rank:1 in
  ok (Var.at_least x Locality Local (reason "x is local"));
  let uppers =
    List.init 3 ~f:(fun i ->
      let y = Var.create ids ~rank:1 in
      ok
        (Var.at_most y Locality Global (reason [%string "y%{i#Int} is global"]));
      ok
        (Var.submode
           ~crossing:(Crossing.while_pointer_free layout)
           x
           y
           (reason [%string "x is y%{i#Int}"]));
      y)
  in
  print_result
    (Layout.attempt (fun () ->
       Layout.raise layout ~because:[ reason "'a turned out to be string" ]));
  print_result (Layout.raise layout ~because:[ reason "again" ]);
  print_s [%message (List.map uppers ~f:Var.lower : Const.t list)];
  [%expect
    {|
    (Error
     ("Mode mismatch" (var m1) (axis locality) (lower Local) (upper Global)
      (lower_because ("x is local" "x is y2" "'a turned out to be string"))
      (upper_because ("y2 is global"))))
    (Ok ())
    ("List.map uppers ~f:Var.lower"
     (((stage Const) (provenance Static) (locality Global))
      ((stage Const) (provenance Static) (locality Global))
      ((stage Const) (provenance Static) (locality Global))))
    |}]
;;

(* Layouts above each other, as nested types: deciding the bottom one decides
   them all, and fires what each guards once. *)
let%expect_test "a chain of layouts" =
  let ids = Id_source.create () in
  let chain = List.init 20 ~f:(fun _ -> Layout.create layouts) in
  List.iter
    (List.zip_exn (List.drop_last_exn chain) (List.tl_exn chain))
    ~f:(fun (lower, upper) -> ok (Layout.add_below ~lower upper));
  let x = Var.create ids ~rank:1 in
  ok (Var.at_least x Locality Local (reason "x is local"));
  let ys =
    List.mapi chain ~f:(fun i layout ->
      let y = Var.create ids ~rank:1 in
      ok
        (Var.submode
           ~crossing:(Crossing.while_pointer_free layout)
           x
           y
           (reason [%string "x is y%{i#Int}"]));
      y)
  in
  ok
    (Layout.raise
       (List.hd_exn chain)
       ~because:[ reason "'a turned out to be string" ]);
  ok (Layout.raise (List.last_exn chain) ~because:[ reason "again" ]);
  print_s
    [%sexp
      (List.map ys ~f:(fun y -> (Var.lower y).locality) : Axis.Locality.t list)];
  [%expect
    {|
    (Local Local Local Local Local Local Local Local Local Local Local Local
     Local Local Local Local Local Local Local Local)
    |}]
;;

(* [low] raises [fine], which fires a constraint that holds, then [failing],
   which fires one that fails: undone, [fine]'s doesn't fire again later. *)
let%expect_test "what a layout raised in a failed attempt guards is dropped" =
  let ids = Id_source.create () in
  let low = Layout.create layouts
  and fine = Layout.create layouts
  and failing = Layout.create layouts in
  ok (Layout.add_below ~lower:low failing);
  ok (Layout.add_below ~lower:low fine);
  let x = Var.create ids ~rank:1
  and y = Var.create ids ~rank:1
  and z = Var.create ids ~rank:1 in
  ok (Var.at_least x Locality Local (reason "x is local"));
  ok (Var.at_most z Locality Global (reason "z is global"));
  ok
    (Var.submode
       ~crossing:(Crossing.while_pointer_free fine)
       x
       y
       (reason "x is y"));
  ok
    (Var.submode
       ~crossing:(Crossing.while_pointer_free failing)
       x
       z
       (reason "x is z"));
  let raise layout what =
    Layout.attempt (fun () -> Layout.raise layout ~because:[ reason what ])
  in
  print_result (raise low "'a turned out to be string");
  print_s [%message "" ~y:(Var.lower y : Const.t)];
  (* [y] could only fail later, if bounded: it fired with the failed binding. *)
  ok (Var.at_most y Locality Global (reason "y is global"));
  print_result (raise fine "'b turned out to be string");
  print_s
    [%message (Layout.has_pointers fine : bool) ~y:(Var.lower y : Const.t)];
  [%expect
    {|
    (Error
     ("Mode mismatch" (var m2) (axis locality) (lower Local) (upper Global)
      (lower_because ("x is local" "x is z" "'a turned out to be string"))
      (upper_because ("z is global"))))
    (y ((stage Const) (provenance Static) (locality Global)))
    (Ok ())
    (("Layout.has_pointers fine" true)
     (y ((stage Const) (provenance Static) (locality Global))))
    |}]
;;

let%expect_test "a layout of both has pointers once both of its own do" =
  let show name layout = printf "%s: %b\n" name (Layout.has_pointers layout) in
  let a = Layout.create layouts
  and b = Layout.create layouts in
  let ab = Layout.both layouts a b in
  ok (Layout.raise a ~because:[ reason "a" ]);
  show "after a" ab;
  ok (Layout.raise b ~because:[ reason "b" ]);
  show "after a and b" ab;
  let a = Layout.create layouts
  and b = Layout.create layouts in
  let ab = Layout.both layouts a b in
  Layout.pointer_free b;
  ok (Layout.raise a ~because:[ reason "a" ]);
  printf "unknown once b is pointer-free: %b\n" (Layout.is_unknown ab);
  let a = Layout.create layouts
  and b = Layout.create layouts in
  let ab = Layout.both layouts a b in
  ok (Layout.bound_by ab (reason "annotated pointer_free"));
  ok (Layout.raise a ~because:[ reason "a" ]);
  print_result (Layout.raise b ~because:[ reason "b" ]);
  (* Undone with the rest. *)
  let a = Layout.create layouts
  and b = Layout.create layouts in
  ok (Layout.raise a ~because:[ reason "a" ]);
  print_result
    (Private.Undo.atomically (fun () ->
       let ab = Layout.both layouts a b in
       let%bind.Result () = Layout.raise b ~because:[ reason "b" ] in
       show "inside" ab;
       Error
         (Error.Has_pointers
            { because = []
            ; bound = { reason = reason "abandoned"; instance = None }
            })));
  printf "b after: %b\n" (Layout.has_pointers b);
  [%expect
    {|
    after a: false
    after a and b: true
    unknown once b is pointer-free: false
    (Error
     ("Has pointers" (because (b a))
      (bound ((reason "annotated pointer_free") (instance ())))))
    inside: true
    (Error
     ("Has pointers" (because ()) (bound ((reason abandoned) (instance ())))))
    b after: false
    |}]
;;

let locality var = Var.node var Locality

let%expect_test "a scheme's bounds include what holds of its guarded \
                 constraints, from outer variables too"
  =
  let ids = Id_source.create () in
  let layout = Layout.create layouts in
  let crossing = Crossing.while_pointer_free layout in
  (* [z] is eliminated: [x] keeps [local <= x unless l]. [outer] stays: [x]
     keeps [outer <= x unless l]. *)
  let outer = Var.create ids ~rank:0
  and z = Var.create ids ~rank:1
  and x = Var.create ids ~rank:1
  and y = Var.create ids ~rank:1 in
  ok (Var.at_least z Locality Regional (reason "z is regional"));
  ok (Var.submode ~crossing z x (reason "z is x"));
  ok (Var.submode ~crossing outer x (reason "outer is x"));
  ok (Var.submode x y (reason "x is y"));
  Var.generalize [ x; y ] ~rank:0;
  ok (Var.at_least outer Locality Local (reason "outer is local"));
  let print () =
    let instance = Var.Instance.instantiate (Var.Instance.create ids ~rank:0) in
    print_s
      [%message
        ""
          ~scheme_lower:(Var.Node.lower (locality x) : int)
          ~instance_lower:((Var.lower (instance x)).locality : Axis.Locality.t)
          ~raised:
            (Var.Node.raised [ locality x, 1 ]
             |> Option.map
                  ~f:
                    (List.map ~f:(fun (node, point) ->
                       Var.Node.var_id node, point))
             : (int * int) list option)]
  in
  print ();
  ok (Layout.raise layout ~because:[ reason "'a is string" ]);
  print ();
  [%expect
    {|
    ((scheme_lower 0) (instance_lower Global) (raised (((2 1) (3 1)))))
    ((scheme_lower 2) (instance_lower Local) (raised (((2 2) (3 2)))))
    |}]
;;

let%expect_test "the reasons of a scheme's guarded bounds start at their origin"
  =
  let ids = Id_source.create () in
  let lower = Layout.create layouts
  and upper = Layout.create layouts in
  let crossing = Crossing.while_pointer_free lower in
  let z1 = Var.create ids ~rank:1
  and z2 = Var.create ids ~rank:1
  and x = Var.create ids ~rank:1 in
  ok (Var.at_least z1 Locality Local (reason "z1 is annotated local"));
  ok (Var.submode z1 z2 (reason "z1 is z2"));
  ok (Var.submode ~crossing z2 x (reason "z2 is x"));
  let w1 = Var.create ids ~rank:1
  and w2 = Var.create ids ~rank:1
  and y = Var.create ids ~rank:1 in
  ok (Var.at_most w2 Locality Global (reason "w2 is annotated global"));
  ok (Var.submode w1 w2 (reason "w1 is w2"));
  ok
    (Var.submode
       ~crossing:(Crossing.while_pointer_free upper)
       y
       w1
       (reason "y is w1"));
  Var.generalize [ x; y ] ~rank:0;
  let instance = Var.Instance.create ids ~rank:0 in
  let x = Var.Instance.instantiate instance x
  and y = Var.Instance.instantiate instance y in
  ok (Var.at_most x Locality Global (reason "x is global"));
  ok (Var.at_least y Locality Local (reason "y is local"));
  print_result (Layout.raise lower ~because:[ reason "'a is string" ]);
  print_result (Layout.raise upper ~because:[ reason "'b is string" ]);
  [%expect
    {|
    (Error
     ("Mode mismatch" (var m6) (axis locality) (lower Local) (upper Global)
      (lower_because
       ("z1 is annotated local" "z1 is z2" "z2 is x" "'a is string"))
      (upper_because ("x is global"))))
    (Error
     ("Mode mismatch" (var m7) (axis locality) (lower Local) (upper Global)
      (lower_because ("y is local"))
      (upper_because
       ("w2 is annotated global" "w1 is w2" "y is w1" "'b is string"))))
    |}]
;;

let%expect_test "a scheme's guarded bounds raise kept nodes above, through \
                 each path"
  =
  let ids = Id_source.create () in
  let layout = Layout.create layouts in
  let crossing = Crossing.while_pointer_free layout in
  (* [outer] reaches [x] through [a] as it is, and through [b] released. *)
  let outer = Var.create ids ~rank:0 in
  let make ~a_first =
    let a = Var.create ids ~rank:1
    and b = Var.create ids ~rank:1
    and x = Var.create ids ~rank:1 in
    let via_a () =
      ok (Var.submode ~crossing outer a (reason "outer is a"));
      ok (Var.submode ~crossing a x (reason "a is x"))
    in
    let via_b () =
      ok
        (Var.submode
           ~crossing
           ~locality:Locality_map.release
           outer
           b
           (reason "outer is b"));
      ok (Var.submode ~crossing b x (reason "b is x"))
    in
    if a_first
    then (
      via_a ();
      via_b ())
    else (
      via_b ();
      via_a ());
    a, b, x
  in
  let first = make ~a_first:true in
  let second = make ~a_first:false in
  let kept (a, b, x) = [ a; b; x ] in
  Var.generalize (kept first @ kept second) ~rank:0;
  ok (Var.at_least outer Locality Regional (reason "outer is regional"));
  ok (Layout.raise layout ~because:[ reason "'a is string" ]);
  let show_raised (a, b, x) =
    let raised =
      Var.Node.raised [ locality a, 0; locality b, 0 ]
      |> Option.value_exn
      |> List.find_map ~f:(fun (node, point) ->
        Option.some_if
          (Var.Node.var_id node = Var.Node.var_id (locality x))
          point)
    in
    print_s
      [%message
        ""
          ~a:(Var.Node.lower (locality a) : int)
          ~b:(Var.Node.lower (locality b) : int)
          ~x:(raised : int option)]
  in
  show_raised first;
  show_raised second;
  [%expect {|
    ((a 1) (b 2) (x (2)))
    ((a 1) (b 2) (x (2)))
    |}]
;;

let%expect_test "a scheme's guarded bounds include those of nodes generalized \
                 later"
  =
  let ids = Id_source.create () in
  let layout = Layout.create layouts in
  let crossing = Crossing.while_pointer_free layout in
  (* [t]'s scheme keeps [lo <= t unless l], then [lo] is generalized too, with
     [o <= lo unless l]. Through both, [o]'s bound reaches [t]. *)
  let o = Var.create ids ~rank:0
  and lo = Var.create ids ~rank:1
  and t = Var.create ids ~rank:2 in
  ok (Var.submode ~crossing lo t (reason "lo is t"));
  Var.generalize [ t ] ~rank:1;
  ok (Var.submode ~crossing o lo (reason "o is lo"));
  Var.generalize [ lo ] ~rank:0;
  ok (Var.at_least o Locality Regional (reason "o is regional"));
  ok (Layout.raise layout ~because:[ reason "'a is string" ]);
  let raised seeds =
    Var.Node.raised seeds
    |> Option.map
         ~f:(List.map ~f:(fun (node, point) -> Var.Node.var_id node, point))
    |> Option.map ~f:(List.sort ~compare:[%compare: int * int])
  in
  print_s
    [%message
      ""
        ~lo:(Var.Node.lower (locality lo) : int)
        ~t:(Var.Node.lower (locality t) : int)
        ~raised_t:(raised [ locality t, 0 ] : (int * int) list option)
        ~raised_lo_t:
          (raised [ locality lo, 0; locality t, 0 ] : (int * int) list option)];
  [%expect
    {| ((lo 1) (t 1) (raised_t (((2 1)))) (raised_lo_t (((1 1) (2 1))))) |}]
;;

let%expect_test "a scheme's guarded bounds through a diamond of later schemes" =
  let ids = Id_source.create () in
  let layout = Layout.create layouts in
  let crossing = Crossing.while_pointer_free layout in
  (* [o] reaches [t] through [a] as it is, and through [b] released, each
     generalized after [t]. *)
  let o = Var.create ids ~rank:0
  and a = Var.create ids ~rank:1
  and b = Var.create ids ~rank:1
  and t = Var.create ids ~rank:2 in
  ok (Var.submode ~crossing a t (reason "a is t"));
  ok (Var.submode ~crossing b t (reason "b is t"));
  Var.generalize [ t ] ~rank:1;
  ok (Var.submode ~crossing o a (reason "o is a"));
  ok
    (Var.submode ~crossing ~locality:Locality_map.release o b (reason "o is b"));
  Var.generalize [ a; b ] ~rank:0;
  ok (Var.at_least o Locality Regional (reason "o is regional"));
  ok (Layout.raise layout ~because:[ reason "'a is string" ]);
  print_s
    [%message
      ""
        ~a:(Var.Node.lower (locality a) : int)
        ~b:(Var.Node.lower (locality b) : int)
        ~t:(Var.Node.lower (locality t) : int)];
  [%expect {| ((a 1) (b 2) (t 2)) |}]
;;

let%expect_test "guarded bounds along a chain of nested schemes take linear \
                 time"
  =
  (* [o <= lo_n <= ... <= lo_1 <= t], each node generalized after the edge into
     it from the next. *)
  let chain n =
    let ids = Id_source.create () in
    let layout = Layout.create layouts in
    let crossing = Crossing.while_pointer_free layout in
    let t = Var.create ids ~rank:(n + 1) in
    let rec build node ~rank ~acc =
      let lo = Var.create ids ~rank in
      ok (Var.submode ~crossing lo node (reason "lo is node"));
      Var.generalize [ node ] ~rank;
      if rank = 0 then lo, acc else build lo ~rank:(rank - 1) ~acc:(lo :: acc)
    in
    let o, nodes = build t ~rank:n ~acc:[ t ] in
    ok (Var.at_least o Locality Regional (reason "o is regional"));
    ok (Layout.raise layout ~because:[ reason "'a is string" ]);
    let (lower, raised), counts =
      Work.Private.measure (fun () ->
        ( Var.Node.lower (locality t)
        , Var.Node.raised (List.map nodes ~f:(fun var -> locality var, 0))
          |> Option.value_exn ))
    in
    lower, List.length raised, counts
  in
  let small_lower, small_raised, _ = chain 200 in
  let large_lower, large_raised, _ = chain 800 in
  let growth =
    Work.Private.growth ~small:200 ~large:800 (fun n ->
      let _, _, counts = chain n in
      counts)
  in
  print_s
    [%message
      ""
        (small_lower : int)
        (small_raised : int)
        (large_lower : int)
        (large_raised : int)
        (growth : Work.Private.Growth.t String.Map.t)];
  [%expect
    {|
    ((small_lower 1) (small_raised 201) (large_lower 1) (large_raised 801)
     (growth ((allocated_words Linear) (mode.graph Linear))))
    |}]
;;
