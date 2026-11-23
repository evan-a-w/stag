open! Import

let reason = Reason.create

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
       (m0.provenance <= (const_is_static m2.stage))
       (m0.provenance <= (const_is_static m0.stage))))
     (m0.locality Global..Local (generic (m0.locality <= m2.locality))))
    ((m1.stage Const..Nonportable) (m1.provenance Static..Unknown)
     (m1.locality Global..Local))
    ((m2.stage Const..Nonportable (generic))
     (m2.provenance Static..Unknown
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
    ((m4.stage Const..Nonportable) (m4.provenance Static..Unknown)
     (m4.locality Global..Local))
    ((m5.stage Const..Nonportable) (m5.provenance Static..Gc)
     (m5.locality Global..Local))
    ((m6.stage Const..Nonportable) (m6.provenance Static..Unknown)
     (m6.locality Global..Local))
    |}];
  ok (Var.at_least x1 Locality Local (reason "local argument"));
  ok (Var.at_most result2 Stage Portable (reason "result in a quote"));
  show [ x1; result1; x2; result2; x; y; result ];
  [%expect
    {|
    ((m3.stage Const..Nonportable) (m3.provenance Static..Gc)
     (m3.locality Local))
    ((m4.stage Const..Nonportable) (m4.provenance Static..Unknown)
     (m4.locality Local))
    ((m5.stage Const..Portable) (m5.provenance Static..Gc)
     (m5.locality Global..Local))
    ((m6.stage Const..Portable) (m6.provenance Static..Unknown)
     (m6.locality Global..Local))
    ((m0.stage Const..Nonportable (generic (m0.stage <= m2.stage)))
     (m0.provenance Static..Gc
      (generic (m0.provenance <= m2.provenance)
       (m0.provenance <= (const_is_static m2.stage))
       (m0.provenance <= (const_is_static m0.stage))))
     (m0.locality Global..Local (generic (m0.locality <= m2.locality))))
    ((m1.stage Const..Nonportable) (m1.provenance Static..Unknown)
     (m1.locality Global..Local))
    ((m2.stage Const..Nonportable (generic))
     (m2.provenance Static..Unknown
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
      (generic (m2.stage <= m1.stage) (m0.stage <= m2.stage)
       (m0.provenance <= (const_is_static m2.stage))))
     (m2.provenance Static..Unknown
      (generic (m2.provenance <= m1.provenance)
       (m2.provenance <= (const_is_static m2.stage))
       (m0.provenance <= m2.provenance)))
     (m2.locality Global..Local
      (generic (m2.locality <= m1.locality) (m0.locality <= m2.locality))))
    ((m0.stage Const..Nonportable) (m0.provenance Static..Unknown)
     (m0.locality Global..Local))
    ((m1.stage Const..Nonportable) (m1.provenance Static..Unknown)
     (m1.locality Global..Local))
    |}];
  let k1 = Var.Instance.instantiate (Var.Instance.create ids ~rank:0) k in
  ok (Var.at_least outer_in Locality Regional (reason "outer_in is regional"));
  show [ k1 ];
  ok (Var.at_least k1 Stage Nonportable (reason "k1 is nonportable"));
  show [ outer_out ];
  [%expect
    {|
    ((m4.stage Const..Nonportable) (m4.provenance Static..Unknown)
     (m4.locality Regional..Local))
    ((m1.stage Nonportable) (m1.provenance Static..Unknown)
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
     (m0.provenance Static..Unknown
      (generic (m0.provenance <= (const_is_static m0.stage))))
     (m0.locality Global..Local (generic)))
    ((m2.stage Const..Nonportable (generic))
     (m2.provenance Static..Unknown
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
    ((m3.stage Nonportable) (m3.provenance Unknown) (m3.locality Local))
    ((m4.stage Portable..Nonportable) (m4.provenance Static..Unknown)
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
     (m0.provenance Static..Unknown
      (generic (m0.provenance <= m1.provenance)
       (m0.provenance <= (const_is_static m0.stage))))
     (m0.locality Global..Local (generic (m0.locality <= m1.locality))))
    ((m1.stage Const..Nonportable (generic (m1.stage <= m0.stage)))
     (m1.provenance Static..Unknown
      (generic (m1.provenance <= m0.provenance)
       (m1.provenance <= (const_is_static m1.stage))))
     (m1.locality Global..Local (generic (m1.locality <= m0.locality))))
    ((m2.stage Const..Nonportable) (m2.provenance Static..Unknown)
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
    ((m3.stage Const..Nonportable) (m3.provenance Static..Unknown)
     (m3.locality Local))
    ((m4.stage Const..Nonportable) (m4.provenance Static..Unknown)
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
     (m2.provenance Static..Unknown
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
     (m0.provenance Static..Unknown
      (generic (m0.provenance <= m1.provenance)
       (m0.provenance <= (const_is_static m1.stage))
       (m0.provenance <= (const_is_static m0.stage))))
     (m0.locality Global..Local (generic (m0.locality <= m1.locality))))
    ((m1.stage Const..Nonportable (generic))
     (m1.provenance Static..Unknown
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
     (m1.provenance Static..Unknown
      (generic (m1.provenance <= (const_is_static m1.stage))
       (m0.provenance <= m1.provenance)))
     (m1.locality Global..Local (generic (m0.locality <= m1.locality))))
    ((m2.stage Portable..Nonportable) (m2.provenance Static..Unknown)
     (m2.locality Global..Local))
    (Error
     ("Mode mismatch" (var m2) (axis stage) (lower Portable) (upper Const)
      (lower_because ("outer is computed" captured))
      (upper_because ("k1 is const"))))
    |}]
;;
