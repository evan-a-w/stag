open! Import

let reason = Reason.create

let ok_exn = function
  | Ok x -> x
  | Error error -> raise_s [%sexp (error : Error.t)]
;;

let print_result result = print_s [%sexp (result : (unit, Error.t) Result.t)]
let show vars = List.iter vars ~f:(fun var -> print_s [%sexp (var : Var.t)])

let%expect_test "a fresh variable is unconstrained, a constant one fixed" =
  let ids = Id_source.create () in
  let fresh = Var.create ids ~rank:0 in
  let const =
    Var.of_const
      ids
      ~rank:0
      { stage = Const; provenance = Static; locality = Global }
    |> ok_exn
  in
  let local =
    Var.of_const ids ~rank:0 { Const.max with locality = Local } |> ok_exn
  in
  show [ fresh; const; local ];
  print_s
    [%message
      ""
        ~lower:(Var.lower fresh : Const.t)
        ~upper:(Var.upper fresh : Const.t)
        ~ids:([ Var.id fresh; Var.id const; Var.id local ] : int list)];
  [%expect
    {|
    ((m0.stage Const..Nonportable) (m0.provenance Static..Unknown)
     (m0.locality Global..Local))
    ((m1.stage Const) (m1.provenance Static) (m1.locality Global))
    ((m2.stage Nonportable) (m2.provenance Unknown) (m2.locality Local))
    ((lower ((stage Const) (provenance Static) (locality Global)))
     (upper ((stage Nonportable) (provenance Unknown) (locality Local)))
     (ids (0 1 2)))
    |}]
;;

let%expect_test "const implies static" =
  let ids = Id_source.create () in
  let a = Var.create ids ~rank:0 in
  print_result (Var.at_most a Stage Const (reason "used at compile time"));
  show [ a ];
  let b = Var.create ids ~rank:0 in
  print_result (Var.at_least b Provenance Gc (reason "allocated"));
  show [ b ];
  print_result (Var.at_most b Stage Const (reason "used at compile time"));
  show [ b ];
  let c = Var.create ids ~rank:0 in
  print_result (Var.at_most c Stage Const (reason "used at compile time"));
  print_result (Var.at_least c Provenance Unknown (reason "from C"));
  show [ c ];
  [%expect
    {|
    (Ok ())
    ((m0.stage Const) (m0.provenance Static) (m0.locality Global..Local))
    (Ok ())
    ((m1.stage Portable..Nonportable) (m1.provenance Gc..Unknown)
     (m1.locality Global..Local))
    (Error
     ("Mode mismatch" (var m1) (axis stage) (lower Portable) (upper Const)
      (lower_because (allocated "const implies static"))
      (upper_because ("used at compile time"))))
    ((m1.stage Portable..Nonportable) (m1.provenance Gc..Unknown)
     (m1.locality Global..Local))
    (Ok ())
    (Error
     ("Mode mismatch" (var m2) (axis provenance) (lower Unknown) (upper Static)
      (lower_because ("from C"))
      (upper_because ("used at compile time" "const implies static"))))
    ((m2.stage Const) (m2.provenance Static) (m2.locality Global..Local))
    |}]
;;

let%expect_test "bounds propagate along edges, and errors explain them" =
  let ids = Id_source.create () in
  let a = Var.create ids ~rank:0
  and b = Var.create ids ~rank:0
  and c = Var.create ids ~rank:0 in
  print_result (Var.submode a b (reason "a flows to b"));
  print_result (Var.submode b c (reason "b flows to c"));
  print_result (Var.at_least a Locality Regional (reason "a is a parameter"));
  print_result (Var.at_most c Stage Portable (reason "c is used in a quote"));
  show [ a; b; c ];
  print_result (Var.at_most c Locality Global (reason "c is returned"));
  show [ a; b; c ];
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Ok ())
    (Ok ())
    ((m0.stage Const..Portable) (m0.provenance Static..Unknown)
     (m0.locality Regional..Local))
    ((m1.stage Const..Portable) (m1.provenance Static..Unknown)
     (m1.locality Regional..Local))
    ((m2.stage Const..Portable) (m2.provenance Static..Unknown)
     (m2.locality Regional..Local))
    (Error
     ("Mode mismatch" (var m2) (axis locality) (lower Regional) (upper Global)
      (lower_because ("a is a parameter" "a flows to b" "b flows to c"))
      (upper_because ("c is returned"))))
    ((m0.stage Const..Portable) (m0.provenance Static..Unknown)
     (m0.locality Regional..Local))
    ((m1.stage Const..Portable) (m1.provenance Static..Unknown)
     (m1.locality Regional..Local))
    ((m2.stage Const..Portable) (m2.provenance Static..Unknown)
     (m2.locality Regional..Local))
    |}]
;;

let%expect_test "a failed constraint changes nothing" =
  let ids = Id_source.create () in
  let a = Var.create ids ~rank:0
  and b = Var.create ids ~rank:0 in
  print_result (Var.at_most b Locality Global (reason "b is returned"));
  print_result (Var.at_least a Stage Portable (reason "a is computed"));
  print_result (Var.at_least a Locality Local (reason "a is local"));
  show [ a; b ];
  (* Stage is constrained first, succeeding, then locality fails. *)
  print_result (Var.submode a b (reason "a flows to b"));
  show [ a; b ];
  print_result
    (Var.at_least_const
       b
       { stage = Nonportable; provenance = Gc; locality = Local }
       (reason "all at once"));
  show [ b ];
  (* The failed edge is not there: [a] is unaffected. *)
  print_result (Var.at_most b Stage Const (reason "b is const"));
  show [ a; b ];
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Ok ())
    ((m0.stage Portable..Nonportable) (m0.provenance Static..Unknown)
     (m0.locality Local))
    ((m1.stage Const..Nonportable) (m1.provenance Static..Unknown)
     (m1.locality Global))
    (Error
     ("Mode mismatch" (var m1) (axis locality) (lower Local) (upper Global)
      (lower_because ("a is local" "a flows to b"))
      (upper_because ("b is returned"))))
    ((m0.stage Portable..Nonportable) (m0.provenance Static..Unknown)
     (m0.locality Local))
    ((m1.stage Const..Nonportable) (m1.provenance Static..Unknown)
     (m1.locality Global))
    (Error
     ("Mode mismatch" (var m1) (axis locality) (lower Local) (upper Global)
      (lower_because ("all at once")) (upper_because ("b is returned"))))
    ((m1.stage Const..Nonportable) (m1.provenance Static..Unknown)
     (m1.locality Global))
    (Ok ())
    ((m0.stage Portable..Nonportable) (m0.provenance Static..Unknown)
     (m0.locality Local))
    ((m1.stage Const) (m1.provenance Static) (m1.locality Global))
    |}]
;;

let%expect_test "equate" =
  let ids = Id_source.create () in
  let a = Var.create ids ~rank:0
  and b = Var.create ids ~rank:0 in
  print_result (Var.equate a b (reason "same arrow"));
  print_result (Var.at_least a Locality Regional (reason "a is regional"));
  print_result (Var.at_most b Provenance Gc (reason "b is gc"));
  show [ a; b ];
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Ok ())
    ((m0.stage Const..Nonportable) (m0.provenance Static..Gc)
     (m0.locality Regional..Local))
    ((m1.stage Const..Nonportable) (m1.provenance Static..Gc)
     (m1.locality Regional..Local))
    |}]
;;

let%expect_test "crossing" =
  let ids = Id_source.create () in
  let value () =
    Var.of_const
      ids
      ~rank:0
      { stage = Nonportable; provenance = Unknown; locality = Local }
    |> ok_exn
  in
  let target () =
    let t = Var.create ids ~rank:0 in
    print_result
      (Var.at_most_const
         t
         { stage = Portable; provenance = Static; locality = Global }
         (reason "target"));
    t
  in
  print_result (Var.submode (value ()) (target ()) (reason "no crossing"));
  print_result
    (Var.submode
       ~crossing:Crossing.immediate
       (value ())
       (target ())
       (reason "an int"));
  print_result
    (Var.submode
       ~crossing:{ Crossing.none with locality = true }
       (value ())
       (target ())
       (reason "crosses locality only"));
  (* Crossing never reaches const. *)
  let const_target = Var.create ids ~rank:0 in
  print_result (Var.at_most const_target Stage Const (reason "compile time"));
  print_result
    (Var.submode
       ~crossing:Crossing.immediate
       (value ())
       const_target
       (reason "an int"));
  print_result
    (Var.at_most_const
       ~crossing:Crossing.immediate
       (value ())
       { Const.min with stage = Portable }
       (reason "an int in a quote"));
  print_result
    (Var.at_most
       ~crossing:Crossing.immediate
       (value ())
       Stage
       Const
       (reason "an int"));
  (* Lower bounds still flow through a crossing edge, lowered. *)
  let a = Var.create ids ~rank:0
  and b = Var.create ids ~rank:0 in
  print_result (Var.submode ~crossing:Crossing.immediate a b (reason "an int"));
  print_result (Var.at_least a Stage Nonportable (reason "a is a parameter"));
  print_result (Var.at_least a Locality Local (reason "a is local"));
  print_result (Var.at_most b Stage Const (reason "b is const"));
  show [ a; b ];
  [%expect
    {|
    (Ok ())
    (Error
     ("Mode mismatch" (var m0) (axis stage) (lower Nonportable) (upper Portable)
      (lower_because (constant "no crossing")) (upper_because (target))))
    (Ok ())
    (Ok ())
    (Ok ())
    (Error
     ("Mode mismatch" (var m4) (axis stage) (lower Nonportable) (upper Portable)
      (lower_because (constant "crosses locality only"))
      (upper_because (target))))
    (Ok ())
    (Error
     ("Mode mismatch" (var m6) (axis stage) (lower Portable) (upper Const)
      (lower_because (constant "an int")) (upper_because ("compile time"))))
    (Ok ())
    (Error
     ("Mode mismatch" (var m9) (axis stage) (lower Nonportable) (upper Const)
      (lower_because (constant)) (upper_because ("an int"))))
    (Ok ())
    (Ok ())
    (Ok ())
    (Error
     ("Mode mismatch" (var m11) (axis stage) (lower Portable) (upper Const)
      (lower_because ("a is a parameter" "an int"))
      (upper_because ("b is const"))))
    ((m10.stage Nonportable) (m10.provenance Static..Unknown)
     (m10.locality Local))
    ((m11.stage Portable..Nonportable) (m11.provenance Static..Unknown)
     (m11.locality Global..Local))
    |}]
;;

let%expect_test "defaulting" =
  let ids = Id_source.create () in
  let a = Var.create ids ~rank:0
  and b = Var.create ids ~rank:0 in
  print_result (Var.submode a b (reason "a flows to b"));
  print_result (Var.at_least a Locality Regional (reason "a is regional"));
  Var.default_to_lower b;
  show [ a; b ];
  let c = Var.create ids ~rank:0
  and d = Var.create ids ~rank:0 in
  print_result (Var.submode c d (reason "c flows to d"));
  print_result (Var.at_most d Stage Portable (reason "d is portable"));
  Var.default_to_upper c;
  show [ c; d ];
  [%expect
    {|
    (Ok ())
    (Ok ())
    ((m0.stage Const) (m0.provenance Static) (m0.locality Regional))
    ((m1.stage Const) (m1.provenance Static) (m1.locality Regional))
    (Ok ())
    (Ok ())
    ((m2.stage Portable) (m2.provenance Unknown) (m2.locality Local))
    ((m3.stage Portable) (m3.provenance Unknown) (m3.locality Local))
    |}]
;;

let%expect_test "a constant that is const but not static is rejected" =
  let ids = Id_source.create () in
  print_s
    [%sexp
      (Var.of_const
         ids
         ~rank:0
         { stage = Const; provenance = Unknown; locality = Global }
       : (Var.t, Error.t) Result.t)];
  [%expect
    {|
    (Error
     ("Mode mismatch" (var m0) (axis stage) (lower Portable) (upper Const)
      (lower_because (constant "const implies static"))
      (upper_because (constant))))
    |}]
;;

let%expect_test "upper bounds carried over several edges explain their path" =
  let ids = Id_source.create () in
  let a = Var.create ids ~rank:0
  and b = Var.create ids ~rank:0
  and c = Var.create ids ~rank:0
  and d = Var.create ids ~rank:0 in
  print_result (Var.submode a b (reason "a flows to b"));
  print_result (Var.submode b c (reason "b flows to c"));
  print_result (Var.submode c d (reason "c flows to d"));
  print_result (Var.at_most d Locality Global (reason "d is returned"));
  print_result (Var.at_least a Locality Local (reason "a is local"));
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Ok ())
    (Ok ())
    (Error
     ("Mode mismatch" (var m0) (axis locality) (lower Local) (upper Global)
      (lower_because ("a is local"))
      (upper_because
       ("d is returned" "c flows to d" "b flows to c" "a flows to b"))))
    |}]
;;

let%expect_test "equate failing on its second half changes nothing" =
  let ids = Id_source.create () in
  let a = Var.create ids ~rank:0
  and b = Var.create ids ~rank:0 in
  print_result (Var.at_most a Locality Global (reason "a is returned"));
  print_result (Var.at_least b Locality Local (reason "b is local"));
  (* [a <= b] holds, [b <= a] does not. *)
  print_result (Var.equate a b (reason "unified"));
  (* Without the [a <= b] edge, [b] is unaffected. *)
  print_result (Var.at_least a Stage Nonportable (reason "a is computed"));
  show [ a; b ];
  [%expect
    {|
    (Ok ())
    (Ok ())
    (Error
     ("Mode mismatch" (var m0) (axis locality) (lower Local) (upper Global)
      (lower_because ("b is local" unified)) (upper_because ("a is returned"))))
    (Ok ())
    ((m0.stage Nonportable) (m0.provenance Static..Unknown) (m0.locality Global))
    ((m1.stage Const..Nonportable) (m1.provenance Static..Unknown)
     (m1.locality Local))
    |}]
;;

let%expect_test "lower bounds assuming some variables at their upper bounds" =
  let ids = Id_source.create () in
  let param = Var.create ids ~rank:1
  and other = Var.create ids ~rank:1
  and mid = Var.create ids ~rank:1
  and result = Var.create ids ~rank:1
  and generic = Var.create ids ~rank:1 in
  List.iter
    [ param, mid; mid, result; other, result; generic, result; result, mid ]
    ~f:(fun (a, b) -> ok_exn (Var.submode a b (reason "flows")));
  ok_exn (Var.at_most param Stage Portable (reason "param is portable"));
  ok_exn (Var.at_most param Provenance Gc (reason "param is gc"));
  ok_exn (Var.at_most other Locality Regional (reason "other is regional"));
  Var.generalize [ generic ] ~rank:0;
  let assuming raised =
    let raised = List.map raised ~f:Var.id in
    let raised id = List.mem raised id ~equal:Int.equal in
    print_s
      [%message
        ""
          ~stage:(Var.lower_assuming result Stage ~raised : Stage.t)
          ~provenance:
            (Var.lower_assuming result Provenance ~raised : Provenance.t)
          ~locality:(Var.lower_assuming result Locality ~raised : Locality.t)]
  in
  assuming [];
  assuming [ param ];
  assuming [ other ];
  assuming [ generic ];
  show [ result ];
  [%expect
    {|
    ((stage Const) (provenance Static) (locality Global))
    ((stage Portable) (provenance Gc) (locality Local))
    ((stage Nonportable) (provenance Unknown) (locality Regional))
    ((stage Const) (provenance Static) (locality Global))
    ((m3.stage Const..Nonportable) (m3.provenance Static..Unknown)
     (m3.locality Global..Local))
    |}]
;;
