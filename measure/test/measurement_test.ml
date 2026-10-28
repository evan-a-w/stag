open! Import

let%expect_test "Plan.create" =
  let plan
        ?(config = Config.default)
        ?(resolution = Time_ns.Span.nanosecond)
        ?setup
        per_iteration
    =
    let plan =
      Measurement.Plan.create ?setup config ~resolution ~per_iteration
    in
    print_s
      [%message
        ""
          (per_iteration : Time_ns.Span.t)
          (setup : (Time_ns.Span.t option[@sexp.option]))
          (resolution : Time_ns.Span.t)
          (plan : Measurement.Plan.t)]
  in
  (* 3s over 100 samples: 30ms a sample. *)
  plan (Time_ns.Span.of_int_ns 100);
  plan (Time_ns.Span.of_int_ms 20);
  plan (Time_ns.Span.of_int_ms 30);
  [%expect
    {|
    ((per_iteration 100ns) (resolution 1ns)
     (plan ((iterations 300000) (samples 100))))
    ((per_iteration 20ms) (resolution 1ns) (plan ((iterations 2) (samples 100))))
    ((per_iteration 30ms) (resolution 1ns) (plan ((iterations 1) (samples 100))))
    |}];
  (* A single iteration overruns the samples' share of [measurement_time]. *)
  plan (Time_ns.Span.of_int_ms 31);
  plan (Time_ns.Span.of_int_ms 100);
  plan (Time_ns.Span.of_int_ms 1000);
  plan (Time_ns.Span.of_int_sec 60);
  [%expect
    {|
    ((per_iteration 31ms) (resolution 1ns) (plan ((iterations 1) (samples 96))))
    ((per_iteration 100ms) (resolution 1ns) (plan ((iterations 1) (samples 30))))
    ((per_iteration 1s) (resolution 1ns) (plan ((iterations 1) (samples 10))))
    ((per_iteration 1m) (resolution 1ns) (plan ((iterations 1) (samples 10))))
    |}];
  (* Samples last at least 1000 resolutions. *)
  plan ~resolution:(Time_ns.Span.of_int_us 100) (Time_ns.Span.of_int_us 1);
  plan ~resolution:(Time_ns.Span.of_int_us 100) (Time_ns.Span.of_int_ms 200);
  [%expect
    {|
    ((per_iteration 1us) (resolution 100us)
     (plan ((iterations 100000) (samples 100))))
    ((per_iteration 200ms) (resolution 100us)
     (plan ((iterations 1) (samples 15))))
    |}];
  (* The untimed setup of each iteration counts towards the samples' time,
     but not towards the 1000 resolutions of their timed part. *)
  plan ~setup:(Time_ns.Span.of_int_us 900) (Time_ns.Span.of_int_us 100);
  plan ~setup:(Time_ns.Span.of_int_ms 100) (Time_ns.Span.of_int_us 2);
  plan
    ~resolution:(Time_ns.Span.of_int_us 1)
    ~setup:(Time_ns.Span.of_int_ms 100)
    (Time_ns.Span.of_int_us 2);
  [%expect
    {|
    ((per_iteration 100us) (setup 900us) (resolution 1ns)
     (plan ((iterations 30) (samples 100))))
    ((per_iteration 2us) (setup 100ms) (resolution 1ns)
     (plan ((iterations 1) (samples 29))))
    ((per_iteration 2us) (setup 100ms) (resolution 1us)
     (plan ((iterations 500) (samples 100))))
    |}];
  (* An iteration too fast to see takes 1ns. *)
  plan Time_ns.Span.zero;
  plan ~config:Config.quick Time_ns.Span.zero;
  [%expect
    {|
    ((per_iteration 0s) (resolution 1ns)
     (plan ((iterations 30000000) (samples 100))))
    ((per_iteration 0s) (resolution 1ns)
     (plan ((iterations 25000000) (samples 20))))
    |}]
;;

let config =
  { Config.quick with
    warm_up = Time_ns.Span.of_int_us 1
  ; measurement_time = Time_ns.Span.of_int_us 10
  ; samples = 5
  ; min_samples = 2
  ; resamples = 100
  }
;;

let ns = Time_ns.Span.of_int_ns

let print_run ?alloc ?(config = config) clock routine =
  print_s
    [%sexp
      (Measurement.run ?alloc config (Clock.Fake.clock clock) routine
       : Measurement.t Or_error.t)]
;;

let%expect_test "a routine of constant time" =
  let clock = Clock.Fake.create () in
  print_run
    clock
    (Plain
       (fun () ->
         Clock.Fake.advance clock (ns 10);
         Ok ()));
  (* Warm-up: batches of 1, 2, ..., 64 iterations, 127 in all, reach 1us at
     1270ns. Samples should take 10us / 5 = 2us, which is 200 iterations. *)
  [%expect
    {|
    (Ok
     ((warm_up_iterations 127) (warm_up_time 1.27us)
      (plan ((iterations 200) (samples 5)))
      (samples
       (((iterations 200) (time 2us) (alloc ()))
        ((iterations 200) (time 2us) (alloc ()))
        ((iterations 200) (time 2us) (alloc ()))
        ((iterations 200) (time 2us) (alloc ()))
        ((iterations 200) (time 2us) (alloc ()))))))
    |}]
;;

let%expect_test "a routine of varying time" =
  let clock = Clock.Fake.create () in
  let calls = ref 0 in
  print_run
    clock
    (Plain
       (fun () ->
         Clock.Fake.advance clock (ns (10 + (10 * (!calls % 3))));
         incr calls;
         Ok ()));
  [%expect
    {|
    (Ok
     ((warm_up_iterations 63) (warm_up_time 1.26us)
      (plan ((iterations 100) (samples 5)))
      (samples
       (((iterations 100) (time 1.99us) (alloc ()))
        ((iterations 100) (time 2us) (alloc ()))
        ((iterations 100) (time 2.01us) (alloc ()))
        ((iterations 100) (time 1.99us) (alloc ()))
        ((iterations 100) (time 2us) (alloc ()))))))
    |}]
;;

let%expect_test "slow iterations take fewer samples" =
  let clock = Clock.Fake.create () in
  print_run
    clock
    (Plain
       (fun () ->
         Clock.Fake.advance clock (Time_ns.Span.of_int_us 3);
         Ok ()));
  (* One iteration warms up for 3us, which is enough. 5 samples would take
     15us, but only 10us / 3us = 3 fit. *)
  [%expect
    {|
    (Ok
     ((warm_up_iterations 1) (warm_up_time 3us)
      (plan ((iterations 1) (samples 3)))
      (samples
       (((iterations 1) (time 3us) (alloc ()))
        ((iterations 1) (time 3us) (alloc ()))
        ((iterations 1) (time 3us) (alloc ()))))))
    |}]
;;

let%expect_test "the clock's ticks" =
  let clock = Clock.Fake.create ~tick:(ns 1) () in
  print_run
    clock
    (Plain
       (fun () ->
         Clock.Fake.advance clock (ns 100);
         Ok ()));
  (* Each read of the clock moves it 1ns, which the timing includes once. *)
  [%expect
    {|
    (Ok
     ((warm_up_iterations 15) (warm_up_time 1.504us)
      (plan ((iterations 20) (samples 5)))
      (samples
       (((iterations 20) (time 2.001us) (alloc ()))
        ((iterations 20) (time 2.001us) (alloc ()))
        ((iterations 20) (time 2.001us) (alloc ()))
        ((iterations 20) (time 2.001us) (alloc ()))
        ((iterations 20) (time 2.001us) (alloc ()))))))
    |}]
;;

let%expect_test "a routine faster than the clock" =
  let clock = Clock.Fake.create () in
  let calls = ref 0 in
  print_run
    clock
    (Plain
       (fun () ->
         incr calls;
         Ok ()));
  print_s [%message (calls : int ref)];
  (* Iterations count as 1ns: the warm-up stops after 1023 > 1us of them, and
     samples of 2us have 2000. *)
  [%expect
    {|
    (Ok
     ((warm_up_iterations 1023) (warm_up_time 0s)
      (plan ((iterations 2000) (samples 5)))
      (samples
       (((iterations 2000) (time 0s) (alloc ()))
        ((iterations 2000) (time 0s) (alloc ()))
        ((iterations 2000) (time 0s) (alloc ()))
        ((iterations 2000) (time 0s) (alloc ()))
        ((iterations 2000) (time 0s) (alloc ()))))))
    (calls 11023)
    |}]
;;

let%expect_test "batched routines make their inputs outside of the timing" =
  let clock = Clock.Fake.create () in
  let log = ref [] in
  let inputs = ref 0 in
  print_run
    ~config:
      { config with samples = 3; measurement_time = Time_ns.Span.of_int_us 3 }
    clock
    (Batched
       { setup =
           (fun () ->
             Clock.Fake.advance clock (Time_ns.Span.of_int_us 1);
             incr inputs;
             Ok !inputs)
       ; run =
           (fun input ->
             log := input :: !log;
             Clock.Fake.advance clock (ns 10);
             Ok ())
       });
  (* Inputs are each used once, in the order they are made. *)
  print_s
    [%message
      ""
        ~inputs:(!inputs : int)
        ~used_in_order:
          (List.equal Int.equal (List.rev !log) (List.init !inputs ~f:(( + ) 1))
           : bool)];
  [%expect
    {|
    (Ok
     ((warm_up_iterations 1) (warm_up_time 10ns)
      (plan ((iterations 100) (samples 3)))
      (samples
       (((iterations 100) (time 1us) (alloc ()))
        ((iterations 100) (time 1us) (alloc ()))
        ((iterations 100) (time 1us) (alloc ()))))))
    ((inputs 301) (used_in_order true))
    |}]
;;

let fail_on_call n =
  let calls = ref 0 in
  fun () ->
    incr calls;
    if !calls = n then error_s [%message "failed" ~call:(n : int)] else Ok ()
;;

let%expect_test "errors stop the run" =
  let clock = Clock.Fake.create ~tick:(ns 1) () in
  let calls = ref 0 in
  let counting f () =
    incr calls;
    f ()
  in
  (* In warm-up. *)
  print_run clock (Plain (counting (fail_on_call 5)));
  print_s [%message (calls : int ref)];
  [%expect
    {|
    (Error (failed (call 5)))
    (calls 5)
    |}];
  (* In the second sample, after 127 warm-up iterations and a sample of 199. *)
  calls := 0;
  let fail = fail_on_call 400 in
  print_run
    clock
    (Plain
       (counting (fun () ->
          Clock.Fake.advance clock (ns 10);
          fail ())));
  print_s [%message (calls : int ref)];
  [%expect
    {|
    (Error (failed (call 400)))
    (calls 400)
    |}];
  (* In setup: no iteration of the batch is run. *)
  calls := 0;
  print_run
    clock
    (Batched { setup = fail_on_call 3; run = counting (fun () -> Ok ()) });
  print_s [%message (calls : int ref)];
  [%expect
    {|
    (Error (failed (call 3)))
    (calls 1)
    |}]
;;

let%expect_test "allocation counters" =
  let clock = Clock.Fake.create () in
  let counters = ref Alloc.zero in
  let alloc () = !counters in
  let routine =
    Routine.Plain
      (fun () ->
        Clock.Fake.advance clock (ns 10);
        counters
        := Alloc.add
             !counters
             { minor_words = 3.
             ; promoted_words = 1.
             ; major_words = 0.5
             ; minor_collections = 0.01
             ; major_collections = 0.
             };
        Ok ())
  in
  let measurement =
    Measurement.run
      ~alloc
      { config with samples = 2 }
      (Clock.Fake.clock clock)
      routine
    |> Or_error.ok_exn
  in
  print_s [%sexp (measurement.samples : Sample.t list)];
  let result = Bench_result.create config ~name:"alloc" measurement in
  print_s [%sexp (result.alloc : Alloc.t option)];
  [%expect
    {|
    (((iterations 500) (time 5us)
      (alloc
       (((minor_words 1500) (promoted_words 500) (major_words 250)
         (minor_collections 4.9999999999999094) (major_collections 0)))))
     ((iterations 500) (time 5us)
      (alloc
       (((minor_words 1500) (promoted_words 500) (major_words 250)
         (minor_collections 4.9999999999998934) (major_collections 0))))))
    (((minor_words 3) (promoted_words 1) (major_words 0.5)
      (minor_collections 0.0099999999999998024) (major_collections 0)))
    |}]
;;

let%expect_test "Alloc.read" =
  let before = Alloc.read () in
  let list = Sys.opaque_identity (List.init 1000 ~f:Fn.id) in
  let after = Alloc.read () in
  let diff = Alloc.diff after before in
  print_s
    [%message
      ""
        ~length:(List.length list : int)
        ~at_least_3000_minor_words:(Float.( >= ) diff.minor_words 3000. : bool)];
  [%expect {| ((length 1000) (at_least_3000_minor_words true)) |}]
;;
