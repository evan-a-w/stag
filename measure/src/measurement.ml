open! Import

module Plan = struct
  type t =
    { iterations : int
    ; samples : int
    }
  [@@deriving sexp]

  let create
        ?(setup = Time_ns.Span.zero)
        (config : Config.t)
        ~resolution
        ~per_iteration
    =
    let measurement_time = Time_ns.Span.to_ns config.measurement_time in
    let per_iteration = Float.max 1. (Time_ns.Span.to_ns per_iteration) in
    let cost = per_iteration +. Float.max 0. (Time_ns.Span.to_ns setup) in
    let iterations =
      Int.max
        1
        (Float.iround_up_exn
           (Float.max
              (measurement_time /. Float.of_int config.samples /. cost)
              (1000. *. Time_ns.Span.to_ns resolution /. per_iteration)))
    in
    let samples =
      if
        iterations = 1
        && Float.( > ) (cost *. Float.of_int config.samples) measurement_time
      then
        Int.min
          config.samples
          (Int.max
             config.min_samples
             (Float.iround_down_exn (measurement_time /. cost)))
      else config.samples
    in
    { iterations; samples }
  ;;
end

type t =
  { warm_up_iterations : int
  ; warm_up_time : Time_ns.Span.t
  ; plan : Plan.t
  ; samples : Sample.t list
  }
[@@deriving sexp]

let rec repeat n ~f =
  if n <= 0
  then Ok ()
  else (
    match f () with
    | Ok () -> repeat (n - 1) ~f
    | Error _ as error -> error)
;;

let make_inputs setup n =
  let rec loop acc n =
    if n <= 0
    then Ok (Array.of_list_rev acc)
    else (
      match setup () with
      | Ok input -> loop (input :: acc) (n - 1)
      | Error _ as error -> error)
  in
  loop [] n
;;

(* Times [n] iterations, with their inputs made beforehand, returning the
   time making them took too. *)
let timed_batch ?alloc clock (routine : Routine.t) n =
  let time run =
    let alloc_before = Option.map alloc ~f:(fun read -> read ()) in
    let before = Clock.now clock in
    let result = run () in
    let after = Clock.now clock in
    let alloc_after = Option.map alloc ~f:(fun read -> read ()) in
    let%map.Or_error () = result in
    let alloc =
      Option.map2 alloc_after alloc_before ~f:(fun after before ->
        Alloc.diff after before)
    in
    { Sample.iterations = n; time = Time_ns.Span.( - ) after before; alloc }
  in
  match routine with
  | Plain run ->
    let%map.Or_error sample = time (fun () -> repeat n ~f:run) in
    sample, Time_ns.Span.zero
  | Batched { setup; run } ->
    let before = Clock.now clock in
    let%bind.Or_error inputs = make_inputs setup n in
    let setup_time = Time_ns.Span.( - ) (Clock.now clock) before in
    let%map.Or_error sample =
      time (fun () ->
        Array.fold_until
          inputs
          ~init:()
          ~f:(fun () input ->
            match run input with
            | Ok () -> Continue ()
            | Error _ as error -> Stop error)
          ~finish:(fun () -> Ok ()))
    in
    sample, setup_time
;;

let warm_up (config : Config.t) clock routine =
  let rec loop ~batch ~iterations ~elapsed ~setup =
    let%bind.Or_error sample, setup_time = timed_batch clock routine batch in
    let iterations = iterations + batch in
    let elapsed = Time_ns.Span.( + ) elapsed sample.time in
    let setup = Time_ns.Span.( + ) setup setup_time in
    if
      Time_ns.Span.( >= )
        (Time_ns.Span.max
           (Time_ns.Span.( + ) elapsed setup)
           (Time_ns.Span.of_int_ns iterations))
        config.warm_up
    then Ok (iterations, elapsed, setup)
    else loop ~batch:(2 * batch) ~iterations ~elapsed ~setup
  in
  loop
    ~batch:1
    ~iterations:0
    ~elapsed:Time_ns.Span.zero
    ~setup:Time_ns.Span.zero
;;

let run ?alloc config clock routine =
  let%bind.Or_error warm_up_iterations, warm_up_time, setup =
    warm_up config clock routine
  in
  let plan =
    Plan.create
      config
      ~setup:(Time_ns.Span.( / ) setup (Float.of_int warm_up_iterations))
      ~resolution:(Clock.resolution clock)
      ~per_iteration:
        (Time_ns.Span.( / ) warm_up_time (Float.of_int warm_up_iterations))
  in
  let rec sample acc n =
    if n <= 0
    then Ok (List.rev acc)
    else (
      let%bind.Or_error s, (_ : Time_ns.Span.t) =
        timed_batch ?alloc clock routine plan.iterations
      in
      sample (s :: acc) (n - 1))
  in
  let%map.Or_error samples = sample [] plan.samples in
  { warm_up_iterations; warm_up_time; plan; samples }
;;
