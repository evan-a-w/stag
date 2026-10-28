open! Import

module Plan = struct
  type t =
    { iterations : int
    ; samples : int
    ; expected_time : Time_ns.Span.t
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
    { iterations
    ; samples
    ; expected_time =
        Time_ns.Span.of_ns (Float.of_int (samples * iterations) *. cost)
    }
  ;;

  let warning t (config : Config.t) =
    Option.some_if
      (t.samples < config.samples
       || (t.iterations = 1
           && Time_ns.Span.( > ) t.expected_time config.measurement_time))
      (sprintf
         "Unable to take %d samples in %s: taking %d, which should take %s"
         config.samples
         (Time_ns.Span.to_string_hum config.measurement_time)
         t.samples
         (Time_ns.Span.to_string_hum ~decimals:1 t.expected_time))
  ;;
end

type t =
  { warm_up_iterations : int
  ; warm_up_time : Time_ns.Span.t
  ; plan : Plan.t
  ; samples : Sample.t list
  }
[@@deriving sexp]

let max_warm_up_iterations = 1 lsl 24
let max_unmoved_iterations = 1 lsl 16
let max_batched_inputs = 64
let setup_allowance = 10.

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

let run_inputs run inputs =
  Array.fold_until
    inputs
    ~init:()
    ~f:(fun () input ->
      match run input with
      | Ok () -> Continue ()
      | Error _ as error -> Stop error)
    ~finish:(fun () -> Ok ())
;;

module Timed = struct
  type t =
    { time : Time_ns.Span.t
    ; alloc : Alloc.t option
    }

  let zero ~alloc =
    { time = Time_ns.Span.zero; alloc = Option.some_if alloc Alloc.zero }
  ;;

  let add a b =
    { time = Time_ns.Span.( + ) a.time b.time
    ; alloc = Option.map2 a.alloc b.alloc ~f:Alloc.add
    }
  ;;

  let measure ?alloc clock f =
    let alloc_before = Option.map alloc ~f:(fun read -> read ()) in
    let before = Clock.now clock in
    let result = f () in
    let after = Clock.now clock in
    let alloc_after = Option.map alloc ~f:(fun read -> read ()) in
    let%map.Or_error () = result in
    { time = Time_ns.Span.( - ) after before
    ; alloc =
        Option.map2 alloc_after alloc_before ~f:(fun after before ->
          Alloc.diff after before)
    }
  ;;
end

(* Times [n] iterations, returning the untimed time their setup took too. *)
let timed_batch ?alloc clock (routine : Routine.t) n =
  match routine with
  | Plain run ->
    let%map.Or_error { Timed.time; alloc } =
      Timed.measure ?alloc clock (fun () -> repeat n ~f:run)
    in
    { Sample.iterations = n; time; alloc }, Time_ns.Span.zero
  | Batched { setup; run } ->
    let rec chunks (timed : Timed.t) ~setup_time n =
      if n <= 0
      then Ok (timed, setup_time)
      else (
        let chunk = Int.min n max_batched_inputs in
        let before = Clock.now clock in
        let%bind.Or_error inputs = make_inputs setup chunk in
        let setup_time =
          Time_ns.Span.( + )
            setup_time
            (Time_ns.Span.( - ) (Clock.now clock) before)
        in
        let%bind.Or_error chunk_timed =
          Timed.measure ?alloc clock (fun () -> run_inputs run inputs)
        in
        chunks (Timed.add timed chunk_timed) ~setup_time (n - chunk))
    in
    let%map.Or_error { time; alloc }, setup_time =
      chunks
        (Timed.zero ~alloc:(Option.is_some alloc))
        ~setup_time:Time_ns.Span.zero
        n
    in
    { Sample.iterations = n; time; alloc }, setup_time
;;

module Warm_up = struct
  type t =
    { iterations : int
    ; time : Time_ns.Span.t
    ; last : Sample.t
    ; last_setup : Time_ns.Span.t
    }

  let run (config : Config.t) clock routine =
    let setup_limit = Time_ns.Span.scale config.warm_up setup_allowance in
    let rec loop ~batch ~iterations ~time ~setup =
      let%bind.Or_error (last : Sample.t), last_setup =
        timed_batch clock routine batch
      in
      let iterations = iterations + batch in
      let time = Time_ns.Span.( + ) time last.time in
      let setup = Time_ns.Span.( + ) setup last_setup in
      if
        Time_ns.Span.( >= ) time config.warm_up
        || Time_ns.Span.( >= ) (Time_ns.Span.( + ) time setup) setup_limit
        || iterations + (2 * batch) > max_warm_up_iterations
        || (Time_ns.Span.equal time Time_ns.Span.zero
            && iterations >= max_unmoved_iterations)
      then Ok { iterations; time; last; last_setup }
      else loop ~batch:(2 * batch) ~iterations ~time ~setup
    in
    loop ~batch:1 ~iterations:0 ~time:Time_ns.Span.zero ~setup:Time_ns.Span.zero
  ;;
end

let run ?alloc config clock routine =
  let%bind.Or_error () = Config.validate config in
  let%bind.Or_error warm_up = Warm_up.run config clock routine in
  let%bind.Or_error () =
    if Time_ns.Span.( > ) warm_up.last.time Time_ns.Span.zero
    then Ok ()
    else
      Or_error.error_s
        [%message
          "the clock did not move while timing a batch of iterations, so it \
           cannot time them"
            ~iterations:(warm_up.last.iterations : int)]
  in
  let per_iteration_of span =
    Time_ns.Span.( / ) span (Float.of_int warm_up.last.iterations)
  in
  let plan =
    Plan.create
      config
      ~setup:(per_iteration_of warm_up.last_setup)
      ~resolution:(Clock.resolution clock)
      ~per_iteration:(per_iteration_of warm_up.last.time)
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
  { warm_up_iterations = warm_up.iterations
  ; warm_up_time = warm_up.time
  ; plan
  ; samples
  }
;;
