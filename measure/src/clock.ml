open! Import

type t =
  { resolution : Time_ns.Span.t
  ; now : unit -> Time_ns.Span.t
  }

let create ~resolution ~now = { resolution; now }

let monotonic () =
  match Core_unix.Clock.gettime, Core_unix.Clock.getres with
  | Ok gettime, Ok getres ->
    { resolution =
        Time_ns.Span.of_int63_ns
          (Int63.max Int63.one (getres Core_unix.Clock.Monotonic))
    ; now =
        (fun () -> Time_ns.Span.of_int63_ns (gettime Core_unix.Clock.Monotonic))
    }
  | Error _, _ | _, Error _ ->
    { resolution = Time_ns.Span.microsecond
    ; now =
        (fun () ->
          Time_ns.Span.of_int63_ns (Time_now.nanosecond_counter_for_timing ()))
    }
;;

let now t = t.now ()
let resolution t = t.resolution

module Fake = struct
  type clock = t

  let create_clock = create

  type t =
    { mutable time : Time_ns.Span.t
    ; tick : Time_ns.Span.t
    ; resolution : Time_ns.Span.t
    }

  let create
        ?(resolution = Time_ns.Span.nanosecond)
        ?(tick = Time_ns.Span.zero)
        ()
    =
    { time = Time_ns.Span.zero; tick; resolution }
  ;;

  let advance t span = t.time <- Time_ns.Span.( + ) t.time span

  let clock t : clock =
    create_clock ~resolution:t.resolution ~now:(fun () ->
      let now = t.time in
      advance t t.tick;
      now)
  ;;
end
