open! Import

(** A monotonic clock, injectable so that measurements can be deterministic. *)
type t

(** [CLOCK_MONOTONIC], with its resolution from [clock_getres], else
    [Time_now.nanosecond_counter_for_timing] with a resolution of 1us. *)
val monotonic : unit -> t

(** The time since an arbitrary fixed origin, never decreasing. *)
val now : t -> Time_ns.Span.t

(** The smallest difference [now] shows. *)
val resolution : t -> Time_ns.Span.t

(** A clock that moves only when [advance]d, and by [tick] after each read. *)
module Fake : sig
  type clock := t
  type t

  (** [resolution] defaults to 1ns, [tick] to zero. *)
  val create : ?resolution:Time_ns.Span.t -> ?tick:Time_ns.Span.t -> unit -> t

  val advance : t -> Time_ns.Span.t -> unit
  val clock : t -> clock
end
