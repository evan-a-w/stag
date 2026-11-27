open! Import

(** Steps counted in the loops whose cost tests bound: unlike timings, the same
    on every run, however loaded the machine. *)
type t

(** A counter, measured by [Private.measure] under [name], with the others of
    that name. *)
val create : string -> t

val incr : t -> unit

module Private : sig
  module Counts : sig
    (** The steps of each counter that took any, and the words allocated, as
        ["allocated_words"], which also count work done where no counter is. *)
    type t = int String.Map.t [@@deriving sexp_of]
  end

  (** [f ()], with the steps taken while it ran. *)
  val measure : (unit -> 'a) -> 'a * Counts.t

  module Growth : sig
    (** By the exponent [e] of [count large / count small = (large / small)^e]:
        [Linear] up to 1.3, [Quadratic] up to 2.3. *)
    type t =
      | Linear
      | Quadratic
      | Worse
    [@@deriving sexp_of]
  end

  (** How each count of [counts size] grows from [size = small] to [large], less
      that of [counts net_of] if given (eg. checking a prelude alone). [counts]
      is called once more first, for what is done only the first time. *)
  val growth
    :  ?net_of:int
    -> small:int
    -> large:int
    -> (int -> Counts.t)
    -> Growth.t String.Map.t
end
