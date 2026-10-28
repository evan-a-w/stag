open! Import

(** A routine's times against those of a baseline, as criterion compares
    them. *)

module Verdict : sig
  type t =
    | Improved
    | Regressed
    | No_change (** The confidence interval of the change contains 0. *)
    | Within_noise
    (** The change is significant, but its estimate is within the noise
        threshold. *)
  [@@deriving sexp]

  (** As criterion: ["Performance has improved."], etc. *)
  val to_string : t -> string
end

type t =
  { change : Stats.Estimate.t
    (** Relative change of the mean, [0.032] for +3.2%: the bootstrap of
        [mean current / mean baseline - 1], resampling both independently. *)
  ; verdict : Verdict.t
  }
[@@deriving sexp]

(** Per-iteration times, with [Config]'s bootstrap and noise threshold. *)
val compare : Config.t -> baseline:float array -> current:float array -> t
