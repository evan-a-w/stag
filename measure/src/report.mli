open! Import

(** Rendering results. *)

module Format : sig
  type t =
    | Human
    | Sexp
    | Json
  [@@deriving sexp, enumerate]

  val to_string : t -> string
end

(** Against the baseline. *)
module Change : sig
  type t =
    | Not_compared (** There is no baseline. *)
    | Not_in_baseline
    | Compared of Comparison.t
  [@@deriving sexp_of]
end

module Entry : sig
  type t =
    { result : Bench_result.t
    ; change : Change.t
    }
  [@@deriving sexp_of]
end

(** A span in nanoseconds with 4 significant digits and a unit: [ns], [us],
    [ms] or [s]. Under a picosecond, eg. rounding errors, it is 0. *)
val time : float -> string

(** A criterion-like block:

    {v
fib/20
  time:     [1.234 ms 1.240 ms 1.246 ms]
  ...
    v}

    The mean and its confidence interval, then the other statistics, the
    allocation, the samples and outliers, and the change, if compared. *)
val human : Config.t -> Entry.t -> string

(** All entries in one document, for [Sexp] or [Json] (raises for
    [Human]). *)
val machine : Format.t -> Entry.t list -> string
