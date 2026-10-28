open! Import

(** Running a set of named routines, as [stag bench] and [stag-build bench]
    do. *)

module Bench : sig
  type t =
    | Single of
        { name : string
        ; routine : Routine.t
        }
    | Parameterised of
        { name : string
        ; args : (unit -> (string * Routine.t) Or_error.t) list
          (** Each makes the routine of an argument, and its label: the bench
            is named [NAME/LABEL]. *)
        }

  val add_prefix : t -> string -> t
end

module Outcome : sig
  type t =
    { name : string
    ; entry : Report.Entry.t Or_error.t
      (** An error of the routine, which stops its measurement. *)
    }
  [@@deriving sexp_of]
end

(** Whether [name] is selected by [filters]: all are if there are none, else
    those containing any of them. *)
val selected : filters:string list -> string -> bool

(** Measures the benches [selected] in order, each after a [Gc.compact], so
    that earlier ones do not leave it garbage, calling [on_outcome] after each,
    and compares them with their times in [baseline], if any.

    The arguments of a [Parameterised] bench are made only if a filter could
    select it: one it contains, or one with a [/] after a suffix of its name
    ([sort/1] or [t/1] for [sort]). An argument that fails is an outcome named
    as the bench, and the others still run. *)
val run
  :  ?alloc:(unit -> Alloc.t)
  -> ?baseline:Baseline.t
  -> ?on_outcome:(Outcome.t -> unit)
  -> ?filters:string list
  -> Config.t
  -> Clock.t
  -> Bench.t list
  -> Outcome.t list
