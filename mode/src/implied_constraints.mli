open! Import

(** Which of a set of constraints the others imply, found for all of them at
    once. The nodes are numbered from [0]; [succ.(n)] are the constraints
    [n <= m(dst)] from [n], as [(dst, m)].

    [src <= m(dst)] is implied when [dst] is reached from [src] at a morphism at
    most [m] other than through that constraint itself. Dropping implied
    constraints one at a time, each one implied by those remaining, keeps only
    those the others don't imply; which ones that is may depend on the order
    they are dropped in, but not for those from nodes no cycle of constraints is
    reachable from: those are decided here, for any order. *)

module Verdict : sig
  type t =
    | Implied (** Whatever others are dropped before it. *)
    | Not_implied (** Even by all the others. *)
    | Unknown
    (** Implied by the others together, but from a node a cycle is reachable
        from, so it depends on which are dropped before it. *)
  [@@deriving sexp_of]
end

type t

val create : succ:(int * Morphism.t) array array -> t

(** Of the [i]th constraint from [src]. *)
val verdict : t -> src:int -> int -> Verdict.t
