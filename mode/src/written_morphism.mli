open! Import

(** The morphisms a [with] clause writes, each as [f(lo) <= hi], [f] the left
    adjoint, or as [lo <= g(hi)], [g] the morphism itself. Every morphism the
    solver composes from its own is one of them, or relates nothing (its left
    adjoint is the bottom). *)
type t =
  | Plain of Axis.Any.t (** [axis(lo) <= axis(hi)] *)
  | Meet of Axis.Any.t (** [meet(axis(lo), middle) <= axis(hi)] *)
  | Release (** [release(locality(lo)) <= locality(hi)] *)
  | Static_if_const (** [provenance(lo) <= static_if_const(stage(hi))] *)
  | Const_if_static (** [stage(lo) <= const_if_static(provenance(hi))] *)
[@@deriving sexp_of]

val all : t list
val morphism : t -> Morphism.t

(** The middle point of the axis, which [Meet] meets with, by index. *)
val middle : Axis.Any.t -> int

(** Whether the morphism relates nothing: [lo <= m(hi)] always holds. *)
val is_trivial : Morphism.t -> bool
