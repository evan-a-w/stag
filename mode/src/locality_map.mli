open! Import

(** How a value's locality relates to the mode it is used at, in
    [submode ~locality]: [a <= m(b)] on the locality axis. Localities are
    relative to a region: [local] to the current one, [regional] to an enclosing
    one. *)
type t [@@deriving sexp_of, compare]

val id : t

(** A value of the region enclosing the current one, used in the current one:
    what is local there is regional here. *)
val borrow : t

(** A value of the current region, as seen once it has ended: what is regional
    here is local to the enclosing region (and what is local is gone, which is
    left to another constraint). *)
val release : t

(** Unconstrained: the value's locality does not reach the use. *)
val ignore : t

(** [compose m1 m2] is [m1 . m2]: [a <= m1(b)] and [b <= m2(c)] give
    [a <= (compose m1 m2)(c)]. *)
val compose : t -> t -> t

val morphism : t -> Morphism.t option
