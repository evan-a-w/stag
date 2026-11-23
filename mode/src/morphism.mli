open! Import

(** A monotone map [right] from the points of [src] to those of [dst], with a
    left adjoint [left]: [left p <= q] iff [p <= right q]. An edge [lo <= m(hi)]
    relates [lo] on [dst] to [hi] on [src]. Compared by what it maps, not its
    name. *)
type t [@@deriving compare, sexp_of]

val identity : Axis.Any.t -> t

(** [x |-> f x] from stage to provenance, with [f const = static] and [unknown]
    otherwise: [provenance <= f stage] says [const] implies [static]. *)
val const_is_static : t

(** On stage, the right adjoint of [x |-> meet x portable]: a value of a type
    crossing [portable] used at [nonportable]. *)
val cross_portable : t

val src : t -> Axis.Any.t
val dst : t -> Axis.Any.t
val is_identity : t -> bool

(** Whether [x <= right x] for every [x], so that [x <= m(x)] always holds. Only
    for morphisms from an axis to itself. *)
val is_inflationary : t -> bool

val right : t -> int -> int
val left : t -> int -> int

(** [compose m1 m2] is [m1 . m2]: [x <= m1(y)] and [y <= m2(z)] give
    [x <= (compose m1 m2)(z)]. *)
val compose : t -> t -> t
