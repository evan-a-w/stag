open! Core

(** Instantiation and generalization of type schemes, whose quantified
    variables are [Generic]. *)

(** Replaces [Generic] variables by fresh ones at [rank]. The types one partial
    application [instantiate fresh ~rank] is applied to share their
    instances. *)
val instantiate : Fresh.t -> rank:Type.rank -> Type.ty -> Type.ty

(** Marks unbound variables of [tys] with rank greater than [rank] as
    [Generic], in place, along with those reached through their bounds and equalities.
    Variables that are not generalized but may still be used lose their
    bounds and equalities with generalized variables: the scheme keeps them,
    as a [Type.kept] of each generalized variable still in them once the
    scheme is simplified, and each instance adds them again, once. The variables not generalized in a kept bound,
    including the one bounded, are lowered to [rank]. Variables that can no
    longer be used keep theirs. An instance of what is kept is added to the
    variables not generalized, as the constraints that held through the
    generalized ones. The scheme is simplified to an equivalent one: entry
    types are simplified (see [Entry_types]), bounds and equalities that an
    instance can always satisfy are dropped, and generalized presences no
    instance can make [Present], such as ones with an equality between types
    that [Unify.clash], are [Absent]. [tys] must have every
    type that may share the variables generalized, as what is not seen in them
    may be simplified away or dropped. *)
val generalize : Fresh.t -> rank:Type.rank -> Type.ty list -> unit
