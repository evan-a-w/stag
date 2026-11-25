open! Import

(** What a [with] clause reads back as, and its reduction to the least set of
    its constraints that still implies exactly what [Mode_closure] has. *)

(** [lo <= morphism(hi)]: raising [lo] raises [hi] by the morphism's left
    adjoint. *)
module Edge : sig
  type t =
    { lo : Mode_node.t
    ; morphism : Mode.Morphism.t
    ; hi : Mode_node.t
    }
end

(** A printed constraint: edges (a relation, or a pair related on every axis),
    or a bound. *)
module Item : sig
  type t =
    | Edges of Edge.t list
    | Lower of Mode_node.t * int
    | Upper of Mode_node.t * int
end

(** Drops each of [items], in order, while the rest imply it, as read back with
    [implicit] (constraints reading back always adds) and nodes at least
    [base_lower] (what they read back at least as). Edges are to be before
    bounds: dropping an edge keeps what the bounds imply. Then checks that the
    rest imply exactly what [closure] does of [nodes], raising if not: every
    item must hold of [closure], and those it has to imply be among them.

    Items may also relate [hidden] nodes, which read back unbounded and only
    pass raises on: what [closure] has of them isn't checked. *)
val reduce
  :  Mode_closure.t
  -> nodes:Mode_node.t list
  -> hidden:Mode_node.t list
  -> base_lower:(Mode_node.t -> int)
  -> implicit:Edge.t list
  -> 'a list
  -> item:('a -> Item.t)
  -> 'a list
