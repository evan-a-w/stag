open! Import

(** For each element of [a], the index of the element of [b] it is matched
    with, if any, by a shortest edit script (of insertions and deletions) from
    [a] to [b], as Myers' diff: quick when they are mostly the same. *)
val align : 'a array -> 'a array -> equal:('a -> 'a -> bool) -> int option array
