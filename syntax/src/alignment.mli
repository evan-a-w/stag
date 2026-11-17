open! Import

(** For each element of [a], the index of the element of [b] it is matched with,
    if any, keeping their order. As patience diff: the elements occurring once
    in each are matched first, in order, and the parts between them by a
    shortest edit script (of insertions and deletions), as Myers' diff, quick
    when they are mostly the same. *)
val align
  :  'a array
  -> 'a array
  -> compare:('a -> 'a -> int)
  -> int option array
