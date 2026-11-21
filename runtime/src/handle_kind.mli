open! Import

(** The OCaml type of the payloads of a family of handles ([Heap.alloc_handle]),
    and how to find the Stag values a payload refers to.

    A handle's traced pointer words are exactly [refs payload], computed by the
    heap when it allocates the handle, so a payload's Stag pointers are kept
    alive as long as the handle is. For that to stay true:
    - [refs] must return every Stag pointer the payload holds, directly or
      through other OCaml values (in any order, duplicates allowed), and be a
      pure function of it, which neither allocates on the Stag heap nor collects
      ([Gc.alloc_handle] raises if it collects);
    - the payload's Stag pointers must never change after creation. A mutable
      cell the payload captures must be a Stag heap block, among [refs], whose
      fields are mutated, never an OCaml [ref] (or mutable field) holding a
      [Memory.Addr.t].

    [Gc.check_invariants] recomputes [refs] of every live payload and reports
    those its handle doesn't hold. To name another handle, a payload should hold
    a [Handle_ref.t], read with [Heap.get_handle_checked], as well as returning
    its address from [refs].

    A payload, and any [Memory.Addr.t] taken from it, is valid only while its
    handle is reachable from the Stag roots: once the handle is swept, the
    blocks its refs name may be freed and their addresses reused, even if the
    OCaml payload is still held. *)

type 'a t

val create
  :  name:string
  -> ('a -> Sexp.t)
  -> refs:('a -> Memory.Addr.t array)
  -> 'a t

val name : _ t -> string
val refs : 'a t -> 'a -> Memory.Addr.t array
val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
