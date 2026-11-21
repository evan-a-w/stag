open! Import

(** The layout of a heap block: a fixed prefix, optionally followed by [n]
    elements, their scalar parts after the prefix's scalars and their pointer
    runs after its pointers. A block is [Bytes] if pointer-free, else [Scanned],
    and [n] is recovered from its header: from the length of [Bytes], from the
    pointer words of elements with pointers. Where it can't be (pointer-free
    elements after a prefix with pointers, or zero-size elements), a hidden
    length word starts the scalar part. *)

type t [@@deriving sexp_of]

val fixed : Layout.t -> t
val variable : prefix:Layout.t -> elt:Layout.t -> t
val prefix : t -> Layout.t
val element : t -> Layout.Element.t option

(** [length] is [n], 0 for a fixed block (raises otherwise). The hidden length
    word, if any, is not written. *)
val header : t -> length:int -> Header.Color.t -> Header.t

(** Raises where there is a hidden length word, or on a header of another kind
    than [header] makes. *)
val length : t -> Header.t -> int

val has_hidden_length : t -> bool

(** Where the prefix's scalars start: after the hidden length word, if any. *)
val prefix_offset : t -> int

(** Where the elements' scalars start. Their pointers start after the prefix's,
    at [Layout.ptr_words (prefix t)]. *)
val elements_offset : t -> int
