open! Import

(** The layout of a heap block: a fixed prefix, optionally followed by [n]
    elements, their scalar parts after the prefix's scalars and their pointer
    runs after its pointers. A block is [Bytes] if pointer-free, else [Scanned],
    and [n] is recovered from its header: from the length of [Bytes], from the
    pointer words of elements with pointers, or from the scalar words of
    pointer-free elements of at least 8 bytes after a prefix with pointers.
    Where it can't be (smaller pointer-free elements after a prefix with
    pointers, or zero-size elements), a hidden length word, an [I64] at scalar
    offset 0, starts the scalar part. *)

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

(** Where the prefix's scalars start: 8 after the hidden length word, if any,
    else 0. *)
val prefix_offset : t -> int

(** Where the elements' scalars start, aligned for them. Their pointers start
    after the prefix's, at [Layout.ptr_words (prefix t)]. Of a fixed block, the
    end of the prefix's scalars. *)
val elements_offset : t -> int
