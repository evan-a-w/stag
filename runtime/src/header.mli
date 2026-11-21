open! Import

(** The word before every heap block: a pointer to a block points to its first
    field, its header at [addr - 8]. The low byte is the kind (bits 2-7) and
    colour (bits 0-1); the upper 56 bits depend on the kind:
    - [Scanned]: [size_words] (bits 32-63) and [scalar_words] (bits 8-31).
    - [Bytes]: [length] in bytes (bits 8-63).
    - [Free]: [size_words] (bits 32-63).
    - [Handle]: zero; the block has one scalar word for its table ID. *)

module Kind : sig
  type t =
    | Scanned (** Scalar words, then pointer words. *)
    | Bytes (** No pointers, with an exact length in bytes (strings). *)
    | Free (** A free slot in a page. *)
    | Handle (** An entry in the heap's OCaml metadata table. *)
  [@@deriving sexp_of, equal, enumerate]
end

module Color : sig
  type t =
    | Unmarked
    | Marked
    | Immortal (** Static data: never marked, swept or freed. *)
  [@@deriving sexp_of, equal, enumerate]
end

type t : bits64

val max_size_words : int
val max_scalar_words : int
val max_length : int

(** Raises unless [0 <= scalar_words <= size_words <= max_size_words] and
    [scalar_words <= max_scalar_words]. *)
val scanned : size_words:int -> scalar_words:int -> Color.t -> t
[@@zero_alloc]

(** Raises unless [0 <= length <= max_length]. *)
val bytes : length:int -> Color.t -> t
[@@zero_alloc]

(** [Unmarked]. Raises unless [0 <= size_words <= max_size_words]. *)
val free : size_words:int -> t
[@@zero_alloc]

val handle : unit -> t [@@zero_alloc]

(** The following raise on a header with an unknown kind or colour. *)

val kind : t -> Kind.t [@@zero_alloc]
val color : t -> Color.t [@@zero_alloc]
val with_color : t -> Color.t -> t [@@zero_alloc]

(** Of the block, without the header. [Bytes] rounds its length up. *)
val size_words : t -> int
[@@zero_alloc]

(** The words the GC skips. All of a [Bytes], [Free] or [Handle] block. *)
val scalar_words : t -> int
[@@zero_alloc]

(** Raises unless [Bytes]. *)
val length : t -> int [@@zero_alloc]

val to_int64 : t -> int64_u [@@zero_alloc]
val of_int64 : int64_u -> t [@@zero_alloc]
val equal : t -> t -> bool [@@zero_alloc]
val sexp_of_t : t -> Sexp.t

(** The header of the block at [addr]. *)
val load : Memory.Addr.t -> t
[@@zero_alloc]

val store : Memory.Addr.t -> t -> unit [@@zero_alloc]
