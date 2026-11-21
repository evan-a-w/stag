open! Import

(** Raw memory outside the OCaml heap. Nothing is checked: loads and stores must
    be within a live chunk, and the caller keeps track of what is where.

    Little-endian hosts only: initializing this module raises on a big-endian
    one. *)

module Addr : sig
  type t : word

  val null : unit -> t [@@zero_alloc]
  val is_null : t -> bool [@@zero_alloc]
  val to_word : t -> Nativeint_u.t [@@zero_alloc]
  val of_word : Nativeint_u.t -> t [@@zero_alloc]
  val add : t -> int -> t [@@zero_alloc]

  (** In bytes, for addresses less than 2^62 bytes apart. *)
  val diff : t -> t -> int
  [@@zero_alloc]

  (** [align] must be a power of two. *)
  val align_up : t -> int -> t
  [@@zero_alloc]

  (** [align] must be a power of two. *)
  val is_aligned : t -> int -> bool
  [@@zero_alloc]

  val equal : t -> t -> bool [@@zero_alloc]

  (** Unsigned. *)
  val compare : t -> t -> int [@@zero_alloc]
end

(** A block of malloc'd memory, owned until [destroy]. The [t] must stay
    reachable until then: its memory is freed if it is collected. *)
module Chunk : sig
  type t

  (** Zeroed, its base aligned to [align] (a power of two). *)
  val create : bytes:int -> align:int -> t

  val base : t -> Addr.t [@@zero_alloc]

  (** [0] once destroyed. *)
  val bytes : t -> int [@@zero_alloc]

  (** Whether [base t <= addr < base t + bytes t], for any [addr]. *)
  val contains : t -> Addr.t -> bool
  [@@zero_alloc]

  (** Accessing its memory afterwards is undefined. Raises if already destroyed. *)
  val destroy : t -> unit
end

(** Little-endian. Addresses need not be aligned. *)

val load_8 : Addr.t -> int8# [@@zero_alloc]
val load_16 : Addr.t -> int16# [@@zero_alloc]
val load_32 : Addr.t -> int32_u [@@zero_alloc]
val load_64 : Addr.t -> int64_u [@@zero_alloc]
val load_f32 : Addr.t -> float32_u [@@zero_alloc]
val load_f64 : Addr.t -> float# [@@zero_alloc]
val load_addr : Addr.t -> Addr.t [@@zero_alloc]
val store_8 : Addr.t -> int8# -> unit [@@zero_alloc]
val store_16 : Addr.t -> int16# -> unit [@@zero_alloc]
val store_32 : Addr.t -> int32_u -> unit [@@zero_alloc]
val store_64 : Addr.t -> int64_u -> unit [@@zero_alloc]
val store_f32 : Addr.t -> float32_u -> unit [@@zero_alloc]
val store_f64 : Addr.t -> float# -> unit [@@zero_alloc]
val store_addr : Addr.t -> Addr.t -> unit [@@zero_alloc]
val fill_zero : Addr.t -> bytes:int -> unit [@@zero_alloc]

(** The ranges may overlap. *)
val copy : src:Addr.t -> dst:Addr.t -> bytes:int -> unit
[@@zero_alloc]
