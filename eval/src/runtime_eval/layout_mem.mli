open! Import

(** Low-level layout-guided reads, writes, copies, and discriminant operations
    over [Memory.Addr.t]. *)

val round_up_word : int -> int [@@zero_alloc]

(** Base address of the pointer words of a [Block.fixed] block of [layout]. *)
val block_ptrs : Layout.t -> Memory.Addr.t -> Memory.Addr.t
[@@zero_alloc]

val copy
  :  Layout.t
  -> src_s:Memory.Addr.t
  -> src_p:Memory.Addr.t
  -> dst_s:Memory.Addr.t
  -> dst_p:Memory.Addr.t
  -> unit
[@@zero_alloc]

val copy_to_block
  :  Layout.t
  -> src_s:Memory.Addr.t
  -> src_p:Memory.Addr.t
  -> dst:Memory.Addr.t
  -> unit
[@@zero_alloc]

val copy_from_block
  :  Layout.t
  -> src:Memory.Addr.t
  -> dst_s:Memory.Addr.t
  -> dst_p:Memory.Addr.t
  -> unit
[@@zero_alloc]

val zero_ptrs : Memory.Addr.t -> count:int -> unit [@@zero_alloc]

(** Unsigned for [I8], [I16], [I32]; signed for [I64]; raw word for [Code].
    Raises on [F32]/[F64]. *)
val load_scalar_int : Shape.Scalar.t -> Memory.Addr.t -> int
[@@zero_alloc]

val store_scalar_int : Shape.Scalar.t -> Memory.Addr.t -> int -> unit
[@@zero_alloc]

val read_discriminant
  :  Layout.Discriminant.t
  -> src_s:Memory.Addr.t
  -> src_p:Memory.Addr.t
  -> int
[@@zero_alloc]

(** Nulls the union's [ptr_words] at [ptr_index] first, then writes the
    discriminant leaf if any. *)
val write_discriminant
  :  Layout.Write_plan.t
  -> dst_s:Memory.Addr.t
  -> dst_p:Memory.Addr.t
  -> unit
[@@zero_alloc]

val read_bytes : Memory.Addr.t -> len:int -> bytes
val write_bytes : bytes -> dst:Memory.Addr.t -> unit
val read_ptrs : Memory.Addr.t -> count:int -> Memory.Addr.t array
val write_ptrs : Memory.Addr.t array -> dst:Memory.Addr.t -> unit [@@zero_alloc]
