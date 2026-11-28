open! Import

(** Finalized size of a function or expression frame on [Data_stack] and
    [Shadow_stack]. [scalar_bytes] is rounded up to a multiple of 8 bytes (0 if
    the frame holds no scalar data). *)
type t =
  { scalar_bytes : int
  ; ptr_words : int
  }
[@@deriving sexp_of, equal]

module Builder : sig
  type frame_layout := t
  type t

  val create : unit -> t

  (** Number of pointer slots allocated in the frame so far. *)
  val ptr_words : t -> int

  (** Reserves a [Slot.t] for a value of [layout]. Zero-size layouts take 0
      scalar bytes and 0 pointer words. *)
  val alloc : t -> Layout.t -> Slot.t

  val finalize : t -> frame_layout
end
