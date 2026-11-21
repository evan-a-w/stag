open! Import

(** A set of root words for the GC: words that are each a pointer, null or a
    small value below 4096. How compiled code's stack is scanned (shadow stack,
    stack maps) or how the interpreter keeps its roots is a root source. *)

module type S = sig
  type t

  val name : string

  (** Whether its roots may point into the data stack's live blocks (local
      values), as frames do. If not, [Gc.check_invariants] reports any that
      point into the data stack. *)
  val holds_locals : bool

  (** Calls [f] on the address of every root word. Must not allocate on the stag
      heap, collect or change the set while iterating. It must not allocate on
      the OCaml heap but in its calls to [f], which it makes as
      [(f [@zero_alloc assume]) root]: [f] is up to the caller. Packing a module
      as [S] does not check this: annotate [iter] [@zero_alloc]. *)
  val iter : t -> f:(Memory.Addr.t -> unit) @ local -> unit
  [@@zero_alloc]
end

type t = T : (module S with type t = 'a) * 'a -> t

val create : (module S with type t = 'a) -> 'a -> t
val name : t -> string
val holds_locals : t -> bool

(** As [S.iter]: zero-alloc but for the calls to [f]. *)
val iter : t -> f:(Memory.Addr.t -> unit) @ local -> unit
[@@zero_alloc]
