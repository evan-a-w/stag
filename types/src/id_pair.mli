open! Import

(** An unordered pair of ids, packed into an immediate. Ids must be in
    [0, max_id], else [create] raises: they come from a [Fresh.t], which is per
    typecheck, so this bounds the type nodes a single check can create. *)
type t = private int [@@deriving sexp_of]

val max_id : int
val create : Type.Id.t -> Type.Id.t -> t

module Hash_set : sig
  type id_pair := t
  type t

  val create : unit -> t

  (** Whether the pair was already present, adding it if not. *)
  val mem_or_add : t -> id_pair -> bool
end
