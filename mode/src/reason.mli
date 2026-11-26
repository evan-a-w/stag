open! Import

(** Why a constraint was added, to explain errors. *)
type t =
  { loc : Loc.t
  ; what : string
  ; layout : bool
  (** Why a type has pointers: the same such reason is given once in a message,
      however many constraints it adds. *)
  }

val create : ?loc:Loc.t -> string -> t
val layout : ?loc:Loc.t -> string -> t

(** [what], with [loc] when it is not [Loc.none]. *)
val sexp_of_t : t -> Sexp.t
