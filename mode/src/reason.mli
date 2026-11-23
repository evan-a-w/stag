open! Import

(** Why a constraint was added, to explain errors. *)
type t =
  { loc : Loc.t
  ; what : string
  }

val create : ?loc:Loc.t -> string -> t

(** [what], with [loc] when it is not [Loc.none]. *)
val sexp_of_t : t -> Sexp.t
