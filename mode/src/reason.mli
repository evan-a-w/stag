open! Import

module Shown : sig
  (** How a reason is given in a message's chain of reasons. *)
  type t =
    | Always
    | Once
    (** Why a type has pointers: the same such reason is given once in a
        message, however many constraints it adds. *)
    | If_alone
    (** A step that only relates one variable to another (as a function used at
        a type with other modes): left out of a chain with other reasons. *)
end

(** Why a constraint was added, to explain errors. *)
type t =
  { loc : Loc.t
  ; what : string
  ; shown : Shown.t
  }

val create : ?loc:Loc.t -> ?shown:Shown.t -> string -> t

(** [create ~shown:Once]. *)
val layout : ?loc:Loc.t -> string -> t

(** [what], with [loc] when it is not [Loc.none]. *)
val sexp_of_t : t -> Sexp.t
