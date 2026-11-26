open! Import

(** Exhaustiveness and redundancy of matches, by Maranget's usefulness algorithm
    ("Warnings for pattern matching", 2007). *)

(** A value no case matches, as syntax located at [Loc.none]: constructors and
    fields by their declared names, records listing only the fields that matter. *)
module Counterexample : sig
  type t = Syntax.Pattern.t

  val to_string : t -> string

  (** As [to_string]. *)
  val sexp_of_t : t -> Sexp.t
end

module Result : sig
  type t =
    { missing : Counterexample.t option
    ; unused : int list (** The indices of the cases no value reaches. *)
    }
  [@@deriving sexp_of]
end

(** [variant_constructors name] is the constructors of the nominal variant type
    of canonical name [name], in order, with their arities, or [None] if it is
    not a variant. *)
val check_match
  :  variant_constructors:(string -> (Typed.Constructor_ref.t * int) list option)
  -> scrutinee:Type.ty
  -> Typed.Pattern.t list
  -> Result.t

(** The indices of the redundant cases [exception p], given each [p]. *)
val check_exception_arms
  :  variant_constructors:(string -> (Typed.Constructor_ref.t * int) list option)
  -> Typed.Pattern.t list
  -> int list
