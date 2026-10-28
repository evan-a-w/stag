open! Import

(** The value of a [Stag.Ref.t]: a declared type, constructor or field by its
    canonical name, as reflection gives. Programs can't make one otherwise. *)

module Kind : sig
  type t =
    | Type
    | Constructor
    | Field
  [@@deriving sexp_of, equal]

  val to_string : t -> string
end

type t =
  { kind : Kind.t
  ; name : string (** Canonical. *)
  }
[@@deriving sexp_of, equal]

(** The last name of [name], as declared, eg. [C] of [M.C]. *)
val short_name : t -> string

(** Eg. [<constructor M.C>], with [name] as by [Prelude_path.to_string_hum]. *)
val to_string_hum : t -> string
