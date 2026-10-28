open! Import

(** The value of a [Stag.Type_ref.t], [Stag.Constructor_ref.t],
    [Stag.Field_ref.t] or [Stag.Value_ref.t]: a declared type, constructor,
    field or value by its canonical name, as reflection and [Stag.Code.erase]
    give. Programs can't make one otherwise. *)

module Kind : sig
  type t =
    | Type
    | Constructor
    | Field
    | Value
end

type t =
  { kind : Kind.t
  ; name : string (** Canonical. *)
  }
[@@deriving sexp_of]

(** The last name of [name], as declared, eg. [C] of [M.C]. *)
val short_name : t -> string

(** Eg. [<constructor M.C>], with [name] as by [Prelude_path.to_string_hum]. *)
val to_string_hum : t -> string
