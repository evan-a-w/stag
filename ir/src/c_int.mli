open! Import

(** The C integer types an [external]'s type may name, the prelude's [C.int]
    etc. Their values are stag [int]s, converted at the call. *)

type t =
  | Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Int (** [int]. *)
  | Uint (** [unsigned int]. *)
  | Long
  | Ulong (** [unsigned long]. *)
  | Size_t
[@@deriving sexp_of, equal, compare, hash, enumerate]

(** Its name in the prelude's module [C], eg. [int32]. *)
val name : t -> string

(** Of the canonical name of a type, eg. of [Prelude_path.canonical "C.int"]. *)
val of_type_name : string -> t option
