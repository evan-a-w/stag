open! Import

module Type_mismatch_reason : sig
  type t =
    | Base_types_not_equal of string * string
    | Args_length_mismatch of int * int
    | Mismatch_arg_kind of Type.arg * Type.arg
    | Structural_mismatch
    | Modes of Mode.Error.t
  [@@deriving sexp_of]
end

module Row_mismatch_reason : sig
  type t =
    | Missing_label of string
    | Presence_mismatch of string
    (** Both rows have the name, but one has it [Absent]. *)
    | Assigned of string
    (** One code context assigns the name, which the other has as not assigned. *)
    | Cyclic_row
  [@@deriving sexp_of]
end

type t =
  | Type_mismatch of Type.ty * Type.ty * Type_mismatch_reason.t
  | Row_mismatch of Type.row * Type.row * Row_mismatch_reason.t
  | Occurs_check_failed of Type.arg
  (** The variable that would occur in the type it is bound to. *)
  | Assigned_in_record of string * Type.row
  (** The row has the name [assigned], but is also a record's or variant's,
      whose entries can't be. *)
[@@deriving sexp_of]

(** With its types as they are now ([Type.Snapshot]). *)
val snapshot : t -> t
