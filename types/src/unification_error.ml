open! Import

module Type_mismatch_reason = struct
  type t = Type.type_mismatch_reason =
    | Base_types_not_equal of string * string
    | Args_length_mismatch of int * int
    | Mismatch_arg_kind of Type.arg * Type.arg
    | Structural_mismatch
    | Modes of Mode.Error.t
  [@@deriving sexp_of]
end

module Row_mismatch_reason = struct
  type t = Type.row_mismatch_reason =
    | Missing_label of string
    | Presence_mismatch of string
    | Assigned of string
    (** One code context assigns the name, which the other has as not assigned. *)
    | Cyclic_row
  [@@deriving sexp_of]
end

type t = Type.unification_error =
  | Type_mismatch of Type.ty * Type.ty * Type_mismatch_reason.t
  | Row_mismatch of Type.row * Type.row * Row_mismatch_reason.t
  | Occurs_check_failed of Type.arg
  | Assigned_in_record of string * Type.row
  | Bound_var of Mode.Error.t
[@@deriving sexp_of]

let snapshot t =
  let snapshot = Type.Snapshot.create () in
  match t with
  | Type_mismatch (ty1, ty2, reason) ->
    Type_mismatch
      (Type.Snapshot.ty snapshot ty1, Type.Snapshot.ty snapshot ty2, reason)
  | Row_mismatch (row1, row2, reason) ->
    Row_mismatch
      (Type.Snapshot.row snapshot row1, Type.Snapshot.row snapshot row2, reason)
  | Occurs_check_failed arg ->
    Occurs_check_failed (Type.Snapshot.arg snapshot arg)
  | Assigned_in_record (name, row) ->
    Assigned_in_record (name, Type.Snapshot.row snapshot row)
  | Bound_var _ -> t
;;
