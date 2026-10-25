open! Core

module Type_mismatch_reason = struct
  type t =
    | Base_types_not_equal of string * string
    | Args_length_mismatch of int * int
    | Mismatch_arg_kind of Type.arg * Type.arg
    | Structural_mismatch
  [@@deriving sexp, compare, equal]
end

module Context_mismatch_reason = struct
  type t =
    | Empty_context_has_variable of string
    | Presence_mismatch of string
    | Cyclic_context
  [@@deriving sexp, compare, equal]
end

type t =
  | Type_mismatch of Type.ty * Type.ty * Type_mismatch_reason.t
  | Context_mismatch of Type.context * Type.context * Context_mismatch_reason.t
  | Occurs_check_failed of Type.Var.t
[@@deriving sexp, compare, equal]
