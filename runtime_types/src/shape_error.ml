open! Import

(** Why a type has no shape. *)
type t =
  | Unresolved_ty of Type.Id.t
  (** A type variable that no declaration expansion binds: the type is not
      concrete. *)
  | Unresolved_row of Type.Id.t
  (** An open row, so a structural record or variant that may have more entries. *)
  | Unresolved_presence of Type.Id.t
  | Recursive_row of Type.Id.t (** A row variable that expands to itself. *)
  | Unknown_type of string
  (** A [TCon] no [Resolver] declares, by canonical name. *)
  | Abstract_type of string
  | Wrong_arity of
      { name : string
      ; expected : int
      ; got : int
      }
  | Recursive_ty of Type.Id.t
  (** A type variable whose instantiation reaches itself, so is not concrete. *)
  | Row_expected of Type.Id.t
  (** A row variable used as a type, or the reverse. *)
  | Recursive of string list
  (** The by-value cycle: the expansions enclosing it, outermost first.
      Declarations by canonical name, structural types as [record 12] or
      [variant 12]. *)
  | Error_type (** [<error>], of what did not check. *)
  | Too_deep of int (** More expansions than this enclose the type. *)
  | Too_large of int
  (** Mapping the type took more steps than this, as under nested polymorphic
      recursion, where each expansion doubles the shape. *)
  | Layout of Sexp.t (** Raised by [Layout.of_shape]. *)
[@@deriving sexp_of]
