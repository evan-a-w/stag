open! Import

(** Why a type has no shape. *)
type t =
  | Unresolved_ty of Type.id
  (** A type variable that no declaration expansion binds: the type is not
      concrete. *)
  | Unresolved_row of Type.id
  (** An open row, so a structural record or variant that may have more entries. *)
  | Unresolved_presence of Type.id
  | Recursive_row of Type.id (** A row variable that expands to itself. *)
  | Unknown_type of string
  (** A [TCon] no [Resolver] declares, by canonical name. *)
  | Abstract_type of string
  | Wrong_arity of
      { name : string
      ; expected : int
      ; got : int
      }
  | Type_expected of Type.id
  | Row_expected of Type.id
  | Presence_expected of Type.id
  (** A variable used as something its instantiation is not. *)
  | Recursive of string list
  (** The by-value cycle: the expansions enclosing it, outermost first.
      Declarations by canonical name, structural types as [record 12] or
      [variant 12]. *)
  | Error_type (** [<error>], of what did not check. *)
  | Too_deep of int (** More expansions than this enclose the type. *)
  | Layout of Sexp.t (** Raised by [Layout.of_shape]. *)
[@@deriving sexp_of]
