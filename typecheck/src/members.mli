open! Import

(** Type-directed lookup: the declared type a constructor or field is known to
    be of, by its canonical name, and its members. *)
module Known : sig
  type 'member t =
    { type_name : string
    ; members : 'member list
    }

  val record
    :  Scope.t
    -> Type.ty option
    -> (Type_env.Field.t t option, Typecheck_error.t) Result.t

  val unqualified_field : Source.Field.t -> string option
end

val field_ref : Type_env.Field.t -> Typed.Field_ref.t

(** Unifies [ty], a fresh instance of a declared type, with [expected] if that
    is known to be the same type, so that what is inside is known too. As
    [expected] is always unified with what has [ty] later, it fails only where
    that would. *)
val unify_known
  :  Fresh.t
  -> expected:Type.ty option
  -> Type.ty
  -> loc:Loc.t
  -> (unit, Typecheck_error.t) Result.t

(** Pairs each of [args] with its instantiated type, along with the constructor
    and the result type. Several [args] of a constructor declared with one are a
    tuple ([tuple]), as in OCaml. *)
val constructor_types
  :  Fresh.t
  -> Scope.t
  -> Source.Constructor.t
  -> args:'a list
  -> tuple:('a list -> 'a)
  -> expected:Type.ty option
  -> loc:Loc.t
  -> ( Typed.Constructor_ref.t * ('a * Type.ty * Modality.t) list * Type.ty
       , Typecheck_error.t )
       Result.t

val is_given : (Type_env.Field.t * _ * _) list -> Type_env.Field.t -> bool

(** Fields resolved to a record type. *)
module Resolved : sig
  type 'a t =
    { known : Type_env.Field.t Known.t
    ; declared : Type.ty (** The record type, as declared. *)
    ; record_type : Typed.Record_type.t
    ; fields : (Type_env.Field.t * Loc.t * 'a) list
    (** Each with its location and its [x]. *)
    ; complete : bool
    (** A missing required field is reported, and [false] here. *)
    }
end

val resolve_record_fields
  :  Scope.t
  -> (Source.Field.t * 'a) list
  -> all_required:bool
  -> expected:Type.ty option
  -> loc:Loc.t
  -> ('a Resolved.t, Typecheck_error.t) Result.t

(** Returns the instantiated record type, its declaration, and each field with
    its [x] paired with its type. *)
val record_fields
  :  Fresh.t
  -> Scope.t
  -> (Source.Field.t * 'a) list
  -> all_required:bool
  -> expected:Type.ty option
  -> loc:Loc.t
  -> ( Type.ty
       * Typed.Record_type.t
       * (Typed.Field_ref.t * ('a * Type.ty)) list
       * bool
       , Typecheck_error.t )
       Result.t

(** Returns the field, the instantiated record type and the field's type. *)
val field_type
  :  Fresh.t
  -> Scope.t
  -> Source.Field.t
  -> record:Type.ty option
  -> loc:Loc.t
  -> (Typed.Field_ref.t * Type.ty * Type.ty, Typecheck_error.t) Result.t
