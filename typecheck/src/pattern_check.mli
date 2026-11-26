open! Import

(** The components of [expected], if it is known to be a tuple of [n]. *)
val tuple_components : Type.ty option -> int -> Type.ty option list

(** The type of [label] in [expected], if it is known to be a polymorphic record
    ([`Record]) or variant ([`Variant]) with it. *)
val row_entry
  :  Type.ty option
  -> [ `Record | `Variant ]
  -> label:string
  -> Type.ty option

(** [ty] if it is known, else [fallback]. *)
val either_known : Type.ty -> Type.ty option -> Type.ty option

(** [expected] is the type the pattern or expression is known to have where it
    is (it is not unified with it here): constructors and fields are looked up
    in it when it is a nominal type (see [Known]). *)
val pattern_type
  :  ?expected:Type.ty
  -> Fresh.t
  -> Scope.t
  -> pattern:Source.Pattern.t
  -> Typed.Pattern.t * Pattern_var.t list
