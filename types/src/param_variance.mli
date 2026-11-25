open! Import

(** The variances of a type declaration's parameters ([params], as in
    [Signature.Type_decl.t]), from where they occur in its definition [kind]:
    fields and constructor arguments are covariant, as is a field's modality
    naming a mode parameter. [con] gives other types' parameters' variances. An
    abstract type's parameters are invariant. *)
val compute
  :  con:Variance.of_con
  -> params:Type.arg list
  -> Signature.Type_decl.Kind.t
  -> Variance.t list
