open! Core

(** The type checker. Ids, which appear in sexps of types, depend only on what
    was done with the [t]. *)
type t

val create : unit -> t

(** Type declarations of every module are checked first. Toplevel bindings, in
    any module, may refer to each other in any order. Each strongly connected
    component is generalized once fully checked, if all its members are
    syntactic values and it is not recursive through a non-function, and
    persistent if all are syntactic functions. Returns the signature of
    [program], mirroring it item by item. *)
val typecheck
  :  t
  -> program:Syntax.Program.t
  -> (Signature.t, Typecheck_error.t) Result.t
