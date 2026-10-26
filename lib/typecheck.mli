open! Core

(** The type checker. Ids, which appear in sexps of types, depend only on what
    was done with the [t]. *)
type t

(** Idents are made by [stamps], so are unique among those of every program
    it checks, and of anything else using it. *)
val create : ?stamps:Stamp_counter.t -> unit -> t

module Checked : sig
  type t =
    { signature : Signature.t
      (** The signature of the program, mirroring it item by item. *)
    ; program : Typed.Program.t
    }
end

(** Type declarations of every module are checked first. Toplevel bindings, in
    any module, may refer to each other in any order. Each strongly connected
    component is generalized once fully checked, if all its members are
    syntactic values and it is not recursive through a non-function, and
    persistent if all are syntactic functions. *)
val typecheck
  :  t
  -> program:Syntax.Program.t
  -> (Checked.t, Typecheck_error.t) Result.t
