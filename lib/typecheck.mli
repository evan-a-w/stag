open! Core

(** The type checker. Ids, which appear in sexps of types, depend only on what
    was done with the [t]. *)
type t

(** Toplevel splices are run by [interpreter] while checking, which also gets
    every checked toplevel, to evaluate those used at compile time on demand,
    and the rest when it is [Treewalk.eval]ed. Idents are made by its stamps,
    so are unique among those of every program it checks. Its budget (see
    [Treewalk.set_limits]) is shared by every program checked with [t], and
    toplevels it evaluates keep their values when [Treewalk.eval]ed. *)
val create : ?interpreter:Treewalk.t -> unit -> t

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
    persistent if all are syntactic functions.

    A toplevel splice is checked, run and replaced by the code it generates,
    inferred anew where it is spliced. Code run at compile time may use any
    toplevel whose component is checked. *)
val typecheck
  :  t
  -> program:Syntax.Program.t
  -> (Checked.t, Typecheck_error.t) Result.t
