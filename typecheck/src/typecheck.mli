open! Import

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
      (** The signature of the program, mirroring it item by item, without its
          expect tests. *)
    ; program : Typed.Program.t
    }
end

(** Declarations, in any module, may refer to each other in any order. They are
    checked in order, depth first, except those used before their turn, which
    are checked on demand: a type declaration once its constructors, fields or
    emptiness are used, a toplevel binding once used. Each strongly connected
    component of toplevel bindings is generalized once fully checked, if all its members are
    syntactic values and it is not recursive through a non-function, and
    persistent if all are syntactic functions.

    A toplevel splice is checked, run and replaced by the code it generates,
    inferred anew where it is spliced. Code run at compile time may use any
    toplevel whose component is checked. *)
val typecheck
  :  t
  -> program:Syntax.Program.t
  -> (Checked.t, Typecheck_error.t) Result.t
