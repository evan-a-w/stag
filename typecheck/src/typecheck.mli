open! Import

(** The type checker. Ids, which appear in sexps of types, depend only on what
    was done with the [t]. *)
type t

(** Toplevel splices are run by [interpreter] while checking, which also gets
    every checked toplevel, to evaluate those used at compile time on demand,
    and the rest when it is [Interpreter.eval]ed. Idents are made by [stamps],
    which [interpreter] must have been created with, so are unique among those
    of every program it checks and those it mints. Its budget (see
    [Interpreter.set_limits]) is shared by every program checked with [t], and
    toplevels it evaluates keep their values when [Interpreter.eval]ed. Programs
    see [prelude], by default [Prelude.toplevels]. With [const_toplevels]
    (default false, as for the interpreter), every toplevel must be [const]. *)
val create
  :  stamps:Stamp_counter.t
  -> interpreter:Interpreter.t
  -> ?const_toplevels:bool
  -> ?prelude:Syntax.Toplevel.t list
  -> unit
  -> t

module Checked : sig
  type t =
    { signature : Signature.t
    (** The signature of the program, mirroring it item by item, without its
        expect tests. *)
    ; program : Typed.Program.t
    ; expansions : Expansion.t list
    (** Of each [include%inline] and [module%inline], in source order. *)
    }
end

(** What checked of a program with errors. *)
module Failed : sig
  type t =
    { errors : Typecheck_error.t list
    (** Not empty, each once, by location, then in the order found. *)
    ; partial : Checked.t
    (** What did not check is [Typed.Expr.Desc.Error] of its parts that did, and
        is left out of [signature]. *)
    }
end

(** Declarations, in any module, may refer to each other in any order. Names are
    first checked unique: members per module, parameters per type. Then
    declarations are checked in order, depth first, except those used before
    their turn, which are checked on demand: a type declaration once its
    constructors, fields or emptiness are used, a toplevel binding once used.
    Each strongly connected component of toplevel bindings is generalized once
    fully checked, if all its members are syntactic values and it is not
    recursive through a non-function, and persistent if all are syntactic
    functions.

    A toplevel splice is checked, run and replaced by the code it generates,
    inferred anew where it is spliced. Code run at compile time may use any
    toplevel whose component is checked. So a splice may run before later type
    declarations are checked.

    Checking goes on past errors: what did not check has type [Type.error], so
    that what uses it is not in error too, and splices whose code did not check
    are not run, generating nothing. *)
val typecheck : t -> program:Syntax.Program.t -> (Checked.t, Failed.t) Result.t

(** Checks the prelude itself, as [typecheck] does a program, all of its
    declarations, not only those a program uses: its [signature] is the
    prelude's. *)
val check_prelude : t -> (Checked.t, Failed.t) Result.t

(** The signature of the prelude ([Prelude]), which every program sees. *)
val prelude_signature : t -> Signature.t
