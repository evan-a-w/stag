open! Import

(** Canonical names of the prelude's items. The prelude is outside the
    program's root module, and its canonical names start with [root ^ "."],
    which no program name does (a lowercase name is never a module). *)

val root : string

(** [canonical "Stag.Expr.t"] is the canonical name of the prelude's
    [Stag.Expr.t]. *)
val canonical : string -> string

(** The path within the prelude of a canonical name, if it is the prelude's. *)
val relative : string -> string option

(** Whether the prelude's name [relative] is shadowed where [has] tells which
    types and modules are in scope: by a type of its name, or a module of the
    first name of its path. *)
val shadowed : string -> has:([ `Type | `Module ] -> string -> bool) -> bool

(** A canonical name for humans: without [root] for the prelude's, unless
    [shadowed] (by default, never), with [root] (eg. [prelude.option]). *)
val to_string_hum : ?shadowed:(string -> bool) -> string -> string
