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

(** A canonical name as it is written where the prelude is not shadowed:
    without [root] for the prelude's. *)
val to_string_hum : string -> string
