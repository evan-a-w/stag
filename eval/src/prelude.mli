open! Import

(** The prelude, [prelude/prelude.stag]: in scope in every program, outside its
    root module, its canonical names those of [Prelude_path]. It has [list],
    [option], and the module [Stag], the syntax of stag as data, for
    declarations generated at compile time (see [Syntax_value]). *)

val path : string
val source : string

(** Parsed once. *)
val toplevels : Syntax.Toplevel.t list Lazy.t
