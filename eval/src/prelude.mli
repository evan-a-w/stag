open! Import

(** The prelude: the module [Stag] of [prelude/stag.stag], the syntax of stag
    as data, for declarations generated at compile time (see [Syntax_value]). *)

val module_name : string

(** [stag.stag], the module [module_name]. *)
val file : Source_file.t

(** The canonical name of an item of the prelude, given by its path within it:
    [canonical "Expr.t"] is ["Stag.Expr.t"]. *)
val canonical : string -> string
