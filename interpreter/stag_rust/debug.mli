(** What the Rust bindings ([interpreter/rust/stag_ast]) convert values to:
    the Rust value's [{:#?}], then its arena's, in which [Ref]s ([#n]) index,
    or [error: ...] if the conversion failed. *)

open! Import

val typed_toplevels : Typed.Toplevel.t list -> string

val ty : Type.ty -> string

(** Only how many types the arena of the conversion has. *)
val ty_count : Type.ty -> string

val syntax_program : Syntax.Program.t -> string

(** Only how many of its parts and stored values there are. *)
val typed_program_summary : Typed.Program.t -> string

(** The program's [ident_info] map. *)
val typed_ident_info : Typed.Program.t -> string
