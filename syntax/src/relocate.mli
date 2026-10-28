open! Import

(** [generated] located as [written], when they are the same declarations up to
    locations and [Decl_path.Resolved] names written as the paths they print
    as, eg. [written] parsed from [generated] printed. [None] if they differ,
    keeping the [Resolved] names of [generated] otherwise. *)
val toplevels
  :  written:Syntax.Toplevel.t list
  -> Syntax.Toplevel.t list
  -> Syntax.Toplevel.t list option
