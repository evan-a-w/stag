open! Import

(** [generated] located as [written], when they are the same declarations up to
    locations and [Decl_path.Resolved] and [Decl_path.Root] paths, which match
    any path written (the caller checks that each is written as it prints), eg.
    [written] parsed from [generated] printed. [None] if they differ, keeping
    the paths of [generated] otherwise. *)
val toplevels
  :  written:Syntax.Toplevel.t list
  -> Syntax.Toplevel.t list
  -> Syntax.Toplevel.t list option
