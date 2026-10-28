open! Import

(** The expansion written in an [include%inline] or [module%inline] differs
    from the declarations its splice generated. *)
type t =
  { loc : Loc.t (** Of the expansion. *)
  ; expected : string (** The expansion, printed. *)
  ; actual : string (** The declarations generated, printed. *)
  ; indent : int (** The column of the toplevel. *)
  }
[@@deriving sexp_of]

(** Those of the expansions of the program, in source order. Expansions are
    compared as printed, so modulo layout and comments. *)
val mismatches : Driver.t -> t list

(** The edit of the file [t.loc.file] writing [actual] as the expansion,
    indented by 2 from the toplevel. *)
val edit : t -> Source_edit.t
