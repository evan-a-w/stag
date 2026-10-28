open! Import

(** The stale expansions of the program, in source order. *)
val stale : Driver.t -> Expansion.t list

(** The edit of [contents], of the file of [t], writing what [t]'s splice
    generated as its expansion, indented by 2 from the line of the toplevel,
    if it is stale. *)
val edit : Expansion.t -> contents:string -> Source_edit.t option

(** That [t] is stale. *)
val diagnostic : Expansion.t -> Diagnostic.t
