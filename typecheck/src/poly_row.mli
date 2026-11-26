open! Import

(** Rows of polymorphic records and variants, whose entries are [Present]. *)

val present : (string * Type.ty) list -> tail:Type.row -> Type.row

(** [entries] before a fresh tail, which is also returned. *)
val open_ : Fresh.t -> Scope.t -> (string * Type.ty) list -> Type.row * Type.row

val variant : Fresh.t -> Scope.t -> tag:string -> arg:Type.ty -> Type.ty
val report_duplicate_labels : Scope.t -> (Syntax.Name.t * _) list -> unit
