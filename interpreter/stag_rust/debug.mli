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

(** Converting to Rust and building back ([stag_ast::to_ocaml]). *)
module type Round_trip = sig
  type t

  (** Raises [Failure] if converting or building fails. With [gc_stress],
      the GC runs before every allocation of the building. *)
  val round_trip : t -> gc_stress:bool -> t

  (** ["same"] if both convert to equal Rust values with equal arenas: so
      they are equal, and share and are cyclic through stored values alike;
      else ["different"]. *)
  val same : t -> t -> string
end

module Round_trip : sig
  module Typed_toplevels : Round_trip with type t = Typed.Toplevel.t list
  module Typed_program : Round_trip with type t = Typed.Program.t
  module Ty : Round_trip with type t = Type.ty
  module Syntax_program : Round_trip with type t = Syntax.Program.t
end
