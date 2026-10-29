(** What the Rust bindings ([interpreter/rust/stag_ast]) convert values to:
    the Rust value's [{:#?}], then its arena's, in which [Ref]s ([#n]) index,
    or [error: ...] if the conversion failed. *)

open! Import

module Gc_stress = struct
  (** How hard building OCaml values makes the GC work, so that values held
      unrooted are caught: [Minor] collects and poisons the minor heap before
      every allocation (so set the minor heap small), [Compact] compacts the
      major heap before every allocation too (so only for small values). *)
  type t =
    | Off
    | Minor
    | Compact
  [@@deriving sexp_of, enumerate]
end

(** Converting to Rust and building back ([stag_ast::to_ocaml]). *)
module type Round_trip = sig
  type t

  (** Raises [Failure] if converting or building fails. *)
  val round_trip : t -> gc_stress:Gc_stress.t -> t

  (** ["same"] if both convert to equal Rust values with equal arenas: so
      they are equal, and share and are cyclic through stored values alike;
      else ["different"]. *)
  val same : t -> t -> string
end

module type Debug = sig
  module Gc_stress = Gc_stress

  module type Round_trip = Round_trip

  val typed_toplevels : Typed.Toplevel.t list -> string
  val ty : Type.ty -> string

  (** Only how many types the arena of the conversion has. *)
  val ty_count : Type.ty -> string

  val syntax_program : Syntax.Program.t -> string

  (** Only how many of its parts and stored values there are. *)
  val typed_program_summary : Typed.Program.t -> string

  (** The program's [ident_info] map. *)
  val typed_ident_info : Typed.Program.t -> string

  (** Why building a map of a key type whose OCaml callbacks are not
      registered fails. *)
  val missing_callback : unit -> string

  module Round_trip : sig
    module Typed_toplevels : Round_trip with type t = Typed.Toplevel.t list
    module Typed_program : Round_trip with type t = Typed.Program.t
    module Ty : Round_trip with type t = Type.ty
    module Syntax_program : Round_trip with type t = Syntax.Program.t

    (** Of [interpreter/gen/example]'s declarations. *)
    module Example_expr : Round_trip with type t = Example.Expr.t

    module Example_ty : Round_trip with type t = Example.ty
    module Example_defs_expr : Round_trip with type t = Example.Defs.expr
    module Example_chained : Round_trip with type t = Example.chained
  end
end
