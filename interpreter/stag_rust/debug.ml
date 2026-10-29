open! Import

external typed_toplevels
  :  Typed.Toplevel.t list
  -> string
  = "stag_rust_debug_typed_toplevels"

external ty : Type.ty -> string = "stag_rust_debug_ty"
external ty_count : Type.ty -> string = "stag_rust_debug_ty_count"

external syntax_program
  :  Syntax.Program.t
  -> string
  = "stag_rust_debug_syntax_program"

external typed_program_summary
  :  Typed.Program.t
  -> string
  = "stag_rust_debug_typed_program_summary"

external typed_ident_info
  :  Typed.Program.t
  -> string
  = "stag_rust_debug_typed_ident_info"

module type Round_trip = sig
  type t

  val round_trip : t -> gc_stress:bool -> t
  val same : t -> t -> string
end

module Round_trip = struct
  module Typed_toplevels = struct
    type t = Typed.Toplevel.t list

    external round_trip
      :  t
      -> gc_stress:bool
      -> t
      = "stag_rust_debug_round_trip_typed_toplevels"

    external same : t -> t -> string = "stag_rust_debug_same_typed_toplevels"
  end

  module Typed_program = struct
    type t = Typed.Program.t

    external round_trip
      :  t
      -> gc_stress:bool
      -> t
      = "stag_rust_debug_round_trip_typed_program"

    external same : t -> t -> string = "stag_rust_debug_same_typed_program"
  end

  module Ty = struct
    type t = Type.ty

    external round_trip
      :  t
      -> gc_stress:bool
      -> t
      = "stag_rust_debug_round_trip_ty"

    external same : t -> t -> string = "stag_rust_debug_same_ty"
  end

  module Syntax_program = struct
    type t = Syntax.Program.t

    external round_trip
      :  t
      -> gc_stress:bool
      -> t
      = "stag_rust_debug_round_trip_syntax_program"

    external same : t -> t -> string = "stag_rust_debug_same_syntax_program"
  end
end
