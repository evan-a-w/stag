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

include Debug_intf

external tys_into_one_arena
  :  Type.ty
  -> Type.ty
  -> gc_stress:Gc_stress.t
  -> int * Type.ty * Type.ty
  = "stag_rust_debug_round_trip_tys_into_one_arena"

external missing_callback : unit -> string = "stag_rust_debug_missing_callback"

module Round_trip = struct
  module Typed_toplevels = struct
    type t = Typed.Toplevel.t list

    external round_trip
      :  t
      -> gc_stress:Gc_stress.t
      -> t
      = "stag_rust_debug_round_trip_typed_toplevels"

    external same : t -> t -> string = "stag_rust_debug_same_typed_toplevels"
  end

  module Typed_program = struct
    type t = Typed.Program.t

    external round_trip
      :  t
      -> gc_stress:Gc_stress.t
      -> t
      = "stag_rust_debug_round_trip_typed_program"

    external same : t -> t -> string = "stag_rust_debug_same_typed_program"
  end

  module Ty = struct
    type t = Type.ty

    external round_trip
      :  t
      -> gc_stress:Gc_stress.t
      -> t
      = "stag_rust_debug_round_trip_ty"

    external same : t -> t -> string = "stag_rust_debug_same_ty"
  end

  module Syntax_program = struct
    type t = Syntax.Program.t

    external round_trip
      :  t
      -> gc_stress:Gc_stress.t
      -> t
      = "stag_rust_debug_round_trip_syntax_program"

    external same : t -> t -> string = "stag_rust_debug_same_syntax_program"
  end

  module Example_expr = struct
    type t = Example.Expr.t

    external round_trip
      :  t
      -> gc_stress:Gc_stress.t
      -> t
      = "stag_rust_debug_round_trip_example_expr"

    external same : t -> t -> string = "stag_rust_debug_same_example_expr"
  end

  module Example_ty = struct
    type t = Example.ty

    external round_trip
      :  t
      -> gc_stress:Gc_stress.t
      -> t
      = "stag_rust_debug_round_trip_example_ty"

    external same : t -> t -> string = "stag_rust_debug_same_example_ty"
  end

  module Example_defs_expr = struct
    type t = Example.Defs.expr

    external round_trip
      :  t
      -> gc_stress:Gc_stress.t
      -> t
      = "stag_rust_debug_round_trip_example_defs_expr"

    external same : t -> t -> string = "stag_rust_debug_same_example_defs_expr"
  end

  module Example_chained = struct
    type t = Example.chained

    external round_trip
      :  t
      -> gc_stress:Gc_stress.t
      -> t
      = "stag_rust_debug_round_trip_example_chained"

    external same : t -> t -> string = "stag_rust_debug_same_example_chained"
  end
end
