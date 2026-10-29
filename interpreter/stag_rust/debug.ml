open! Import

external typed_toplevels
  :  Typed.Toplevel.t list
  -> string
  = "stag_rust_debug_typed_toplevels"

external typed_toplevels_summary
  :  Typed.Toplevel.t list
  -> string
  = "stag_rust_debug_typed_toplevels_summary"

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
