open! Import
module Env = Env
module Value = Value
include Interpreter_intf.S with module Value := Value

(** The binders from outside the closure [ident] that it reads when called,
    including itself if it is recursive and those embedded by quotes it builds.
    Only the expansions of toplevel splices are read. *)
val captures
  :  t
  -> ident:Typed.Ident.t
  -> args:Typed.Ident.t list
  -> body:Typed.Expr.t
  -> Typed.Ident.Hash_set.t

(** The value of a toplevel, once evaluated. *)
val find : t -> ident:Typed.Ident.t -> Value.t Or_error.t
