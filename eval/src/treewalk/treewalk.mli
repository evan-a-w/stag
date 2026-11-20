open! Import
module Env = Env
module Syntax_value = Syntax_value

module Value : sig
  include module type of struct
    include Value
  end

  val to_syntax : 'a Generated_syntax.t -> t -> loc:Loc.t -> 'a Or_error.t
end

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
