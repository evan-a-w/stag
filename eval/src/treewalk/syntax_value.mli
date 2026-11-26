open! Import
include Syntax_value_intf.Syntax_value with type value := Value.t

(** The prelude's constructor [name] (eg. ["Some"]) applied to [args]. *)
val prelude_constructor : string -> Value.t list -> Value.t
