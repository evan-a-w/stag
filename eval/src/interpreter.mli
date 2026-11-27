open! Import

(** An interpreter of any implementation, with its state. Its functions are
    those of [Interpreter_intf.S]. *)
type t = T : (module Interpreter_intf.S with type t = 'a) * 'a -> t

(** A value of any interpreter's. *)
module Value : sig
  type t [@@deriving sexp_of]

  val to_string_hum : ?max_length:int -> t -> string
  val to_syntax : t -> 'a Generated_syntax.t -> loc:Loc.t -> 'a Or_error.t
end

val create : (module Interpreter_intf.S) -> stamps:Stamp_counter.t -> t
val set_output : t -> (string -> unit) -> unit
val set_argv : t -> string list -> unit
val with_output : t -> (string -> unit) -> f:(unit -> 'a) @ local -> 'a
val set_limits : t -> Limits.t -> unit
val add_ident_info : t -> ident:Typed.Ident.t -> info:Typed.Ident_info.t -> unit
val add_scc : t -> Typed.Scc.t -> unit

val splice
  :  t
  -> code:Typed.Expr.t
  -> level:int
  -> entry:(string -> Typed.Var_ref.t option)
  -> reflect:(string -> Reflected_decl.Reflection.t Or_error.t)
  -> Typed.Expr.t Or_error.t

val run
  :  t
  -> Typed.Expr.t
  -> level:int
  -> reflect:(string -> Reflected_decl.Reflection.t Or_error.t)
  -> Value.t Or_error.t

val global_info : t -> ident:Typed.Ident.t -> Typed.Ident_info.t option

val run_expect_test
  :  t
  -> Typed.Expect_test.t
  -> on_expect:(string Located.t -> unit)
  -> unit Or_error.t

val eval_toplevel : t -> ident:Typed.Ident.t -> unit Or_error.t
val find : t -> ident:Typed.Ident.t -> Value.t Or_error.t

(** The state, if [t] is of the implementation [id] names. *)
val state : t -> 'a Type_equal.Id.t -> 'a option
