open! Import

(** The bindings of one activation: a closure's captures and arguments, and
    the locals bound while evaluating its body (or the toplevels). Idents are
    unique, so a binding is never shadowed or removed. Lookups fall back to
    [fallback] (the toplevels). *)
module Env : sig
  type 'value t

  val create : unit -> _ t

  (** Errors if [ident] is unbound, or declared but not yet [set]. *)
  val lookup
    :  ?fallback:'value t
    -> 'value t
    -> ident:Typed.Ident.t
    -> 'value Or_error.t

  (** Binds [ident] to an empty slot, to be [set] later, eg. for each member
      of a recursive binding before evaluating their bodies. *)
  val declare : _ t -> ident:Typed.Ident.t -> unit

  (** Fills the slot of [ident], binding it first if needed. *)
  val set : 'value t -> ident:Typed.Ident.t -> value:'value -> unit

  (** A new env sharing the slots of [captures] with [t], so slots filled
      later are seen by both. *)
  val of_captures
    :  ?fallback:'value t
    -> 'value t
    -> captures:Typed.Ident.Hash_set.t
    -> 'value t Or_error.t
end

module Value : sig
  type t =
    | Int of int
    | Bool of bool
    | String of string
    | Char of char
    | Unit
    | Tuple of t list
    | Construct of Typed.Constructor_ref.t * t list
    | Record of t array (** By field index. *)
    | Poly_record of t String.Map.t
    | Poly_variant of string * t (** A tag without an argument has [Unit]. *)
    | Function of
        { args : Typed.Ident.t list (** Those not yet applied, non-empty. *)
        ; body : Typed.Expr.t
        ; env : t Env.t
        ; level : int
        }
    | Code of
        { expr : Typed.Expr.t
        ; persistent : t Typed.Ident.Map.t
          (** The values of the persistent binders [expr] embeds. *)
        }
    | Prim of
        { prim : Prim.t
        ; applied : t list (** Fewer than its arity, latest first. *)
        }

  (** Functions are [<fun>], as their envs may contain themselves. Here and in
      [to_string_hum], constructors of the prelude are named as within it
      ([Prelude_path.to_string_hum]). *)
  val sexp_of_t : t -> Sexp.t

  (** As stag syntax, eg. [C (1, `A)], for messages. Functions are [<fun>],
      code [<code>] and nominal records [<record>]. *)
  val to_string_hum : t -> string
end

type t

(** [stamps] should be the counter that made the idents of the programs
    evaluated, so idents minted while evaluating are distinct from theirs. *)
val create : stamps:Stamp_counter.t -> unit -> t

val stamps : t -> Stamp_counter.t

(** Where prims print to, stdout by default. *)
val set_output : t -> (string -> unit) -> unit

val output : t -> string -> unit

(** Evaluation from now on, until [set_limits] again, is bounded by [limits],
    counted from now: one budget shared by everything evaluated meanwhile, eg.
    every toplevel splice of every program checked with [t]. Building and
    instantiating code takes a step per node. Evaluation is unlimited until
    [set_limits] is first called.

    Toplevels are evaluated at most once, so those evaluated while one budget
    is in force are not evaluated, nor charged, again under the next. *)
val set_limits : t -> Limits.t -> unit

(** Every ident evaluated needs its info: the typechecker adds each as it
    binds it. *)
val add_ident_info : t -> ident:Typed.Ident.t -> info:Typed.Ident_info.t -> unit

(** Queues [scc] to [eval]. Its toplevels are evaluated earlier if used
    before, eg. by a toplevel splice: each is evaluated at most once, when
    first used, and the component's functions are bound when any member is. *)
val add_scc : t -> Typed.Scc.t -> unit

(** Evaluates the toplevel splice [code], at [level] in an empty env, to code,
    binding the values it embeds globally. Returns that code with fresh
    binders and the free variables outside its quotes replaced by [entry].
    Instantiating takes a step per node, which stands for inferring the code
    too, so code too large to infer within the budget is an error. *)
val splice
  :  t
  -> code:Typed.Expr.t
  -> level:int
  -> entry:(string -> Typed.Var_ref.t option)
  -> Typed.Expr.t Or_error.t

(** Evaluates [expr] at [level] in an empty env, eg. a declaration splice's
    expression at compile time. *)
val run : t -> Typed.Expr.t -> level:int -> Value.t Or_error.t

(** The info of [ident] if it is bound globally, eg. a value embedded by a
    toplevel splice's code. *)
val global_info : t -> ident:Typed.Ident.t -> Typed.Ident_info.t option

(** The binders from outside the closure [ident] that it reads when called,
    including itself if it is recursive and those embedded by quotes it builds.
    Only the expansions of toplevel splices are read. *)
val captures
  :  t
  -> ident:Typed.Ident.t
  -> args:Typed.Ident.t list
  -> body:Typed.Expr.t
  -> Typed.Ident.Hash_set.t

(** Evaluates the components queued not yet evaluated, in order. *)
val eval : t -> unit Or_error.t

(** Evaluates the test's body, after [eval], calling [on_expect] at each
    [[%expect]] it reaches. *)
val run_expect_test
  :  t
  -> Typed.Expect_test.t
  -> on_expect:(string Located.t -> unit)
  -> unit Or_error.t

(** Evaluates the toplevel [ident], of a component queued, unless it already
    is. *)
val eval_toplevel : t -> ident:Typed.Ident.t -> unit Or_error.t

(** The value of a toplevel, once evaluated. *)
val find : t -> ident:Typed.Ident.t -> Value.t Or_error.t
