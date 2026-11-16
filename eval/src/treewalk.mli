open! Import
module Env = Env
module Value = Value

type t

(** The error of evaluating code that did not check ([Typed.Expr.Desc.Error]). *)
exception Reached_error
[@@deriving sexp_of]

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

    Toplevels are evaluated at most once, so those evaluated while one budget is
    in force are not evaluated, nor charged, again under the next. *)
val set_limits : t -> Limits.t -> unit

(** Every ident evaluated needs its info: the typechecker adds each as it binds
    it. *)
val add_ident_info : t -> ident:Typed.Ident.t -> info:Typed.Ident_info.t -> unit

(** Queues [scc] to [eval]. Its toplevels are evaluated earlier if used before,
    eg. by a toplevel splice: each is evaluated at most once, when first used,
    and the component's functions are bound when any member is. *)
val add_scc : t -> Typed.Scc.t -> unit

(** Evaluates the toplevel splice [code], at [level] in an empty env, to code,
    binding the values it embeds globally. Returns that code with fresh binders
    and the free variables outside its quotes replaced by [entry]. Instantiating
    takes a step per node, which stands for inferring the code too, so code too
    large to infer within the budget is an error.

    It runs at compile time: [Stag.Type.decl] is [reflect] of the canonical name
    of the type, which it is a runtime error to call otherwise. *)
val splice
  :  t
  -> code:Typed.Expr.t
  -> level:int
  -> entry:(string -> Typed.Var_ref.t option)
  -> reflect:(string -> Value.t Or_error.t)
  -> Typed.Expr.t Or_error.t

(** Evaluates [expr] at compile time, as [splice], at [level] in an empty env,
    eg. a declaration splice's expression. *)
val run
  :  t
  -> Typed.Expr.t
  -> level:int
  -> reflect:(string -> Value.t Or_error.t)
  -> Value.t Or_error.t

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

(** Evaluates [expr] at runtime (level 0), after [eval], in an env of
    [bindings], eg. the body of a [let%bench]. *)
val eval_expr
  :  ?bindings:(Typed.Ident.t * Value.t) list
  -> t
  -> Typed.Expr.t
  -> Value.t Or_error.t

(** Evaluates the toplevel [ident], of a component queued, unless it already is. *)
val eval_toplevel : t -> ident:Typed.Ident.t -> unit Or_error.t

(** The value of a toplevel, once evaluated. *)
val find : t -> ident:Typed.Ident.t -> Value.t Or_error.t
