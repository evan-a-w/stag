open! Import

(** An evaluator of typed programs, as the typechecker and driver use it. Its
    errors are [Runtime_error]s: of kind [Reached_error] on code that did not
    check ([Typed.Expr.Desc.Error]). *)
module type Value = sig
  type t [@@deriving sexp_of]

  (** As stag syntax, eg. [C (1, `A)], for messages. If longer than
      [max_length], it is cut there and followed by [...]. *)
  val to_string_hum : ?max_length:int -> t -> string

  (** The syntax [t] represents as a value of the [Prelude]'s type of it
      ([Stag.Expr.t] etc.), as [Syntax_value_intf.S.of_value]. *)
  val to_syntax : 'a Generated_syntax.t -> t -> loc:Loc.t -> 'a Or_error.t
end

module type S = sig
  module Value : Value

  type t

  (** Names [t], to recover it from an [Interpreter.t]. *)
  val type_id : t Type_equal.Id.t

  (** [stamps] should be the counter that made the idents of the programs
      evaluated, so idents minted while evaluating are distinct from theirs. *)
  val create : stamps:Stamp_counter.t -> unit -> t

  (** Where prims print to, stdout by default. *)
  val set_output : t -> (string -> unit) -> unit

  (** The program's arguments, without its name, as [argv ()] returns them: none
      by default. *)
  val set_argv : t -> string list -> unit

  (** Runs [f] with prims printing to [output], then restores the previous. *)
  val with_output : t -> (string -> unit) -> f:(unit -> 'a) @ local -> 'a

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
  val add_ident_info
    :  t
    -> ident:Typed.Ident.t
    -> info:Typed.Ident_info.t
    -> unit

  (** Registers the checked type declaration [decl] under its canonical [name]
      ([decl.name] is the short name). Builtin types ([int], [bool], [string],
      [char], [unit], [exn], [ptr], [<error>]) are not pushed. Each canonical
      name is pushed at most once, when its declaration is checked: non-splice
      declarations reachable from an SCC or evaluated expression before it runs,
      and declarations involving splices when their definitions are checked
      (which may also happen re-entrantly during [run] or [splice], eg. via
      [reflect]). *)
  val add_type_decl : t -> name:string -> decl:Signature.Type_decl.t -> unit

  (** Queues [scc], with the mode-inferred [allocations] of its sites. Its
      toplevels are evaluated by [eval_toplevel], or earlier when first used,
      eg. by a toplevel splice: each at most once, and the component's functions
      are bound when any member is. *)
  val add_scc : t -> Typed.Scc.t -> allocations:Typed.Allocation.t list -> unit

  (** Evaluates the toplevel splice [code], at [level] in an empty env, to code,
      binding the values it embeds globally. Returns that code with fresh
      binders and the free variables outside its quotes replaced by [entry].
      Instantiating takes a step per node, which stands for inferring the code
      too, so code too large to infer within the budget is an error.

      It runs at compile time: [Stag.Type.decl] is [reflect] of the canonical
      name of the type, which it is a runtime error to call otherwise. *)
  val splice
    :  t
    -> code:Typed.Expr.t
    -> level:int
    -> entry:(string -> Typed.Var_ref.t option)
    -> reflect:(string -> Reflected_decl.Reflection.t Or_error.t)
    -> Typed.Expr.t Or_error.t

  (** Evaluates [expr] at compile time, as [splice], at [level] in an empty env,
      eg. a declaration splice's expression. *)
  val run
    :  t
    -> Typed.Expr.t
    -> level:int
    -> reflect:(string -> Reflected_decl.Reflection.t Or_error.t)
    -> Value.t Or_error.t

  (** The info of [ident] if it is bound globally, eg. a value embedded by a
      toplevel splice's code. *)
  val global_info : t -> ident:Typed.Ident.t -> Typed.Ident_info.t option

  (** Evaluates the test's body, after the program's toplevels, calling
      [on_expect] at each [[%expect]] it reaches. *)
  val run_expect_test
    :  t
    -> Typed.Expect_test.t
    -> on_expect:(string Located.t -> unit)
    -> unit Or_error.t

  (** Evaluates [expr] at runtime (level 0), after the program's toplevels, in
      an env of [bindings], eg. the body of a [let%bench]. *)
  val eval_expr
    :  ?bindings:(Typed.Ident.t * Value.t) list
    -> t
    -> Typed.Expr.t
    -> Value.t Or_error.t

  (** Evaluates the toplevel [ident], of a component queued, unless it already
      is. *)
  val eval_toplevel : t -> ident:Typed.Ident.t -> unit Or_error.t

  (** The value of a toplevel, once evaluated. *)
  val find : t -> ident:Typed.Ident.t -> Value.t Or_error.t
end
