open! Import

(** The mode rules of expressions. Each checks a subexpression against the mode
    it is expected at, and relates that to the mode its value is part of (or is
    only evaluated for). *)

module Ctx : sig
  type t

  (** [holds] reports a constraint that doesn't hold, returning whether it did. *)
  val create
    :  modes:Mode.Id_source.t
    -> rank:int
    -> holds:(loc:Loc.t -> (unit, Mode.Error.t) Result.t -> bool)
    -> crossing:(Type.ty -> Mode.Crossing.t)
    -> t
end

(** The mode an expression's value is checked against: what is evaluated for the
    value is at most it. It knows whether the expression is in tail position,
    and whether [stack_] applies to it. A subexpression that is the whole value
    is checked at the same [Expected.t]. *)
module Expected : sig
  type t

  (** Of a value unrelated to any other: a toplevel's, a test's, or what is only
      checked for its errors. *)
  val fresh : Ctx.t -> t

  (** The value of the body of a [let] or [match]: in tail position if it is,
      but not what [stack_] applies to. *)
  val whole : t -> t

  (** Of code in a quote: whether it is in tail position is decided where it is
      spliced. *)
  val quoted : Ctx.t -> t

  (** [e [@nontail]]. *)
  val nontail : t -> t
end

(** A mode on an arrow: of an argument, a result or a partial application. *)
val arrow_mode : Ctx.t -> Mode.Var.t

(** What the cell of a pointer of a type not yet known may hold. *)
val fresh_held : Ctx.t -> Mode_arg.t

(** Its value is at least [stage], e.g. as computing it runs code. *)
val runs
  :  Ctx.t
  -> Expected.t
  -> Mode.Stage.t
  -> loc:Loc.t
  -> why:string
  -> unit

(** A subexpression only evaluated for the value, not part of it, at a mode of
    [inner] (by default the [Ctx.t]): returns its mode. *)
val evaluate
  :  ?inner:Ctx.t
  -> Ctx.t
  -> Expected.t
  -> loc:Loc.t
  -> why:string
  -> (Expected.t -> 'a)
  -> 'a * Mode.Var.t

(** A fresh mode fixed to the annotation on the axes it names. *)
val annotated
  :  Ctx.t
  -> Syntax.Mode_expr.t option
  -> var:(string -> loc:Loc.t -> Mode.Var.t)
  -> Mode.Var.t

(** [e @ annot]: with a mode, [e] is checked at a mode of its own that the
    annotation bounds, if it checks. *)
val annotated_use
  :  Ctx.t
  -> Expected.t
  -> annot:Syntax.Mode_expr.t option
  -> var:(string -> loc:Loc.t -> Mode.Var.t)
  -> loc:Loc.t
  -> (Expected.t -> (Typed.Expr.t, 'e) Result.t)
  -> (Typed.Expr.t, 'e) Result.t

(** A part of a block being built, held by it through the modality, of [ty] (by
    default its type). *)
val part
  :  ?ty:Type.ty
  -> Ctx.t
  -> Expected.t
  -> Modality.t
  -> (Expected.t -> Typed.Expr.t)
  -> Typed.Expr.t

(** Reads a part of a block, checked at the mode given: the value is the part,
    through the modality it returns. *)
val project
  :  Ctx.t
  -> Expected.t
  -> loc:Loc.t
  -> why:string
  -> (Expected.t -> 'a * Modality.t)
  -> 'a

(** The mode of a variable: only these make one. *)
module Binding : sig
  type t = Binding.t [@@deriving sexp_of]

  (** Bound by [let x @ annot = e], [e] being of [mode]: the annotation's, on
      the axes it names, else [e]'s. *)
  val value
    :  Ctx.t
    -> Mode.Var.t
    -> Syntax.Mode_expr.t option
    -> var:(string -> loc:Loc.t -> Mode.Var.t)
    -> t

  (** A function, of its closure's mode. *)
  val closure : Mode.Var.t -> t

  (** A non-function in its own definition, being checked at [Expected.t]: at
      least its value, which isn't known yet. *)
  val defining : Ctx.t -> Expected.t -> name:string -> loc:Loc.t -> t

  (** A [let mut] initialized by a value of mode [value] ([ty] crossing as its
      type does): its cell, and the values it holds. *)
  val mutable_
    :  ?ty:Type.ty
    -> Ctx.t
    -> value:Mode.Var.t
    -> Syntax.Mode_expr.t option
    -> var:(string -> loc:Loc.t -> Mode.Var.t)
    -> name:string
    -> loc:Loc.t
    -> t * t

  (** Bound by a pattern to a part of a block of mode [block]. *)
  val project : Ctx.t -> block:Mode.Var.t -> Modality.t -> loc:Loc.t -> t

  (** A value computed at compile time, embedded in the code. *)
  val embedded : Ctx.t -> name:string -> loc:Loc.t -> t

  (** A toplevel being checked, used where its mode isn't known yet. *)
  val being_checked : Ctx.t -> name:string -> loc:Loc.t -> t
end

(** Of a raised exception, matched on. *)
val raised : Ctx.t -> loc:Loc.t -> Mode.Var.t

(** Allocation sites: [stack_] applies to those the value is made of. *)
module Alloc : sig
  (** A closure of the mode given. *)
  val closure : Ctx.t -> Expected.t -> loc:Loc.t -> (Mode.Var.t -> 'a) -> 'a

  (** A quote: the mode of its code. *)
  val code : Ctx.t -> Expected.t -> loc:Loc.t -> Mode.Var.t

  (** A pointer, whose mode is the value's. *)
  val pointer : Expected.t -> unit

  (** [stack_ e]: [Error] if [e] allocates nothing. *)
  val stack
    :  Ctx.t
    -> Expected.t
    -> loc:Loc.t
    -> (Expected.t -> Typed.Expr.t)
    -> (Typed.Expr.t, Typed.Expr.t) Result.t
end

module Function : sig
  (** The [curry] of the arrow of a function's last parameter: its result is a
      value it returns, not the closure of a partial application. A function
      whose body is a [fun] is one function of the parameters of both, so its
      last arrow is not one. *)
  val result_curry : Ctx.t -> loc:Loc.t -> Mode.Var.t

  (** The closures of partial applications hold what the function captures and
      the arguments given so far. *)
  val partial_applications
    :  Ctx.t
    -> closure:Mode.Var.t
    -> arg_modes:Mode.Var.t list
    -> partial_modes:Mode.Var.t list
    -> loc:Loc.t
    -> unit

  (** A parameter in the body, passed at [arg_mode]: the body is a region of its
      own. *)
  val param
    :  Ctx.t
    -> arg_mode:Mode.Var.t
    -> name:string
    -> loc:Loc.t
    -> Binding.t

  (** The body, of type [ty], returned at [res_mode]. *)
  val body
    :  Ctx.t
    -> res_mode:Mode.Var.t
    -> ty:Type.ty
    -> (Expected.t -> Typed.Expr.t)
    -> Typed.Expr.t
end

(** [exclave_ e]: [None] if not in tail position. In a quote, it is checked
    where the code is spliced. *)
val exclave
  :  Ctx.t
  -> Expected.t
  -> loc:Loc.t
  -> (Expected.t -> Typed.Expr.t)
  -> Typed.Expr.t option

(** [f x], [f] being of an arrow of those types and modes. With [curried], [f]
    is itself an application, which a tail call makes at once: then the result
    it gives further arguments is at most the [curry] of its arrow. *)
val apply
  :  Ctx.t
  -> Expected.t
  -> loc:Loc.t
  -> curried:bool
  -> arg_mode:Mode.Var.t
  -> res_mode:Mode.Var.t
  -> curry:Mode.Var.t
  -> arg_ty:Type.ty
  -> res_ty:Type.ty
  -> callee:(Expected.t -> Typed.Expr.t)
  -> arg:(Expected.t -> Typed.Expr.t)
  -> Typed.Expr.t * Typed.Expr.t

(** Assigns a [let mut] holding [held] (unless a free variable of the quote),
    across the locks [crossed]. *)
val assign
  :  Ctx.t
  -> Expected.t
  -> held:Binding.t option
  -> crossed:Env.Lock.t list
  -> ty:Type.ty
  -> name:string
  -> loc:Loc.t
  -> (Expected.t -> 'a)
  -> 'a

(** [target := value], [value] being checked at the target's type. [target]
    gives the modality of what it stores into: the cell of the pointer
    dereferenced last, then the fields stored into. *)
val store
  :  Ctx.t
  -> Expected.t
  -> loc:Loc.t
  -> target:(Expected.t -> Typed.Expr.t * Modality.t)
  -> value:(Expected.t -> ty:Type.ty -> Typed.Expr.t)
  -> Typed.Expr.t * Typed.Expr.t

(** Uses [entry] under [locks]: constrains its mode by the locks the use
    crosses. Toplevels are referred to by symbol, so closures and code don't
    capture them: they cross a quote lock if [portable], and otherwise if
    [by_symbol] (mutable, or not embeddable), when what the code reads is not
    known until it runs. A mutable's value is the one it holds, but closures
    capture its cell. *)
val use
  :  Ctx.t
  -> Expected.t
  -> Env.Entry.t
  -> locks:Env.Lock.t list
  -> by_symbol:bool
  -> toplevel:bool
  -> name:string
  -> loc:Loc.t
  -> mutable_captured_in_exclave:(unit -> unit)
  -> unit

(** A free variable of the enclosing quote, provided where the code is spliced:
    its mode is unknown. *)
val use_free
  :  Ctx.t
  -> Expected.t
  -> locks:Env.Lock.t list
  -> loc:Loc.t
  -> why:string
  -> unit

module Splice : sig
  (** The code a splice in a quote splices, part of the code being built, of
      mode [code]. *)
  val code : code:Mode.Var.t -> (Expected.t -> 'a) -> 'a

  (** What spliced code may use from the splice's scope: it may be part of the
      value. *)
  val captured : Expected.t -> Expected.t

  (** An explicit substitution, bound around the spliced code: its mode. *)
  val substitution
    :  Ctx.t
    -> Expected.t
    -> loc:Loc.t
    -> (Expected.t -> Typed.Expr.t)
    -> Typed.Expr.t * Binding.t
end

(** Whether [mode] is at most [const], reporting it if not. *)
val const : Ctx.t -> Mode.Var.t -> loc:Loc.t -> why:string -> bool

module Toplevel : sig
  type t = private
    { expected : Expected.t (** Of its value, unless a function. *)
    ; value : Mode.Var.t (** Of its value or closure. *)
    ; binding : Binding.t
    ; held : Binding.t option (** Of the values a mutable one holds. *)
    }

  (** [annot] is of its value, or of a function's result. *)
  val create
    :  Ctx.t
    -> is_function:bool
    -> mut:bool
    -> Syntax.Mode_expr.t option
    -> var:(string -> loc:Loc.t -> Mode.Var.t)
    -> name:string
    -> loc:Loc.t
    -> t

  (** A toplevel outlives every region. *)
  val outlives : Ctx.t -> t -> ty:Type.ty -> name:string -> loc:Loc.t -> unit
end

(** The argument and result modes of [Unsafe.assume_global]. *)
val assume_global : Ctx.t -> Mode.Var.t * Mode.Var.t

(** The argument and result modes of [Ptr.make], and what its cell may hold: the
    pointer is allocated at run time, and its cell holds the argument. *)
val ptr_make : Ctx.t -> Mode.Var.t * Mode.Var.t * Mode_arg.t

(** The argument and result modes of [Box.make]: the box is allocated at run
    time, and holds the argument as a part. *)
val box_make : Ctx.t -> Mode.Var.t * Mode.Var.t

(** The modes of the arrows of a prim taking arguments [args], outermost first.
    A prim only reads its arguments, so takes any, and returns a new value; a
    partial application holds the arguments so far. *)
val prim : Ctx.t -> args:Type.ty list -> Arrow_modes.t list

(** Fixes a mode argument of the type of a toplevel not generalized, once
    nothing more can constrain it. Where it occurs ([None]: in several ways):
    covariantly, at its lower bound, and contravariantly, at its upper one, the
    most a use at another type allows (its bound from the value it was made
    from); else, as what a cell holds, at [nonportable gc] on each axis it may,
    else what it must. *)
val default_mode_arg
  :  Mode.Var.t
  -> Mode_arg_position.t option
  -> name:string
  -> unit
