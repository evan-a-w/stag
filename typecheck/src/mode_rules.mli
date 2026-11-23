open! Import

(** The mode rules of expressions. Each checks a subexpression against the mode
    it is expected at, and relates that to the mode its value is part of (or is
    only evaluated for). *)

module Ctx : sig
  type t =
    { modes : Mode.Id_source.t
    ; rank : int
    ; holds : loc:Loc.t -> (unit, Mode.Error.t) Result.t -> bool
    (** Whether the constraint held: if not, it is reported. *)
    ; crossing : Type.ty -> Mode.Crossing.t
    }

  val fresh : t -> Mode.Var.t
end

(** The mode an expression's value is checked against: what is evaluated for the
    value is at most it. *)
module Expected : sig
  type t

  val of_mode : Mode.Var.t -> t
  val fresh : Ctx.t -> t
  val mode : t -> Mode.Var.t

  (** The whole value, in tail position if it is. *)
  val whole : t -> t

  (** A part of the value, at the same mode. *)
  val part : t -> t

  (** At the same mode, not in tail position. *)
  val within : t -> t
end

(** Its value is at least [stage], e.g. as computing it runs code. *)
val runs
  :  Ctx.t
  -> Expected.t
  -> Mode.Stage.t
  -> loc:Loc.t
  -> why:string
  -> unit

(** A subexpression only evaluated for the value, not part of it, at a mode of
    [inner] (by default the [Ctx.t]). *)
val evaluate
  :  ?inner:Ctx.t
  -> Ctx.t
  -> Expected.t
  -> loc:Loc.t
  -> why:string
  -> (Expected.t -> 'a)
  -> 'a

(** A fresh mode at least [stage]. *)
val stage_at_least
  :  Ctx.t
  -> Mode.Stage.t
  -> loc:Loc.t
  -> why:string
  -> Mode.Var.t

(** Bounds [mode] by the annotation on the axes it names; with [exact], fixes it
    to it. [var] is the mode an annotation variable stands for. *)
val annotate
  :  ?exact:bool
  -> Ctx.t
  -> Mode.Var.t
  -> Syntax.Mode_expr.t option
  -> var:(string -> Mode.Var.t)
  -> unit

(** A fresh mode fixed to the annotation on the axes it names. *)
val annotated
  :  Ctx.t
  -> Syntax.Mode_expr.t option
  -> var:(string -> Mode.Var.t)
  -> Mode.Var.t

(** The mode of a variable bound by [let x @ annot = e], [e] being of [mode]:
    the annotation's, on the axes it names, else [e]'s. *)
val bound
  :  Ctx.t
  -> Mode.Var.t
  -> Syntax.Mode_expr.t option
  -> var:(string -> Mode.Var.t)
  -> Mode.Var.t

(** [e @ annot]: with a mode, [e] is checked at a mode of its own that the
    annotation bounds, if it checks. *)
val annotated_use
  :  Ctx.t
  -> Expected.t
  -> annot:Syntax.Mode_expr.t option
  -> var:(string -> Mode.Var.t)
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

(** The mode of a variable a pattern binds to a part of a block of mode [block]. *)
val project_binding
  :  Ctx.t
  -> block:Mode.Var.t
  -> Modality.t
  -> loc:Loc.t
  -> Mode.Var.t

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
  (** The closures of partial applications hold what the function captures and
      the arguments given so far. *)
  val partial_applications
    :  Ctx.t
    -> closure:Mode.Var.t
    -> arg_modes:Mode.Var.t list
    -> partial_modes:Mode.Var.t list
    -> loc:Loc.t
    -> unit

  (** The mode of a parameter in the body, passed at [arg_mode]: the body is a
      region of its own. *)
  val param
    :  Ctx.t
    -> arg_mode:Mode.Var.t
    -> name:string
    -> loc:Loc.t
    -> Mode.Var.t

  (** The body, of type [ty], returned at [res_mode]. *)
  val body
    :  Ctx.t
    -> res_mode:Mode.Var.t
    -> ty:Type.ty
    -> (Expected.t -> Typed.Expr.t)
    -> Typed.Expr.t
end

(** [exclave_ e]: [None] if not in tail position. *)
val exclave
  :  Ctx.t
  -> Expected.t
  -> loc:Loc.t
  -> (Expected.t -> Typed.Expr.t)
  -> Typed.Expr.t option

(** [f x], [f] being of an arrow of those types and modes. With [curried], [f]
    is itself an application, which a tail call makes at once. *)
val apply
  :  Ctx.t
  -> Expected.t
  -> loc:Loc.t
  -> curried:bool
  -> arg_mode:Mode.Var.t
  -> res_mode:Mode.Var.t
  -> arg_ty:Type.ty
  -> res_ty:Type.ty
  -> callee:(Expected.t -> Typed.Expr.t)
  -> arg:(Expected.t -> Typed.Expr.t)
  -> Typed.Expr.t * Typed.Expr.t

(** A [let mut] initialized by a value of mode [value], with [ty] crossing as
    its type does. *)
val initializes
  :  ?ty:Type.ty
  -> Ctx.t
  -> value:Mode.Var.t
  -> held:Mode.Var.t
  -> name:string
  -> loc:Loc.t
  -> unit

(** Assigns a [let mut] holding [held], across the locks [crossed]. *)
val assign
  :  Ctx.t
  -> Expected.t
  -> held:Mode.Var.t option
  -> crossed:Env.Lock.t list
  -> ty:Type.ty
  -> name:string
  -> loc:Loc.t
  -> (Expected.t -> 'a)
  -> 'a

(** [target := value], [value] being checked at the target's type. *)
val store
  :  Ctx.t
  -> Expected.t
  -> loc:Loc.t
  -> target:(Expected.t -> Typed.Expr.t)
  -> value:(Expected.t -> ty:Type.ty -> Typed.Expr.t)
  -> Typed.Expr.t * Typed.Expr.t

(** Uses [entry] under [locks]: constrains its mode by the locks the use
    crosses. Toplevels are referred to by symbol, so closures and code don't
    capture them: they only cross a quote lock if [portable], so as not to be
    free variables of the quote, unless mutable. A mutable's value is the one it
    holds, but closures capture its cell. *)
val use
  :  Ctx.t
  -> Expected.t
  -> Env.Entry.t
  -> locks:Env.Lock.t list
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

(** Whether [mode] may be generalized, which then requires it [const]. *)
val generalizes : Ctx.t -> Mode.Var.t -> loc:Loc.t -> bool

val const : Ctx.t -> Mode.Var.t -> loc:Loc.t -> why:string -> bool

(** A toplevel outlives every region. *)
val toplevel
  :  Ctx.t
  -> Mode.Var.t
  -> ty:Type.ty
  -> name:string
  -> loc:Loc.t
  -> unit

(** The argument and result modes of [Unsafe.assume_global]. *)
val assume_global : Ctx.t -> Mode.Var.t * Mode.Var.t
