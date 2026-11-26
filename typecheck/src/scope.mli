open! Import

module Param : sig
  type t =
    { ty : Type.ty
    ; mode : Mode.Var.t
    }
end

type t =
  { program : Program_env.t
  ; level : Env.Level.t
  ; rank : Type.Rank.t
  ; locals : Env.t
  ; quote : quote option
  ; module_ : Module_scope.t
  ; scc : Scc_frame.t
  ; annotation_vars : Type_env.Annotation_vars.t
  ; in_expect_test : bool
  ; generated : Generated.t option
  ; syntax_splice : Syntax_splice.t option
  ; locks : Env.Lock.t list (** Innermost first. *)
  ; params : Param.t list
  (** Of the functions being checked: their modes are decided by each use of
      their function. *)
  ; allocations : Allocation_sites.t option
  (** Of the toplevel being checked: its allocation sites, as its run time code
      is checked. *)
  ; owner : Allocation_sites.Frame.t option
  (** The innermost [let] whose function (or value) is being checked. *)
  ; uses : Local_uses.t
  (** The uses of toplevels being checked made by the code, by the innermost
      [let] whose definition is being checked. *)
  }

and quote =
  { id : int
  ; context : Type.row
  ; outside : t
  ; mode : Mode.Var.t
  (** The code's: of what it embeds from outside, and of the splices run when it
      is built. *)
  }

val quote_id : t -> int option

val entry
  :  ?defining:bool
  -> ?held:Binding.t
  -> ?shape:Mode_shape.t
  -> t
  -> ident:Typed.Ident.t
  -> scheme:Type.ty
  -> mode:Binding.t
  -> Env.Entry.t

module Bounds : sig
  (** Upper bounds as they are when [known_stage] is called ([Snapshot]),
      however they are bounded later: as what a quote embeds then makes them.
      Else as they are when its result is. *)
  type t =
    | Snapshot
    | Live
end

(** What [mode] is at least however the functions being checked are used: with
    the modes of their parameters, those within their types, and those of the
    types of the components not finished yet raised to their upper bounds, and
    those of the parameters of every function, as one not generalized may be
    used later, raising what it returned before. *)
val known_stage : t -> bounds:Bounds.t -> Mode.Var.t -> Mode.Stage.t

val enter_closure : Fresh.t -> t -> Mode.Var.t -> t

(** Whether the quote may embed a local of [ty] and [mode], by [known_stage]:
    plain data may be, else what is [portable] however the functions being
    checked are used. [`Wait]s while whether it has pointers, or whether what it
    is built from does, isn't known: unless [Final], assuming they do. *)
val embeddable
  :  t
  -> Type.ty
  -> Mode.Var.t
  -> known_stage:(Mode.Var.t -> Mode.Stage.t)
  -> finality:Var_layout.Finality.t
  -> [ `No | `Wait | `Yes ]

val fresh_ty : Fresh.t -> t -> Type.ty
val join_ty : Fresh.t -> t -> Type.ty
val fresh_row : Fresh.t -> t -> Type.row
val fresh_presence : Fresh.t -> t -> Type.presence
val instantiate : ?at:Loc.t -> Fresh.t -> t -> Type.ty -> Type.ty

(** Type splices are evaluated in declarations, not in what is evaluated or
    quoted then. *)
val splices : t -> bool

val reporter : t -> Reporter.t
val report : ?once:string -> t -> Typecheck_error.t -> unit

(** Whether the mode constraint held: if not, it is reported. Each limit is
    shown once, at what first exceeded it. *)
val mode_holds : t -> loc:Loc.t -> (unit, Mode.Error.t) Result.t -> bool

val modes : t -> Mode_rules.Ctx.t
val arrow_mode : t -> Mode.Var.t
val mode_var : t -> string -> loc:Loc.t -> Mode.Var.t

(** Whether they unify: if not, what checked is to become [Error]. *)
val unifies : Fresh.t -> t -> ty1:Type.ty -> ty2:Type.ty -> loc:Loc.t -> bool

(** Like [unifies], but what has [lower] may be used as an [upper]: the modes in
    them are related by variance. *)
val subtypes
  :  Fresh.t
  -> t
  -> lower:Type.ty
  -> upper:Type.ty
  -> loc:Loc.t
  -> bool

val elaborate_resolved
  :  t
  -> Syntax.Type_expr.t
  -> (Type.ty * Syntax.Type_expr.t, Typecheck_error.t) Result.t
