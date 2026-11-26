open! Import

(** A toplevel [let], [let mut] or [external] binding, checked on demand. *)

(** [external name : ty = "symbol"], whose [expr] is a placeholder. *)
module External : sig
  type t =
    { ty : Syntax.Type_expr.t
    ; symbol : string Located.t
    ; at_compile_time : Compile_time_c_calls.Policy.t
    }
  [@@deriving sexp_of]
end

module Kind : sig
  type t =
    | Value
    | Mut
    | Function of Syntax.Binding.t Nonempty_list.t
    | External of External.t
  [@@deriving sexp_of]
end

module State : sig
  type t =
    | Unchecked
    | In_progress of
        { index : int
        ; ty : Type.ty
        ; args : Typed.Ident.t list
        ; mode : Binding.t
        ; init_mode : Mode.Var.t
        (** Of its initializer: its [mode], but for a mutable one, whose
            variable is referred to by symbol. *)
        ; held : Binding.t option (** Of the values a mutable one holds. *)
        ; mutable body : Typed.Expr.t option
        ; mutable partials : Typed.Allocation.Id.t list
        (** As [Typed.Toplevel.partials]. *)
        ; mutable recursive : bool
        ; sites : Allocation_sites.t
        }
    | Done of
        { entry : Env.Entry.t
        ; typed : Typed.Toplevel.t
        ; component : Component.t
        (** If it failed (an error in it, or in a toplevel it uses), it is not
            run at compile time. *)
        ; runtime_only : string option
        (** The mutable toplevel it may use when run, if any, so it can't be run
            at compile time, where there is none. *)
        ; awaiting : Awaiting.t option
        }
  [@@deriving sexp_of]
end

type t = private
  { ident : Typed.Ident.t
  ; module_ : Module_scope.t
  ; annot : Syntax.Type_expr.t option
  ; mode : Syntax.Mode_expr.t option
  ; expr : Syntax.Expr.t
  ; kind : Kind.t
  ; generated : Generated.t option
  ; mutable state : State.t
  ; mutable pending_uses : Quoted_use.t list
  (** Of code finished without generalizing them, or not of a toplevel, to unify
      with an instance of its scheme. They outlive an abandoned check of it, as
      the code quoting it may have been finished. *)
  ; mutable awaited_by : Awaiting.t list (** Those with uses of it. *)
  }
[@@deriving sexp_of]

val create
  :  ident:Typed.Ident.t
  -> module_:Module_scope.t
  -> kind:Kind.t
  -> annot:Syntax.Type_expr.t option
  -> mode:Syntax.Mode_expr.t option
  -> expr:Syntax.Expr.t
  -> generated:Generated.t option
  -> t

val set_state : t -> State.t -> unit
val add_pending_use : t -> Quoted_use.t -> unit

(** Its [pending_uses], which it no longer has. *)
val take_pending_uses : t -> Quoted_use.t list

(** Adds to its [awaited_by], if not there yet. *)
val await : t -> Awaiting.t -> unit

(** Its [awaited_by], which it no longer has. *)
val take_awaited_by : t -> Awaiting.t list

val is_function : t -> bool
val is_mut : t -> bool
val args : t -> Syntax.Binding.t list
