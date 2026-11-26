open! Import

(** An instance of [scheme], [toplevel]'s, made by [instantiate]; with the
    instances of the uses its scheme awaits, passed to [on_use]. *)
val instance
  :  Toplevel_binding.t
  -> scheme:Type.ty
  -> instantiate:(Type.ty -> 'a)
  -> on_use:(Quoted_use.t -> ty:'a -> unit)
  -> 'a

val quoted_use
  :  Fresh.t
  -> Scope.t
  -> Toplevel_binding.t
  -> loc:Loc.t
  -> Type.ty

(** An instance of [entry]'s scheme, at [loc]. A toplevel's may await others
    (see [Awaiting]): the instance's uses of them are uses by this code. *)
val instantiate_entry
  :  Fresh.t
  -> Scope.t
  -> Env.Entry.t
  -> loc:Loc.t
  -> Type.ty

val use_entry
  :  ?by_symbol:bool
  -> ?embedding:((unit -> unit) -> unit)
  -> Scope.t
  -> at:Mode_rules.Expected.t
  -> Env.Entry.t
  -> name:string
  -> loc:Loc.t
  -> unit

val field_modality
  :  Scope.t
  -> Typed.Field_ref.t
  -> record:Type.ty
  -> Modality.t

val use_free
  :  Scope.t
  -> at:Mode_rules.Expected.t
  -> why:string
  -> loc:Loc.t
  -> unit

(** A closure computes nothing, so is generalized whatever it captures; other
    values only when [const], which is then required of them. *)
val generalizes : Scope.t -> Mode.Var.t -> is_function:bool -> loc:Loc.t -> bool

(** [name] is the context entry the variable is provided as, if it becomes a
    free variable of the quote. *)
val resolution_type
  :  Fresh.t
  -> Scope.t
  -> at:Mode_rules.Expected.t
  -> name:string
  -> loc:Loc.t
  -> Resolution.t
  -> (Type.ty * Typed.Var_ref.t, Typecheck_error.t) Result.t
