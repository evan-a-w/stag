open! Import

(** What one call of [typecheck] checks. *)

module Report : sig
  type t
end

type t = private
  { type_env : Type_env.t
  ; toplevels : Toplevel_binding.t Lazy.t String.Table.t
  (** By canonical name. The prelude's are made when first used, so that those
      unused take no idents. *)
  ; stamps : Stamp_counter.t
  ; interpreter : Interpreter.t
  ; mutable ident_info : Typed.Ident_info.t Typed.Ident.Map.t
  ; sccs : Typed.Scc.t Queue.t
  ; declaration_splices : Typed.Expr.t Queue.t
  ; unfinished : Type.ty Stack.t
  (** The types of the toplevels and binders of components not yet finished.
      Code checked later can reach them, so once their component is finished,
      the variables not generalized are lowered to rank 0, so that later
      components don't generalize them. *)
  ; mutable reports : Report.t list (** Latest first. *)
  ; mutable reported : int
  (** How many errors were reported, [Already_reported] and forgotten ones
      included. *)
  ; mutable syntax_errors : bool
  (** Whether the program has code that did not parse, reported by the parser. *)
  ; splice_starts : int Stack.t
  (** The index each declaration splice being generated began at, innermost
      first. *)
  ; mutable_idents : Typed.Ident.Hash_set.t
  (** Of the [let mut]s, local and toplevel. *)
  ; modes : Mode.Id_source.t
  ; const_toplevels : bool
  ; compile_time_c_calls : Compile_time_c_calls.t
  ; parameter_modes : Mode.Var.t Int.Table.t
  (** Of every function, by id, until their stage is fixed: each use of a
      function not generalized raises them, also once it is checked. *)
  ; checking : Scc_frame.t Stack.t
  (** The frames of the toplevels being checked, innermost first. *)
  ; allocations : Typed.Allocation.t Queue.t
  ; mutable allocation_ids : int
  }

(** [type_env] is given the [t] it is in, not to be forced before it is made. *)
val create
  :  type_env:(t Lazy.t -> Type_env.t)
  -> stamps:Stamp_counter.t
  -> modes:Mode.Id_source.t
  -> const_toplevels:bool
  -> compile_time_c_calls:Compile_time_c_calls.t
  -> Interpreter.t
  -> t

val mark_syntax_errors : t -> unit
val allocation_id : t -> Typed.Allocation.Id.t

val report
  :  ?component:Component.t
  -> ?once:string
  -> ?fallback:bool
  -> t
  -> Typecheck_error.t
  -> unit

(** A [fallback] error is shown only if no other of the component is. *)
val report_by
  :  ?fallback:bool
  -> t
  -> Reporter.t
  -> ?once:string
  -> Typecheck_error.t
  -> unit

(** A use of a toplevel being checked, in a quote: its error is shown once,
    however many instances of the quoting code have it, and in an expansion,
    only as a fallback (see [Reporter.expansion]). The code using it did not
    check. *)
val report_use : t -> Quoted_use.t -> Unification_error.t -> unit

val errors : t -> Typecheck_error.t list
val fresh_ident : t -> string -> Typed.Ident.t
val find_toplevel : t -> string -> Toplevel_binding.t option
val toplevel : t -> string -> Toplevel_binding.t
val add_info : t -> Typed.Ident.t -> ty:Type.ty -> level:int -> unit

(** Generated code's binders keep their idents, but get their info anew. *)
val bind : t -> Source.Binder.t -> Type.ty -> level:int -> Typed.Ident.t

(** Values a toplevel splice's code embedded in its code. *)
val embedded : t -> Typed.Ident.t -> Typed.Ident_info.t option

val is_mutable : t -> Typed.Ident.t -> bool

(** Raised where code run at compile time by a declaration splice uses a
    toplevel whose checking was underway before the splice began, directly or
    through toplevels in its component, so it is left to be forced again. *)
exception Uses_unfinished

(** Evaluates at compile time, reflecting on the program's types. Why reflection
    failed, eg. a cycle, is the error of the evaluation. *)
val at_compile_time
  :  t
  -> (reflect:(string -> (Reflected_decl.Reflection.t, Error.t) Result.t)
      -> ('a, Error.t) Result.t)
  -> loc:Loc.t
  -> error:(Error.t -> Typecheck_error.t)
  -> ('a, Typecheck_error.t) Result.t
