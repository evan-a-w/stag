open! Import

(** Lazy monomorphiser and layout/frame lowering pass from [Typed.Expr.t] to
    [Compiled]. *)

type t

val create : unit -> t
val runtime_types : t -> Runtime_types.t
val add_type_decl : t -> name:string -> decl:Signature.Type_decl.t -> unit
val add_ident_info : t -> ident:Typed.Ident.t -> info:Typed.Ident_info.t -> unit
val add_scc : t -> Typed.Scc.t -> allocations:Typed.Allocation.t list -> unit

(** Specializes the toplevel [ident] at [ty] (or its least/default instantiation
    if [ty] is omitted), returning its memoized [Compiled.Global_id.t]. *)
val specialize_toplevel
  :  t
  -> ident:Typed.Ident.t
  -> ?ty:Type.ty
  -> unit
  -> Compiled.Global_id.t Or_error.t

(** Compiles a standalone expression (eg. an expect test, benchmark body, or
    compile-time splice) with optional pre-bound locals. *)
val compile_expr
  :  t
  -> ?bindings:(Typed.Ident.t * Type.ty) list
  -> Typed.Expr.t
  -> Compiled.Expr_unit.t Or_error.t

val fn : t -> Compiled.Fn_id.t -> Compiled.Fn.t
val global : t -> Compiled.Global_id.t -> Compiled.Global.t

(** All compiled functions, in id order. *)
val fns : t -> Compiled.Fn.t list

(** All compiled globals, in id order. *)
val globals : t -> Compiled.Global.t list
