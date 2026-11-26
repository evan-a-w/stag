open! Import

module External = struct
  type t =
    { ty : Syntax.Type_expr.t
    ; symbol : string Located.t
    ; at_compile_time : Compile_time_c_calls.Policy.t
    }
  [@@deriving sexp_of]
end

module Kind = struct
  type t =
    | Value
    | Mut
    | Function of Syntax.Binding.t Nonempty_list.t
    | External of External.t
  [@@deriving sexp_of]
end

module State = struct
  type t =
    | Unchecked
    | In_progress of
        { index : int
        ; ty : Type.ty
        ; args : Typed.Ident.t list
        ; mode : Binding.t
        ; init_mode : Mode.Var.t
        ; held : Binding.t option
        ; mutable body : Typed.Expr.t option
        ; mutable partials : Typed.Allocation.Id.t list
        ; mutable recursive : bool
        ; sites : (Allocation_sites.t[@sexp.opaque])
        }
    | Done of
        { entry : Env.Entry.t
        ; typed : Typed.Toplevel.t
        ; component : (Component.t[@sexp.opaque])
        ; runtime_only : string option
        ; awaiting : Awaiting.t option
        }
  [@@deriving sexp_of]
end

type t =
  { ident : Typed.Ident.t
  ; module_ : Module_scope.t
  ; annot : Syntax.Type_expr.t option
  ; mode : Syntax.Mode_expr.t option
  ; expr : Syntax.Expr.t
  ; kind : Kind.t
  ; generated : Generated.t option
  ; mutable state : State.t
  ; mutable pending_uses : Quoted_use.t list
  ; mutable awaited_by : Awaiting.t list
  }
[@@deriving sexp_of]

let create ~ident ~module_ ~kind ~annot ~mode ~expr ~generated =
  { ident
  ; module_
  ; kind
  ; annot
  ; mode
  ; expr
  ; generated
  ; state = Unchecked
  ; pending_uses = []
  ; awaited_by = []
  }
;;

let set_state t state = t.state <- state
let add_pending_use t use = t.pending_uses <- use :: t.pending_uses

let take_pending_uses t =
  let uses = t.pending_uses in
  t.pending_uses <- [];
  uses
;;

let await t awaiting =
  if not (List.mem t.awaited_by awaiting ~equal:phys_equal)
  then t.awaited_by <- awaiting :: t.awaited_by
;;

let take_awaited_by t =
  let awaiting = t.awaited_by in
  t.awaited_by <- [];
  awaiting
;;

let is_function t =
  match t.kind with
  | Function _ -> true
  | Value | Mut | External _ -> false
;;

let is_mut t =
  match t.kind with
  | Mut -> true
  | Value | Function _ | External _ -> false
;;

let args t =
  match t.kind with
  | Function args -> Nonempty_list.to_list args
  | Value | Mut | External _ -> []
;;
