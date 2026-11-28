open! Import

(** Runtime object layouts, intern tables (closures, partial applications,
    exceptions), immutable flat-value snapshots, and [Handle_kind.t]
    registrations for [Shape.Ptr] objects. *)

module Value_snapshot : sig
  type t =
    { ty : Mono_type.Mono_ty.t
    ; layout : Layout.t
    ; scalars : bytes
    ; ptrs : Memory.Addr.t array
    }
  [@@deriving sexp_of]

  val of_memory
    :  ty:Type.ty
    -> layout:Layout.t
    -> src_s:Memory.Addr.t
    -> src_p:Memory.Addr.t
    -> t

  val write_to_memory : t -> dst_s:Memory.Addr.t -> dst_p:Memory.Addr.t -> unit
end

(** Descriptor for a partial-application block stored in [Heap] or [Data_stack].
    Word 0 holds [-(id + 1)] (negative, distinguishing it from an unapplied
    closure whose word 0 is [Fn_id.to_int fn_id >= 0]). *)
module Partial_desc : sig
  type t =
    { id : int
    ; fn_id : Compiled.Fn_id.t
    ; applied_count : int
    ; env_layout : Layout.t
    ; arg_layouts : Layout.t array
    ; env_slot : Slot.t
    ; arg_slots : Slot.t array
    ; payload_layout : Frame_layout.t
    }
  [@@deriving sexp_of]

  val tag_word : t -> int [@@zero_alloc]
  val payload_scalars : Memory.Addr.t -> Memory.Addr.t [@@zero_alloc]
  val payload_ptrs : t -> Memory.Addr.t -> Memory.Addr.t [@@zero_alloc]
end

module Partial_table : sig
  type t

  val create : unit -> t

  val intern
    :  t
    -> fn_id:Compiled.Fn_id.t
    -> applied_count:int
    -> env_layout:Layout.t
    -> arg_layouts:Layout.t array
    -> Partial_desc.t

  val find : t -> int -> Partial_desc.t
end

(** Descriptor for a fast exception block in [Heap]. Word 0 holds [id], followed
    by [payload_layout]'s scalars (at offset 8) and pointers. *)
module Exn_desc : sig
  type t =
    { id : int
    ; name : string
    ; arg_tys : Mono_type.Mono_ty.t array
    ; payload_layout : Layout.t
    }
  [@@deriving sexp_of]
end

module Exn_table : sig
  type t

  val create : unit -> t

  val intern
    :  t
    -> name:string
    -> arg_tys:Type.ty array
    -> payload_layout:Layout.t
    -> Exn_desc.t

  val find : t -> int -> Exn_desc.t
  val find_opt : t -> int -> Exn_desc.t option
end

(** Reading tags and payload bases of closure and exception blocks. *)

val closure_tag : Memory.Addr.t -> int [@@zero_alloc]
val closure_env_scalars : Memory.Addr.t -> Memory.Addr.t [@@zero_alloc]
val closure_env_ptrs : Layout.t -> Memory.Addr.t -> Memory.Addr.t [@@zero_alloc]
val exn_id : Memory.Addr.t -> int [@@zero_alloc]
val exn_payload_scalars : Memory.Addr.t -> Memory.Addr.t [@@zero_alloc]
val exn_payload_ptrs : Layout.t -> Memory.Addr.t -> Memory.Addr.t [@@zero_alloc]

module Callable : sig
  type t =
    | Prim of
        { prim : Prim.t
        ; applied : Value_snapshot.t list
        }
    | Intrinsic of Intrinsic.t
    | External of
        { symbol : string
        ; signature : C_signature.t
        ; at_compile_time : Compile_time_c_calls.Policy.t
        ; applied : Value_snapshot.t list
        }
  [@@deriving sexp_of]

  val handle_kind : t Handle_kind.t
end

module Persistent_entry : sig
  type t =
    | Value of Value_snapshot.t
    | Poly_fn of
        { args : Typed.Ident.t list
        ; body : Typed.Expr.t
        ; scheme : Mono_type.Mono_ty.t
        ; captures : (Typed.Ident.t * t) list
        ; parent_subst : Mono_type.Subst.t
        }
    | Poly_value of
        { body : Typed.Expr.t
        ; scheme : Mono_type.Mono_ty.t
        ; captures : (Typed.Ident.t * t) list
        ; parent_subst : Mono_type.Subst.t
        }
  [@@deriving sexp_of]
end

module Code : sig
  type t =
    { expr : Typed.Expr.t
    ; persistent : Persistent_entry.t Typed.Ident.Map.t
    }
  [@@deriving sexp_of]

  val handle_kind : t Handle_kind.t
end

module Type_val : sig
  type t = Syntax.Type_expr.t [@@deriving sexp_of]

  val handle_kind : t Handle_kind.t
end

module Decl_ref_val : sig
  type t = Decl_ref.t [@@deriving sexp_of]

  val handle_kind : t Handle_kind.t
end
