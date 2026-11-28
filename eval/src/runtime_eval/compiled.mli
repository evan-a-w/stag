open! Import

(** Monomorphised, layout-lowered IR produced by [Lower].

    {1 Evaluation & GC invariants}

    - {b Destination-rooting}: every expression evaluates into a caller-provided
      destination whose pointer words are already rooted (a frame [Slot.t] on
      [Shadow_stack], a global's rooted storage, or a newly allocated block).
    - {b Frame & global zeroing}: all [Shadow_stack] pointer words of a frame
      must be zeroed on frame entry, and global storage starts zeroed.
    - {b Region pointer clearing}:
      [Region { body; clear_ptrs = (first, count) }] records the contiguous
      range of [Shadow_stack] pointer slots allocated inside [body]. Immediately
      after exiting the region's [Data_stack] frame, the evaluator must zero
      those [count] pointer words starting at [first] so no stale pointers into
      freed region memory remain visible to the GC.
    - {b Match exception handlers}: value arms of a [Match] may carry
      [tail = true] even when [exception_cases] is non-empty; the evaluator must
      uninstall the exception handler once [scrutinee] succeeds and before
      entering the matched value arm. *)

module Fn_id : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  val of_int : int -> t
  val to_int : t -> int
end

module Global_id : sig
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  val of_int : int -> t
  val to_int : t -> int
end

module Placement : sig
  type t =
    | Heap
    | Region
    | Caller_region
  [@@deriving sexp_of, compare, equal]

  val of_allocation : Typed.Allocation.Placement.t -> t
end

(** Fast-path specialized saturated primitives. *)
module Prim_op : sig
  type t =
    | Int_add
    | Int_sub
    | Int_mul
    | Int_div
    | Int_rem
    | Int_neg
    | Int_equal
    | Int_compare
    | Int_lt
    | Int_le
    | Int_gt
    | Int_ge
    | Bool_not
    | Bool_equal
    | Char_equal
    | Char_code
    | Char_of_code
    | String_equal
    | String_compare
    | String_length
    | String_get
  [@@deriving sexp_of, compare, equal]

  val of_prim : Prim.t -> t option
end

module Capture_src : sig
  type t =
    | Local of Slot.t
    | Outer_capture of Layout.Place.t
    | Self_closure
  [@@deriving sexp_of]
end

module Capture : sig
  type t =
    { src : Capture_src.t
    ; dst : Layout.Place.t
    }
  [@@deriving sexp_of]
end

module Quote_capture : sig
  type t =
    { ident : Typed.Ident.t
    ; kind : kind
    }

  and kind =
    | Value of
        { src : Capture_src.t
        ; is_cell : bool
        ; ty : Mono_type.Mono_ty.t
        }
    | Fn of
        { fn_id : Fn_id.t
        ; ty : Mono_type.Mono_ty.t
        }
    | Generic_fn of
        { params : Typed.Ident.t list
        ; body : Typed.Expr.t
        ; partials : Typed.Allocation.Id.t list
        ; made : Typed.Allocation.Id.t option
        ; scheme : Mono_type.Mono_ty.t
        ; captures : t array
        ; parent_subst : Mono_type.Subst.t
        }
    | Generic_value of
        { body : Typed.Expr.t
        ; scheme : Mono_type.Mono_ty.t
        ; captures : t array
        ; parent_subst : Mono_type.Subst.t
        }
  [@@deriving sexp_of]

  module Kind : sig
    type capture := t

    type t = kind =
      | Value of
          { src : Capture_src.t
          ; is_cell : bool
          ; ty : Mono_type.Mono_ty.t
          }
      | Fn of
          { fn_id : Fn_id.t
          ; ty : Mono_type.Mono_ty.t
          }
      | Generic_fn of
          { params : Typed.Ident.t list
          ; body : Typed.Expr.t
          ; partials : Typed.Allocation.Id.t list
          ; made : Typed.Allocation.Id.t option
          ; scheme : Mono_type.Mono_ty.t
          ; captures : capture array
          ; parent_subst : Mono_type.Subst.t
          }
      | Generic_value of
          { body : Typed.Expr.t
          ; scheme : Mono_type.Mono_ty.t
          ; captures : capture array
          ; parent_subst : Mono_type.Subst.t
          }
    [@@deriving sexp_of]
  end
end

module Pattern : sig
  module Variant_remap : sig
    type t =
      { src_case : int
      ; src_payload : Layout.Place.t
      ; dst_write_plan : Layout.Write_plan.t option
      ; dst_payload : Layout.Place.t
      }
    [@@deriving sexp_of]
  end

  type t =
    | Any
    | Never
    (** Unmatchable in this monomorphic instantiation (eg. an absent polymorphic
        variant tag or an unstored uninhabited case). *)
    | Bind of Slot.t
    | Bind_variant_narrow of
        { slot : Slot.t
        ; src_discriminant : Layout.Discriminant.t
        ; cases : Variant_remap.t array
        }
    (** Binds a catch-all variable whose polymorphic variant type was narrowed
        by earlier arms, remapping active cases from the scrutinee's union
        layout into [slot]'s narrower union layout. *)
    | Lit of Syntax.Lit.t
    | Tuple of (Layout.Place.t * t) array
    | Deref of
        { content_layout : Layout.t
        ; inner : t
        }
    | Construct of
        { discriminant : Layout.Discriminant.t
        ; case : int
        ; args : (Layout.Place.t * t) array
        }
    | Construct_exn of
        { name : string
        ; payload_layout : Layout.t
        ; args : (Layout.Place.t * t) array
        }
    | Record of (Layout.Place.t * t) array
    | Poly_record_rest of
        { fields : (Layout.Place.t * t) array
        ; rest_slot : Slot.t
        ; rest_copy : (Layout.Place.t * Layout.Place.t) array
        (** [(src_in_scrutinee, dst_in_rest)] *)
        }
    | Or of t * t
    | Error of Loc.t
  [@@deriving sexp_of]
end

module Expr : sig
  type t =
    | Lit of Syntax.Lit.t
    | Local of Slot.t
    | Capture of Layout.Place.t
    | Self_closure
    | Global of
        { id : Global_id.t
        ; loc : Loc.t
        }
    | Free_var of
        { name : string
        ; loc : Loc.t
        }
    | Make_closure of
        { fn : Fn_id.t
        ; placement : Placement.t
        ; env_layout : Layout.t
        ; captures : Capture.t array
        }
    | Call_direct of
        { fn : Fn_id.t
        ; args : t array
        ; arg_slots : Slot.t array
        ; tail : bool
        ; loc : Loc.t
        }
    | Call_indirect of
        { fn : t
        ; fn_slot : Slot.t
        ; arg : t
        ; arg_slot : Slot.t
        ; tail : bool
        ; loc : Loc.t
        }
    | Prim_op of
        { op : Prim_op.t
        ; args : t array
        ; arg_slots : Slot.t array
        ; loc : Loc.t
        }
    | Prim_call of
        { prim : Prim.t
        ; args : t array
        ; arg_slots : Slot.t array
        ; loc : Loc.t
        }
    | Prim_partial of
        { prim : Prim.t
        ; args : t array
        ; arg_slots : Slot.t array
        ; placement : Placement.t
        ; loc : Loc.t
        }
    | Prim_val of
        { prim : Prim.t
        ; placement : Placement.t
        }
    | Ptr_make of
        { value : t
        ; value_slot : Slot.t
        ; content_layout : Layout.t
        ; placement : Placement.t
        }
    | Box_make of
        { value : t
        ; value_slot : Slot.t
        ; content_layout : Layout.t
        ; placement : Placement.t
        }
    | Intrinsic_call of
        { intrinsic : Intrinsic.t
        ; arg : t
        ; arg_slot : Slot.t
        ; loc : Loc.t
        }
    | Intrinsic_val of
        { intrinsic : Intrinsic.t
        ; placement : Placement.t
        }
    | External_call of
        { symbol : string
        ; signature : C_signature.t
        ; at_compile_time : Compile_time_c_calls.Policy.t
        ; args : t array
        ; arg_slots : Slot.t array
        ; loc : Loc.t
        }
    | External_val of
        { symbol : string
        ; signature : C_signature.t
        ; at_compile_time : Compile_time_c_calls.Policy.t
        }
    | Tuple of (Layout.Place.t * t) array
    | Record of (Layout.Place.t * t) array
    | Record_update of
        { base : t
        ; base_slot : Slot.t
        ; copy_fields : (Layout.Place.t * Layout.Place.t) array
        (** [(src_in_base, dst_in_result)] for unchanged fields. *)
        ; updates : (Layout.Place.t * t) array
        (** [(dst_in_result, expr)] for updated fields. *)
        }
    | Poly_extend of
        { added : (Layout.Place.t * Slot.t * t) array
        ; base : t
        ; base_slot : Slot.t
        ; copy_fields : (Layout.Place.t * Layout.Place.t) array
        (** [(src_in_base, dst_in_result)] *)
        }
    | Poly_restrict of
        { base : t
        ; base_slot : Slot.t
        ; copy_fields : (Layout.Place.t * Layout.Place.t) array
        (** [(src_in_base, dst_in_result)] *)
        }
    | Field of
        { expr : t
        ; expr_slot : Slot.t
        ; place : Layout.Place.t
        }
    | Construct of
        { case : int
        ; write_plan : Layout.Write_plan.t option
        (** [None] if the case is uninhabited and never stored. *)
        ; args : (Layout.Place.t * Slot.t * t) array
        }
    | Construct_exn of
        { name : string
        ; payload_layout : Layout.t
        ; args : (Layout.Place.t * Slot.t * t) array
        ; arg_tys : Mono_type.Mono_ty.t array
        }
    | Deref of
        { ptr : t
        ; ptr_slot : Slot.t
        ; content_layout : Layout.t
        }
    | Store of
        { ptr : t
        ; ptr_slot : Slot.t
        ; place : Layout.Place.t
        ; value : t
        ; value_slot : Slot.t
        }
    | Let of
        { slot : Slot.t
        ; body : t
        ; in_ : t
        }
    | Let_mut of
        { slot : Slot.t
        ; body : t
        ; in_ : t
        }
    | Let_mut_cell of
        { slot : Slot.t
        ; value_slot : Slot.t
        ; content_layout : Layout.t
        ; placement : Placement.t
        ; body : t
        ; in_ : t
        }
    | Assign_local of
        { slot : Slot.t
        ; value : t
        ; value_slot : Slot.t
        }
    | Assign_global of
        { global : Global_id.t
        ; value : t
        ; value_slot : Slot.t
        ; loc : Loc.t
        }
    | Seq of
        { first : t
        ; first_slot : Slot.t
        ; second : t
        }
    | Region of
        { body : t
        ; clear_ptrs : int * int
        (** [(first_ptr, count)] of [Shadow_stack] pointer slots allocated
            inside [body] to zero immediately after exiting the region. *)
        }
    | Exclave of t
    | Match of
        { scrutinee : t
        ; scrutinee_slot : Slot.t
        ; scrutinee_ty : Mono_type.Mono_ty.t
        ; cases : match_case array
        ; exception_cases : match_case array
        ; loc : Loc.t
        }
    | Quote of
        { body : Typed.Expr.t
        ; splices : (t * Slot.t) array
        ; captures : Quote_capture.t array
        ; placement : Placement.t
        }
    | Type_val of Syntax.Type_expr.t
    | Expect of string Located.t
    | Error of Loc.t

  and match_case =
    { pattern : Pattern.t
    ; body : t
    }
  [@@deriving sexp_of]
end

module Match_case : sig
  type t = Expr.match_case =
    { pattern : Pattern.t
    ; body : Expr.t
    }
  [@@deriving sexp_of]
end

module Fn : sig
  type t =
    { id : Fn_id.t
    ; name : string
    ; params : Slot.t array
    ; frame : Frame_layout.t
    ; env_layout : Layout.t
    ; partials : Placement.t array
    ; body : Expr.t
    ; result_layout : Layout.t
    ; result_ty : Mono_type.Mono_ty.t
    }
  [@@deriving sexp_of]
end

module Global : sig
  module Kind : sig
    type t =
      | Value of
          { init : Expr.t
          ; frame : Frame_layout.t
          ; mutable_ : bool
          }
      | Function of Fn_id.t
    [@@deriving sexp_of]
  end

  type t =
    { id : Global_id.t
    ; name : string
    ; ident : Typed.Ident.t
    ; layout : Layout.t
    ; ty : Mono_type.Mono_ty.t
    ; kind : Kind.t
    }
  [@@deriving sexp_of]
end

module Expr_unit : sig
  type t =
    { bindings : (Typed.Ident.t * Slot.t) array
    ; frame : Frame_layout.t
    ; body : Expr.t
    ; result_layout : Layout.t
    ; result_ty : Mono_type.Mono_ty.t
    }
  [@@deriving sexp_of]
end
