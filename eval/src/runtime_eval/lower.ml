open! Import
open Or_error.Let_syntax

module Mono_key = struct
  module T = struct
    type t =
      { scope_id : int
      ; ident : Typed.Ident.t
      ; key : Mono_type.Key.t
      ; local_sides : Mono_type.Local_sides.t
      ; mode_pins : Mono_type.Mode_pins.t
      }
    [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)
end

module Binding = struct
  type t =
    | Local of
        { slot : Slot.t
        ; is_cell : bool
        ; ty : Type.ty
        }
    | Capture of
        { place : Layout.Place.t
        ; is_cell : bool
        ; ty : Type.ty
        }
    | Known_fn of
        { fn_id : Compiled.Fn_id.t
        ; arity : int
        ; placement : Compiled.Placement.t
        ; ty : Type.ty
        }
    | Self_fn of
        { fn_id : Compiled.Fn_id.t
        ; arity : int
        ; has_captures : bool
        ; placement : Compiled.Placement.t
        ; ty : Type.ty
        }
    | Generic_value of
        { body : Typed.Expr.t
        ; scheme : Type.ty
        ; owner : Typed.Allocation.Owner.t option
        ; mutable evaluating : bool
        }
    | Generic_fn of
        { ident : Typed.Ident.t
        ; params : Typed.Ident.t list
        ; body : Typed.Expr.t
        ; partials : Typed.Allocation.Id.t list
        ; made : Typed.Allocation.Id.t option
        ; scheme : Type.ty
        ; owner : Typed.Allocation.Owner.t option
        ; scope_id : int
        }
end

module Frame_ctx = struct
  type t =
    { builder : Frame_layout.Builder.t
    ; subst : Mono_type.Subst.t
    ; owner : Typed.Allocation.Owner.t option
    ; scc_id : int option
    ; scope_id : int
    }
end

module Tx = struct
  type t =
    { prev_next_fn_id : int
    ; prev_next_global_id : int
    ; prev_next_scope_id : int
    ; mutable added_fn_memos : Mono_key.t list
    ; mutable added_global_memos : Mono_key.t list
    ; mutable added_fns : Compiled.Fn_id.t list
    ; mutable added_globals : Compiled.Global_id.t list
    }
end

type t =
  { runtime_types : Runtime_types.t
  ; ident_info : Typed.Ident_info.t Typed.Ident.Table.t
  ; toplevels : Typed.Toplevel.t Typed.Ident.Table.t
  ; toplevel_scc : int Typed.Ident.Table.t
  ; sites : Typed.Allocation.t Typed.Allocation.Id.Table.t
  ; owner_sides : Typed.Allocation.Side.t list Mono_type.Owner_key.Table.t
  ; fn_memo : (Compiled.Fn_id.t * int) Mono_key.Table.t
  ; global_memo : Compiled.Global_id.t Mono_key.Table.t
  ; fns : Compiled.Fn.t Compiled.Fn_id.Table.t
  ; globals : Compiled.Global.t Compiled.Global_id.Table.t
  ; layout_cache : Layout.t Mono_type.Key.Table.t
  ; shape_cache : Shape.t Mono_type.Key.Table.t
  ; mutable next_fn_id : int
  ; mutable next_global_id : int
  ; mutable next_scope_id : int
  ; mutable next_scc_id : int
  ; mutable next_synthetic_id : int
  ; mutable active_tx : Tx.t option
  }

let create () =
  { runtime_types = Runtime_types.create Runtime_types.Resolver.empty
  ; ident_info = Typed.Ident.Table.create ()
  ; toplevels = Typed.Ident.Table.create ()
  ; toplevel_scc = Typed.Ident.Table.create ()
  ; sites = Typed.Allocation.Id.Table.create ()
  ; owner_sides = Mono_type.Owner_key.Table.create ()
  ; fn_memo = Mono_key.Table.create ()
  ; global_memo = Mono_key.Table.create ()
  ; fns = Compiled.Fn_id.Table.create ()
  ; globals = Compiled.Global_id.Table.create ()
  ; layout_cache = Mono_type.Key.Table.create ()
  ; shape_cache = Mono_type.Key.Table.create ()
  ; next_fn_id = 0
  ; next_global_id = 0
  ; next_scope_id = 1
  ; next_scc_id = 0
  ; next_synthetic_id = -1_000_000_000
  ; active_tx = None
  }
;;

let runtime_types t = t.runtime_types

let add_type_decl t ~name ~decl =
  Runtime_types.add_decl t.runtime_types ~name ~decl
;;

let add_ident_info t ~ident ~info =
  Hashtbl.set t.ident_info ~key:ident ~data:info
;;

let add_scc t (scc : Typed.Scc.t) ~allocations =
  let scc_id = t.next_scc_id in
  t.next_scc_id <- scc_id + 1;
  List.iter allocations ~f:(fun (allocation : Typed.Allocation.t) ->
    Hashtbl.set t.sites ~key:allocation.id ~data:allocation;
    Option.iter allocation.follows ~f:(fun { owner; side; _ } ->
      let key = Mono_type.Owner_key.of_owner owner in
      let sides = Hashtbl.find t.owner_sides key |> Option.value ~default:[] in
      if not (List.mem sides side ~equal:Typed.Allocation.Side.equal)
      then Hashtbl.set t.owner_sides ~key ~data:(sides @ [ side ])));
  List.iter scc.members ~f:(fun (top : Typed.Toplevel.t) ->
    Hashtbl.set t.toplevels ~key:top.ident ~data:top;
    Hashtbl.set t.toplevel_scc ~key:top.ident ~data:scc_id)
;;

let fresh_id t () =
  t.next_synthetic_id <- t.next_synthetic_id - 1;
  t.next_synthetic_id
;;

let add_fn_memo t key data =
  Hashtbl.set t.fn_memo ~key ~data;
  Option.iter t.active_tx ~f:(fun tx ->
    tx.added_fn_memos <- key :: tx.added_fn_memos)
;;

let add_global_memo t key data =
  Hashtbl.set t.global_memo ~key ~data;
  Option.iter t.active_tx ~f:(fun tx ->
    tx.added_global_memos <- key :: tx.added_global_memos)
;;

let set_fn t id fn =
  Hashtbl.set t.fns ~key:id ~data:fn;
  Option.iter t.active_tx ~f:(fun tx -> tx.added_fns <- id :: tx.added_fns)
;;

let set_global t id g =
  Hashtbl.set t.globals ~key:id ~data:g;
  Option.iter t.active_tx ~f:(fun tx ->
    tx.added_globals <- id :: tx.added_globals)
;;

let rollback t (tx : Tx.t) =
  List.iter tx.added_fn_memos ~f:(Hashtbl.remove t.fn_memo);
  List.iter tx.added_global_memos ~f:(Hashtbl.remove t.global_memo);
  List.iter tx.added_fns ~f:(Hashtbl.remove t.fns);
  List.iter tx.added_globals ~f:(Hashtbl.remove t.globals);
  t.next_fn_id <- tx.prev_next_fn_id;
  t.next_global_id <- tx.prev_next_global_id;
  t.next_scope_id <- tx.prev_next_scope_id;
  t.active_tx <- None
;;

let with_transaction t ~f =
  match t.active_tx with
  | Some _ -> f ()
  | None ->
    let tx : Tx.t =
      { prev_next_fn_id = t.next_fn_id
      ; prev_next_global_id = t.next_global_id
      ; prev_next_scope_id = t.next_scope_id
      ; added_fn_memos = []
      ; added_global_memos = []
      ; added_fns = []
      ; added_globals = []
      }
    in
    t.active_tx <- Some tx;
    (match f () with
     | Ok _ as ok ->
       t.active_tx <- None;
       ok
     | Error _ as err ->
       rollback t tx;
       err
     | exception exn ->
       rollback t tx;
       raise exn)
;;

let alloc_fn_id t =
  let id = Compiled.Fn_id.of_int t.next_fn_id in
  t.next_fn_id <- t.next_fn_id + 1;
  id
;;

let alloc_global_id t =
  let id = Compiled.Global_id.of_int t.next_global_id in
  t.next_global_id <- t.next_global_id + 1;
  id
;;

let alloc_scope_id t =
  let id = t.next_scope_id in
  t.next_scope_id <- id + 1;
  id
;;

let owner_sides t owner_key =
  Hashtbl.find t.owner_sides owner_key |> Option.value ~default:[]
;;

let resolver t = Runtime_types.resolver t.runtime_types

let find_ident_info t ident =
  match Hashtbl.find t.ident_info ident with
  | Some info -> Ok info
  | None ->
    Or_error.error_s
      [%message "Lower: missing ident_info" (ident : Typed.Ident.t)]
;;

let rec shapes_equal (a : Shape.t) (b : Shape.t) =
  match a, b with
  | Scalar a, Scalar b ->
    Shape.Scalar.equal a.scalar b.scalar
    && [%equal: (int * int) option] a.valid b.valid
  | Ptr, Ptr -> true
  | Struct a, Struct b ->
    Shape.Repr.equal a.repr b.repr && List.equal shapes_equal a.fields b.fields
  | Union a, Union b ->
    Shape.Repr.equal a.repr b.repr && List.equal shapes_equal a.cases b.cases
  | Array a, Array b -> Int.equal a.length b.length && shapes_equal a.elt b.elt
  | (Scalar _ | Ptr | Struct _ | Union _ | Array _), _ -> false
;;

let shape_of_concrete_ty t ty =
  let key = Mono_type.Subst.key_of_concrete_ty ty in
  match Hashtbl.find t.shape_cache key with
  | Some shape -> Ok shape
  | None ->
    (match Runtime_types.shape t.runtime_types ty with
     | Ok shape ->
       Hashtbl.set t.shape_cache ~key ~data:shape;
       Ok shape
     | Error error ->
       Or_error.error_s
         [%message
           "Lower: cannot compute shape"
             ~ty:(Type.to_string_hum ty : string)
             (error : Runtime_types.Error.t)])
;;

let layout_of_concrete_ty t ty =
  let key = Mono_type.Subst.key_of_concrete_ty ty in
  match Hashtbl.find t.layout_cache key with
  | Some layout -> Ok layout
  | None ->
    (match Runtime_types.layout t.runtime_types ty with
     | Ok layout ->
       Hashtbl.set t.layout_cache ~key ~data:layout;
       Ok layout
     | Error error ->
       Or_error.error_s
         [%message
           "Lower: cannot compute layout"
             ~ty:(Type.to_string_hum ty : string)
             (error : Runtime_types.Error.t)])
;;

let check_compatible_shapes t ~what ~label ~src_ty ~dst_ty =
  let%bind src_shape = shape_of_concrete_ty t src_ty in
  let%bind dst_shape = shape_of_concrete_ty t dst_ty in
  if shapes_equal src_shape dst_shape
  then Ok ()
  else
    Or_error.error_s
      [%message
        "Lower: shadowed row label layout mismatch"
          (what : string)
          (label : string)
          ~src_ty:(Type.to_string_hum src_ty : string)
          ~dst_ty:(Type.to_string_hum dst_ty : string)]
;;

let layout_of_ty t subst ty =
  let concrete = Mono_type.Subst.concretize subst ~fresh_id:(fresh_id t) ty in
  let%map layout = layout_of_concrete_ty t concrete in
  concrete, layout
;;

let placement_of_site t subst id : Compiled.Placement.t =
  match Hashtbl.find t.sites id with
  | None -> Heap
  | Some allocation ->
    let p =
      match allocation.follows with
      | Some { owner; side; if_local }
        when Mono_type.Local_sides.is_local
               (Mono_type.Subst.local_sides subst)
               ~owner:(Mono_type.Owner_key.of_owner owner)
               ~side -> if_local
      | Some _ | None -> allocation.placement
    in
    Compiled.Placement.of_allocation p
;;

let placement_of_made t subst (made : Typed.Allocation.Id.t option)
  : Compiled.Placement.t
  =
  match made with
  | None -> Heap
  | Some id -> placement_of_site t subst id
;;

let partials_placements t subst ~arity (partials : Typed.Allocation.Id.t list) =
  let expected = Int.max 0 (arity - 1) in
  if List.length partials = expected
  then Array.of_list_map partials ~f:(placement_of_site t subst)
  else Array.create ~len:expected Compiled.Placement.Heap
;;

let rec merged_params params (body : Typed.Expr.t) =
  match body.desc with
  | Fun { args; body; _ } ->
    merged_params (params @ Nonempty_list.to_list args) body
  | _ -> params, body
;;

let block_kind t (expr : Typed.Expr.t) =
  match expr.made.made with
  | Some id ->
    (match Hashtbl.find t.sites id with
     | Some { kind = Box; _ } -> Some `Box
     | Some { kind = Pointer; _ } -> Some `Pointer
     | Some { kind = Closure | Partial_application | Code | Cell; _ } | None ->
       None)
  | None -> None
;;

let is_block_app t (expr : Typed.Expr.t) =
  match expr.desc with
  | App _ -> Option.is_some (block_kind t expr)
  | _ -> false
;;

let rec collect_spine t (expr : Typed.Expr.t) args =
  match expr.desc with
  | App (f, arg) when not (is_block_app t expr) ->
    collect_spine t f (arg :: args)
  | _ -> expr, args
;;

let toplevel_owner (top : Typed.Toplevel.t) : Typed.Allocation.Owner.t =
  { toplevel = top.ident; lets = [] }
;;

let extend_owner
  (ctx : Frame_ctx.t)
  ~(ident : Typed.Ident.t)
  ~(loc : Loc.t)
  ~(owns : Typed.Allocation.Id.t option)
  : Typed.Allocation.Owner.t option
  =
  match ctx.owner, owns with
  | Some owner, Some id ->
    Some { owner with lets = owner.lets @ [ { id; name = ident.name; loc } ] }
  | Some _, None | None, _ -> None
;;

let is_captured_in_closure ident (expr : Typed.Expr.t) =
  let rec go ~in_closure (e : Typed.Expr.t) =
    let own =
      if in_closure
      then (
        let found = ref false in
        Typed.Expr.iter_own_refs e ~f:(function
          | Binder (Local id) when Typed.Ident.equal id ident -> found := true
          | Binder _ | Free _ -> ());
        !found)
      else false
    in
    own
    ||
    match e.desc with
    | Fun { body; _ } -> go ~in_closure:true body
    | Let { args = _ :: _; body; in_; _ } ->
      go ~in_closure:true body || go ~in_closure in_
    | _ -> List.exists (Typed.Expr.sub_exprs e) ~f:(go ~in_closure)
  in
  go ~in_closure:false expr
;;

(* Computes the deterministic list of enclosing frame locals captured by [body]
   (excluding [params] and [self]), expanding through any generalized local
   [let]s referenced inside [body]. *)
let collect_capture_idents
  ~(env : Binding.t Typed.Ident.Map.t)
  ~params
  ~self
  (body : Typed.Expr.t)
  =
  let captured_order = Queue.create () in
  let captured_set = Typed.Ident.Hash_set.create () in
  let visited_generics = Typed.Ident.Hash_set.create () in
  let rec scan bound (expr : Typed.Expr.t) =
    Typed.Expr.iter_own_refs expr ~f:(function
      | Free _ | Binder (Toplevel _) -> ()
      | Binder (Local ident) ->
        if not (Set.mem bound ident)
        then (
          match Map.find env ident with
          | None
          | Some (Known_fn _)
          | Some (Self_fn { has_captures = false; _ }) -> ()
          | Some (Local _ | Capture _ | Self_fn { has_captures = true; _ }) ->
            if not (Hash_set.mem captured_set ident)
            then (
              Hash_set.add captured_set ident;
              Queue.enqueue captured_order ident)
          | Some (Generic_value gv) ->
            if not (Hash_set.mem visited_generics ident)
            then (
              Hash_set.add visited_generics ident;
              scan (Typed.Ident.Set.singleton ident) gv.body)
          | Some (Generic_fn gf) ->
            if not (Hash_set.mem visited_generics ident)
            then (
              Hash_set.add visited_generics ident;
              let gf_bound = Typed.Ident.Set.of_list (gf.ident :: gf.params) in
              scan gf_bound gf.body)));
    Typed.Expr.iter_children expr ~f:(fun child sub ->
      let bound = List.fold child.binds ~init:bound ~f:Set.add in
      scan bound sub)
  in
  let init_bound = Typed.Ident.Set.of_list (Option.to_list self @ params) in
  scan init_bound body;
  Queue.to_list captured_order
;;

let build_capture_env t ~(env : Binding.t Typed.Ident.Map.t) cap_idents =
  let%bind capture_info =
    List.fold_result cap_idents ~init:[] ~f:(fun acc ident ->
      match Map.find env ident with
      | Some (Local { slot; is_cell; ty }) ->
        let%map shape =
          if is_cell then Ok Shape.Ptr else shape_of_concrete_ty t ty
        in
        (ident, Compiled.Capture_src.Local slot, is_cell, ty, shape) :: acc
      | Some (Capture { place; is_cell; ty }) ->
        let%map shape =
          if is_cell then Ok Shape.Ptr else shape_of_concrete_ty t ty
        in
        (ident, Compiled.Capture_src.Outer_capture place, is_cell, ty, shape)
        :: acc
      | Some (Self_fn { has_captures = true; ty; _ }) ->
        Ok
          ((ident, Compiled.Capture_src.Self_closure, false, ty, Shape.Ptr)
           :: acc)
      | Some
          ( Known_fn _
          | Self_fn { has_captures = false; _ }
          | Generic_value _ | Generic_fn _ )
      | None ->
        Or_error.error_s
          [%message "Lower: unexpected capture binding" (ident : Typed.Ident.t)])
    >>| List.rev
  in
  let env_shape =
    Shape.Struct
      { repr = Default
      ; fields = List.map capture_info ~f:(fun (_, _, _, _, shape) -> shape)
      }
  in
  let%map env_layout = Layout.of_shape env_shape in
  let captures =
    Array.of_list_mapi capture_info ~f:(fun i (_, src, _, _, _) ->
      let dst = Layout.place env_layout [ Field i ] in
      { Compiled.Capture.src; dst })
  in
  (* Carry forward non-frame-dependent bindings ([Known_fn], captureless
     [Self_fn] as [Known_fn], [Generic_value], [Generic_fn]) into the child
     function, and bind captured idents to their [Capture] places. *)
  let child_env =
    Map.filter_map env ~f:(function
      | (Known_fn _ | Generic_value _ | Generic_fn _) as binding -> Some binding
      | Self_fn { fn_id; arity; has_captures = false; placement; ty } ->
        Some (Binding.Known_fn { fn_id; arity; placement; ty })
      | Local _ | Capture _ | Self_fn { has_captures = true; _ } -> None)
  in
  let child_env =
    List.foldi
      capture_info
      ~init:child_env
      ~f:(fun i acc (ident, _, is_cell, ty, _) ->
        let place = Layout.place env_layout [ Field i ] in
        Map.set acc ~key:ident ~data:(Binding.Capture { place; is_cell; ty }))
  in
  env_layout, captures, child_env
;;

let poly_record_entries_concrete t concrete =
  match
    Mono_type.Subst.expand_aliases (resolver t) ~fresh_id:(fresh_id t) concrete
  with
  | TRecord { row; _ } -> Ok (Mono_type.Subst.sorted_concrete_row_entries row)
  | expanded ->
    Or_error.error_s
      [%message
        "Lower: expected polymorphic record type"
          ~ty:(Type.to_string_hum expanded : string)]
;;

let poly_record_entries t subst ty =
  let concrete = Mono_type.Subst.concretize subst ~fresh_id:(fresh_id t) ty in
  poly_record_entries_concrete t concrete
;;

let poly_variant_entries_concrete t concrete =
  match
    Mono_type.Subst.expand_aliases (resolver t) ~fresh_id:(fresh_id t) concrete
  with
  | TVariant { row; _ } -> Ok (Mono_type.Subst.sorted_concrete_row_entries row)
  | expanded ->
    Or_error.error_s
      [%message
        "Lower: expected polymorphic variant type"
          ~ty:(Type.to_string_hum expanded : string)]
;;

let find_entry_index entries label =
  List.findi entries ~f:(fun _ (name, _) -> String.equal name label)
  |> Option.map ~f:fst
;;

let find_entry_with_ty entries label =
  List.find_mapi entries ~f:(fun idx (name, ty) ->
    Option.some_if (String.equal name label) (idx, ty))
;;

let case_is_stored (union_layout : Layout.t) ~case =
  let case_place = Layout.place union_layout [ Case case ] in
  let has_rep =
    Layout.inhabited case_place.layout
    || Layout.scalar_bytes case_place.layout > 0
    || Layout.ptr_words case_place.layout > 0
  in
  has_rep
  &&
  match Layout.discriminant union_layout with
  | None | Some Uninhabited -> false
  | Some (Single { case = only }) -> Int.equal case only
  | Some (Tag _) -> true
  | Some (Niche { untagged; first; last; _ }) ->
    Int.equal case untagged || (case >= first && case <= last)
;;

let write_plan_opt union_layout ~case =
  if case_is_stored union_layout ~case
  then Some (Layout.write_plan union_layout [] ~case)
  else None
;;

let param_ty t subst param =
  let%map info = find_ident_info t param in
  Mono_type.Subst.concretize subst ~fresh_id:(fresh_id t) info.ty
;;

let same_scc t ~caller_scc (top : Typed.Toplevel.t) =
  match caller_scc, Hashtbl.find t.toplevel_scc top.ident with
  | Some a, Some b -> Int.equal a b
  | None, _ | _, None -> false
;;

let rec specialize_toplevel_fn
  t
  ~parent_subst
  ~caller_scc
  (top : Typed.Toplevel.t)
  ~instance_ty
  =
  let owner = toplevel_owner top in
  let top_scc = Hashtbl.find t.toplevel_scc top.ident in
  let inherit_pins = same_scc t ~caller_scc top in
  let child_subst =
    match instance_ty with
    | Some instance ->
      Mono_type.Subst.match_scheme
        ~resolver:(resolver t)
        ~fresh_id:(fresh_id t)
        ~is_toplevel:true
        ~inherit_pins
        ~owner:(Some owner)
        ~owner_sides:(owner_sides t)
        ~parent:parent_subst
        ~scheme:top.scheme
        ~instance
    | None ->
      Mono_type.Subst.least_instance
        ~owner
        ~owner_sides:(owner_sides t)
        ~scheme:top.scheme
  in
  let key =
    Mono_type.Subst.key_of_ty child_subst ~fresh_id:(fresh_id t) top.scheme
  in
  let local_sides = Mono_type.Subst.local_sides child_subst in
  let mode_pins = Mono_type.Subst.mode_pins child_subst in
  let mono_key =
    { Mono_key.scope_id = 0; ident = top.ident; key; local_sides; mode_pins }
  in
  match Hashtbl.find t.fn_memo mono_key with
  | Some (fn_id, arity) -> Ok (fn_id, arity)
  | None ->
    let args =
      match top.kind with
      | Function args -> Nonempty_list.to_list args
      | Value | Mut -> assert false
    in
    let params, body = merged_params args top.body in
    let arity = List.length params in
    let fn_id = alloc_fn_id t in
    add_fn_memo t mono_key (fn_id, arity);
    let%map () =
      compile_fn_body
        t
        ~fn_id
        ~name:top.ident.name
        ~params
        ~body
        ~partials:top.partials
        ~subst:child_subst
        ~owner:(Some owner)
        ~scc_id:top_scc
        ~env:Typed.Ident.Map.empty
        ~env_layout:Slot.unit_layout
    in
    fn_id, arity

and specialize_toplevel_global
  t
  ~parent_subst
  ~caller_scc
  (top : Typed.Toplevel.t)
  ~instance_ty
  =
  let owner = toplevel_owner top in
  let top_scc = Hashtbl.find t.toplevel_scc top.ident in
  let inherit_pins = same_scc t ~caller_scc top in
  let child_subst =
    match instance_ty with
    | Some instance ->
      Mono_type.Subst.match_scheme
        ~resolver:(resolver t)
        ~fresh_id:(fresh_id t)
        ~is_toplevel:true
        ~inherit_pins
        ~owner:(Some owner)
        ~owner_sides:(owner_sides t)
        ~parent:parent_subst
        ~scheme:top.scheme
        ~instance
    | None ->
      Mono_type.Subst.least_instance
        ~owner
        ~owner_sides:(owner_sides t)
        ~scheme:top.scheme
  in
  let key =
    Mono_type.Subst.key_of_ty child_subst ~fresh_id:(fresh_id t) top.scheme
  in
  let local_sides = Mono_type.Subst.local_sides child_subst in
  let mode_pins = Mono_type.Subst.mode_pins child_subst in
  let mono_key =
    { Mono_key.scope_id = 0; ident = top.ident; key; local_sides; mode_pins }
  in
  match Hashtbl.find t.global_memo mono_key with
  | Some gid -> Ok gid
  | None ->
    let gid = alloc_global_id t in
    add_global_memo t mono_key gid;
    let%map () =
      match top.kind with
      | Function _ ->
        let%map fn_id, _arity =
          specialize_toplevel_fn t ~parent_subst ~caller_scc top ~instance_ty
        in
        let concrete_ty =
          Mono_type.Subst.concretize
            child_subst
            ~fresh_id:(fresh_id t)
            top.scheme
        in
        let global : Compiled.Global.t =
          { id = gid
          ; name = top.ident.name
          ; ident = top.ident
          ; layout = Slot.ptr_layout
          ; ty = concrete_ty
          ; kind = Function fn_id
          }
        in
        set_global t gid global
      | (Value | Mut) as kind ->
        let concrete_ty =
          Mono_type.Subst.concretize
            child_subst
            ~fresh_id:(fresh_id t)
            top.scheme
        in
        let%bind layout = layout_of_concrete_ty t concrete_ty in
        let builder = Frame_layout.Builder.create () in
        let ctx : Frame_ctx.t =
          { builder
          ; subst = child_subst
          ; owner = Some owner
          ; scc_id = top_scc
          ; scope_id = alloc_scope_id t
          }
        in
        let%map init =
          lower_expr t ctx Typed.Ident.Map.empty top.body ~tail:false
        in
        let frame = Frame_layout.Builder.finalize builder in
        let mutable_ =
          match kind with
          | Mut -> true
          | Value -> false
          | Function _ -> assert false
        in
        let global : Compiled.Global.t =
          { id = gid
          ; name = top.ident.name
          ; ident = top.ident
          ; layout
          ; ty = concrete_ty
          ; kind = Value { init; frame; mutable_ }
          }
        in
        set_global t gid global
    in
    gid

and compile_fn_body
  t
  ~fn_id
  ~name
  ~params
  ~body
  ~partials
  ~subst
  ~owner
  ~scc_id
  ~env
  ~env_layout
  =
  let builder = Frame_layout.Builder.create () in
  let%bind param_bindings =
    List.fold_result params ~init:[] ~f:(fun acc param ->
      let%bind ty = param_ty t subst param in
      let%map layout = layout_of_concrete_ty t ty in
      let slot = Frame_layout.Builder.alloc builder layout in
      (param, slot, ty) :: acc)
    >>| List.rev
  in
  let env =
    List.fold param_bindings ~init:env ~f:(fun env (param, slot, ty) ->
      Map.set env ~key:param ~data:(Binding.Local { slot; is_cell = false; ty }))
  in
  let ctx : Frame_ctx.t =
    { builder; subst; owner; scc_id; scope_id = alloc_scope_id t }
  in
  let%bind compiled_body = lower_expr t ctx env body ~tail:true in
  let%map result_ty, result_layout = layout_of_ty t subst body.ty in
  let frame = Frame_layout.Builder.finalize builder in
  let arity = List.length params in
  let fn : Compiled.Fn.t =
    { id = fn_id
    ; name
    ; params = Array.of_list_map param_bindings ~f:(fun (_, slot, _) -> slot)
    ; frame
    ; env_layout
    ; partials = partials_placements t subst ~arity partials
    ; body = compiled_body
    ; result_layout
    ; result_ty
    }
  in
  set_fn t fn_id fn

and specialize_generic_fn
  t
  (ctx : Frame_ctx.t)
  ~(env : Binding.t Typed.Ident.Map.t)
  ~ident
  ~params
  ~body
  ~partials
  ~made
  ~scheme
  ~owner
  ~scope_id
  ~instance_ty
  =
  let child_subst =
    Mono_type.Subst.match_scheme
      ~resolver:(resolver t)
      ~fresh_id:(fresh_id t)
      ~is_toplevel:false
      ~inherit_pins:true
      ~owner
      ~owner_sides:(owner_sides t)
      ~parent:ctx.subst
      ~scheme
      ~instance:instance_ty
  in
  let fn_ty =
    Mono_type.Subst.concretize child_subst ~fresh_id:(fresh_id t) scheme
  in
  let key = Mono_type.Subst.key_of_concrete_ty fn_ty in
  let local_sides = Mono_type.Subst.local_sides child_subst in
  let mode_pins = Mono_type.Subst.mode_pins child_subst in
  let mono_key = { Mono_key.scope_id; ident; key; local_sides; mode_pins } in
  let cap_idents =
    collect_capture_idents ~env ~params ~self:(Some ident) body
  in
  let%bind env_layout, captures, child_env =
    build_capture_env t ~env cap_idents
  in
  let placement = placement_of_made t child_subst made in
  let arity = List.length params in
  match Hashtbl.find t.fn_memo mono_key with
  | Some (fn_id, _) -> Ok (fn_id, arity, env_layout, captures, placement)
  | None ->
    let fn_id = alloc_fn_id t in
    add_fn_memo t mono_key (fn_id, arity);
    let has_captures = not (Array.is_empty captures) in
    let child_env =
      Map.set
        child_env
        ~key:ident
        ~data:
          (Binding.Self_fn { fn_id; arity; has_captures; placement; ty = fn_ty })
    in
    let%map () =
      compile_fn_body
        t
        ~fn_id
        ~name:ident.name
        ~params
        ~body
        ~partials
        ~subst:child_subst
        ~owner
        ~scc_id:ctx.scc_id
        ~env:child_env
        ~env_layout
    in
    fn_id, arity, env_layout, captures, placement

and lower_args t (ctx : Frame_ctx.t) env args =
  let%map pairs =
    List.fold_result args ~init:[] ~f:(fun acc (arg : Typed.Expr.t) ->
      let%bind _, layout = layout_of_ty t ctx.subst arg.ty in
      let slot = Frame_layout.Builder.alloc ctx.builder layout in
      let%map compiled = lower_expr t ctx env arg ~tail:false in
      (compiled, slot) :: acc)
    >>| List.rev
  in
  Array.of_list_map pairs ~f:fst, Array.of_list_map pairs ~f:snd

and lower_single_indirect
  t
  (ctx : Frame_ctx.t)
  env
  ~fn_expr
  ~(arg_expr : Typed.Expr.t)
  ~tail
  ~loc
  =
  let%bind fn = lower_expr t ctx env fn_expr ~tail:false in
  let fn_slot = Frame_layout.Builder.alloc ctx.builder Slot.ptr_layout in
  let%bind _, arg_layout = layout_of_ty t ctx.subst arg_expr.ty in
  let arg_slot = Frame_layout.Builder.alloc ctx.builder arg_layout in
  let%map arg = lower_expr t ctx env arg_expr ~tail:false in
  Compiled.Expr.Call_indirect { fn; fn_slot; arg; arg_slot; tail; loc }

and lower_chain_indirect t (ctx : Frame_ctx.t) env ~init ~rest_args ~tail ~loc =
  let rec loop fn = function
    | [] -> Ok fn
    | (arg_expr : Typed.Expr.t) :: more ->
      let fn_slot = Frame_layout.Builder.alloc ctx.builder Slot.ptr_layout in
      let%bind _, arg_layout = layout_of_ty t ctx.subst arg_expr.ty in
      let arg_slot = Frame_layout.Builder.alloc ctx.builder arg_layout in
      let%bind arg = lower_expr t ctx env arg_expr ~tail:false in
      let is_last = List.is_empty more in
      let call =
        Compiled.Expr.Call_indirect
          { fn; fn_slot; arg; arg_slot; tail = tail && is_last; loc }
      in
      loop call more
  in
  loop init rest_args

and lower_call_known_fn
  t
  (ctx : Frame_ctx.t)
  env
  ~fn_id
  ~arity
  ~make_fn_val
  ~args
  ~tail
  ~loc
  =
  let given = List.length args in
  if given = arity
  then (
    let%map args, arg_slots = lower_args t ctx env args in
    Compiled.Expr.Call_direct { fn = fn_id; args; arg_slots; tail; loc })
  else if given > arity
  then (
    let first_args = List.take args arity in
    let rest_args = List.drop args arity in
    let%bind first_compiled, first_slots = lower_args t ctx env first_args in
    let inner =
      Compiled.Expr.Call_direct
        { fn = fn_id
        ; args = first_compiled
        ; arg_slots = first_slots
        ; tail = false
        ; loc
        }
    in
    lower_chain_indirect t ctx env ~init:inner ~rest_args ~tail ~loc)
  else (
    let%bind fn = make_fn_val () in
    lower_chain_indirect t ctx env ~init:fn ~rest_args:args ~tail ~loc)

and lower_app t (ctx : Frame_ctx.t) env (expr : Typed.Expr.t) ~tail =
  (* Handle [Ptr.make] / [Box.make] single-argument block allocations first,
     whether written via the prelude wrapper (marked by [expr.made.made]) or
     directly via the intrinsic. *)
  match expr.desc, block_kind t expr with
  | App (_, arg), Some `Pointer
  | App ({ desc = Var (Intrinsic Ptr_make); _ }, arg), _ ->
    let%bind _, content_layout = layout_of_ty t ctx.subst arg.ty in
    let value_slot = Frame_layout.Builder.alloc ctx.builder content_layout in
    let%map value = lower_expr t ctx env arg ~tail:false in
    let placement = placement_of_made t ctx.subst expr.made.made in
    Compiled.Expr.Ptr_make { value; value_slot; content_layout; placement }
  | App (_, arg), Some `Box
  | App ({ desc = Var (Intrinsic Box_make); _ }, arg), _ ->
    let%bind _, content_layout = layout_of_ty t ctx.subst arg.ty in
    let value_slot = Frame_layout.Builder.alloc ctx.builder content_layout in
    let%map value = lower_expr t ctx env arg ~tail:false in
    let placement = placement_of_made t ctx.subst expr.made.made in
    Compiled.Expr.Box_make { value; value_slot; content_layout; placement }
  | App ({ desc = Var (Intrinsic Assume_global); _ }, arg), _ ->
    lower_expr t ctx env arg ~tail
  | App (fn_expr, arg_expr), _ ->
    let head, args = collect_spine t expr [] in
    let loc = expr.loc in
    let fallback_indirect () =
      lower_single_indirect t ctx env ~fn_expr ~arg_expr ~tail ~loc
    in
    (match head.desc with
     | Var (Prim prim) ->
       let arity = Prim.arity prim in
       let given = List.length args in
       if given = arity
       then (
         let%map args, arg_slots = lower_args t ctx env args in
         match Compiled.Prim_op.of_prim prim with
         | Some op -> Compiled.Expr.Prim_op { op; args; arg_slots; loc }
         | None -> Compiled.Expr.Prim_call { prim; args; arg_slots; loc })
       else if given < arity
       then (
         let%map args, arg_slots = lower_args t ctx env args in
         let placement = placement_of_made t ctx.subst expr.made.made in
         Compiled.Expr.Prim_partial { prim; args; arg_slots; placement; loc })
       else (
         let first = List.take args arity in
         let rest = List.drop args arity in
         let%bind first_args, first_slots = lower_args t ctx env first in
         let inner =
           match Compiled.Prim_op.of_prim prim with
           | Some op ->
             Compiled.Expr.Prim_op
               { op; args = first_args; arg_slots = first_slots; loc }
           | None ->
             Compiled.Expr.Prim_call
               { prim; args = first_args; arg_slots = first_slots; loc }
         in
         lower_chain_indirect t ctx env ~init:inner ~rest_args:rest ~tail ~loc)
     | External { symbol; signature; at_compile_time } ->
       let arity = C_signature.arity signature in
       let given = List.length args in
       if given = arity
       then (
         let%map args, arg_slots = lower_args t ctx env args in
         Compiled.Expr.External_call
           { symbol; signature; at_compile_time; args; arg_slots; loc })
       else if given > arity
       then (
         let first = List.take args arity in
         let rest = List.drop args arity in
         let%bind first_args, first_slots = lower_args t ctx env first in
         let inner =
           Compiled.Expr.External_call
             { symbol
             ; signature
             ; at_compile_time
             ; args = first_args
             ; arg_slots = first_slots
             ; loc
             }
         in
         lower_chain_indirect t ctx env ~init:inner ~rest_args:rest ~tail ~loc)
       else fallback_indirect ()
     | Var (Intrinsic intrinsic) ->
       (match intrinsic, args with
        | Assume_global, arg :: rest ->
          if List.is_empty rest
          then lower_expr t ctx env arg ~tail
          else (
            let%bind init = lower_expr t ctx env arg ~tail:false in
            lower_chain_indirect t ctx env ~init ~rest_args:rest ~tail ~loc)
        | _, arg :: rest ->
          let%bind _, arg_layout = layout_of_ty t ctx.subst arg.ty in
          let arg_slot = Frame_layout.Builder.alloc ctx.builder arg_layout in
          let%bind arg = lower_expr t ctx env arg ~tail:false in
          let call =
            Compiled.Expr.Intrinsic_call { intrinsic; arg; arg_slot; loc }
          in
          if List.is_empty rest
          then Ok call
          else
            lower_chain_indirect t ctx env ~init:call ~rest_args:rest ~tail ~loc
        | _, [] -> lower_expr t ctx env head ~tail)
     | Var (Bound (Toplevel ident) | Persistent (Toplevel ident)) ->
       (match Hashtbl.find t.toplevels ident with
        | Some ({ kind = Function _; _ } as top) ->
          let%bind fn_id, arity =
            specialize_toplevel_fn
              t
              ~parent_subst:ctx.subst
              ~caller_scc:ctx.scc_id
              top
              ~instance_ty:(Some head.ty)
          in
          lower_call_known_fn
            t
            ctx
            env
            ~fn_id
            ~arity
            ~make_fn_val:(fun () ->
              let%map gid =
                specialize_toplevel_global
                  t
                  ~parent_subst:ctx.subst
                  ~caller_scc:ctx.scc_id
                  top
                  ~instance_ty:(Some head.ty)
              in
              Compiled.Expr.Global { id = gid; loc = head.loc })
            ~args
            ~tail
            ~loc
        | Some { kind = Value | Mut; _ } | None -> fallback_indirect ())
     | Var (Bound (Local ident) | Persistent (Local ident)) ->
       (match Map.find env ident with
        | Some (Known_fn { fn_id; arity; placement; _ }) ->
          lower_call_known_fn
            t
            ctx
            env
            ~fn_id
            ~arity
            ~make_fn_val:(fun () ->
              Ok
                (Compiled.Expr.Make_closure
                   { fn = fn_id
                   ; placement
                   ; env_layout = Slot.unit_layout
                   ; captures = [||]
                   }))
            ~args
            ~tail
            ~loc
        | Some (Self_fn { fn_id; arity; has_captures = false; placement; _ }) ->
          lower_call_known_fn
            t
            ctx
            env
            ~fn_id
            ~arity
            ~make_fn_val:(fun () ->
              Ok
                (Compiled.Expr.Make_closure
                   { fn = fn_id
                   ; placement
                   ; env_layout = Slot.unit_layout
                   ; captures = [||]
                   }))
            ~args
            ~tail
            ~loc
        | Some
            (Generic_fn
              { ident = gf_ident
              ; params
              ; body
              ; partials
              ; made
              ; scheme
              ; owner
              ; scope_id
              }) ->
          let%bind fn_id, arity, env_layout, captures, placement =
            specialize_generic_fn
              t
              ctx
              ~env
              ~ident:gf_ident
              ~params
              ~body
              ~partials
              ~made
              ~scheme
              ~owner
              ~scope_id
              ~instance_ty:head.ty
          in
          if Array.is_empty captures
          then
            lower_call_known_fn
              t
              ctx
              env
              ~fn_id
              ~arity
              ~make_fn_val:(fun () ->
                Ok
                  (Compiled.Expr.Make_closure
                     { fn = fn_id; placement; env_layout; captures }))
              ~args
              ~tail
              ~loc
          else (
            let fn_val =
              Compiled.Expr.Make_closure
                { fn = fn_id; placement; env_layout; captures }
            in
            lower_chain_indirect
              t
              ctx
              env
              ~init:fn_val
              ~rest_args:args
              ~tail
              ~loc)
        | Some
            ( Local _ | Capture _
            | Self_fn { has_captures = true; _ }
            | Generic_value _ )
        | None -> fallback_indirect ())
     | _ -> fallback_indirect ())
  | _ ->
    Or_error.error_s
      [%message "Lower: lower_app called on non-App" (expr : Typed.Expr.t)]

and lower_var
  t
  (ctx : Frame_ctx.t)
  (env : Binding.t Typed.Ident.Map.t)
  (expr : Typed.Expr.t)
  ~tail:_
  : Typed.Var_ref.t -> Compiled.Expr.t Or_error.t
  = function
  | Bound (Local ident) | Persistent (Local ident) ->
    (match Map.find env ident with
     | Some (Local { slot; is_cell = false; _ }) ->
       Ok (Compiled.Expr.Local slot)
     | Some (Local { slot; is_cell = true; ty }) ->
       let%map content_layout = layout_of_concrete_ty t ty in
       let ptr_slot = Frame_layout.Builder.alloc ctx.builder Slot.ptr_layout in
       Compiled.Expr.Deref { ptr = Local slot; ptr_slot; content_layout }
     | Some (Capture { place; is_cell = false; _ }) ->
       Ok (Compiled.Expr.Capture place)
     | Some (Capture { place; is_cell = true; ty }) ->
       let%map content_layout = layout_of_concrete_ty t ty in
       let ptr_slot = Frame_layout.Builder.alloc ctx.builder Slot.ptr_layout in
       Compiled.Expr.Deref { ptr = Capture place; ptr_slot; content_layout }
     | Some (Known_fn { fn_id; placement; _ })
     | Some (Self_fn { fn_id; has_captures = false; placement; _ }) ->
       Ok
         (Compiled.Expr.Make_closure
            { fn = fn_id
            ; placement
            ; env_layout = Slot.unit_layout
            ; captures = [||]
            })
     | Some (Self_fn { has_captures = true; _ }) ->
       Ok Compiled.Expr.Self_closure
     | Some (Generic_value gv) ->
       if gv.evaluating
       then
         Or_error.error_s
           [%message
             "Lower: recursive generalized value" (ident : Typed.Ident.t)]
       else (
         let child_subst =
           Mono_type.Subst.match_scheme
             ~resolver:(resolver t)
             ~fresh_id:(fresh_id t)
             ~is_toplevel:false
             ~inherit_pins:true
             ~owner:gv.owner
             ~owner_sides:(owner_sides t)
             ~parent:ctx.subst
             ~scheme:gv.scheme
             ~instance:expr.ty
         in
         let inline_scope_id = alloc_scope_id t in
         gv.evaluating <- true;
         Exn.protect
           ~finally:(fun () -> gv.evaluating <- false)
           ~f:(fun () ->
             lower_expr
               t
               { ctx with
                 subst = child_subst
               ; owner = gv.owner
               ; scope_id = inline_scope_id
               }
               env
               gv.body
               ~tail:false))
     | Some
         (Generic_fn
           { ident = gf_ident
           ; params
           ; body
           ; partials
           ; made
           ; scheme
           ; owner
           ; scope_id
           }) ->
       let%map fn_id, _arity, env_layout, captures, placement =
         specialize_generic_fn
           t
           ctx
           ~env
           ~ident:gf_ident
           ~params
           ~body
           ~partials
           ~made
           ~scheme
           ~owner
           ~scope_id
           ~instance_ty:expr.ty
       in
       Compiled.Expr.Make_closure
         { fn = fn_id; placement; env_layout; captures }
     | None ->
       Or_error.error_s
         [%message "Lower: unbound local ident" (ident : Typed.Ident.t)])
  | Bound (Toplevel ident) | Persistent (Toplevel ident) ->
    (match block_kind t expr with
     | Some `Pointer ->
       let placement = placement_of_made t ctx.subst expr.made.made in
       Ok (Compiled.Expr.Intrinsic_val { intrinsic = Ptr_make; placement })
     | Some `Box ->
       let placement = placement_of_made t ctx.subst expr.made.made in
       Ok (Compiled.Expr.Intrinsic_val { intrinsic = Box_make; placement })
     | None ->
       (match Hashtbl.find t.toplevels ident with
        | Some top ->
          let%map gid =
            specialize_toplevel_global
              t
              ~parent_subst:ctx.subst
              ~caller_scc:ctx.scc_id
              top
              ~instance_ty:(Some expr.ty)
          in
          Compiled.Expr.Global { id = gid; loc = expr.loc }
        | None ->
          Or_error.error_s
            [%message "Lower: unknown toplevel ident" (ident : Typed.Ident.t)]))
  | Free name -> Ok (Compiled.Expr.Free_var { name; loc = expr.loc })
  | Prim prim ->
    let placement = placement_of_made t ctx.subst expr.made.made in
    Ok (Compiled.Expr.Prim_val { prim; placement })
  | Intrinsic intrinsic ->
    let placement = placement_of_made t ctx.subst expr.made.made in
    Ok (Compiled.Expr.Intrinsic_val { intrinsic; placement })

and lower_pattern
  t
  (ctx : Frame_ctx.t)
  ~(bind_slots : (Slot.t * Type.ty) Typed.Ident.Map.t)
  ~(scrutinee_ty : Type.ty)
  ~(scrutinee_layout : Layout.t)
  (pattern : Typed.Pattern.t)
  : Compiled.Pattern.t Or_error.t
  =
  match pattern.desc with
  | Any -> Ok Compiled.Pattern.Any
  | Error _ -> Ok (Compiled.Pattern.Error pattern.loc)
  | Var ident ->
    let slot, var_ty = Map.find_exn bind_slots ident in
    (match
       ( Mono_type.Subst.expand_aliases
           (resolver t)
           ~fresh_id:(fresh_id t)
           scrutinee_ty
       , Mono_type.Subst.expand_aliases
           (resolver t)
           ~fresh_id:(fresh_id t)
           var_ty )
     with
     | TVariant { row = src_row; _ }, TVariant { row = dst_row; _ } ->
       let src_entries = Mono_type.Subst.sorted_concrete_row_entries src_row in
       let dst_entries = Mono_type.Subst.sorted_concrete_row_entries dst_row in
       let same_tags =
         List.equal
           (fun (a, _) (b, _) -> String.equal a b)
           src_entries
           dst_entries
       in
       if same_tags
       then (
         let%map () =
           List.fold2_exn
             src_entries
             dst_entries
             ~init:(Ok ())
             ~f:(fun acc (label, src_ty) (_, dst_ty) ->
               let%bind () = acc in
               check_compatible_shapes
                 t
                 ~what:"variant pattern binding"
                 ~label
                 ~src_ty
                 ~dst_ty)
         in
         Compiled.Pattern.Bind slot)
       else (
         let%map cases =
           List.foldi
             dst_entries
             ~init:(Ok [])
             ~f:(fun dst_case acc (label, dst_ty) ->
               let%bind acc in
               match find_entry_with_ty src_entries label with
               | None -> Ok acc
               | Some (src_case, src_ty) ->
                 let%map () =
                   check_compatible_shapes
                     t
                     ~what:"narrowed variant case"
                     ~label
                     ~src_ty
                     ~dst_ty
                 in
                 if not (case_is_stored scrutinee_layout ~case:src_case)
                 then acc
                 else (
                   let src_payload =
                     Layout.place scrutinee_layout [ Case src_case; Field 0 ]
                   in
                   let dst_payload =
                     Layout.place slot.layout [ Case dst_case; Field 0 ]
                   in
                   let dst_write_plan =
                     write_plan_opt slot.layout ~case:dst_case
                   in
                   { Compiled.Pattern.Variant_remap.src_case
                   ; src_payload
                   ; dst_write_plan
                   ; dst_payload
                   }
                   :: acc))
           >>| List.rev
         in
         if List.is_empty cases
         then Compiled.Pattern.Never
         else (
           let src_discriminant =
             Layout.discriminant scrutinee_layout |> Option.value_exn
           in
           Compiled.Pattern.Bind_variant_narrow
             { slot; src_discriminant; cases = Array.of_list cases }))
     | _ -> Ok (Compiled.Pattern.Bind slot))
  | Lit lit -> Ok (Compiled.Pattern.Lit lit)
  | Or (left, right) ->
    let%bind left =
      lower_pattern t ctx ~bind_slots ~scrutinee_ty ~scrutinee_layout left
    in
    let%map right =
      lower_pattern t ctx ~bind_slots ~scrutinee_ty ~scrutinee_layout right
    in
    Compiled.Pattern.Or (left, right)
  | Tuple pats ->
    let%map items =
      List.foldi pats ~init:(Ok []) ~f:(fun i acc pat ->
        let%bind acc in
        let place = Layout.place scrutinee_layout [ Field i ] in
        let pat_ty =
          Mono_type.Subst.concretize ctx.subst ~fresh_id:(fresh_id t) pat.ty
        in
        let%map compiled =
          lower_pattern
            t
            ctx
            ~bind_slots
            ~scrutinee_ty:pat_ty
            ~scrutinee_layout:place.layout
            pat
        in
        (place, compiled) :: acc)
      >>| List.rev
    in
    Compiled.Pattern.Tuple (Array.of_list items)
  | Deref inner ->
    let%bind content_ty, content_layout = layout_of_ty t ctx.subst inner.ty in
    let%map inner =
      lower_pattern
        t
        ctx
        ~bind_slots
        ~scrutinee_ty:content_ty
        ~scrutinee_layout:content_layout
        inner
    in
    Compiled.Pattern.Deref { content_layout; inner }
  | Construct (ctor, pats) when Typed.Constructor_ref.is_exception ctor ->
    let%bind arg_info =
      List.fold_result pats ~init:[] ~f:(fun acc (pat : Typed.Pattern.t) ->
        let pat_ty =
          Mono_type.Subst.concretize ctx.subst ~fresh_id:(fresh_id t) pat.ty
        in
        let%map shape = shape_of_concrete_ty t pat_ty in
        (pat, pat_ty, shape) :: acc)
      >>| List.rev
    in
    let payload_shape =
      Shape.Struct
        { repr = Default; fields = List.map arg_info ~f:(fun (_, _, s) -> s) }
    in
    let%bind payload_layout = Layout.of_shape payload_shape in
    let%map args =
      List.foldi arg_info ~init:(Ok []) ~f:(fun i acc (pat, pat_ty, _) ->
        let%bind acc in
        let place = Layout.place payload_layout [ Field i ] in
        let%map compiled =
          lower_pattern
            t
            ctx
            ~bind_slots
            ~scrutinee_ty:pat_ty
            ~scrutinee_layout:place.layout
            pat
        in
        (place, compiled) :: acc)
      >>| List.rev
    in
    Compiled.Pattern.Construct_exn
      { name = ctor.name; payload_layout; args = Array.of_list args }
  | Construct (ctor, pats) ->
    let case = ctor.index in
    if not (case_is_stored scrutinee_layout ~case)
    then Ok Compiled.Pattern.Never
    else (
      let discriminant =
        Layout.discriminant scrutinee_layout |> Option.value_exn
      in
      let%map args =
        List.foldi pats ~init:(Ok []) ~f:(fun j acc pat ->
          let%bind acc in
          let place = Layout.place scrutinee_layout [ Case case; Field j ] in
          let pat_ty =
            Mono_type.Subst.concretize ctx.subst ~fresh_id:(fresh_id t) pat.ty
          in
          let%map compiled =
            lower_pattern
              t
              ctx
              ~bind_slots
              ~scrutinee_ty:pat_ty
              ~scrutinee_layout:place.layout
              pat
          in
          (place, compiled) :: acc)
        >>| List.rev
      in
      Compiled.Pattern.Construct
        { discriminant; case; args = Array.of_list args })
  | Poly_variant (tag, arg_opt) ->
    let%bind entries = poly_variant_entries_concrete t scrutinee_ty in
    (match find_entry_index entries tag with
     | None -> Ok Compiled.Pattern.Never
     | Some case ->
       if not (case_is_stored scrutinee_layout ~case)
       then Ok Compiled.Pattern.Never
       else (
         let discriminant =
           Layout.discriminant scrutinee_layout |> Option.value_exn
         in
         let%map args =
           match arg_opt with
           | None -> Ok [||]
           | Some pat ->
             let place = Layout.place scrutinee_layout [ Case case; Field 0 ] in
             let pat_ty =
               Mono_type.Subst.concretize
                 ctx.subst
                 ~fresh_id:(fresh_id t)
                 pat.ty
             in
             let%map compiled =
               lower_pattern
                 t
                 ctx
                 ~bind_slots
                 ~scrutinee_ty:pat_ty
                 ~scrutinee_layout:place.layout
                 pat
             in
             [| place, compiled |]
         in
         Compiled.Pattern.Construct { discriminant; case; args }))
  | Record fields ->
    let%map fields =
      List.fold_result
        fields
        ~init:[]
        ~f:(fun acc ((field : Typed.Field_ref.t), pat) ->
          let place = Layout.place scrutinee_layout [ Field field.index ] in
          let pat_ty =
            Mono_type.Subst.concretize ctx.subst ~fresh_id:(fresh_id t) pat.ty
          in
          let%map compiled =
            lower_pattern
              t
              ctx
              ~bind_slots
              ~scrutinee_ty:pat_ty
              ~scrutinee_layout:place.layout
              pat
          in
          (place, compiled) :: acc)
      >>| List.rev
    in
    Compiled.Pattern.Record (Array.of_list fields)
  | Poly_record { fields; rest } ->
    let%bind entries = poly_record_entries_concrete t scrutinee_ty in
    let%bind field_pats =
      List.fold_result fields ~init:[] ~f:(fun acc (label, pat) ->
        match find_entry_index entries label with
        | None ->
          Or_error.error_s
            [%message
              "Lower: polymorphic record pattern field not in concrete record"
                (label : string)
                ~scrutinee_ty:(Type.to_string_hum scrutinee_ty : string)]
        | Some idx ->
          let place = Layout.place scrutinee_layout [ Field idx ] in
          let pat_ty =
            Mono_type.Subst.concretize ctx.subst ~fresh_id:(fresh_id t) pat.ty
          in
          let%map compiled =
            lower_pattern
              t
              ctx
              ~bind_slots
              ~scrutinee_ty:pat_ty
              ~scrutinee_layout:place.layout
              pat
          in
          (place, compiled) :: acc)
      >>| List.rev
    in
    (match rest with
     | Closed | Open -> Ok (Compiled.Pattern.Record (Array.of_list field_pats))
     | Bind ident ->
       let rest_slot, rest_ty = Map.find_exn bind_slots ident in
       let%bind rest_entries = poly_record_entries_concrete t rest_ty in
       let%map rest_copy =
         List.foldi
           rest_entries
           ~init:(Ok [])
           ~f:(fun dst_idx acc (label, dst_ty) ->
             let%bind acc in
             match find_entry_with_ty entries label with
             | None ->
               Or_error.error_s
                 [%message
                   "Lower: rest field missing from scrutinee" (label : string)]
             | Some (src_idx, src_ty) ->
               let%map () =
                 check_compatible_shapes
                   t
                   ~what:"poly record rest field"
                   ~label
                   ~src_ty
                   ~dst_ty
               in
               let src_place =
                 Layout.place scrutinee_layout [ Field src_idx ]
               in
               let dst_place =
                 Layout.place rest_slot.layout [ Field dst_idx ]
               in
               (src_place, dst_place) :: acc)
         >>| List.rev
       in
       Compiled.Pattern.Poly_record_rest
         { fields = Array.of_list field_pats
         ; rest_slot
         ; rest_copy = Array.of_list rest_copy
         })

and lower_match_cases
  t
  (ctx : Frame_ctx.t)
  env
  ~scrutinee_ty
  ~scrutinee_layout
  ~tail
  cases
  =
  List.fold_result cases ~init:[] ~f:(fun acc (pattern, body) ->
    (* Allocate one frame slot per distinct ident bound by [pattern] so both
       branches of an [Or] pattern share the same slots. *)
    let%bind bind_slots =
      List.fold_result
        (Typed.Pattern.idents pattern)
        ~init:Typed.Ident.Map.empty
        ~f:(fun acc ident ->
          let%bind info = find_ident_info t ident in
          let%map concrete_ty, layout = layout_of_ty t ctx.subst info.ty in
          let slot = Frame_layout.Builder.alloc ctx.builder layout in
          Map.set acc ~key:ident ~data:(slot, concrete_ty))
    in
    let%bind compiled_pat =
      lower_pattern t ctx ~bind_slots ~scrutinee_ty ~scrutinee_layout pattern
    in
    let arm_env =
      Map.fold bind_slots ~init:env ~f:(fun ~key:ident ~data:(slot, ty) env ->
        Map.set
          env
          ~key:ident
          ~data:(Binding.Local { slot; is_cell = false; ty }))
    in
    let%map compiled_body = lower_expr t ctx arm_env body ~tail in
    { Compiled.Match_case.pattern = compiled_pat; body = compiled_body } :: acc)
  >>| List.rev
  >>| Array.of_list

and collect_quote_ref_idents
  ~(env : Binding.t Typed.Ident.Map.t)
  ~bound
  (body : Typed.Expr.t)
  =
  let order = Queue.create () in
  let seen = Typed.Ident.Hash_set.create () in
  let rec scan bound (e : Typed.Expr.t) =
    Typed.Expr.iter_own_refs e ~f:(function
      | Free _ | Binder (Toplevel _) -> ()
      | Binder (Local ident) ->
        if (not (Set.mem bound ident))
           && Map.mem env ident
           && not (Hash_set.mem seen ident)
        then (
          Hash_set.add seen ident;
          Queue.enqueue order ident));
    Typed.Expr.iter_children e ~f:(fun child sub ->
      let bound = List.fold child.binds ~init:bound ~f:Set.add in
      scan bound sub)
  in
  scan bound body;
  Queue.to_list order

and capture_quote_ident
  ~(env : Binding.t Typed.Ident.Map.t)
  ~(subst : Mono_type.Subst.t)
  ~visiting
  (ident : Typed.Ident.t)
  : Compiled.Quote_capture.t option
  =
  if Set.mem visiting ident
  then None
  else (
    match Map.find env ident with
    | Some (Local { slot; is_cell; ty }) ->
      Some
        { Compiled.Quote_capture.ident
        ; kind = Value { src = Local slot; is_cell; ty }
        }
    | Some (Capture { place; is_cell; ty }) ->
      Some
        { Compiled.Quote_capture.ident
        ; kind = Value { src = Outer_capture place; is_cell; ty }
        }
    | Some (Self_fn { has_captures = true; ty; _ }) ->
      Some
        { Compiled.Quote_capture.ident
        ; kind = Value { src = Self_closure; is_cell = false; ty }
        }
    | Some (Known_fn { fn_id; ty; _ })
    | Some (Self_fn { fn_id; has_captures = false; ty; _ }) ->
      Some { Compiled.Quote_capture.ident; kind = Fn { fn_id; ty } }
    | Some (Generic_fn gf) ->
      let bound = Typed.Ident.Set.of_list (gf.ident :: gf.params) in
      let ref_idents = collect_quote_ref_idents ~env ~bound gf.body in
      let visiting = Set.add visiting ident in
      let captures =
        List.filter_map
          ref_idents
          ~f:(capture_quote_ident ~env ~subst ~visiting)
        |> Array.of_list
      in
      Some
        { Compiled.Quote_capture.ident
        ; kind =
            Generic_fn
              { params = gf.params
              ; body = gf.body
              ; partials = gf.partials
              ; made = gf.made
              ; scheme = gf.scheme
              ; captures
              ; parent_subst = subst
              }
        }
    | Some (Generic_value gv) ->
      let bound = Typed.Ident.Set.singleton ident in
      let ref_idents = collect_quote_ref_idents ~env ~bound gv.body in
      let visiting = Set.add visiting ident in
      let captures =
        List.filter_map
          ref_idents
          ~f:(capture_quote_ident ~env ~subst ~visiting)
        |> Array.of_list
      in
      Some
        { Compiled.Quote_capture.ident
        ; kind =
            Generic_value
              { body = gv.body
              ; scheme = gv.scheme
              ; captures
              ; parent_subst = subst
              }
        }
    | None -> None)

and lower_quote t (ctx : Frame_ctx.t) env (expr : Typed.Expr.t) body =
  let splices = Queue.create () in
  let captures = Queue.create () in
  let seen_captures = Typed.Ident.Hash_set.create () in
  let record_capture ident =
    if not (Hash_set.mem seen_captures ident)
    then (
      match
        capture_quote_ident
          ~env
          ~subst:ctx.subst
          ~visiting:Typed.Ident.Set.empty
          ident
      with
      | None -> ()
      | Some cap ->
        Hash_set.add seen_captures ident;
        Queue.enqueue captures cap)
  in
  let record_implicit_providers (providers : Typed.Binder.t String.Map.t) =
    Map.iter providers ~f:(function
      | Local ident -> record_capture ident
      | Toplevel _ -> ())
  in
  let rec walk ~depth (e : Typed.Expr.t) =
    match e.desc with
    | Splice { code; subs } when depth = 1 ->
      let%bind compiled_code = lower_expr t ctx env code ~tail:false in
      let slot = Frame_layout.Builder.alloc ctx.builder Slot.ptr_layout in
      Queue.enqueue splices (compiled_code, slot);
      (match subs with
       | Explicit subs ->
         List.fold_result subs ~init:() ~f:(fun () (_, sub) -> walk ~depth sub)
       | Implicit providers ->
         record_implicit_providers providers;
         Ok ())
    | Splice { code; subs = Implicit providers } ->
      record_implicit_providers providers;
      walk ~depth:(depth - 1) code
    | Var (Persistent (Local ident)) ->
      record_capture ident;
      Ok ()
    | Assign (Persistent (Local ident), rhs) ->
      record_capture ident;
      walk ~depth rhs
    | _ ->
      let children =
        Typed.Expr.fold_children e ~init:[] ~f:(fun acc child sub ->
          (child.depth, sub) :: acc)
        |> List.rev
      in
      List.fold_result children ~init:() ~f:(fun () (delta, sub) ->
        walk ~depth:(depth + delta) sub)
  in
  let%map () = walk ~depth:1 body in
  let placement = placement_of_made t ctx.subst expr.made.made in
  Compiled.Expr.Quote
    { body
    ; splices = Queue.to_array splices
    ; captures = Queue.to_array captures
    ; placement
    }

and lower_expr t (ctx : Frame_ctx.t) env (expr : Typed.Expr.t) ~tail
  : Compiled.Expr.t Or_error.t
  =
  match expr.desc with
  | Lit lit -> Ok (Compiled.Expr.Lit lit)
  | Var var_ref -> lower_var t ctx env expr ~tail var_ref
  | App _ -> lower_app t ctx env expr ~tail
  | Annotated (inner, _) -> lower_expr t ctx env inner ~tail
  | Toplevel_splice { code = _; expansion } ->
    lower_expr t ctx env expansion ~tail
  | Marked (Stack, inner) -> lower_expr t ctx env inner ~tail
  | Marked (Nontail, inner) -> lower_expr t ctx env inner ~tail:false
  | Marked (Region, inner) ->
    let first_ptr = Frame_layout.Builder.ptr_words ctx.builder in
    let%map body = lower_expr t ctx env inner ~tail:false in
    let after_ptr = Frame_layout.Builder.ptr_words ctx.builder in
    Compiled.Expr.Region { body; clear_ptrs = first_ptr, after_ptr - first_ptr }
  | Marked (Exclave, inner) ->
    let%map inner = lower_expr t ctx env inner ~tail in
    Compiled.Expr.Exclave inner
  | Seq (first, second) ->
    let%bind _, first_layout = layout_of_ty t ctx.subst first.ty in
    let first_slot = Frame_layout.Builder.alloc ctx.builder first_layout in
    let%bind first = lower_expr t ctx env first ~tail:false in
    let%map second = lower_expr t ctx env second ~tail in
    Compiled.Expr.Seq { first; first_slot; second }
  | Let { ident; args = []; generalized; body; in_; _ } ->
    let%bind info = find_ident_info t ident in
    let scheme = info.ty in
    let let_owner =
      if generalized
      then extend_owner ctx ~ident ~loc:expr.loc ~owns:expr.made.owns
      else ctx.owner
    in
    let needs_specialization =
      generalized
      && not
           (Mono_type.Subst.is_monomorphic
              ctx.subst
              ~scheme
              ~owner:let_owner
              ~owner_sides:(owner_sides t))
    in
    if needs_specialization
    then (
      let env' =
        Map.set
          env
          ~key:ident
          ~data:
            (Binding.Generic_value
               { body; scheme; owner = let_owner; evaluating = false })
      in
      lower_expr t ctx env' in_ ~tail)
    else (
      let%bind concrete_ty, layout = layout_of_ty t ctx.subst body.ty in
      let slot = Frame_layout.Builder.alloc ctx.builder layout in
      let body_ctx = { ctx with owner = let_owner } in
      let%bind body = lower_expr t body_ctx env body ~tail:false in
      let env' =
        Map.set
          env
          ~key:ident
          ~data:(Binding.Local { slot; is_cell = false; ty = concrete_ty })
      in
      let%map in_ = lower_expr t ctx env' in_ ~tail in
      Compiled.Expr.Let { slot; body; in_ })
  | Let { ident; args = _ :: _ as args; generalized; body; in_; _ } ->
    let params, body = merged_params args body in
    let%bind info = find_ident_info t ident in
    let scheme = info.ty in
    let let_owner =
      if generalized
      then extend_owner ctx ~ident ~loc:expr.loc ~owns:expr.made.owns
      else ctx.owner
    in
    let needs_specialization =
      generalized
      && not
           (Mono_type.Subst.is_monomorphic
              ctx.subst
              ~scheme
              ~owner:let_owner
              ~owner_sides:(owner_sides t))
    in
    if needs_specialization
    then (
      let env' =
        Map.set
          env
          ~key:ident
          ~data:
            (Binding.Generic_fn
               { ident
               ; params
               ; body
               ; partials = expr.made.partials
               ; made = expr.made.made
               ; scheme
               ; owner = let_owner
               ; scope_id = ctx.scope_id
               })
      in
      lower_expr t ctx env' in_ ~tail)
    else (
      let arity = List.length params in
      let cap_idents =
        collect_capture_idents ~env ~params ~self:(Some ident) body
      in
      let%bind env_layout, captures, child_env =
        build_capture_env t ~env cap_idents
      in
      let placement = placement_of_made t ctx.subst expr.made.made in
      let fn_id = alloc_fn_id t in
      let has_captures = not (Array.is_empty captures) in
      let fn_ty =
        Mono_type.Subst.concretize ctx.subst ~fresh_id:(fresh_id t) scheme
      in
      let child_env =
        Map.set
          child_env
          ~key:ident
          ~data:
            (Binding.Self_fn
               { fn_id; arity; has_captures; placement; ty = fn_ty })
      in
      let%bind () =
        compile_fn_body
          t
          ~fn_id
          ~name:ident.name
          ~params
          ~body
          ~partials:expr.made.partials
          ~subst:ctx.subst
          ~owner:let_owner
          ~scc_id:ctx.scc_id
          ~env:child_env
          ~env_layout
      in
      if not has_captures
      then (
        let env' =
          Map.set
            env
            ~key:ident
            ~data:(Binding.Known_fn { fn_id; arity; placement; ty = fn_ty })
        in
        lower_expr t ctx env' in_ ~tail)
      else (
        let slot = Frame_layout.Builder.alloc ctx.builder Slot.ptr_layout in
        let env' =
          Map.set
            env
            ~key:ident
            ~data:(Binding.Local { slot; is_cell = false; ty = fn_ty })
        in
        let%map in_ = lower_expr t ctx env' in_ ~tail in
        let closure =
          Compiled.Expr.Make_closure
            { fn = fn_id; placement; env_layout; captures }
        in
        Compiled.Expr.Let { slot; body = closure; in_ }))
  | Let_mut { ident; body; in_; _ } ->
    let%bind concrete_ty, content_layout = layout_of_ty t ctx.subst body.ty in
    let%bind body = lower_expr t ctx env body ~tail:false in
    if is_captured_in_closure ident in_
    then (
      let slot = Frame_layout.Builder.alloc ctx.builder Slot.ptr_layout in
      let value_slot = Frame_layout.Builder.alloc ctx.builder content_layout in
      let placement = placement_of_made t ctx.subst expr.made.made in
      let env' =
        Map.set
          env
          ~key:ident
          ~data:(Binding.Local { slot; is_cell = true; ty = concrete_ty })
      in
      let%map in_ = lower_expr t ctx env' in_ ~tail in
      Compiled.Expr.Let_mut_cell
        { slot; value_slot; content_layout; placement; body; in_ })
    else (
      let slot = Frame_layout.Builder.alloc ctx.builder content_layout in
      let env' =
        Map.set
          env
          ~key:ident
          ~data:(Binding.Local { slot; is_cell = false; ty = concrete_ty })
      in
      let%map in_ = lower_expr t ctx env' in_ ~tail in
      Compiled.Expr.Let_mut { slot; body; in_ })
  | Assign (var_ref, value_expr) ->
    let%bind _, value_layout = layout_of_ty t ctx.subst value_expr.ty in
    let value_slot = Frame_layout.Builder.alloc ctx.builder value_layout in
    let%bind value = lower_expr t ctx env value_expr ~tail:false in
    (match var_ref with
     | Bound (Local ident) | Persistent (Local ident) ->
       (match Map.find env ident with
        | Some (Local { slot; is_cell = false; _ }) ->
          Ok (Compiled.Expr.Assign_local { slot; value; value_slot })
        | Some (Local { slot; is_cell = true; _ }) ->
          let ptr_slot =
            Frame_layout.Builder.alloc ctx.builder Slot.ptr_layout
          in
          Ok
            (Compiled.Expr.Store
               { ptr = Local slot
               ; ptr_slot
               ; place = Slot.root_place value_layout
               ; value
               ; value_slot
               })
        | Some (Capture { place; is_cell = true; _ }) ->
          let ptr_slot =
            Frame_layout.Builder.alloc ctx.builder Slot.ptr_layout
          in
          Ok
            (Compiled.Expr.Store
               { ptr = Capture place
               ; ptr_slot
               ; place = Slot.root_place value_layout
               ; value
               ; value_slot
               })
        | _ ->
          Or_error.error_s
            [%message
              "Lower: assigning non-mutable local" (ident : Typed.Ident.t)])
     | Bound (Toplevel ident) | Persistent (Toplevel ident) ->
       (match Hashtbl.find t.toplevels ident with
        | Some top ->
          let%map global =
            specialize_toplevel_global
              t
              ~parent_subst:ctx.subst
              ~caller_scc:ctx.scc_id
              top
              ~instance_ty:None
          in
          Compiled.Expr.Assign_global
            { global; value; value_slot; loc = expr.loc }
        | None ->
          Or_error.error_s
            [%message
              "Lower: assigning unknown toplevel" (ident : Typed.Ident.t)])
     | Free _ | Prim _ | Intrinsic _ ->
       Or_error.error_s
         [%message "Lower: assigning non-variable" (var_ref : Typed.Var_ref.t)])
  | Fun { args; body; _ } ->
    let params, body = merged_params (Nonempty_list.to_list args) body in
    let cap_idents = collect_capture_idents ~env ~params ~self:None body in
    let%bind env_layout, captures, child_env =
      build_capture_env t ~env cap_idents
    in
    let placement = placement_of_made t ctx.subst expr.made.made in
    let fn_id = alloc_fn_id t in
    let%map () =
      compile_fn_body
        t
        ~fn_id
        ~name:"<fun>"
        ~params
        ~body
        ~partials:expr.made.partials
        ~subst:ctx.subst
        ~owner:ctx.owner
        ~scc_id:ctx.scc_id
        ~env:child_env
        ~env_layout
    in
    Compiled.Expr.Make_closure { fn = fn_id; placement; env_layout; captures }
  | Tuple exprs ->
    let%bind _, tuple_layout = layout_of_ty t ctx.subst expr.ty in
    let%map items =
      List.foldi exprs ~init:(Ok []) ~f:(fun i acc e ->
        let%bind acc in
        let place = Layout.place tuple_layout [ Field i ] in
        let%map compiled = lower_expr t ctx env e ~tail:false in
        (place, compiled) :: acc)
      >>| List.rev
    in
    Compiled.Expr.Tuple (Array.of_list items)
  | Record { record = _; fields } ->
    let%bind _, rec_layout = layout_of_ty t ctx.subst expr.ty in
    let%map items =
      List.fold_result
        fields
        ~init:[]
        ~f:(fun acc ((field : Typed.Field_ref.t), e) ->
          let place = Layout.place rec_layout [ Field field.index ] in
          let%map compiled = lower_expr t ctx env e ~tail:false in
          (place, compiled) :: acc)
      >>| List.rev
    in
    Compiled.Expr.Record (Array.of_list items)
  | Poly_record fields ->
    let%bind concrete_ty, rec_layout = layout_of_ty t ctx.subst expr.ty in
    let%bind entries = poly_record_entries_concrete t concrete_ty in
    let%map items =
      List.fold_result fields ~init:[] ~f:(fun acc (label, e) ->
        match find_entry_index entries label with
        | None ->
          Or_error.error_s
            [%message "Lower: poly record field not in row" (label : string)]
        | Some idx ->
          let place = Layout.place rec_layout [ Field idx ] in
          let%map compiled = lower_expr t ctx env e ~tail:false in
          (place, compiled) :: acc)
      >>| List.rev
    in
    Compiled.Expr.Record (Array.of_list items)
  | Field (rec_expr, field) ->
    let%bind _, rec_layout = layout_of_ty t ctx.subst rec_expr.ty in
    let expr_slot = Frame_layout.Builder.alloc ctx.builder rec_layout in
    let place = Layout.place rec_layout [ Field field.index ] in
    let%map compiled = lower_expr t ctx env rec_expr ~tail:false in
    Compiled.Expr.Field { expr = compiled; expr_slot; place }
  | Poly_field (rec_expr, label) ->
    let%bind concrete_ty, rec_layout = layout_of_ty t ctx.subst rec_expr.ty in
    let%bind entries = poly_record_entries_concrete t concrete_ty in
    let%bind idx =
      match find_entry_index entries label with
      | Some idx -> Ok idx
      | None ->
        Or_error.error_s
          [%message "Lower: poly field not in record row" (label : string)]
    in
    let expr_slot = Frame_layout.Builder.alloc ctx.builder rec_layout in
    let place = Layout.place rec_layout [ Field idx ] in
    let%map compiled = lower_expr t ctx env rec_expr ~tail:false in
    Compiled.Expr.Field { expr = compiled; expr_slot; place }
  | Record_update { record; expr = base_expr; fields } ->
    let%bind _, base_layout = layout_of_ty t ctx.subst base_expr.ty in
    let%bind _, result_layout = layout_of_ty t ctx.subst expr.ty in
    let base_slot = Frame_layout.Builder.alloc ctx.builder base_layout in
    let%bind base = lower_expr t ctx env base_expr ~tail:false in
    let updated_indices =
      List.map fields ~f:(fun ((field : Typed.Field_ref.t), _) -> field.index)
      |> Int.Set.of_list
    in
    let copy_fields =
      List.init (Array.length record.fields) ~f:Fn.id
      |> List.filter_map ~f:(fun i ->
        if Set.mem updated_indices i
        then None
        else (
          let src_place = Layout.place base_layout [ Field i ] in
          let dst_place = Layout.place result_layout [ Field i ] in
          Some (src_place, dst_place)))
      |> Array.of_list
    in
    let%map updates =
      List.fold_result
        fields
        ~init:[]
        ~f:(fun acc ((field : Typed.Field_ref.t), e) ->
          let place = Layout.place result_layout [ Field field.index ] in
          let%map compiled = lower_expr t ctx env e ~tail:false in
          (place, compiled) :: acc)
      >>| List.rev
    in
    Compiled.Expr.Record_update
      { base; base_slot; copy_fields; updates = Array.of_list updates }
  | Poly_update (base_expr, fields) ->
    let%bind base_ty, base_layout = layout_of_ty t ctx.subst base_expr.ty in
    let%bind result_ty, result_layout = layout_of_ty t ctx.subst expr.ty in
    let%bind base_entries = poly_record_entries_concrete t base_ty in
    let%bind result_entries = poly_record_entries_concrete t result_ty in
    let base_slot = Frame_layout.Builder.alloc ctx.builder base_layout in
    let%bind base = lower_expr t ctx env base_expr ~tail:false in
    let updated_labels = List.map fields ~f:fst |> String.Set.of_list in
    let%bind copy_fields =
      List.foldi
        result_entries
        ~init:(Ok [])
        ~f:(fun dst_idx acc (label, dst_ty) ->
          let%bind acc in
          if Set.mem updated_labels label
          then Ok acc
          else (
            match find_entry_with_ty base_entries label with
            | None ->
              Or_error.error_s
                [%message
                  "Lower: poly update unchanged field missing from base"
                    (label : string)]
            | Some (src_idx, src_ty) ->
              let%map () =
                check_compatible_shapes
                  t
                  ~what:"poly update field"
                  ~label
                  ~src_ty
                  ~dst_ty
              in
              let src_place = Layout.place base_layout [ Field src_idx ] in
              let dst_place = Layout.place result_layout [ Field dst_idx ] in
              (src_place, dst_place) :: acc))
      >>| List.rev
    in
    let%map updates =
      List.fold_result fields ~init:[] ~f:(fun acc (label, e) ->
        match find_entry_index result_entries label with
        | None ->
          Or_error.error_s
            [%message
              "Lower: poly update field not in result row" (label : string)]
        | Some idx ->
          let place = Layout.place result_layout [ Field idx ] in
          let%map compiled = lower_expr t ctx env e ~tail:false in
          (place, compiled) :: acc)
      >>| List.rev
    in
    Compiled.Expr.Record_update
      { base
      ; base_slot
      ; copy_fields = Array.of_list copy_fields
      ; updates = Array.of_list updates
      }
  | Poly_extend (fields, base_expr) ->
    let%bind result_ty, result_layout = layout_of_ty t ctx.subst expr.ty in
    let%bind result_entries = poly_record_entries_concrete t result_ty in
    let%bind base_ty, base_layout = layout_of_ty t ctx.subst base_expr.ty in
    let%bind base_entries = poly_record_entries_concrete t base_ty in
    let%bind added =
      List.fold_result fields ~init:[] ~f:(fun acc (label, e) ->
        match find_entry_index result_entries label with
        | None ->
          Or_error.error_s
            [%message "Lower: poly extend field not in result" (label : string)]
        | Some dst_idx ->
          let dst_place = Layout.place result_layout [ Field dst_idx ] in
          let slot = Frame_layout.Builder.alloc ctx.builder dst_place.layout in
          let%map compiled = lower_expr t ctx env e ~tail:false in
          (dst_place, slot, compiled) :: acc)
      >>| List.rev
    in
    let base_slot = Frame_layout.Builder.alloc ctx.builder base_layout in
    let%bind base = lower_expr t ctx env base_expr ~tail:false in
    let added_labels = List.map fields ~f:fst |> String.Set.of_list in
    let%map copy_fields =
      List.foldi
        base_entries
        ~init:(Ok [])
        ~f:(fun src_idx acc (label, src_ty) ->
          let%bind acc in
          if Set.mem added_labels label
          then Ok acc
          else (
            match find_entry_with_ty result_entries label with
            | None -> Ok acc
            | Some (dst_idx, dst_ty) ->
              let%map () =
                check_compatible_shapes
                  t
                  ~what:"poly extend field"
                  ~label
                  ~src_ty
                  ~dst_ty
              in
              let src_place = Layout.place base_layout [ Field src_idx ] in
              let dst_place = Layout.place result_layout [ Field dst_idx ] in
              (src_place, dst_place) :: acc))
      >>| List.rev
    in
    Compiled.Expr.Poly_extend
      { added = Array.of_list added
      ; base
      ; base_slot
      ; copy_fields = Array.of_list copy_fields
      }
  | Poly_restrict (base_expr, _label) ->
    let%bind result_ty, result_layout = layout_of_ty t ctx.subst expr.ty in
    let%bind result_entries = poly_record_entries_concrete t result_ty in
    let%bind base_ty, base_layout = layout_of_ty t ctx.subst base_expr.ty in
    let%bind base_entries = poly_record_entries_concrete t base_ty in
    let base_slot = Frame_layout.Builder.alloc ctx.builder base_layout in
    let%bind base = lower_expr t ctx env base_expr ~tail:false in
    let%map copy_fields =
      List.foldi
        result_entries
        ~init:(Ok [])
        ~f:(fun dst_idx acc (label, dst_ty) ->
          let%bind acc in
          match find_entry_with_ty base_entries label with
          | None ->
            Or_error.error_s
              [%message
                "Lower: poly restrict field missing from base" (label : string)]
          | Some (src_idx, src_ty) ->
            let%map () =
              check_compatible_shapes
                t
                ~what:"poly restrict field"
                ~label
                ~src_ty
                ~dst_ty
            in
            let src_place = Layout.place base_layout [ Field src_idx ] in
            let dst_place = Layout.place result_layout [ Field dst_idx ] in
            (src_place, dst_place) :: acc)
      >>| List.rev
    in
    Compiled.Expr.Poly_restrict
      { base; base_slot; copy_fields = Array.of_list copy_fields }
  | Construct (ctor, args) when Typed.Constructor_ref.is_exception ctor ->
    let%bind arg_info =
      List.fold_result args ~init:[] ~f:(fun acc (arg : Typed.Expr.t) ->
        let arg_ty =
          Mono_type.Subst.concretize ctx.subst ~fresh_id:(fresh_id t) arg.ty
        in
        let%map shape = shape_of_concrete_ty t arg_ty in
        (arg, arg_ty, shape) :: acc)
      >>| List.rev
    in
    let payload_shape =
      Shape.Struct
        { repr = Default; fields = List.map arg_info ~f:(fun (_, _, s) -> s) }
    in
    let%bind payload_layout = Layout.of_shape payload_shape in
    let%map compiled_args =
      List.foldi arg_info ~init:(Ok []) ~f:(fun i acc (arg, _, _) ->
        let%bind acc in
        let place = Layout.place payload_layout [ Field i ] in
        let slot = Frame_layout.Builder.alloc ctx.builder place.layout in
        let%map compiled = lower_expr t ctx env arg ~tail:false in
        (place, slot, compiled) :: acc)
      >>| List.rev
    in
    Compiled.Expr.Construct_exn
      { name = ctor.name
      ; payload_layout
      ; args = Array.of_list compiled_args
      ; arg_tys = Array.of_list_map arg_info ~f:(fun (_, ty, _) -> ty)
      }
  | Construct (ctor, args) ->
    let%bind _, union_layout = layout_of_ty t ctx.subst expr.ty in
    let case = ctor.index in
    let write_plan = write_plan_opt union_layout ~case in
    let%map compiled_args =
      List.foldi args ~init:(Ok []) ~f:(fun j acc arg ->
        let%bind acc in
        let place = Layout.place union_layout [ Case case; Field j ] in
        let slot = Frame_layout.Builder.alloc ctx.builder place.layout in
        let%map compiled = lower_expr t ctx env arg ~tail:false in
        (place, slot, compiled) :: acc)
      >>| List.rev
    in
    Compiled.Expr.Construct
      { case; write_plan; args = Array.of_list compiled_args }
  | Poly_variant (tag, arg_opt) ->
    let%bind concrete_ty, union_layout = layout_of_ty t ctx.subst expr.ty in
    let%bind entries = poly_variant_entries_concrete t concrete_ty in
    let%bind case =
      match find_entry_index entries tag with
      | Some idx -> Ok idx
      | None ->
        Or_error.error_s
          [%message "Lower: poly variant tag not in row" (tag : string)]
    in
    let write_plan = write_plan_opt union_layout ~case in
    let%map args =
      match arg_opt with
      | None -> Ok [||]
      | Some arg ->
        let place = Layout.place union_layout [ Case case; Field 0 ] in
        let slot = Frame_layout.Builder.alloc ctx.builder place.layout in
        let%map compiled = lower_expr t ctx env arg ~tail:false in
        [| place, slot, compiled |]
    in
    Compiled.Expr.Construct { case; write_plan; args }
  | Deref ptr_expr ->
    let%bind _, content_layout = layout_of_ty t ctx.subst expr.ty in
    let ptr_slot = Frame_layout.Builder.alloc ctx.builder Slot.ptr_layout in
    let%map ptr = lower_expr t ctx env ptr_expr ~tail:false in
    Compiled.Expr.Deref { ptr; ptr_slot; content_layout }
  | Store (target, value_expr) ->
    let rec store_target (e : Typed.Expr.t) steps =
      match e.desc with
      | Deref ptr -> Ok (ptr, e.ty, steps)
      | Field (inner, field) ->
        store_target inner (Layout.Path.Step.Field field.index :: steps)
      | Poly_field (inner, label) ->
        let%bind entries = poly_record_entries t ctx.subst inner.ty in
        (match find_entry_index entries label with
         | Some idx -> store_target inner (Layout.Path.Step.Field idx :: steps)
         | None ->
           Or_error.error_s
             [%message "Lower: store poly field not in row" (label : string)])
      | Annotated (inner, _) -> store_target inner steps
      | _ ->
        Or_error.error_s
          [%message
            "Lower: store target is not a deref/field chain" (e : Typed.Expr.t)]
    in
    let%bind ptr_expr, deref_ty, steps = store_target target [] in
    let%bind _, root_layout = layout_of_ty t ctx.subst deref_ty in
    let place = Layout.place root_layout steps in
    let ptr_slot = Frame_layout.Builder.alloc ctx.builder Slot.ptr_layout in
    let value_slot = Frame_layout.Builder.alloc ctx.builder place.layout in
    let%bind ptr = lower_expr t ctx env ptr_expr ~tail:false in
    let%map value = lower_expr t ctx env value_expr ~tail:false in
    Compiled.Expr.Store { ptr; ptr_slot; place; value; value_slot }
  | Match { scrutinee; cases; exception_cases } ->
    let%bind scrutinee_ty, scrutinee_layout =
      layout_of_ty t ctx.subst scrutinee.ty
    in
    let scrutinee_slot =
      Frame_layout.Builder.alloc ctx.builder scrutinee_layout
    in
    let%bind compiled_scrutinee = lower_expr t ctx env scrutinee ~tail:false in
    let%bind compiled_cases =
      lower_match_cases t ctx env ~scrutinee_ty ~scrutinee_layout ~tail cases
    in
    let%map compiled_exn_cases =
      lower_match_cases
        t
        ctx
        env
        ~scrutinee_ty:Type.exn
        ~scrutinee_layout:Slot.ptr_layout
        ~tail
        exception_cases
    in
    Compiled.Expr.Match
      { scrutinee = compiled_scrutinee
      ; scrutinee_slot
      ; scrutinee_ty
      ; cases = compiled_cases
      ; exception_cases = compiled_exn_cases
      ; loc = expr.loc
      }
  | Quote body -> lower_quote t ctx env expr body
  | Type ty -> Ok (Compiled.Expr.Type_val ty)
  | Expect expected -> Ok (Compiled.Expr.Expect expected)
  | External { symbol; signature; at_compile_time } ->
    Ok (Compiled.Expr.External_val { symbol; signature; at_compile_time })
  | Error _ | Splice _ -> Ok (Compiled.Expr.Error expr.loc)
;;

let specialize_toplevel t ~ident ?ty () =
  with_transaction t ~f:(fun () ->
    match Hashtbl.find t.toplevels ident with
    | None ->
      Or_error.error_s
        [%message
          "Lower.specialize_toplevel: unknown toplevel" (ident : Typed.Ident.t)]
    | Some top ->
      specialize_toplevel_global
        t
        ~parent_subst:Mono_type.Subst.empty
        ~caller_scc:None
        top
        ~instance_ty:ty)
;;

let compile_expr t ?(bindings = []) (expr : Typed.Expr.t) =
  with_transaction t ~f:(fun () ->
    let builder = Frame_layout.Builder.create () in
    let%bind bound_pairs =
      List.fold_result bindings ~init:[] ~f:(fun acc (ident, ty) ->
        let%map concrete_ty, layout = layout_of_ty t Mono_type.Subst.empty ty in
        let slot = Frame_layout.Builder.alloc builder layout in
        (ident, slot, concrete_ty) :: acc)
      >>| List.rev
    in
    let env =
      List.fold
        bound_pairs
        ~init:Typed.Ident.Map.empty
        ~f:(fun env (ident, slot, ty) ->
          Map.set
            env
            ~key:ident
            ~data:(Binding.Local { slot; is_cell = false; ty }))
    in
    let ctx : Frame_ctx.t =
      { builder
      ; subst = Mono_type.Subst.empty
      ; owner = None
      ; scc_id = None
      ; scope_id = alloc_scope_id t
      }
    in
    let%bind body = lower_expr t ctx env expr ~tail:false in
    let%map result_ty, result_layout =
      match expr.desc with
      | Error _ -> Ok (Type.unit, Slot.unit_layout)
      | _ -> layout_of_ty t Mono_type.Subst.empty expr.ty
    in
    let frame = Frame_layout.Builder.finalize builder in
    { Compiled.Expr_unit.bindings =
        Array.of_list_map bound_pairs ~f:(fun (ident, slot, _) -> ident, slot)
    ; frame
    ; body
    ; result_layout
    ; result_ty
    })
;;

let fn t id = Hashtbl.find_exn t.fns id
let global t id = Hashtbl.find_exn t.globals id

let fns t =
  Hashtbl.to_alist t.fns
  |> List.sort ~compare:(fun (a, _) (b, _) -> Compiled.Fn_id.compare a b)
  |> List.map ~f:snd
;;

let globals t =
  Hashtbl.to_alist t.globals
  |> List.sort ~compare:(fun (a, _) (b, _) -> Compiled.Global_id.compare a b)
  |> List.map ~f:snd
;;
