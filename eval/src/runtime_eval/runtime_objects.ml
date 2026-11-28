open! Import
module Addr = Memory.Addr

let concat_addr_arrays (arrays : Addr.t array list) : Addr.t array =
  let total = List.fold arrays ~init:0 ~f:(fun acc a -> acc + Array.length a) in
  if total = 0
  then [||]
  else (
    let out = Array.create ~len:total (Addr.null ()) in
    let pos = ref 0 in
    List.iter arrays ~f:(fun a ->
      for i = 0 to Array.length a - 1 do
        out.(!pos) <- a.(i);
        incr pos
      done);
    out)
;;

module Value_snapshot = struct
  type t =
    { ty : Mono_type.Mono_ty.t
    ; layout : Layout.t
    ; scalars : bytes
    ; ptrs : Addr.t array
    }

  let sexp_of_t { ty; layout; scalars; ptrs } =
    [%sexp
      { ty : Mono_type.Mono_ty.t
      ; layout : Layout.t
      ; scalar_bytes = (Bytes.length scalars : int)
      ; ptr_words = (Array.length ptrs : int)
      }]
  ;;

  let of_memory ~ty ~layout ~src_s ~src_p =
    let scalars =
      Layout_mem.read_bytes src_s ~len:(Layout.scalar_bytes layout)
    in
    let ptrs = Layout_mem.read_ptrs src_p ~count:(Layout.ptr_words layout) in
    { ty; layout; scalars; ptrs }
  ;;

  let write_to_memory t ~dst_s ~dst_p =
    if Bytes.length t.scalars > 0
    then Layout_mem.write_bytes t.scalars ~dst:dst_s;
    if Array.length t.ptrs > 0 then Layout_mem.write_ptrs t.ptrs ~dst:dst_p
  ;;
end

module Partial_desc = struct
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

  let[@inline] [@zero_alloc] tag_word t = -(t.id + 1)
  let[@inline] [@zero_alloc] payload_scalars block = Addr.add block 8

  let[@inline] [@zero_alloc] payload_ptrs t block =
    Addr.add block (8 + Frame_layout.scalar_bytes t.payload_layout)
  ;;
end

module Partial_key = struct
  module T = struct
    type t =
      { fn_id : Compiled.Fn_id.t
      ; applied_count : int
      }
    [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)
end

module Partial_table = struct
  type t =
    { by_key : Partial_desc.t Partial_key.Table.t
    ; by_id : Partial_desc.t Int.Table.t
    ; mutable next_id : int
    }

  let create () =
    { by_key = Partial_key.Table.create ()
    ; by_id = Int.Table.create ()
    ; next_id = 0
    }
  ;;

  let intern t ~fn_id ~applied_count ~env_layout ~arg_layouts =
    let key = { Partial_key.fn_id; applied_count } in
    match Hashtbl.find t.by_key key with
    | Some desc -> desc
    | None ->
      let id = t.next_id in
      t.next_id <- id + 1;
      let builder = Frame_layout.Builder.create () in
      let env_slot = Frame_layout.Builder.alloc builder env_layout in
      let arg_slots =
        Array.map arg_layouts ~f:(Frame_layout.Builder.alloc builder)
      in
      let payload_layout = Frame_layout.Builder.finish builder in
      let desc =
        { Partial_desc.id
        ; fn_id
        ; applied_count
        ; env_layout
        ; arg_layouts
        ; env_slot
        ; arg_slots
        ; payload_layout
        }
      in
      Hashtbl.add_exn t.by_key ~key ~data:desc;
      Hashtbl.add_exn t.by_id ~key:id ~data:desc;
      desc
  ;;

  let find t id = Hashtbl.find_exn t.by_id id
end

module Exn_desc = struct
  type t =
    { id : int
    ; name : string
    ; arg_tys : Mono_type.Mono_ty.t array
    ; payload_layout : Layout.t
    }
  [@@deriving sexp_of]
end

module Exn_key = struct
  module T = struct
    type t =
      { name : string
      ; arg_keys : Mono_type.Key.t list
      }
    [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)
end

module Exn_table = struct
  type t =
    { by_key : Exn_desc.t Exn_key.Table.t
    ; by_id : Exn_desc.t Int.Table.t
    ; mutable next_id : int
    }

  let create () =
    { by_key = Exn_key.Table.create ()
    ; by_id = Int.Table.create ()
    ; next_id = 0
    }
  ;;

  let intern t ~name ~arg_tys ~payload_layout =
    let arg_keys =
      Array.to_list arg_tys |> List.map ~f:Mono_type.Subst.key_of_concrete_ty
    in
    let key = { Exn_key.name; arg_keys } in
    match Hashtbl.find t.by_key key with
    | Some desc -> desc
    | None ->
      let id = t.next_id in
      t.next_id <- id + 1;
      let desc = { Exn_desc.id; name; arg_tys; payload_layout } in
      Hashtbl.add_exn t.by_key ~key ~data:desc;
      Hashtbl.add_exn t.by_id ~key:id ~data:desc;
      desc
  ;;

  let find t id = Hashtbl.find_exn t.by_id id
  let find_opt t id = Hashtbl.find t.by_id id
end

let[@inline] [@zero_alloc] closure_tag block =
  Int64_u.to_int (Memory.load_64 block)
;;

let[@inline] [@zero_alloc] closure_env_scalars block = Addr.add block 8

let[@inline] [@zero_alloc] closure_env_ptrs env_layout block =
  Addr.add block (8 + Layout_mem.round_up_word (Layout.scalar_bytes env_layout))
;;

let[@inline] [@zero_alloc] exn_id block = Int64_u.to_int (Memory.load_64 block)
let[@inline] [@zero_alloc] exn_payload_scalars block = Addr.add block 8

let[@inline] [@zero_alloc] exn_payload_ptrs payload_layout block =
  Addr.add
    block
    (8 + Layout_mem.round_up_word (Layout.scalar_bytes payload_layout))
;;

module Callable = struct
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

  let refs = function
    | Intrinsic _ -> [||]
    | Prim { applied; _ } | External { applied; _ } ->
      concat_addr_arrays
        (List.map applied ~f:(fun (s : Value_snapshot.t) -> s.ptrs))
  ;;

  let handle_kind = Handle_kind.create ~name:"callable" sexp_of_t ~refs
end

module Persistent_entry = struct
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

  let rec collect_arrays acc = function
    | Value snap -> if Array.length snap.ptrs > 0 then snap.ptrs :: acc else acc
    | Poly_fn { captures; _ } | Poly_value { captures; _ } ->
      List.fold captures ~init:acc ~f:(fun acc (_, entry) ->
        collect_arrays acc entry)
  ;;
end

module Code = struct
  type t =
    { expr : Typed.Expr.t
    ; persistent : Persistent_entry.t Typed.Ident.Map.t
    }
  [@@deriving sexp_of]

  let refs { expr = _; persistent } =
    let arrays =
      Map.fold persistent ~init:[] ~f:(fun ~key:_ ~data:entry acc ->
        Persistent_entry.collect_arrays acc entry)
    in
    concat_addr_arrays arrays
  ;;

  let handle_kind = Handle_kind.create ~name:"code" sexp_of_t ~refs
end

module Type_val = struct
  type t = Syntax.Type_expr.t [@@deriving sexp_of]

  let handle_kind =
    Handle_kind.create ~name:"type" sexp_of_t ~refs:(fun _ -> [||])
  ;;
end

module Decl_ref_val = struct
  type t = Decl_ref.t [@@deriving sexp_of]

  let handle_kind =
    Handle_kind.create ~name:"decl_ref" sexp_of_t ~refs:(fun _ -> [||])
  ;;
end
