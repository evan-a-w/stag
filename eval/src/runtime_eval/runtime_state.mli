open! Import

(** Owns the [Stag_runtime] heap, stacks, static data, globals, root table, GC,
    and object intern tables for one runtime evaluator instance. *)

type t

val create : ?heap_config:Heap.Config.t -> ?gc_config:Gc.Config.t -> unit -> t

(** Frees all off-heap memory (heap, stacks, static data, root table, GC mark
    stack). Using [t] afterwards raises. *)
val destroy : t -> unit

val heap : t -> Heap.t
val data_stack : t -> Data_stack.t
val shadow_stack : t -> Shadow_stack.t
val scalar_stack : t -> Scalar_stack.t
val static_data : t -> Static_data.t
val globals : t -> Globals.t
val root_table : t -> Root_table.t
val gc : t -> Gc.t
val exn_table : t -> Runtime_objects.Exn_table.t
val partial_table : t -> Runtime_objects.Partial_table.t
val check_invariants : t -> unit Or_error.t
val collect : t -> Gc.Stats.t

(** {2 Allocation helpers} *)

val alloc_heap_block : t -> Layout.t -> Memory.Addr.t
val alloc_region_block : t -> Layout.t -> Memory.Addr.t

(** [Heap] allocates on the GC heap; [Region] on the data stack; [Caller_region]
    on the data stack when [not in_inner_region], otherwise on the heap to
    preserve LIFO region safety. *)
val alloc_block
  :  t
  -> placement:Compiled.Placement.t
  -> in_inner_region:bool
  -> Layout.t
  -> Memory.Addr.t

val alloc_closure_block
  :  t
  -> placement:Compiled.Placement.t
  -> in_inner_region:bool
  -> fn_id:Compiled.Fn_id.t
  -> env_layout:Layout.t
  -> Memory.Addr.t

val alloc_partial_block
  :  t
  -> placement:Compiled.Placement.t
  -> in_inner_region:bool
  -> Runtime_objects.Partial_desc.t
  -> Memory.Addr.t

val alloc_exn_block
  :  t
  -> exn_id:int
  -> payload_layout:Layout.t
  -> Memory.Addr.t

val alloc_string : t -> string -> Memory.Addr.t
val alloc_static_string : t -> string -> Memory.Addr.t
val read_string : Memory.Addr.t -> string
val alloc_handle : t -> 'a Handle_kind.t -> 'a -> Memory.Addr.t
val get_handle : t -> 'a Handle_kind.t -> Memory.Addr.t -> 'a option
val alloc_global : t -> Layout.t -> Static_data.Global.t

(** {2 Root management & temporary stack slots} *)

val register_root : t -> Memory.Addr.t array -> Root_table.Id.t
val unregister_root : t -> Root_table.Id.t -> unit
val defer_unregister_root : t -> Root_table.Id.t -> unit

(** Runs [f] with zeroed temporary [Scalar_stack] and [Shadow_stack] storage of
    [layout], popped on return or exception. *)
val with_temp_value
  :  t
  -> Layout.t
  -> f:(scalars:Memory.Addr.t -> ptrs:Memory.Addr.t -> 'a)
  -> 'a

(** Runs [f] with [snapshot] materialised in temporary [Scalar_stack] and
    [Shadow_stack] storage. *)
val with_snapshot
  :  t
  -> Runtime_objects.Value_snapshot.t
  -> f:(scalars:Memory.Addr.t -> ptrs:Memory.Addr.t -> 'a)
  -> 'a
