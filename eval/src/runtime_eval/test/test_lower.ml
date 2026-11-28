open! Import

(* Wrapper interpreter that delegates compile-time evaluation to [Treewalk]
   while feeding [add_type_decl], [add_ident_info], and [add_scc] into a
   [Lower.t]. *)
module Lowering_interpreter = struct
  module Value = Treewalk.Value

  type t =
    { inner : Treewalk.t
    ; lower : Lower.t
    }

  let type_id : t Type_equal.Id.t =
    Type_equal.Id.create ~name:"Lowering_interpreter" sexp_of_opaque
  ;;

  let create ~stamps () =
    { inner = Treewalk.create ~stamps (); lower = Lower.create () }
  ;;

  let set_output t output = Treewalk.set_output t.inner output
  let set_argv t argv = Treewalk.set_argv t.inner argv
  let with_output t output ~f = Treewalk.with_output t.inner output ~f
  let set_limits t limits = Treewalk.set_limits t.inner limits

  let add_ident_info t ~ident ~info =
    Lower.add_ident_info t.lower ~ident ~info;
    Treewalk.add_ident_info t.inner ~ident ~info
  ;;

  let add_type_decl t ~name ~decl =
    Lower.add_type_decl t.lower ~name ~decl;
    Treewalk.add_type_decl t.inner ~name ~decl
  ;;

  let add_scc t scc ~allocations =
    Lower.add_scc t.lower scc ~allocations;
    Treewalk.add_scc t.inner scc ~allocations
  ;;

  let splice t ~code ~level ~entry ~reflect =
    Treewalk.splice t.inner ~code ~level ~entry ~reflect
  ;;

  let run t expr ~level ~reflect = Treewalk.run t.inner expr ~level ~reflect
  let global_info t ~ident = Treewalk.global_info t.inner ~ident

  let run_expect_test t test ~on_expect =
    Treewalk.run_expect_test t.inner test ~on_expect
  ;;

  let eval_expr ?bindings t expr = Treewalk.eval_expr ?bindings t.inner expr
  let eval_toplevel t ~ident = Treewalk.eval_toplevel t.inner ~ident
  let find t ~ident = Treewalk.find t.inner ~ident
end

let lowering_interpreter = (module Lowering_interpreter : Interpreter_intf.S)

let check_and_lower source =
  let program =
    match Parser.program ~file:"t.stag" source with
    | Ok p -> p
    | Error error -> raise_s [%message "Parse failed" (error : Parser.Error.t)]
  in
  let driver =
    match Driver.check ~interpreter:lowering_interpreter program with
    | Ok d -> d
    | Error failed ->
      raise_s
        [%message "Typecheck failed" (failed.errors : Typecheck_error.t list)]
  in
  let state =
    Interpreter.state (Driver.interpreter driver) Lowering_interpreter.type_id
    |> Option.value_exn
  in
  let checked = Driver.checked driver in
  let find_top name =
    List.find_map_exn checked.program.sccs ~f:(fun scc ->
      List.find_map scc.members ~f:(fun top ->
        Option.some_if (String.equal top.ident.name name) top.ident))
  in
  state.lower, find_top, checked
;;

(* Compact, deterministic sexp rendering for [Compiled] structures so expect
   tests stay readable while exposing every layout offset, discriminant, write
   plan, placement, capture, and tail flag. *)
module Pretty = struct
  let layout_size (l : Layout.t) =
    [%sexp { s : int = Layout.scalar_bytes l; p : int = Layout.ptr_words l }]
  ;;

  let slot ({ Slot.scalar_offset; ptr_index; layout } : Slot.t) =
    Sexp.List
      [ Atom "slot"
      ; Atom (sprintf "s%d+%d" scalar_offset (Layout.scalar_bytes layout))
      ; Atom (sprintf "p%d+%d" ptr_index (Layout.ptr_words layout))
      ]
  ;;

  let place ({ Layout.Place.scalar_offset; ptr_index; layout } : Layout.Place.t)
    =
    Sexp.List
      [ Atom "place"
      ; Atom (sprintf "s%d+%d" scalar_offset (Layout.scalar_bytes layout))
      ; Atom (sprintf "p%d+%d" ptr_index (Layout.ptr_words layout))
      ]
  ;;

  let capture_src : Compiled.Capture_src.t -> Sexp.t = function
    | Local s -> List [ Atom "Local"; slot s ]
    | Outer_capture p -> List [ Atom "Outer_capture"; place p ]
    | Self_closure -> Atom "Self_closure"
  ;;

  let capture ({ Compiled.Capture.src; dst } : Compiled.Capture.t) =
    [%sexp { src : Sexp.t = capture_src src; dst : Sexp.t = place dst }]
  ;;

  let rec quote_capture
    ({ Compiled.Quote_capture.ident; kind } : Compiled.Quote_capture.t)
    =
    let kind_sexp =
      match kind with
      | Value { src; is_cell; ty } ->
        [%sexp
          Value
            { src : Sexp.t = capture_src src
            ; is_cell : bool
            ; ty : Mono_type.Mono_ty.t
            }]
      | Fn { fn_id; ty } ->
        [%sexp Fn { fn_id : Compiled.Fn_id.t; ty : Mono_type.Mono_ty.t }]
      | Generic_fn
          { params
          ; body = _
          ; partials = _
          ; made = _
          ; scheme
          ; captures
          ; parent_subst = _
          } ->
        [%sexp
          Generic_fn
            { params : string list = List.map params ~f:(fun p -> p.name)
            ; scheme : Mono_type.Mono_ty.t
            ; captures : Sexp.t array = Array.map captures ~f:quote_capture
            }]
      | Generic_value { body = _; scheme; captures; parent_subst = _ } ->
        [%sexp
          Generic_value
            { scheme : Mono_type.Mono_ty.t
            ; captures : Sexp.t array = Array.map captures ~f:quote_capture
            }]
    in
    [%sexp { name : string = ident.name; kind : Sexp.t = kind_sexp }]
  ;;

  let rec pattern : Compiled.Pattern.t -> Sexp.t = function
    | Any -> Atom "Any"
    | Never -> Atom "Never"
    | Bind s -> List [ Atom "Bind"; slot s ]
    | Bind_variant_narrow { slot = s; src_discriminant; cases } ->
      [%sexp
        Bind_variant_narrow
          { slot : Sexp.t = slot s
          ; src_discriminant : Layout.Discriminant.t
          ; cases : Sexp.t array =
              Array.map
                cases
                ~f:
                  (fun
                    { Compiled.Pattern.Variant_remap.src_case
                    ; src_payload
                    ; dst_write_plan
                    ; dst_payload
                    }
                  ->
                  [%sexp
                    { src_case : int
                    ; src_payload : Sexp.t = place src_payload
                    ; dst_write_plan : Layout.Write_plan.t option
                    ; dst_payload : Sexp.t = place dst_payload
                    }])
          }]
    | Lit l -> [%sexp Lit (l : Syntax.Lit.t)]
    | Tuple ps ->
      List
        (Atom "Tuple"
         :: Array.to_list
              (Array.map ps ~f:(fun (p, pat) ->
                 Sexp.List [ place p; pattern pat ])))
    | Deref { content_layout; inner } ->
      [%sexp
        Deref
          { content : Sexp.t = layout_size content_layout
          ; inner : Sexp.t = pattern inner
          }]
    | Construct { discriminant; case; args } ->
      [%sexp
        Construct
          { case : int
          ; discriminant : Layout.Discriminant.t
          ; args : Sexp.t array =
              Array.map args ~f:(fun (p, pat) ->
                Sexp.List [ place p; pattern pat ])
          }]
    | Construct_exn { name; payload_layout; args } ->
      [%sexp
        Construct_exn
          { name : string
          ; payload : Sexp.t = layout_size payload_layout
          ; args : Sexp.t array =
              Array.map args ~f:(fun (p, pat) ->
                Sexp.List [ place p; pattern pat ])
          }]
    | Record fs ->
      List
        (Atom "Record"
         :: Array.to_list
              (Array.map fs ~f:(fun (p, pat) ->
                 Sexp.List [ place p; pattern pat ])))
    | Poly_record_rest { fields; rest_slot; rest_copy } ->
      [%sexp
        Poly_record_rest
          { fields : Sexp.t array =
              Array.map fields ~f:(fun (p, pat) ->
                Sexp.List [ place p; pattern pat ])
          ; rest_slot : Sexp.t = slot rest_slot
          ; rest_copy : Sexp.t array =
              Array.map rest_copy ~f:(fun (src, dst) ->
                Sexp.List [ place src; Atom "->"; place dst ])
          }]
    | Or (l, r) -> List [ Atom "Or"; pattern l; pattern r ]
    | Error _ -> Atom "Error"
  ;;

  let rec expr : Compiled.Expr.t -> Sexp.t = function
    | Lit l -> [%sexp Lit (l : Syntax.Lit.t)]
    | Local s -> List [ Atom "Local"; slot s ]
    | Capture p -> List [ Atom "Capture"; place p ]
    | Self_closure -> Atom "Self_closure"
    | Global { id; loc = _ } -> [%sexp Global (id : Compiled.Global_id.t)]
    | Free_var { name; loc = _ } -> [%sexp Free_var (name : string)]
    | Make_closure { fn; placement; env_layout; captures } ->
      [%sexp
        Make_closure
          { fn : Compiled.Fn_id.t
          ; placement : Compiled.Placement.t
          ; env : Sexp.t = layout_size env_layout
          ; captures : Sexp.t array = Array.map captures ~f:capture
          }]
    | Call_direct { fn; args; arg_slots; tail; loc = _ } ->
      [%sexp
        Call_direct
          { fn : Compiled.Fn_id.t
          ; tail : bool
          ; arg_slots : Sexp.t array = Array.map arg_slots ~f:slot
          ; args : Sexp.t array = Array.map args ~f:expr
          }]
    | Call_indirect { fn; fn_slot; arg; arg_slot; tail; loc = _ } ->
      [%sexp
        Call_indirect
          { fn_slot : Sexp.t = slot fn_slot
          ; fn : Sexp.t = expr fn
          ; arg_slot : Sexp.t = slot arg_slot
          ; arg : Sexp.t = expr arg
          ; tail : bool
          }]
    | Prim_op { op; args; arg_slots; loc = _ } ->
      [%sexp
        Prim_op
          { op : Compiled.Prim_op.t
          ; arg_slots : Sexp.t array = Array.map arg_slots ~f:slot
          ; args : Sexp.t array = Array.map args ~f:expr
          }]
    | Prim_call { prim; args; arg_slots = _; loc = _ } ->
      [%sexp
        Prim_call
          { prim : Prim.t; args : Sexp.t array = Array.map args ~f:expr }]
    | Prim_partial { prim; args; arg_slots = _; placement; loc = _ } ->
      [%sexp
        Prim_partial
          { prim : Prim.t
          ; placement : Compiled.Placement.t
          ; args : Sexp.t array = Array.map args ~f:expr
          }]
    | Prim_val { prim; placement } ->
      [%sexp Prim_val { prim : Prim.t; placement : Compiled.Placement.t }]
    | Ptr_make { value; value_slot = _; content_layout; placement } ->
      [%sexp
        Ptr_make
          { placement : Compiled.Placement.t
          ; content : Sexp.t = layout_size content_layout
          ; value : Sexp.t = expr value
          }]
    | Box_make { value; value_slot = _; content_layout; placement } ->
      [%sexp
        Box_make
          { placement : Compiled.Placement.t
          ; content : Sexp.t = layout_size content_layout
          ; value : Sexp.t = expr value
          }]
    | Intrinsic_call { intrinsic; arg; arg_slot = _; loc = _ } ->
      [%sexp
        Intrinsic_call { intrinsic : Intrinsic.t; arg : Sexp.t = expr arg }]
    | Intrinsic_val { intrinsic; placement } ->
      [%sexp
        Intrinsic_val
          { intrinsic : Intrinsic.t; placement : Compiled.Placement.t }]
    | External_call
        { symbol
        ; signature = _
        ; at_compile_time = _
        ; args
        ; arg_slots = _
        ; loc = _
        } ->
      [%sexp
        External_call
          { symbol : string; args : Sexp.t array = Array.map args ~f:expr }]
    | External_val { symbol; signature = _; at_compile_time = _ } ->
      [%sexp External_val (symbol : string)]
    | Tuple elems ->
      List
        (Atom "Tuple"
         :: Array.to_list
              (Array.map elems ~f:(fun (p, e) -> Sexp.List [ place p; expr e ]))
        )
    | Record fields ->
      List
        (Atom "Record"
         :: Array.to_list
              (Array.map fields ~f:(fun (p, e) -> Sexp.List [ place p; expr e ]))
        )
    | Record_update { base; base_slot; copy_fields; updates } ->
      [%sexp
        Record_update
          { base_slot : Sexp.t = slot base_slot
          ; base : Sexp.t = expr base
          ; copy_fields : Sexp.t array =
              Array.map copy_fields ~f:(fun (src, dst) ->
                Sexp.List [ place src; Atom "->"; place dst ])
          ; updates : Sexp.t array =
              Array.map updates ~f:(fun (p, e) -> Sexp.List [ place p; expr e ])
          }]
    | Poly_extend { added; base; base_slot = _; copy_fields } ->
      [%sexp
        Poly_extend
          { added : Sexp.t array =
              Array.map added ~f:(fun (p, _, e) ->
                Sexp.List [ place p; expr e ])
          ; base : Sexp.t = expr base
          ; copy_fields : Sexp.t array =
              Array.map copy_fields ~f:(fun (src, dst) ->
                Sexp.List [ place src; Atom "->"; place dst ])
          }]
    | Poly_restrict { base; base_slot = _; copy_fields } ->
      [%sexp
        Poly_restrict
          { base : Sexp.t = expr base
          ; copy_fields : Sexp.t array =
              Array.map copy_fields ~f:(fun (src, dst) ->
                Sexp.List [ place src; Atom "->"; place dst ])
          }]
    | Field { expr = e; expr_slot = _; place = p } ->
      [%sexp Field { expr : Sexp.t = expr e; place : Sexp.t = place p }]
    | Construct { case; write_plan; args } ->
      [%sexp
        Construct
          { case : int
          ; write_plan : Layout.Write_plan.t option
          ; args : Sexp.t array =
              Array.map args ~f:(fun (p, _, e) -> Sexp.List [ place p; expr e ])
          }]
    | Construct_exn { name; payload_layout; args; arg_tys } ->
      [%sexp
        Construct_exn
          { name : string
          ; payload : Sexp.t = layout_size payload_layout
          ; arg_tys : Mono_type.Mono_ty.t array
          ; args : Sexp.t array =
              Array.map args ~f:(fun (p, _, e) -> Sexp.List [ place p; expr e ])
          }]
    | Deref { ptr; ptr_slot = _; content_layout } ->
      [%sexp
        Deref
          { content : Sexp.t = layout_size content_layout
          ; ptr : Sexp.t = expr ptr
          }]
    | Store { ptr; ptr_slot = _; place = p; value; value_slot = _ } ->
      [%sexp
        Store
          { ptr : Sexp.t = expr ptr
          ; place : Sexp.t = place p
          ; value : Sexp.t = expr value
          }]
    | Let { slot = s; body; in_ } ->
      [%sexp
        Let
          { slot : Sexp.t = slot s
          ; body : Sexp.t = expr body
          ; in_ : Sexp.t = expr in_
          }]
    | Let_mut { slot = s; body; in_ } ->
      [%sexp
        Let_mut
          { slot : Sexp.t = slot s
          ; body : Sexp.t = expr body
          ; in_ : Sexp.t = expr in_
          }]
    | Let_mut_cell
        { slot = s; value_slot = _; content_layout; placement; body; in_ } ->
      [%sexp
        Let_mut_cell
          { slot : Sexp.t = slot s
          ; content : Sexp.t = layout_size content_layout
          ; placement : Compiled.Placement.t
          ; body : Sexp.t = expr body
          ; in_ : Sexp.t = expr in_
          }]
    | Assign_local { slot = s; value; value_slot = _ } ->
      [%sexp
        Assign_local { slot : Sexp.t = slot s; value : Sexp.t = expr value }]
    | Assign_global { global; value; value_slot = _; loc = _ } ->
      [%sexp
        Assign_global
          { global : Compiled.Global_id.t; value : Sexp.t = expr value }]
    | Seq { first; first_slot = _; second } ->
      [%sexp Seq { first : Sexp.t = expr first; second : Sexp.t = expr second }]
    | Region { body; clear_ptrs } ->
      [%sexp Region { clear_ptrs : int * int; body : Sexp.t = expr body }]
    | Exclave e -> List [ Atom "Exclave"; expr e ]
    | Match
        { scrutinee
        ; scrutinee_slot
        ; scrutinee_ty
        ; cases
        ; exception_cases
        ; loc = _
        } ->
      [%sexp
        Match
          { scrutinee_slot : Sexp.t = slot scrutinee_slot
          ; scrutinee_ty : Mono_type.Mono_ty.t
          ; scrutinee : Sexp.t = expr scrutinee
          ; cases : Sexp.t array =
              Array.map
                cases
                ~f:(fun { Compiled.Match_case.pattern = p; body } ->
                  Sexp.List [ pattern p; Atom "=>"; expr body ])
          ; exception_cases : Sexp.t array =
              Array.map
                exception_cases
                ~f:(fun { Compiled.Match_case.pattern = p; body } ->
                  Sexp.List [ pattern p; Atom "=>"; expr body ])
          }]
    | Quote { body = _; splices; captures; placement } ->
      [%sexp
        Quote
          { placement : Compiled.Placement.t
          ; splices : Sexp.t array =
              Array.map splices ~f:(fun (e, s) -> Sexp.List [ slot s; expr e ])
          ; captures : Sexp.t array = Array.map captures ~f:quote_capture
          }]
    | Type_val ty -> [%sexp Type_val (ty : Syntax.Type_expr.t)]
    | Expect s -> [%sexp Expect (s.txt : string)]
    | Error _ -> Atom "Error"
  ;;

  let fn (fn : Compiled.Fn.t) =
    [%sexp
      { id : Compiled.Fn_id.t = fn.id
      ; name : string = fn.name
      ; params : Sexp.t array = Array.map fn.params ~f:slot
      ; frame : Frame_layout.t = fn.frame
      ; env : Sexp.t = layout_size fn.env_layout
      ; partials : Compiled.Placement.t array = fn.partials
      ; result : Sexp.t = layout_size fn.result_layout
      ; result_ty : Mono_type.Mono_ty.t = fn.result_ty
      ; body : Sexp.t = expr fn.body
      }]
  ;;

  let global (g : Compiled.Global.t) =
    let kind =
      match g.kind with
      | Function fn_id -> [%sexp Function (fn_id : Compiled.Fn_id.t)]
      | Value { init; frame; mutable_ } ->
        [%sexp
          Value
            { mutable_ : bool
            ; frame : Frame_layout.t
            ; init : Sexp.t = expr init
            }]
    in
    [%sexp
      { id : Compiled.Global_id.t = g.id
      ; name : string = g.name
      ; layout : Sexp.t = layout_size g.layout
      ; ty : Mono_type.Mono_ty.t = g.ty
      ; kind : Sexp.t
      }]
  ;;
end

let print_lower lower =
  print_s
    [%message "fns" ~_:(List.map (Lower.fns lower) ~f:Pretty.fn : Sexp.t list)];
  print_s
    [%message
      "globals"
        ~_:(List.map (Lower.globals lower) ~f:Pretty.global : Sexp.t list)]
;;

let%expect_test "fib: direct recursion, saturated prim ops, zero pointer slots" =
  let lower, find_top, _ =
    check_and_lower
      {|
let fib (n : int) : int =
  match Int.lt n 2 with
  | true -> n
  | false -> Int.add (fib (Int.sub n 1)) (fib (Int.sub n 2))

let fib_tail (n : int) (a : int) (b : int) : int =
  match Int.equal n 0 with
  | true -> a
  | false -> fib_tail (Int.sub n 1) b (Int.add a b)
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "fib") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "fib_tail") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name fib) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 96) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Match
         ((scrutinee_slot (slot s8+1 p0+0)) (scrutinee_ty bool)
          (scrutinee
           (Prim_op
            ((op Int_lt) (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
             (args ((Local (slot s0+8 p0+0)) (Lit (Int 2)))))))
          (cases
           (((Lit (Bool true)) => (Local (slot s0+8 p0+0)))
            ((Lit (Bool false)) =>
             (Prim_op
              ((op Int_add) (arg_slots ((slot s32+8 p0+0) (slot s64+8 p0+0)))
               (args
                ((Call_direct
                  ((fn 0) (tail false) (arg_slots ((slot s40+8 p0+0)))
                   (args
                    ((Prim_op
                      ((op Int_sub)
                       (arg_slots ((slot s48+8 p0+0) (slot s56+8 p0+0)))
                       (args ((Local (slot s0+8 p0+0)) (Lit (Int 1))))))))))
                 (Call_direct
                  ((fn 0) (tail false) (arg_slots ((slot s72+8 p0+0)))
                   (args
                    ((Prim_op
                      ((op Int_sub)
                       (arg_slots ((slot s80+8 p0+0) (slot s88+8 p0+0)))
                       (args ((Local (slot s0+8 p0+0)) (Lit (Int 2)))))))))))))))))
          (exception_cases ())))))
      ((id 1) (name fib_tail)
       (params ((slot s0+8 p0+0) (slot s8+8 p0+0) (slot s16+8 p0+0)))
       (frame ((scalar_bytes 104) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials (Heap Heap)) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Match
         ((scrutinee_slot (slot s24+1 p0+0)) (scrutinee_ty bool)
          (scrutinee
           (Prim_op
            ((op Int_equal) (arg_slots ((slot s32+8 p0+0) (slot s40+8 p0+0)))
             (args ((Local (slot s0+8 p0+0)) (Lit (Int 0)))))))
          (cases
           (((Lit (Bool true)) => (Local (slot s8+8 p0+0)))
            ((Lit (Bool false)) =>
             (Call_direct
              ((fn 1) (tail true)
               (arg_slots
                ((slot s48+8 p0+0) (slot s72+8 p0+0) (slot s80+8 p0+0)))
               (args
                ((Prim_op
                  ((op Int_sub) (arg_slots ((slot s56+8 p0+0) (slot s64+8 p0+0)))
                   (args ((Local (slot s0+8 p0+0)) (Lit (Int 1))))))
                 (Local (slot s16+8 p0+0))
                 (Prim_op
                  ((op Int_add) (arg_slots ((slot s88+8 p0+0) (slot s96+8 p0+0)))
                   (args ((Local (slot s8+8 p0+0)) (Local (slot s16+8 p0+0)))))))))))))
          (exception_cases ())))))))
    (globals
     (((id 0) (name fib) (layout ((s 0) (p 1))) (ty "int -> int")
       (kind (Function 0)))
      ((id 1) (name fib_tail) (layout ((s 0) (p 1)))
       (ty "int -> int -> int -> int") (kind (Function 1)))))
    |}]
;;

let%expect_test "polymorphic list functions: distinct specializations, \
                 caching, niche layout"
  =
  let lower, find_top, _ =
    check_and_lower
      {|
let rev_append xs acc =
  match xs with
  | Nil -> acc
  | Cons cell ->
    match cell.* with
    | (x, rest) -> rev_append rest (Cons (Box.make (x, acc)))

let length xs =
  match xs with
  | Nil -> 0
  | Cons cell ->
    match cell.* with
    | (_, rest) -> Int.add 1 (length rest)

let test_lists =
  let ints = [1; 2] in
  let strs = ["a"; "b"] in
  let _ = rev_append ints Nil in
  let _ = rev_append ints Nil in
  let _ = rev_append strs Nil in
  (length ints, length ints, length strs)
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "test_lists") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name rev_append) (params ((slot s0+0 p0+1) (slot s0+0 p1+1)))
       (frame ((scalar_bytes 24) (ptr_words 11))) (env ((s 0) (p 0)))
       (partials (Caller_region)) (result ((s 0) (p 1))) (result_ty list[int])
       (body
        (Match
         ((scrutinee_slot (slot s0+0 p2+1)) (scrutinee_ty list[int])
          (scrutinee (Local (slot s0+0 p0+1)))
          (cases
           (((Construct
              ((case 0)
               (discriminant
                (Niche (leaf (Ptr (ptr_index 0))) (untagged 1) (first 0)
                 (last 0) (start 0)))
               (args ())))
             => (Local (slot s0+0 p1+1)))
            ((Construct
              ((case 1)
               (discriminant
                (Niche (leaf (Ptr (ptr_index 0))) (untagged 1) (first 0)
                 (last 0) (start 0)))
               (args (((place s0+0 p0+1) (Bind (slot s0+0 p3+1)))))))
             =>
             (Match
              ((scrutinee_slot (slot s0+8 p4+1)) (scrutinee_ty "int * list[int]")
               (scrutinee
                (Deref ((content ((s 8) (p 1))) (ptr (Local (slot s0+0 p3+1))))))
               (cases
                (((Tuple ((place s0+8 p0+0) (Bind (slot s8+8 p0+0)))
                   ((place s8+0 p0+1) (Bind (slot s0+0 p6+1))))
                  =>
                  (Call_direct
                   ((fn 0) (tail true)
                    (arg_slots ((slot s0+0 p7+1) (slot s0+0 p8+1)))
                    (args
                     ((Local (slot s0+0 p6+1))
                      (Construct
                       ((case 1)
                        (write_plan
                         (((ptr_index 0) (ptr_words 1) (discriminant ()))))
                        (args
                         (((place s0+0 p0+1)
                           (Box_make
                            ((placement Heap) (content ((s 8) (p 1)))
                             (value
                              (Tuple ((place s0+8 p0+0) (Local (slot s8+8 p0+0)))
                               ((place s8+0 p0+1) (Local (slot s0+0 p1+1)))))))))))))))))))
               (exception_cases ()))))))
          (exception_cases ())))))
      ((id 1) (name rev_append) (params ((slot s0+0 p0+1) (slot s0+0 p1+1)))
       (frame ((scalar_bytes 0) (ptr_words 14))) (env ((s 0) (p 0)))
       (partials (Caller_region)) (result ((s 0) (p 1))) (result_ty list[string])
       (body
        (Match
         ((scrutinee_slot (slot s0+0 p2+1)) (scrutinee_ty list[string])
          (scrutinee (Local (slot s0+0 p0+1)))
          (cases
           (((Construct
              ((case 0)
               (discriminant
                (Niche (leaf (Ptr (ptr_index 0))) (untagged 1) (first 0)
                 (last 0) (start 0)))
               (args ())))
             => (Local (slot s0+0 p1+1)))
            ((Construct
              ((case 1)
               (discriminant
                (Niche (leaf (Ptr (ptr_index 0))) (untagged 1) (first 0)
                 (last 0) (start 0)))
               (args (((place s0+0 p0+1) (Bind (slot s0+0 p3+1)))))))
             =>
             (Match
              ((scrutinee_slot (slot s0+0 p4+2))
               (scrutinee_ty "string * list[string]")
               (scrutinee
                (Deref ((content ((s 0) (p 2))) (ptr (Local (slot s0+0 p3+1))))))
               (cases
                (((Tuple ((place s0+0 p0+1) (Bind (slot s0+0 p7+1)))
                   ((place s0+0 p1+1) (Bind (slot s0+0 p8+1))))
                  =>
                  (Call_direct
                   ((fn 1) (tail true)
                    (arg_slots ((slot s0+0 p9+1) (slot s0+0 p10+1)))
                    (args
                     ((Local (slot s0+0 p8+1))
                      (Construct
                       ((case 1)
                        (write_plan
                         (((ptr_index 0) (ptr_words 1) (discriminant ()))))
                        (args
                         (((place s0+0 p0+1)
                           (Box_make
                            ((placement Heap) (content ((s 0) (p 2)))
                             (value
                              (Tuple ((place s0+0 p0+1) (Local (slot s0+0 p7+1)))
                               ((place s0+0 p1+1) (Local (slot s0+0 p1+1)))))))))))))))))))
               (exception_cases ()))))))
          (exception_cases ())))))
      ((id 2) (name length) (params ((slot s0+0 p0+1)))
       (frame ((scalar_bytes 24) (ptr_words 7))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Match
         ((scrutinee_slot (slot s0+0 p1+1)) (scrutinee_ty list[int])
          (scrutinee (Local (slot s0+0 p0+1)))
          (cases
           (((Construct
              ((case 0)
               (discriminant
                (Niche (leaf (Ptr (ptr_index 0))) (untagged 1) (first 0)
                 (last 0) (start 0)))
               (args ())))
             => (Lit (Int 0)))
            ((Construct
              ((case 1)
               (discriminant
                (Niche (leaf (Ptr (ptr_index 0))) (untagged 1) (first 0)
                 (last 0) (start 0)))
               (args (((place s0+0 p0+1) (Bind (slot s0+0 p2+1)))))))
             =>
             (Match
              ((scrutinee_slot (slot s0+8 p3+1)) (scrutinee_ty "int * list[int]")
               (scrutinee
                (Deref ((content ((s 8) (p 1))) (ptr (Local (slot s0+0 p2+1))))))
               (cases
                (((Tuple ((place s0+8 p0+0) Any)
                   ((place s8+0 p0+1) (Bind (slot s0+0 p5+1))))
                  =>
                  (Prim_op
                   ((op Int_add) (arg_slots ((slot s8+8 p0+0) (slot s16+8 p0+0)))
                    (args
                     ((Lit (Int 1))
                      (Call_direct
                       ((fn 2) (tail false) (arg_slots ((slot s0+0 p6+1)))
                        (args ((Local (slot s0+0 p5+1)))))))))))))
               (exception_cases ()))))))
          (exception_cases ())))))
      ((id 3) (name length) (params ((slot s0+0 p0+1)))
       (frame ((scalar_bytes 16) (ptr_words 8))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Match
         ((scrutinee_slot (slot s0+0 p1+1)) (scrutinee_ty list[string])
          (scrutinee (Local (slot s0+0 p0+1)))
          (cases
           (((Construct
              ((case 0)
               (discriminant
                (Niche (leaf (Ptr (ptr_index 0))) (untagged 1) (first 0)
                 (last 0) (start 0)))
               (args ())))
             => (Lit (Int 0)))
            ((Construct
              ((case 1)
               (discriminant
                (Niche (leaf (Ptr (ptr_index 0))) (untagged 1) (first 0)
                 (last 0) (start 0)))
               (args (((place s0+0 p0+1) (Bind (slot s0+0 p2+1)))))))
             =>
             (Match
              ((scrutinee_slot (slot s0+0 p3+2))
               (scrutinee_ty "string * list[string]")
               (scrutinee
                (Deref ((content ((s 0) (p 2))) (ptr (Local (slot s0+0 p2+1))))))
               (cases
                (((Tuple ((place s0+0 p0+1) Any)
                   ((place s0+0 p1+1) (Bind (slot s0+0 p6+1))))
                  =>
                  (Prim_op
                   ((op Int_add) (arg_slots ((slot s0+8 p0+0) (slot s8+8 p0+0)))
                    (args
                     ((Lit (Int 1))
                      (Call_direct
                       ((fn 3) (tail false) (arg_slots ((slot s0+0 p7+1)))
                        (args ((Local (slot s0+0 p6+1)))))))))))))
               (exception_cases ()))))))
          (exception_cases ())))))))
    (globals
     (((id 0) (name test_lists) (layout ((s 24) (p 0))) (ty "int * int * int")
       (kind
        (Value
         ((mutable_ false) (frame ((scalar_bytes 16) (ptr_words 24)))
          (init
           (Let
            ((slot (slot s0+0 p0+1))
             (body
              (Construct
               ((case 1)
                (write_plan (((ptr_index 0) (ptr_words 1) (discriminant ()))))
                (args
                 (((place s0+0 p0+1)
                   (Box_make
                    ((placement Region) (content ((s 8) (p 1)))
                     (value
                      (Tuple ((place s0+8 p0+0) (Lit (Int 1)))
                       ((place s8+0 p0+1)
                        (Construct
                         ((case 1)
                          (write_plan
                           (((ptr_index 0) (ptr_words 1) (discriminant ()))))
                          (args
                           (((place s0+0 p0+1)
                             (Box_make
                              ((placement Region) (content ((s 8) (p 1)))
                               (value
                                (Tuple ((place s0+8 p0+0) (Lit (Int 2)))
                                 ((place s8+0 p0+1)
                                  (Construct
                                   ((case 0)
                                    (write_plan
                                     (((ptr_index 0) (ptr_words 1)
                                       (discriminant (((Ptr (ptr_index 0)) 0))))))
                                    (args ()))))))))))))))))))))))))
             (in_
              (Let
               ((slot (slot s0+0 p5+1))
                (body
                 (Construct
                  ((case 1)
                   (write_plan (((ptr_index 0) (ptr_words 1) (discriminant ()))))
                   (args
                    (((place s0+0 p0+1)
                      (Box_make
                       ((placement Region) (content ((s 0) (p 2)))
                        (value
                         (Tuple ((place s0+0 p0+1) (Lit (String a)))
                          ((place s0+0 p1+1)
                           (Construct
                            ((case 1)
                             (write_plan
                              (((ptr_index 0) (ptr_words 1) (discriminant ()))))
                             (args
                              (((place s0+0 p0+1)
                                (Box_make
                                 ((placement Region) (content ((s 0) (p 2)))
                                  (value
                                   (Tuple ((place s0+0 p0+1) (Lit (String b)))
                                    ((place s0+0 p1+1)
                                     (Construct
                                      ((case 0)
                                       (write_plan
                                        (((ptr_index 0) (ptr_words 1)
                                          (discriminant
                                           (((Ptr (ptr_index 0)) 0))))))
                                       (args ()))))))))))))))))))))))))
                (in_
                 (Let
                  ((slot (slot s0+0 p12+1))
                   (body
                    (Call_direct
                     ((fn 0) (tail false)
                      (arg_slots ((slot s0+0 p13+1) (slot s0+0 p14+1)))
                      (args
                       ((Local (slot s0+0 p0+1))
                        (Construct
                         ((case 0)
                          (write_plan
                           (((ptr_index 0) (ptr_words 1)
                             (discriminant (((Ptr (ptr_index 0)) 0))))))
                          (args ()))))))))
                   (in_
                    (Let
                     ((slot (slot s0+0 p15+1))
                      (body
                       (Call_direct
                        ((fn 0) (tail false)
                         (arg_slots ((slot s0+0 p16+1) (slot s0+0 p17+1)))
                         (args
                          ((Local (slot s0+0 p0+1))
                           (Construct
                            ((case 0)
                             (write_plan
                              (((ptr_index 0) (ptr_words 1)
                                (discriminant (((Ptr (ptr_index 0)) 0))))))
                             (args ()))))))))
                      (in_
                       (Let
                        ((slot (slot s0+0 p18+1))
                         (body
                          (Call_direct
                           ((fn 1) (tail false)
                            (arg_slots ((slot s0+0 p19+1) (slot s0+0 p20+1)))
                            (args
                             ((Local (slot s0+0 p5+1))
                              (Construct
                               ((case 0)
                                (write_plan
                                 (((ptr_index 0) (ptr_words 1)
                                   (discriminant (((Ptr (ptr_index 0)) 0))))))
                                (args ()))))))))
                         (in_
                          (Tuple
                           ((place s0+8 p0+0)
                            (Call_direct
                             ((fn 2) (tail false) (arg_slots ((slot s0+0 p21+1)))
                              (args ((Local (slot s0+0 p0+1)))))))
                           ((place s8+8 p0+0)
                            (Call_direct
                             ((fn 2) (tail false) (arg_slots ((slot s0+0 p22+1)))
                              (args ((Local (slot s0+0 p0+1)))))))
                           ((place s16+8 p0+0)
                            (Call_direct
                             ((fn 3) (tail false) (arg_slots ((slot s0+0 p23+1)))
                              (args ((Local (slot s0+0 p5+1))))))))))))))))))))))))))))))
    |}]
;;

let%expect_test "polymorphic records, variants, aliases, and rest patterns" =
  let lower, find_top, _ =
    check_and_lower
      {|
type pt = {`x : int; `y : int}

let poly_rec_ops =
  let r = {`a = 1; `b = "x"} in
  let r2 = {r with `a = 2} in
  let r3 = {`c = true | r2} in
  let r4 = r3 \ `a in
  match r3 with
  | {`a = a; ..rest} -> (a, rest.`b, r4.`c)

let open_variant_handler v =
  match v with
  | `A n -> n
  | `B s -> String.length s
  | _ -> 0

let test_variants =
  let only_a = open_variant_handler (`A 42) in
  let both =
    match true with
    | true -> open_variant_handler (`A 1)
    | false -> open_variant_handler (`B "hi")
  in
  let narrow v k =
    match v with
    | `A -> 1
    | other -> k other
  in
  let narrowed =
    narrow (`B 10) (fun o -> match o with `B n -> n)
  in
  let p : pt = {`x = 10; `y = 20} in
  (only_a, both, narrowed, p.`x)
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "poly_rec_ops") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "test_variants") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name open_variant_handler) (params ((slot s0+8 p0+1)))
       (frame ((scalar_bytes 24) (ptr_words 4))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Match
         ((scrutinee_slot (slot s8+8 p1+1))
          (scrutinee_ty "[`A of int | `B of string]")
          (scrutinee (Local (slot s0+8 p0+1)))
          (cases
           (((Construct
              ((case 0)
               (discriminant
                (Niche (leaf (Ptr (ptr_index 0))) (untagged 1) (first 0)
                 (last 0) (start 0)))
               (args (((place s0+8 p0+0) (Bind (slot s16+8 p0+0)))))))
             => (Local (slot s16+8 p0+0)))
            ((Construct
              ((case 1)
               (discriminant
                (Niche (leaf (Ptr (ptr_index 0))) (untagged 1) (first 0)
                 (last 0) (start 0)))
               (args (((place s0+0 p0+1) (Bind (slot s0+0 p2+1)))))))
             =>
             (Prim_op
              ((op String_length) (arg_slots ((slot s0+0 p3+1)))
               (args ((Local (slot s0+0 p2+1)))))))
            (Any => (Lit (Int 0)))))
          (exception_cases ())))))
      ((id 1) (name narrow) (params ((slot s0+16 p0+0) (slot s0+0 p0+1)))
       (frame ((scalar_bytes 48) (ptr_words 2))) (env ((s 0) (p 0)))
       (partials (Caller_region)) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Match
         ((scrutinee_slot (slot s16+16 p0+0)) (scrutinee_ty "[`A | `B of int]")
          (scrutinee (Local (slot s0+16 p0+0)))
          (cases
           (((Construct
              ((case 0)
               (discriminant (Tag (Scalar (scalar_offset 0) (scalar I8))))
               (args ())))
             => (Lit (Int 1)))
            ((Bind_variant_narrow
              ((slot (slot s32+8 p0+0))
               (src_discriminant (Tag (Scalar (scalar_offset 0) (scalar I8))))
               (cases
                (((src_case 1) (src_payload (place s8+8 p0+0))
                  (dst_write_plan
                   (((ptr_index 0) (ptr_words 0) (discriminant ()))))
                  (dst_payload (place s0+8 p0+0)))))))
             =>
             (Call_indirect
              ((fn_slot (slot s0+0 p1+1)) (fn (Local (slot s0+0 p0+1)))
               (arg_slot (slot s40+8 p0+0)) (arg (Local (slot s32+8 p0+0)))
               (tail true))))))
          (exception_cases ())))))
      ((id 2) (name <fun>) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 24) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Match
         ((scrutinee_slot (slot s8+8 p0+0)) (scrutinee_ty "[`B of int]")
          (scrutinee (Local (slot s0+8 p0+0)))
          (cases
           (((Construct
              ((case 0) (discriminant (Single (case 0)))
               (args (((place s0+8 p0+0) (Bind (slot s16+8 p0+0)))))))
             => (Local (slot s16+8 p0+0)))))
          (exception_cases ())))))))
    (globals
     (((id 0) (name poly_rec_ops) (layout ((s 16) (p 1)))
       (ty "int * string * bool")
       (kind
        (Value
         ((mutable_ false) (frame ((scalar_bytes 120) (ptr_words 11)))
          (init
           (Let
            ((slot (slot s0+8 p0+1))
             (body
              (Record ((place s0+8 p0+0) (Lit (Int 1)))
               ((place s8+0 p0+1) (Lit (String x)))))
             (in_
              (Let
               ((slot (slot s8+8 p1+1))
                (body
                 (Record_update
                  ((base_slot (slot s16+8 p2+1)) (base (Local (slot s0+8 p0+1)))
                   (copy_fields (((place s8+0 p0+1) -> (place s8+0 p0+1))))
                   (updates (((place s0+8 p0+0) (Lit (Int 2))))))))
                (in_
                 (Let
                  ((slot (slot s24+16 p3+1))
                   (body
                    (Poly_extend
                     ((added (((place s8+1 p1+0) (Lit (Bool true)))))
                      (base (Local (slot s8+8 p1+1)))
                      (copy_fields
                       (((place s0+8 p0+0) -> (place s0+8 p0+0))
                        ((place s8+0 p0+1) -> (place s8+0 p0+1)))))))
                   (in_
                    (Let
                     ((slot (slot s56+1 p5+1))
                      (body
                       (Poly_restrict
                        ((base (Local (slot s24+16 p3+1)))
                         (copy_fields
                          (((place s8+0 p0+1) -> (place s0+0 p0+1))
                           ((place s8+1 p1+0) -> (place s0+1 p1+0)))))))
                      (in_
                       (Match
                        ((scrutinee_slot (slot s80+16 p7+1))
                         (scrutinee_ty "{`a : int; `b : string; `c : bool}")
                         (scrutinee (Local (slot s24+16 p3+1)))
                         (cases
                          (((Poly_record_rest
                             ((fields
                               (((place s0+8 p0+0) (Bind (slot s104+8 p0+0)))))
                              (rest_slot (slot s96+1 p8+1))
                              (rest_copy
                               (((place s8+0 p0+1) -> (place s0+0 p0+1))
                                ((place s8+1 p1+0) -> (place s0+1 p1+0))))))
                            =>
                            (Tuple ((place s0+8 p0+0) (Local (slot s104+8 p0+0)))
                             ((place s8+0 p0+1)
                              (Field
                               ((expr (Local (slot s96+1 p8+1)))
                                (place (place s0+0 p0+1)))))
                             ((place s8+1 p1+0)
                              (Field
                               ((expr (Local (slot s56+1 p5+1)))
                                (place (place s0+1 p1+0)))))))))
                         (exception_cases ()))))))))))))))))))))
      ((id 1) (name test_variants) (layout ((s 32) (p 0)))
       (ty "int * int * int * int")
       (kind
        (Value
         ((mutable_ false) (frame ((scalar_bytes 128) (ptr_words 5)))
          (init
           (Let
            ((slot (slot s0+8 p0+0))
             (body
              (Call_direct
               ((fn 0) (tail false) (arg_slots ((slot s8+8 p0+1)))
                (args
                 ((Construct
                   ((case 0)
                    (write_plan
                     (((ptr_index 0) (ptr_words 1)
                       (discriminant (((Ptr (ptr_index 0)) 0))))))
                    (args (((place s0+8 p0+0) (Lit (Int 42))))))))))))
             (in_
              (Let
               ((slot (slot s24+8 p0+0))
                (body
                 (Match
                  ((scrutinee_slot (slot s32+1 p0+0)) (scrutinee_ty bool)
                   (scrutinee (Lit (Bool true)))
                   (cases
                    (((Lit (Bool true)) =>
                      (Call_direct
                       ((fn 0) (tail false) (arg_slots ((slot s40+8 p1+1)))
                        (args
                         ((Construct
                           ((case 0)
                            (write_plan
                             (((ptr_index 0) (ptr_words 1)
                               (discriminant (((Ptr (ptr_index 0)) 0))))))
                            (args (((place s0+8 p0+0) (Lit (Int 1))))))))))))
                     ((Lit (Bool false)) =>
                      (Call_direct
                       ((fn 0) (tail false) (arg_slots ((slot s56+8 p2+1)))
                        (args
                         ((Construct
                           ((case 1)
                            (write_plan
                             (((ptr_index 0) (ptr_words 1) (discriminant ()))))
                            (args (((place s0+0 p0+1) (Lit (String hi))))))))))))))
                   (exception_cases ()))))
                (in_
                 (Let
                  ((slot (slot s64+8 p0+0))
                   (body
                    (Call_direct
                     ((fn 1) (tail false)
                      (arg_slots ((slot s72+16 p0+0) (slot s0+0 p4+1)))
                      (args
                       ((Construct
                         ((case 1)
                          (write_plan
                           (((ptr_index 0) (ptr_words 0)
                             (discriminant
                              (((Scalar (scalar_offset 0) (scalar I8)) 1))))))
                          (args (((place s8+8 p0+0) (Lit (Int 10)))))))
                        (Make_closure
                         ((fn 2) (placement Region) (env ((s 0) (p 0)))
                          (captures ()))))))))
                   (in_
                    (Let
                     ((slot (slot s96+16 p0+0))
                      (body
                       (Record ((place s0+8 p0+0) (Lit (Int 10)))
                        ((place s8+8 p0+0) (Lit (Int 20)))))
                      (in_
                       (Tuple ((place s0+8 p0+0) (Local (slot s0+8 p0+0)))
                        ((place s8+8 p0+0) (Local (slot s24+8 p0+0)))
                        ((place s16+8 p0+0) (Local (slot s64+8 p0+0)))
                        ((place s24+8 p0+0)
                         (Field
                          ((expr (Local (slot s96+16 p0+0)))
                           (place (place s0+8 p0+0)))))))))))))))))))))))))
    |}]
;;

let%expect_test "Store nested field paths and let mut (uncaptured vs captured)" =
  let lower, find_top, _ =
    check_and_lower
      {|
type inner = { pad_inner : int; b : int; c : string }
type outer = { pad_outer : int; a : inner; d : bool }

let test_store (p : ptr[outer]) (q : ptr[int]) =
  q.* <- 7;
  p.*.a.b <- 42

let test_let_mut =
  let mut uncaptured = 0 in
  uncaptured <- Int.add uncaptured 1;
  let mut captured = 10 in
  let inc (u : unit) =
    captured <- Int.add captured 1;
    captured
  in
  (uncaptured, inc ())
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "test_store") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "test_let_mut") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name test_store) (params ((slot s0+0 p0+1) (slot s0+0 p1+1)))
       (frame ((scalar_bytes 16) (ptr_words 4))) (env ((s 0) (p 0)))
       (partials (Heap)) (result ((s 0) (p 0))) (result_ty unit)
       (body
        (Seq
         ((first
           (Store
            ((ptr (Local (slot s0+0 p1+1))) (place (place s0+8 p0+0))
             (value (Lit (Int 7))))))
          (second
           (Store
            ((ptr (Local (slot s0+0 p0+1))) (place (place s16+8 p0+0))
             (value (Lit (Int 42))))))))))
      ((id 1) (name inc) (params ((slot s0+0 p0+0)))
       (frame ((scalar_bytes 24) (ptr_words 3))) (env ((s 0) (p 1)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Seq
         ((first
           (Store
            ((ptr (Capture (place s0+0 p0+1))) (place (place s0+8 p0+0))
             (value
              (Prim_op
               ((op Int_add) (arg_slots ((slot s8+8 p0+0) (slot s16+8 p0+0)))
                (args
                 ((Deref
                   ((content ((s 8) (p 0))) (ptr (Capture (place s0+0 p0+1)))))
                  (Lit (Int 1))))))))))
          (second
           (Deref ((content ((s 8) (p 0))) (ptr (Capture (place s0+0 p0+1))))))))))))
    (globals
     (((id 0) (name test_store) (layout ((s 0) (p 1)))
       (ty "ptr[outer] -> ptr[int] -> unit") (kind (Function 0)))
      ((id 1) (name test_let_mut) (layout ((s 16) (p 0))) (ty "int * int")
       (kind
        (Value
         ((mutable_ false) (frame ((scalar_bytes 40) (ptr_words 2)))
          (init
           (Let_mut
            ((slot (slot s0+8 p0+0)) (body (Lit (Int 0)))
             (in_
              (Seq
               ((first
                 (Assign_local
                  ((slot (slot s0+8 p0+0))
                   (value
                    (Prim_op
                     ((op Int_add)
                      (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
                      (args ((Local (slot s0+8 p0+0)) (Lit (Int 1))))))))))
                (second
                 (Let_mut_cell
                  ((slot (slot s0+0 p0+1)) (content ((s 8) (p 0)))
                   (placement Region) (body (Lit (Int 10)))
                   (in_
                    (Tuple ((place s0+8 p0+0) (Local (slot s0+8 p0+0)))
                     ((place s8+8 p0+0)
                      (Call_indirect
                       ((fn_slot (slot s0+0 p1+1))
                        (fn
                         (Make_closure
                          ((fn 1) (placement Region) (env ((s 0) (p 1)))
                           (captures
                            (((src (Local (slot s0+0 p0+1)))
                              (dst (place s0+0 p0+1))))))))
                        (arg_slot (slot s0+0 p0+0)) (arg (Lit Unit))
                        (tail false)))))))))))))))))))))
    |}]
;;

let%expect_test "mode-polymorphic Allocation.Follows, region_, exclave_, \
                 stack_, and nontail"
  =
  let lower, find_top, _ =
    check_and_lower
      {|
let mk (n : int) = Box.make n
let sum (b : box[int] @ local) = b.*
let f (n : int) : int = Int.add n 1

let test_modes =
  let g = mk 1 in
  let r =
    region_ (
      let l @ local = mk 2 in
      Int.add (sum l) (f 3 [@nontail])
    )
  in
  (g, r)

let make_in_caller (n : int) =
  exclave_ stack_ (Box.make n)

let call_tail (n : int) = f n
let call_nontail (n : int) = f n [@nontail]
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "test_modes") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "make_in_caller") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "call_tail") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "call_nontail") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name mk) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 0) (p 1))) (result_ty box[int])
       (body
        (Box_make
         ((placement Heap) (content ((s 8) (p 0)))
          (value (Local (slot s0+8 p0+0)))))))
      ((id 1) (name mk) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 0) (p 1))) (result_ty box[int])
       (body
        (Box_make
         ((placement Caller_region) (content ((s 8) (p 0)))
          (value (Local (slot s0+8 p0+0)))))))
      ((id 2) (name sum) (params ((slot s0+0 p0+1)))
       (frame ((scalar_bytes 0) (ptr_words 2))) (env ((s 0) (p 0))) (partials ())
       (result ((s 8) (p 0))) (result_ty int)
       (body (Deref ((content ((s 8) (p 0))) (ptr (Local (slot s0+0 p0+1)))))))
      ((id 3) (name f) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 24) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Prim_op
         ((op Int_add) (arg_slots ((slot s8+8 p0+0) (slot s16+8 p0+0)))
          (args ((Local (slot s0+8 p0+0)) (Lit (Int 1))))))))
      ((id 4) (name make_in_caller) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 0) (p 1))) (result_ty box[int])
       (body
        (Exclave
         (Box_make
          ((placement Caller_region) (content ((s 8) (p 0)))
           (value (Local (slot s0+8 p0+0))))))))
      ((id 5) (name call_tail) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Call_direct
         ((fn 3) (tail true) (arg_slots ((slot s8+8 p0+0)))
          (args ((Local (slot s0+8 p0+0))))))))
      ((id 6) (name call_nontail) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Call_direct
         ((fn 3) (tail false) (arg_slots ((slot s8+8 p0+0)))
          (args ((Local (slot s0+8 p0+0))))))))))
    (globals
     (((id 0) (name test_modes) (layout ((s 8) (p 1))) (ty "box[int] * int")
       (kind
        (Value
         ((mutable_ false) (frame ((scalar_bytes 48) (ptr_words 3)))
          (init
           (Let
            ((slot (slot s0+0 p0+1))
             (body
              (Call_direct
               ((fn 0) (tail false) (arg_slots ((slot s0+8 p0+0)))
                (args ((Lit (Int 1)))))))
             (in_
              (Let
               ((slot (slot s8+8 p0+0))
                (body
                 (Region
                  ((clear_ptrs (1 2))
                   (body
                    (Let
                     ((slot (slot s0+0 p1+1))
                      (body
                       (Call_direct
                        ((fn 1) (tail false) (arg_slots ((slot s16+8 p0+0)))
                         (args ((Lit (Int 2)))))))
                      (in_
                       (Prim_op
                        ((op Int_add)
                         (arg_slots ((slot s24+8 p0+0) (slot s32+8 p0+0)))
                         (args
                          ((Call_direct
                            ((fn 2) (tail false) (arg_slots ((slot s0+0 p2+1)))
                             (args ((Local (slot s0+0 p1+1))))))
                           (Call_direct
                            ((fn 3) (tail false) (arg_slots ((slot s40+8 p0+0)))
                             (args ((Lit (Int 3)))))))))))))))))
                (in_
                 (Tuple ((place s8+0 p0+1) (Local (slot s0+0 p0+1)))
                  ((place s0+8 p1+0) (Local (slot s8+8 p0+0)))))))))))))))
      ((id 1) (name make_in_caller) (layout ((s 0) (p 1)))
       (ty "int -> box[int] @ local") (kind (Function 4)))
      ((id 2) (name call_tail) (layout ((s 0) (p 1))) (ty "int -> int")
       (kind (Function 5)))
      ((id 3) (name call_nontail) (layout ((s 0) (p 1))) (ty "int -> int")
       (kind (Function 6)))))
    |}]
;;

let%expect_test "generalized polymorphic values, unconstrained None, \
                 exceptions, mutual recursion"
  =
  let lower, find_top, _ =
    check_and_lower
      {|
exception Boom of int * string

let empty = Nil

let even n =
  match Int.equal n 0 with
  | true -> true
  | false -> odd (Int.sub n 1)

let odd n =
  match Int.equal n 0 with
  | true -> false
  | false -> even (Int.sub n 1)

let test_misc =
  let local_empty = Nil in
  let a = Cons (Box.make (1, empty)) in
  let b = Cons (Box.make ("x", empty)) in
  let c = Cons (Box.make (true, local_empty)) in
  let d = None in
  let e =
    match raise (Boom (1, "err")) with
    | x -> x
    | exception Boom (code, msg) -> Int.add code (String.length msg)
  in
  (a, b, c, d, e, even 4)
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "test_misc") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name even) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 56) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 1) (p 0))) (result_ty bool)
       (body
        (Match
         ((scrutinee_slot (slot s8+1 p0+0)) (scrutinee_ty bool)
          (scrutinee
           (Prim_op
            ((op Int_equal) (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
             (args ((Local (slot s0+8 p0+0)) (Lit (Int 0)))))))
          (cases
           (((Lit (Bool true)) => (Lit (Bool true)))
            ((Lit (Bool false)) =>
             (Call_direct
              ((fn 1) (tail true) (arg_slots ((slot s32+8 p0+0)))
               (args
                ((Prim_op
                  ((op Int_sub) (arg_slots ((slot s40+8 p0+0) (slot s48+8 p0+0)))
                   (args ((Local (slot s0+8 p0+0)) (Lit (Int 1)))))))))))))
          (exception_cases ())))))
      ((id 1) (name odd) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 56) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 1) (p 0))) (result_ty bool)
       (body
        (Match
         ((scrutinee_slot (slot s8+1 p0+0)) (scrutinee_ty bool)
          (scrutinee
           (Prim_op
            ((op Int_equal) (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
             (args ((Local (slot s0+8 p0+0)) (Lit (Int 0)))))))
          (cases
           (((Lit (Bool true)) => (Lit (Bool false)))
            ((Lit (Bool false)) =>
             (Call_direct
              ((fn 2) (tail true) (arg_slots ((slot s32+8 p0+0)))
               (args
                ((Prim_op
                  ((op Int_sub) (arg_slots ((slot s40+8 p0+0) (slot s48+8 p0+0)))
                   (args ((Local (slot s0+8 p0+0)) (Lit (Int 1)))))))))))))
          (exception_cases ())))))
      ((id 2) (name even) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 56) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 1) (p 0))) (result_ty bool)
       (body
        (Match
         ((scrutinee_slot (slot s8+1 p0+0)) (scrutinee_ty bool)
          (scrutinee
           (Prim_op
            ((op Int_equal) (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
             (args ((Local (slot s0+8 p0+0)) (Lit (Int 0)))))))
          (cases
           (((Lit (Bool true)) => (Lit (Bool true)))
            ((Lit (Bool false)) =>
             (Call_direct
              ((fn 1) (tail true) (arg_slots ((slot s32+8 p0+0)))
               (args
                ((Prim_op
                  ((op Int_sub) (arg_slots ((slot s40+8 p0+0) (slot s48+8 p0+0)))
                   (args ((Local (slot s0+8 p0+0)) (Lit (Int 1)))))))))))))
          (exception_cases ())))))))
    (globals
     (((id 0) (name test_misc) (layout ((s 16) (p 3)))
       (ty "list[int] * list[string] * list[bool] * option[unit] * int * bool")
       (kind
        (Value
         ((mutable_ false) (frame ((scalar_bytes 80) (ptr_words 15)))
          (init
           (Let
            ((slot (slot s0+0 p0+1))
             (body
              (Construct
               ((case 1)
                (write_plan (((ptr_index 0) (ptr_words 1) (discriminant ()))))
                (args
                 (((place s0+0 p0+1)
                   (Box_make
                    ((placement Heap) (content ((s 8) (p 1)))
                     (value
                      (Tuple ((place s0+8 p0+0) (Lit (Int 1)))
                       ((place s8+0 p0+1) (Global 1))))))))))))
             (in_
              (Let
               ((slot (slot s0+0 p3+1))
                (body
                 (Construct
                  ((case 1)
                   (write_plan (((ptr_index 0) (ptr_words 1) (discriminant ()))))
                   (args
                    (((place s0+0 p0+1)
                      (Box_make
                       ((placement Heap) (content ((s 0) (p 2)))
                        (value
                         (Tuple ((place s0+0 p0+1) (Lit (String x)))
                          ((place s0+0 p1+1) (Global 2))))))))))))
                (in_
                 (Let
                  ((slot (slot s0+0 p7+1))
                   (body
                    (Construct
                     ((case 1)
                      (write_plan
                       (((ptr_index 0) (ptr_words 1) (discriminant ()))))
                      (args
                       (((place s0+0 p0+1)
                         (Box_make
                          ((placement Heap) (content ((s 1) (p 1)))
                           (value
                            (Tuple ((place s0+1 p0+0) (Lit (Bool true)))
                             ((place s1+0 p0+1)
                              (Construct
                               ((case 0)
                                (write_plan
                                 (((ptr_index 0) (ptr_words 1)
                                   (discriminant (((Ptr (ptr_index 0)) 0))))))
                                (args ()))))))))))))))
                   (in_
                    (Let
                     ((slot (slot s16+8 p0+0))
                      (body
                       (Match
                        ((scrutinee_slot (slot s24+8 p0+0)) (scrutinee_ty int)
                         (scrutinee
                          (Call_indirect
                           ((fn_slot (slot s0+0 p10+1)) (fn (Global 3))
                            (arg_slot (slot s0+0 p11+1))
                            (arg
                             (Construct_exn
                              ((name Boom) (payload ((s 8) (p 1)))
                               (arg_tys (int string))
                               (args
                                (((place s0+8 p0+0) (Lit (Int 1)))
                                 ((place s8+0 p0+1) (Lit (String err))))))))
                            (tail false))))
                         (cases
                          (((Bind (slot s40+8 p0+0)) =>
                            (Local (slot s40+8 p0+0)))))
                         (exception_cases
                          (((Construct_exn
                             ((name Boom) (payload ((s 8) (p 1)))
                              (args
                               (((place s0+8 p0+0) (Bind (slot s48+8 p0+0)))
                                ((place s8+0 p0+1) (Bind (slot s0+0 p13+1)))))))
                            =>
                            (Prim_op
                             ((op Int_add)
                              (arg_slots ((slot s56+8 p0+0) (slot s64+8 p0+0)))
                              (args
                               ((Local (slot s48+8 p0+0))
                                (Prim_op
                                 ((op String_length)
                                  (arg_slots ((slot s0+0 p14+1)))
                                  (args ((Local (slot s0+0 p13+1))))))))))))))))
                      (in_
                       (Tuple ((place s8+0 p0+1) (Local (slot s0+0 p0+1)))
                        ((place s8+0 p1+1) (Local (slot s0+0 p3+1)))
                        ((place s8+0 p2+1) (Local (slot s0+0 p7+1)))
                        ((place s8+1 p3+0)
                         (Construct
                          ((case 0)
                           (write_plan
                            (((ptr_index 0) (ptr_words 0)
                              (discriminant
                               (((Scalar (scalar_offset 0) (scalar I8)) 0))))))
                           (args ()))))
                        ((place s0+8 p3+0) (Local (slot s16+8 p0+0)))
                        ((place s9+1 p3+0)
                         (Call_direct
                          ((fn 0) (tail false) (arg_slots ((slot s72+8 p0+0)))
                           (args ((Lit (Int 4)))))))))))))))))))))))))
      ((id 1) (name empty) (layout ((s 0) (p 1))) (ty list[int])
       (kind
        (Value
         ((mutable_ false) (frame ((scalar_bytes 0) (ptr_words 0)))
          (init
           (Construct
            ((case 0)
             (write_plan
              (((ptr_index 0) (ptr_words 1)
                (discriminant (((Ptr (ptr_index 0)) 0))))))
             (args ()))))))))
      ((id 2) (name empty) (layout ((s 0) (p 1))) (ty list[string])
       (kind
        (Value
         ((mutable_ false) (frame ((scalar_bytes 0) (ptr_words 0)))
          (init
           (Construct
            ((case 0)
             (write_plan
              (((ptr_index 0) (ptr_words 1)
                (discriminant (((Ptr (ptr_index 0)) 0))))))
             (args ()))))))))
      ((id 3) (name prelude.raise) (layout ((s 0) (p 1))) (ty "exn -> int")
       (kind
        (Value
         ((mutable_ false) (frame ((scalar_bytes 0) (ptr_words 0)))
          (init (Intrinsic_val ((intrinsic Raise) (placement Heap))))))))))
    |}]
;;

let%expect_test "closures with direct, transitive, self-recursive, and \
                 polymorphic captures"
  =
  let lower, find_top, _ =
    check_and_lower
      {|
let nested_captures (a : int) (b : string) =
  let f x =
    let g y = Int.add (Int.add a x) (Int.add y (String.length b)) in
    g 1
  in
  f 2

let rec_local_with_capture (k : int) =
  let loop n =
    match Int.equal n 0 with
    | true -> k
    | false -> loop (Int.sub n 1)
  in
  loop 5

let poly_outer (a : 'a @ global) =
  let poly_inner x =
    let _ = a in
    x
  in
  (poly_inner 1, poly_inner 2, poly_inner "hi")

let test_closures =
  (nested_captures 1 "a", rec_local_with_capture 3, poly_outer 10, poly_outer "z")
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "test_closures") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name nested_captures) (params ((slot s0+8 p0+0) (slot s0+0 p0+1)))
       (frame ((scalar_bytes 16) (ptr_words 2))) (env ((s 0) (p 0)))
       (partials (Caller_region)) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Call_indirect
         ((fn_slot (slot s0+0 p1+1))
          (fn
           (Make_closure
            ((fn 1) (placement Heap) (env ((s 8) (p 1)))
             (captures
              (((src (Local (slot s0+8 p0+0))) (dst (place s0+8 p0+0)))
               ((src (Local (slot s0+0 p0+1))) (dst (place s8+0 p0+1))))))))
          (arg_slot (slot s8+8 p0+0)) (arg (Lit (Int 2))) (tail true)))))
      ((id 1) (name f) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 1))) (env ((s 8) (p 1)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Call_indirect
         ((fn_slot (slot s0+0 p0+1))
          (fn
           (Make_closure
            ((fn 2) (placement Heap) (env ((s 16) (p 1)))
             (captures
              (((src (Outer_capture (place s0+8 p0+0))) (dst (place s0+8 p0+0)))
               ((src (Local (slot s0+8 p0+0))) (dst (place s8+8 p0+0)))
               ((src (Outer_capture (place s8+0 p0+1))) (dst (place s16+0 p0+1))))))))
          (arg_slot (slot s8+8 p0+0)) (arg (Lit (Int 1))) (tail true)))))
      ((id 2) (name g) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 56) (ptr_words 1))) (env ((s 16) (p 1)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Prim_op
         ((op Int_add) (arg_slots ((slot s8+8 p0+0) (slot s32+8 p0+0)))
          (args
           ((Prim_op
             ((op Int_add) (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
              (args ((Capture (place s0+8 p0+0)) (Capture (place s8+8 p0+0))))))
            (Prim_op
             ((op Int_add) (arg_slots ((slot s40+8 p0+0) (slot s48+8 p0+0)))
              (args
               ((Local (slot s0+8 p0+0))
                (Prim_op
                 ((op String_length) (arg_slots ((slot s0+0 p0+1)))
                  (args ((Capture (place s16+0 p0+1))))))))))))))))
      ((id 3) (name rec_local_with_capture) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 1))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Call_indirect
         ((fn_slot (slot s0+0 p0+1))
          (fn
           (Make_closure
            ((fn 4) (placement Heap) (env ((s 8) (p 0)))
             (captures
              (((src (Local (slot s0+8 p0+0))) (dst (place s0+8 p0+0))))))))
          (arg_slot (slot s8+8 p0+0)) (arg (Lit (Int 5))) (tail true)))))
      ((id 4) (name loop) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 56) (ptr_words 1))) (env ((s 8) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Match
         ((scrutinee_slot (slot s8+1 p0+0)) (scrutinee_ty bool)
          (scrutinee
           (Prim_op
            ((op Int_equal) (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
             (args ((Local (slot s0+8 p0+0)) (Lit (Int 0)))))))
          (cases
           (((Lit (Bool true)) => (Capture (place s0+8 p0+0)))
            ((Lit (Bool false)) =>
             (Call_indirect
              ((fn_slot (slot s0+0 p0+1)) (fn Self_closure)
               (arg_slot (slot s32+8 p0+0))
               (arg
                (Prim_op
                 ((op Int_sub) (arg_slots ((slot s40+8 p0+0) (slot s48+8 p0+0)))
                  (args ((Local (slot s0+8 p0+0)) (Lit (Int 1)))))))
               (tail true))))))
          (exception_cases ())))))
      ((id 5) (name poly_outer) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 24) (ptr_words 4))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 16) (p 1))) (result_ty "int * int * string")
       (body
        (Tuple
         ((place s0+8 p0+0)
          (Call_indirect
           ((fn_slot (slot s0+0 p0+1))
            (fn
             (Make_closure
              ((fn 6) (placement Region) (env ((s 8) (p 0)))
               (captures
                (((src (Local (slot s0+8 p0+0))) (dst (place s0+8 p0+0))))))))
            (arg_slot (slot s8+8 p0+0)) (arg (Lit (Int 1))) (tail false))))
         ((place s8+8 p0+0)
          (Call_indirect
           ((fn_slot (slot s0+0 p1+1))
            (fn
             (Make_closure
              ((fn 6) (placement Region) (env ((s 8) (p 0)))
               (captures
                (((src (Local (slot s0+8 p0+0))) (dst (place s0+8 p0+0))))))))
            (arg_slot (slot s16+8 p0+0)) (arg (Lit (Int 2))) (tail false))))
         ((place s16+0 p0+1)
          (Call_indirect
           ((fn_slot (slot s0+0 p2+1))
            (fn
             (Make_closure
              ((fn 7) (placement Region) (env ((s 8) (p 0)))
               (captures
                (((src (Local (slot s0+8 p0+0))) (dst (place s0+8 p0+0))))))))
            (arg_slot (slot s0+0 p3+1)) (arg (Lit (String hi))) (tail false)))))))
      ((id 6) (name poly_inner) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 0))) (env ((s 8) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Let
         ((slot (slot s8+8 p0+0)) (body (Capture (place s0+8 p0+0)))
          (in_ (Local (slot s0+8 p0+0)))))))
      ((id 7) (name poly_inner) (params ((slot s0+0 p0+1)))
       (frame ((scalar_bytes 8) (ptr_words 1))) (env ((s 8) (p 0))) (partials ())
       (result ((s 0) (p 1))) (result_ty string)
       (body
        (Let
         ((slot (slot s0+8 p0+0)) (body (Capture (place s0+8 p0+0)))
          (in_ (Local (slot s0+0 p0+1)))))))
      ((id 8) (name poly_outer) (params ((slot s0+0 p0+1)))
       (frame ((scalar_bytes 16) (ptr_words 5))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 16) (p 1))) (result_ty "int * int * string")
       (body
        (Tuple
         ((place s0+8 p0+0)
          (Call_indirect
           ((fn_slot (slot s0+0 p1+1))
            (fn
             (Make_closure
              ((fn 9) (placement Region) (env ((s 0) (p 1)))
               (captures
                (((src (Local (slot s0+0 p0+1))) (dst (place s0+0 p0+1))))))))
            (arg_slot (slot s0+8 p0+0)) (arg (Lit (Int 1))) (tail false))))
         ((place s8+8 p0+0)
          (Call_indirect
           ((fn_slot (slot s0+0 p2+1))
            (fn
             (Make_closure
              ((fn 9) (placement Region) (env ((s 0) (p 1)))
               (captures
                (((src (Local (slot s0+0 p0+1))) (dst (place s0+0 p0+1))))))))
            (arg_slot (slot s8+8 p0+0)) (arg (Lit (Int 2))) (tail false))))
         ((place s16+0 p0+1)
          (Call_indirect
           ((fn_slot (slot s0+0 p3+1))
            (fn
             (Make_closure
              ((fn 10) (placement Region) (env ((s 0) (p 1)))
               (captures
                (((src (Local (slot s0+0 p0+1))) (dst (place s0+0 p0+1))))))))
            (arg_slot (slot s0+0 p4+1)) (arg (Lit (String hi))) (tail false)))))))
      ((id 9) (name poly_inner) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 8) (ptr_words 1))) (env ((s 0) (p 1))) (partials ())
       (result ((s 8) (p 0))) (result_ty int)
       (body
        (Let
         ((slot (slot s0+0 p0+1)) (body (Capture (place s0+0 p0+1)))
          (in_ (Local (slot s0+8 p0+0)))))))
      ((id 10) (name poly_inner) (params ((slot s0+0 p0+1)))
       (frame ((scalar_bytes 0) (ptr_words 2))) (env ((s 0) (p 1))) (partials ())
       (result ((s 0) (p 1))) (result_ty string)
       (body
        (Let
         ((slot (slot s0+0 p1+1)) (body (Capture (place s0+0 p0+1)))
          (in_ (Local (slot s0+0 p0+1)))))))))
    (globals
     (((id 0) (name test_closures) (layout ((s 48) (p 2)))
       (ty "int * int * (int * int * string) * (int * int * string)")
       (kind
        (Value
         ((mutable_ false) (frame ((scalar_bytes 24) (ptr_words 2)))
          (init
           (Tuple
            ((place s0+8 p0+0)
             (Call_direct
              ((fn 0) (tail false)
               (arg_slots ((slot s0+8 p0+0) (slot s0+0 p0+1)))
               (args ((Lit (Int 1)) (Lit (String a)))))))
            ((place s8+8 p0+0)
             (Call_direct
              ((fn 3) (tail false) (arg_slots ((slot s8+8 p0+0)))
               (args ((Lit (Int 3)))))))
            ((place s16+16 p0+1)
             (Call_direct
              ((fn 5) (tail false) (arg_slots ((slot s16+8 p0+0)))
               (args ((Lit (Int 10)))))))
            ((place s32+16 p1+1)
             (Call_direct
              ((fn 8) (tail false) (arg_slots ((slot s0+0 p1+1)))
               (args ((Lit (String z)))))))))))))))
    |}]
;;

let%expect_test "Quote with persistent local captures and depth-1 splices" =
  let lower, find_top, _ =
    check_and_lower
      {|
let power (n : int) (x : <int>{}) : <int>{} =
  match Int.equal n 0 with
  | true -> .< 1 >.
  | false -> .< Int.mul .~x .~(power (Int.sub n 1) x) >.

let quote_capture (n : int) =
  .< Int.add n 1 >.
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "power") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "quote_capture") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name power) (params ((slot s0+8 p0+0) (slot s0+0 p0+1)))
       (frame ((scalar_bytes 56) (ptr_words 4))) (env ((s 0) (p 0)))
       (partials (Heap)) (result ((s 0) (p 1))) (result_ty <int>{})
       (body
        (Match
         ((scrutinee_slot (slot s8+1 p0+0)) (scrutinee_ty bool)
          (scrutinee
           (Prim_op
            ((op Int_equal) (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
             (args ((Local (slot s0+8 p0+0)) (Lit (Int 0)))))))
          (cases
           (((Lit (Bool true)) =>
             (Quote ((placement Heap) (splices ()) (captures ()))))
            ((Lit (Bool false)) =>
             (Quote
              ((placement Heap)
               (splices
                (((slot s0+0 p1+1) (Local (slot s0+0 p0+1)))
                 ((slot s0+0 p3+1)
                  (Call_direct
                   ((fn 0) (tail false)
                    (arg_slots ((slot s32+8 p0+0) (slot s0+0 p2+1)))
                    (args
                     ((Prim_op
                       ((op Int_sub)
                        (arg_slots ((slot s40+8 p0+0) (slot s48+8 p0+0)))
                        (args ((Local (slot s0+8 p0+0)) (Lit (Int 1))))))
                      (Local (slot s0+0 p0+1)))))))))
               (captures ()))))))
          (exception_cases ())))))
      ((id 1) (name quote_capture) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 8) (ptr_words 0))) (env ((s 0) (p 0))) (partials ())
       (result ((s 0) (p 1))) (result_ty <int>{})
       (body
        (Quote
         ((placement Heap) (splices ())
          (captures
           (((name n)
             (kind
              (Value ((src (Local (slot s0+8 p0+0))) (is_cell false) (ty int)))))))))))))
    (globals
     (((id 0) (name power) (layout ((s 0) (p 1)))
       (ty "int -> <int>{} -> <int>{}") (kind (Function 0)))
      ((id 1) (name quote_capture) (layout ((s 0) (p 1))) (ty "int -> <int>{}")
       (kind (Function 1)))))
    |}]
;;

let%expect_test "partial application, over-application, and compile_expr with \
                 bindings"
  =
  let lower, find_top, checked =
    check_and_lower
      {|
let add2 (x : int) (y : int) : int = Int.add x y
let make_adder (x : int) =
  print_int x;
  fun (y : int) -> Int.add x y

let test_apps =
  let p = add2 1 in
  (p 2, make_adder 3 4)
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "test_apps") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  (* Also test [compile_expr] on a toplevel's body with explicit bindings. *)
  let add2_top =
    List.find_map_exn checked.program.sccs ~f:(fun scc ->
      List.find scc.members ~f:(fun top -> String.equal top.ident.name "add2"))
  in
  let params =
    match add2_top.kind with
    | Function args -> Nonempty_list.to_list args
    | Value | Mut -> assert false
  in
  let bindings = List.map params ~f:(fun ident -> ident, Type.int) in
  let unit =
    Lower.compile_expr lower ~bindings add2_top.body |> Or_error.ok_exn
  in
  print_s
    [%message
      "compile_expr"
        ~bindings:
          (Array.map unit.bindings ~f:(fun (id, s) -> id.name, Pretty.slot s)
           : (string * Sexp.t) array)
        ~frame:(unit.frame : Frame_layout.t)
        ~result:(Pretty.layout_size unit.result_layout : Sexp.t)
        ~body:(Pretty.expr unit.body : Sexp.t)];
  [%expect
    {|
    (fns
     (((id 0) (name add2) (params ((slot s0+8 p0+0) (slot s8+8 p0+0)))
       (frame ((scalar_bytes 32) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials (Caller_region)) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Prim_op
         ((op Int_add) (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
          (args ((Local (slot s0+8 p0+0)) (Local (slot s8+8 p0+0))))))))
      ((id 1) (name make_adder) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 0) (p 1))) (result_ty "int -> int")
       (body
        (Seq
         ((first
           (Prim_call ((prim print_int) (args ((Local (slot s0+8 p0+0)))))))
          (second
           (Make_closure
            ((fn 2) (placement Caller_region) (env ((s 8) (p 0)))
             (captures
              (((src (Local (slot s0+8 p0+0))) (dst (place s0+8 p0+0))))))))))))
      ((id 2) (name <fun>) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 24) (ptr_words 0))) (env ((s 8) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Prim_op
         ((op Int_add) (arg_slots ((slot s8+8 p0+0) (slot s16+8 p0+0)))
          (args ((Capture (place s0+8 p0+0)) (Local (slot s0+8 p0+0))))))))))
    (globals
     (((id 0) (name test_apps) (layout ((s 16) (p 0))) (ty "int * int")
       (kind
        (Value
         ((mutable_ false) (frame ((scalar_bytes 32) (ptr_words 4)))
          (init
           (Let
            ((slot (slot s0+0 p0+1))
             (body
              (Call_indirect
               ((fn_slot (slot s0+0 p1+1)) (fn (Global 1))
                (arg_slot (slot s0+8 p0+0)) (arg (Lit (Int 1))) (tail false))))
             (in_
              (Tuple
               ((place s0+8 p0+0)
                (Call_indirect
                 ((fn_slot (slot s0+0 p2+1)) (fn (Local (slot s0+0 p0+1)))
                  (arg_slot (slot s8+8 p0+0)) (arg (Lit (Int 2))) (tail false))))
               ((place s8+8 p0+0)
                (Call_indirect
                 ((fn_slot (slot s0+0 p3+1))
                  (fn
                   (Call_direct
                    ((fn 1) (tail false) (arg_slots ((slot s16+8 p0+0)))
                     (args ((Lit (Int 3)))))))
                  (arg_slot (slot s24+8 p0+0)) (arg (Lit (Int 4))) (tail false)))))))))))))
      ((id 1) (name add2) (layout ((s 0) (p 1))) (ty "int -> int -> int")
       (kind (Function 0)))))
    (compile_expr (bindings ((x (slot s0+8 p0+0)) (y (slot s8+8 p0+0))))
     (frame ((scalar_bytes 32) (ptr_words 0))) (result ((s 8) (p 0)))
     (body
      (Prim_op
       ((op Int_add) (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
        (args ((Local (slot s0+8 p0+0)) (Local (slot s8+8 p0+0))))))))
    |}]
;;

let%expect_test "polymorphic insertion sort at list[int] and list[string]" =
  let lower, find_top, _ =
    check_and_lower
      {|
let insert cmp x xs =
  match xs with
  | Nil -> [x]
  | Cons cell ->
    match cell.* with
    | (y, ys) ->
      match Int.le (cmp x y) 0 with
      | true -> Cons (Box.make (x, xs))
      | false -> Cons (Box.make (y, insert cmp x ys))

let sort cmp xs =
  match xs with
  | Nil -> Nil
  | Cons cell ->
    match cell.* with
    | (x, xs) -> insert cmp x (sort cmp xs)

let test_sort =
  let a = sort Int.compare [3; 1; 2] in
  let b = sort Int.compare [5; 4] in
  let c = sort String.compare ["b"; "a"] in
  (a, b, c)
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "test_sort") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  let summary =
    List.map (Lower.fns lower) ~f:(fun fn ->
      fn.id, fn.name, fn.frame, fn.result_ty)
  in
  print_s
    [%sexp
      (summary
       : (Compiled.Fn_id.t * string * Frame_layout.t * Mono_type.Mono_ty.t) list)];
  [%expect
    {|
    ((0 sort ((scalar_bytes 24) (ptr_words 11)) list[int])
     (1 insert ((scalar_bytes 96) (ptr_words 17)) list[int])
     (2 sort ((scalar_bytes 0) (ptr_words 14)) list[string])
     (3 insert ((scalar_bytes 24) (ptr_words 26)) list[string]))
    |}]
;;

let%expect_test "Mono_type.Key handles equi-recursive cycles canonically" =
  (* Two structurally identical cyclic records with different ids must produce
     the exact same [Mono_type.Key.t]. *)
  let make_cyclic id1 id2 =
    let r = Type.Var.create id1 (Unbound 1) in
    let rec_ty =
      Type.TRecord
        { id = id2
        ; row =
            Type.RExtend
              ( "next"
              , Type.Present
              , Type.Absent
              , Type.ptr (Type.TVar r)
              , Type.RExtend
                  ("v", Type.Present, Type.Absent, Type.int, Type.REmpty) )
        }
    in
    Type.Var.set_state r (Link rec_ty);
    rec_ty
  in
  let next_id = ref (-1000) in
  let fresh_id () =
    decr next_id;
    !next_id
  in
  let t1 = make_cyclic 100 101 in
  let t2 = make_cyclic 200 201 in
  let k1 = Mono_type.Subst.key_of_ty Mono_type.Subst.empty ~fresh_id t1 in
  let k2 = Mono_type.Subst.key_of_ty Mono_type.Subst.empty ~fresh_id t2 in
  print_s
    [%message
      "cyclic keys"
        (k1 : Mono_type.Key.t)
        ~equal:(Mono_type.Key.equal k1 k2 : bool)
        ~same_hash:
          (Int.equal (Mono_type.Key.hash k1) (Mono_type.Key.hash k2) : bool)];
  [%expect
    {|
    ("cyclic keys"
     (k1 (Mu (Record ((next (Con ptr ((Ty (Var 0))))) (v (Con int ()))))))
     (equal true) (same_hash true))
    |}]
;;

let%expect_test "A1: type-changing Record_update and Poly_update copy \
                 unchanged fields into result layout"
  =
  let lower, find_top, _ =
    check_and_lower
      {|
type bx['a] = { v : 'a; n : int }

let update_nominal (b : bx[int]) : bx[string] =
  { b with v = "s" }

let update_poly (r : { `x : int; `y : int }) : { `x : string; `y : int } =
  { r with `x = "s" }
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "update_nominal") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "update_poly") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name update_nominal) (params ((slot s0+16 p0+0)))
       (frame ((scalar_bytes 32) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 1))) (result_ty bx[string])
       (body
        (Record_update
         ((base_slot (slot s16+16 p0+0)) (base (Local (slot s0+16 p0+0)))
          (copy_fields (((place s8+8 p0+0) -> (place s0+8 p1+0))))
          (updates (((place s8+0 p0+1) (Lit (String s)))))))))
      ((id 1) (name update_poly) (params ((slot s0+16 p0+0)))
       (frame ((scalar_bytes 32) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 1))) (result_ty "{`x : string; `y : int}")
       (body
        (Record_update
         ((base_slot (slot s16+16 p0+0)) (base (Local (slot s0+16 p0+0)))
          (copy_fields (((place s8+8 p0+0) -> (place s0+8 p1+0))))
          (updates (((place s8+0 p0+1) (Lit (String s)))))))))))
    (globals
     (((id 0) (name update_nominal) (layout ((s 0) (p 1)))
       (ty "bx[int] -> bx[string]") (kind (Function 0)))
      ((id 1) (name update_poly) (layout ((s 0) (p 1)))
       (ty "{`x : int; `y : int} -> {`x : string; `y : int}")
       (kind (Function 1)))))
    |}]
;;

let%expect_test "A2: captureless local recursive fn called from inside a \
                 nested closure (mono and generic)"
  =
  let lower, find_top, _ =
    check_and_lower
      {|
let test_self_in_closure (x : int) : int =
  let rec_mono (n : int) : int =
    match Int.le n 0 with
    | true -> 0
    | false -> (fun (u : unit) -> rec_mono (Int.sub n 1)) ()
  in
  let rec_poly n v =
    match Int.le n 0 with
    | true -> v
    | false -> (fun (u : unit) -> rec_poly (Int.sub n 1) v) ()
  in
  Int.add (rec_mono x) (Int.add (rec_poly x 1) (String.length (rec_poly x "hi")))
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "test_self_in_closure") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name test_self_in_closure) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 72) (ptr_words 2))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Prim_op
         ((op Int_add) (arg_slots ((slot s8+8 p0+0) (slot s24+8 p0+0)))
          (args
           ((Call_direct
             ((fn 1) (tail false) (arg_slots ((slot s16+8 p0+0)))
              (args ((Local (slot s0+8 p0+0))))))
            (Prim_op
             ((op Int_add) (arg_slots ((slot s32+8 p0+0) (slot s56+8 p0+0)))
              (args
               ((Call_direct
                 ((fn 3) (tail false)
                  (arg_slots ((slot s40+8 p0+0) (slot s48+8 p0+0)))
                  (args ((Local (slot s0+8 p0+0)) (Lit (Int 1))))))
                (Prim_op
                 ((op String_length) (arg_slots ((slot s0+0 p0+1)))
                  (args
                   ((Call_direct
                     ((fn 5) (tail false)
                      (arg_slots ((slot s64+8 p0+0) (slot s0+0 p1+1)))
                      (args ((Local (slot s0+8 p0+0)) (Lit (String hi))))))))))))))))))))
      ((id 1) (name rec_mono) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 32) (ptr_words 1))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Match
         ((scrutinee_slot (slot s8+1 p0+0)) (scrutinee_ty bool)
          (scrutinee
           (Prim_op
            ((op Int_le) (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
             (args ((Local (slot s0+8 p0+0)) (Lit (Int 0)))))))
          (cases
           (((Lit (Bool true)) => (Lit (Int 0)))
            ((Lit (Bool false)) =>
             (Call_indirect
              ((fn_slot (slot s0+0 p0+1))
               (fn
                (Make_closure
                 ((fn 2) (placement Heap) (env ((s 8) (p 0)))
                  (captures
                   (((src (Local (slot s0+8 p0+0))) (dst (place s0+8 p0+0))))))))
               (arg_slot (slot s0+0 p0+0)) (arg (Lit Unit)) (tail true))))))
          (exception_cases ())))))
      ((id 2) (name <fun>) (params ((slot s0+0 p0+0)))
       (frame ((scalar_bytes 24) (ptr_words 0))) (env ((s 8) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Call_direct
         ((fn 1) (tail true) (arg_slots ((slot s0+8 p0+0)))
          (args
           ((Prim_op
             ((op Int_sub) (arg_slots ((slot s8+8 p0+0) (slot s16+8 p0+0)))
              (args ((Capture (place s0+8 p0+0)) (Lit (Int 1))))))))))))
      ((id 3) (name rec_poly) (params ((slot s0+8 p0+0) (slot s8+8 p0+0)))
       (frame ((scalar_bytes 40) (ptr_words 1))) (env ((s 0) (p 0)))
       (partials (Caller_region)) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Match
         ((scrutinee_slot (slot s16+1 p0+0)) (scrutinee_ty bool)
          (scrutinee
           (Prim_op
            ((op Int_le) (arg_slots ((slot s24+8 p0+0) (slot s32+8 p0+0)))
             (args ((Local (slot s0+8 p0+0)) (Lit (Int 0)))))))
          (cases
           (((Lit (Bool true)) => (Local (slot s8+8 p0+0)))
            ((Lit (Bool false)) =>
             (Call_indirect
              ((fn_slot (slot s0+0 p0+1))
               (fn
                (Make_closure
                 ((fn 4) (placement Heap) (env ((s 16) (p 0)))
                  (captures
                   (((src (Local (slot s0+8 p0+0))) (dst (place s0+8 p0+0)))
                    ((src (Local (slot s8+8 p0+0))) (dst (place s8+8 p0+0))))))))
               (arg_slot (slot s0+0 p0+0)) (arg (Lit Unit)) (tail true))))))
          (exception_cases ())))))
      ((id 4) (name <fun>) (params ((slot s0+0 p0+0)))
       (frame ((scalar_bytes 32) (ptr_words 0))) (env ((s 16) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Call_direct
         ((fn 3) (tail true) (arg_slots ((slot s0+8 p0+0) (slot s24+8 p0+0)))
          (args
           ((Prim_op
             ((op Int_sub) (arg_slots ((slot s8+8 p0+0) (slot s16+8 p0+0)))
              (args ((Capture (place s0+8 p0+0)) (Lit (Int 1))))))
            (Capture (place s8+8 p0+0))))))))
      ((id 5) (name rec_poly) (params ((slot s0+8 p0+0) (slot s0+0 p0+1)))
       (frame ((scalar_bytes 32) (ptr_words 2))) (env ((s 0) (p 0)))
       (partials (Caller_region)) (result ((s 0) (p 1))) (result_ty string)
       (body
        (Match
         ((scrutinee_slot (slot s8+1 p0+0)) (scrutinee_ty bool)
          (scrutinee
           (Prim_op
            ((op Int_le) (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
             (args ((Local (slot s0+8 p0+0)) (Lit (Int 0)))))))
          (cases
           (((Lit (Bool true)) => (Local (slot s0+0 p0+1)))
            ((Lit (Bool false)) =>
             (Call_indirect
              ((fn_slot (slot s0+0 p1+1))
               (fn
                (Make_closure
                 ((fn 6) (placement Heap) (env ((s 8) (p 1)))
                  (captures
                   (((src (Local (slot s0+8 p0+0))) (dst (place s0+8 p0+0)))
                    ((src (Local (slot s0+0 p0+1))) (dst (place s8+0 p0+1))))))))
               (arg_slot (slot s0+0 p0+0)) (arg (Lit Unit)) (tail true))))))
          (exception_cases ())))))
      ((id 6) (name <fun>) (params ((slot s0+0 p0+0)))
       (frame ((scalar_bytes 24) (ptr_words 1))) (env ((s 8) (p 1)))
       (partials ()) (result ((s 0) (p 1))) (result_ty string)
       (body
        (Call_direct
         ((fn 5) (tail true) (arg_slots ((slot s0+8 p0+0) (slot s0+0 p0+1)))
          (args
           ((Prim_op
             ((op Int_sub) (arg_slots ((slot s8+8 p0+0) (slot s16+8 p0+0)))
              (args ((Capture (place s0+8 p0+0)) (Lit (Int 1))))))
            (Capture (place s8+0 p0+1))))))))))
    (globals
     (((id 0) (name test_self_in_closure) (layout ((s 0) (p 1)))
       (ty "int -> int") (kind (Function 0)))))
    |}]
;;

let%expect_test "A3: generic function inside inlined generalized value gets \
                 distinct specializations per instance"
  =
  let lower, find_top, _ =
    check_and_lower
      {|
let test_gen_val_fn (u : unit) : int =
  let v =
    match None with
    | z ->
      let h x = (z, x) in
      h
  in
  let a : option[int] * int = v 1 in
  let b : option[string] * string = v "hi" in
  match (a, b) with
  | ((None, n), (None, s)) -> Int.add n (String.length s)
  | _ -> 0
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "test_gen_val_fn") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  let h_fns =
    List.filter (Lower.fns lower) ~f:(fun fn -> String.equal fn.name "h")
    |> List.map ~f:(fun fn -> fn.id, fn.env_layout, fn.result_ty)
  in
  print_s
    [%message
      "h specializations"
        ~_:(h_fns : (Compiled.Fn_id.t * Layout.t * Mono_type.Mono_ty.t) list)];
  [%expect
    {|
    ("h specializations"
     ((1
       ((scalar_bytes 16) (scalar_align 8) (ptr_words 0) (inhabited true)
        (niche
         ((leaf (Scalar (scalar_offset 0) (scalar I8))) (start 2)
          (available 254)))
        (desc
         (Struct (repr Default)
          (places
           (((scalar_offset 0) (ptr_index 0)
             (layout
              ((scalar_bytes 16) (scalar_align 8) (ptr_words 0) (inhabited true)
               (niche
                ((leaf (Scalar (scalar_offset 0) (scalar I8))) (start 2)
                 (available 254)))
               (desc
                (Union
                 (cases
                  (((scalar_offset 1) (ptr_index 0)
                    (layout
                     ((scalar_bytes 0) (scalar_align 1) (ptr_words 0)
                      (inhabited true)
                      (desc (Struct (repr Default) (places ()) (reserved 0))))))
                   ((scalar_offset 8) (ptr_index 0)
                    (layout
                     ((scalar_bytes 8) (scalar_align 8) (ptr_words 0)
                      (inhabited true)
                      (desc
                       (Struct (repr Default)
                        (places
                         (((scalar_offset 0) (ptr_index 0)
                           (layout
                            ((scalar_bytes 8) (scalar_align 8) (ptr_words 0)
                             (inhabited true) (desc (Scalar I64)))))))
                        (reserved 0))))))))
                 (discriminant (Tag (Scalar (scalar_offset 0) (scalar I8)))))))))))
          (reserved 0))))
       "option[int] * int")
      (2
       ((scalar_bytes 0) (scalar_align 1) (ptr_words 1) (inhabited true)
        (niche ((leaf (Ptr (ptr_index 0))) (start 1) (available 4095)))
        (desc
         (Struct (repr Default)
          (places
           (((scalar_offset 0) (ptr_index 0)
             (layout
              ((scalar_bytes 0) (scalar_align 1) (ptr_words 1) (inhabited true)
               (niche ((leaf (Ptr (ptr_index 0))) (start 1) (available 4095)))
               (desc
                (Union
                 (cases
                  (((scalar_offset 0) (ptr_index 0)
                    (layout
                     ((scalar_bytes 0) (scalar_align 1) (ptr_words 0)
                      (inhabited true)
                      (desc (Struct (repr Default) (places ()) (reserved 0))))))
                   ((scalar_offset 0) (ptr_index 0)
                    (layout
                     ((scalar_bytes 0) (scalar_align 1) (ptr_words 1)
                      (inhabited true)
                      (niche
                       ((leaf (Ptr (ptr_index 0))) (start 0) (available 4096)))
                      (desc
                       (Struct (repr Default)
                        (places
                         (((scalar_offset 0) (ptr_index 0)
                           (layout
                            ((scalar_bytes 0) (scalar_align 1) (ptr_words 1)
                             (inhabited true)
                             (niche
                              ((leaf (Ptr (ptr_index 0))) (start 0)
                               (available 4096)))
                             (desc Ptr))))))
                        (reserved 0))))))))
                 (discriminant
                  (Niche (leaf (Ptr (ptr_index 0))) (untagged 1) (first 0)
                   (last 0) (start 0))))))))))
          (reserved 0))))
       "option[string] * string")))
    |}]
;;

let%expect_test "A4: Quote captures Known_fn, Generic_fn, Generic_value, \
                 Outer_capture, nested quotes, Explicit and Implicit subs"
  =
  let lower, find_top, _ =
    check_and_lower
      {|
let test_quote_captures (n : int) =
  let known_mono (x : int @ global) : int @ global = Int.add x 1 in
  let rec_fn (k : int) : int =
    match Int.le k 0 with
    | true -> 0
    | false -> rec_fn (Int.sub k 1)
  in
  let poly x = (n, x) in
  let gval = None in
  let base_code = .< known_mono (rec_fn (fst (poly 1))) >. in
  let q_splice = .< .~(base_code) >. in
  let q_gval = .< (gval : option[int]) >. in
  let open_code = .< Int.add x 1 >. in
  let q_implicit = .< let x = 1 in .~(open_code) >. in
  let q_explicit_sub = .< .~(open_code).{x = Int.add n 2} >. in
  let q_nested = .< let y = 1 in .< Int.add n y >. >. in
  let q_in_closure = (fun (u : unit) -> .< Int.add n 3 >.) () in
  (q_splice, q_gval, q_implicit, q_explicit_sub, q_nested, q_in_closure)
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "test_quote_captures") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name test_quote_captures) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 8) (ptr_words 9))) (env ((s 0) (p 0))) (partials ())
       (result ((s 0) (p 6)))
       (result_ty
        "<int>{fst : immut int * int -> int} * <option[int]>{} * <int>{} * <int>{} * <<int>{}>{} * <int>{}")
       (body
        (Let
         ((slot (slot s0+0 p0+1))
          (body
           (Quote
            ((placement Heap) (splices ())
             (captures
              (((name known_mono)
                (kind
                 (Generic_fn ((params (x)) (scheme "int -> int") (captures ())))))
               ((name rec_fn)
                (kind
                 (Generic_fn ((params (k)) (scheme "int -> int") (captures ())))))
               ((name poly)
                (kind
                 (Generic_fn
                  ((params (x)) (scheme "'a -> int * 'a")
                   (captures
                    (((name n)
                      (kind
                       (Value
                        ((src (Local (slot s0+8 p0+0))) (is_cell false) (ty int))))))))))))))))
          (in_
           (Let
            ((slot (slot s0+0 p1+1))
             (body
              (Quote
               ((placement Heap)
                (splices (((slot s0+0 p2+1) (Local (slot s0+0 p0+1)))))
                (captures ()))))
             (in_
              (Let
               ((slot (slot s0+0 p3+1))
                (body
                 (Quote
                  ((placement Heap)
                   (splices
                    (((slot s0+0 p4+1)
                      (Quote ((placement Heap) (splices ()) (captures ()))))))
                   (captures
                    (((name n)
                      (kind
                       (Value
                        ((src (Local (slot s0+8 p0+0))) (is_cell false) (ty int))))))))))
                (in_
                 (Let
                  ((slot (slot s0+0 p5+1))
                   (body
                    (Quote
                     ((placement Heap) (splices ())
                      (captures
                       (((name n)
                         (kind
                          (Value
                           ((src (Local (slot s0+8 p0+0))) (is_cell false)
                            (ty int))))))))))
                   (in_
                    (Let
                     ((slot (slot s0+0 p6+1))
                      (body
                       (Call_indirect
                        ((fn_slot (slot s0+0 p7+1))
                         (fn
                          (Make_closure
                           ((fn 1) (placement Region) (env ((s 8) (p 0)))
                            (captures
                             (((src (Local (slot s0+8 p0+0)))
                               (dst (place s0+8 p0+0))))))))
                         (arg_slot (slot s0+0 p0+0)) (arg (Lit Unit))
                         (tail false))))
                      (in_
                       (Tuple ((place s0+0 p0+1) (Local (slot s0+0 p1+1)))
                        ((place s0+0 p1+1)
                         (Quote
                          ((placement Heap) (splices ())
                           (captures
                            (((name gval)
                              (kind
                               (Generic_value
                                ((scheme option['a]) (captures ()))))))))))
                        ((place s0+0 p2+1)
                         (Quote
                          ((placement Heap)
                           (splices
                            (((slot s0+0 p8+1)
                              (Quote
                               ((placement Heap) (splices ()) (captures ()))))))
                           (captures ()))))
                        ((place s0+0 p3+1) (Local (slot s0+0 p3+1)))
                        ((place s0+0 p4+1) (Local (slot s0+0 p5+1)))
                        ((place s0+0 p5+1) (Local (slot s0+0 p6+1)))))))))))))))))))))
      ((id 1) (name <fun>) (params ((slot s0+0 p0+0)))
       (frame ((scalar_bytes 0) (ptr_words 0))) (env ((s 8) (p 0))) (partials ())
       (result ((s 0) (p 1))) (result_ty <int>{})
       (body
        (Quote
         ((placement Heap) (splices ())
          (captures
           (((name n)
             (kind
              (Value
               ((src (Outer_capture (place s0+8 p0+0))) (is_cell false) (ty int)))))))))))))
    (globals
     (((id 0) (name test_quote_captures) (layout ((s 0) (p 1)))
       (ty
        "int -> <int>{fst : immut int * int -> int} * <option[int]>{} * <int>{} * <int>{} * <<int>{}>{} * <int>{}")
       (kind (Function 0)))))
    |}]
;;

let%expect_test "A5 & B2: mode-polymorphic fn with nested generalized let \
                 creates distinct mode-pinned instances"
  =
  let lower, find_top, _ =
    check_and_lower
      {|
let outer (x : int) =
  let inner (y : int) = Box.make (Int.add x y) in
  inner x

let use_global (n : int) : box[int] = outer n

let use_local (n : int) : int =
  region_ (
    let b @ local = outer n in
    b.*
  )
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "use_global") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "use_local") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name use_global) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 0) (p 1))) (result_ty box[int])
       (body
        (Call_direct
         ((fn 1) (tail true) (arg_slots ((slot s8+8 p0+0)))
          (args ((Local (slot s0+8 p0+0))))))))
      ((id 1) (name outer) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 1))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 0) (p 1))) (result_ty box[int])
       (body
        (Call_indirect
         ((fn_slot (slot s0+0 p0+1))
          (fn
           (Make_closure
            ((fn 2) (placement Heap) (env ((s 8) (p 0)))
             (captures
              (((src (Local (slot s0+8 p0+0))) (dst (place s0+8 p0+0))))))))
          (arg_slot (slot s8+8 p0+0)) (arg (Local (slot s0+8 p0+0))) (tail true)))))
      ((id 2) (name inner) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 32) (ptr_words 0))) (env ((s 8) (p 0)))
       (partials ()) (result ((s 0) (p 1))) (result_ty box[int])
       (body
        (Box_make
         ((placement Heap) (content ((s 8) (p 0)))
          (value
           (Prim_op
            ((op Int_add) (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
             (args ((Capture (place s0+8 p0+0)) (Local (slot s0+8 p0+0)))))))))))
      ((id 3) (name use_local) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 2))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Region
         ((clear_ptrs (0 2))
          (body
           (Let
            ((slot (slot s0+0 p0+1))
             (body
              (Call_direct
               ((fn 4) (tail false) (arg_slots ((slot s8+8 p0+0)))
                (args ((Local (slot s0+8 p0+0)))))))
             (in_
              (Deref ((content ((s 8) (p 0))) (ptr (Local (slot s0+0 p0+1)))))))))))))
      ((id 4) (name outer) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 1))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 0) (p 1))) (result_ty box[int])
       (body
        (Call_indirect
         ((fn_slot (slot s0+0 p0+1))
          (fn
           (Make_closure
            ((fn 5) (placement Heap) (env ((s 8) (p 0)))
             (captures
              (((src (Local (slot s0+8 p0+0))) (dst (place s0+8 p0+0))))))))
          (arg_slot (slot s8+8 p0+0)) (arg (Local (slot s0+8 p0+0))) (tail true)))))
      ((id 5) (name inner) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 32) (ptr_words 0))) (env ((s 8) (p 0)))
       (partials ()) (result ((s 0) (p 1))) (result_ty box[int])
       (body
        (Box_make
         ((placement Caller_region) (content ((s 8) (p 0)))
          (value
           (Prim_op
            ((op Int_add) (arg_slots ((slot s16+8 p0+0) (slot s24+8 p0+0)))
             (args ((Capture (place s0+8 p0+0)) (Local (slot s0+8 p0+0)))))))))))))
    (globals
     (((id 0) (name use_global) (layout ((s 0) (p 1))) (ty "int -> box[int]")
       (kind (Function 0)))
      ((id 1) (name use_local) (layout ((s 0) (p 1))) (ty "int -> int")
       (kind (Function 3)))))
    |}]
;;

let%expect_test "B1: transactional rollback leaves Lower clean after error" =
  let lower, find_top, checked =
    check_and_lower
      {|
let good (x : int) : int = Int.add x 1
let calls_good (x : int) : int = good x
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "good") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  let fns_before = List.length (Lower.fns lower) in
  let globals_before = List.length (Lower.globals lower) in
  (* Build a standalone expression that first specializes [calls_good] (adding
     new fn/global entries during lowering) and then references an unbound local
     ident so lowering fails mid-flight. *)
  let calls_good_ident = find_top "calls_good" in
  let calls_good_top =
    List.find_map_exn checked.program.sccs ~f:(fun scc ->
      List.find scc.members ~f:(fun top ->
        Typed.Ident.equal top.ident calls_good_ident))
  in
  let bogus_ident : Typed.Ident.t =
    { name = "unbound_bogus"; stamp = 999_999 }
  in
  let dummy_loc = Loc.none in
  let mk desc ty : Typed.Expr.t =
    { desc; ty; loc = dummy_loc; made = Typed.Expr.Made.none }
  in
  let call_expr =
    mk
      (App
         ( mk (Var (Bound (Toplevel calls_good_ident))) calls_good_top.scheme
         , mk (Lit (Int 1)) Type.int ))
      Type.int
  in
  let bad_expr =
    mk (Seq (call_expr, mk (Var (Bound (Local bogus_ident))) Type.int)) Type.int
  in
  let result = Lower.compile_expr lower bad_expr in
  let fns_after = List.length (Lower.fns lower) in
  let globals_after = List.length (Lower.globals lower) in
  (* Subsequent valid specialization still works cleanly with contiguous ids. *)
  let gid =
    Lower.specialize_toplevel lower ~ident:calls_good_ident ()
    |> Or_error.ok_exn
  in
  let all_fns_valid =
    List.for_all (Lower.fns lower) ~f:(fun f ->
      Compiled.Fn_id.equal (Lower.fn lower f.id).id f.id)
  in
  print_s
    [%message
      "rollback"
        ~failed:(Result.is_error result : bool)
        (fns_before : int)
        (fns_after : int)
        (globals_before : int)
        (globals_after : int)
        ~subsequent_gid:(gid : Compiled.Global_id.t)
        ~fns_final:(List.length (Lower.fns lower) : int)
        (all_fns_valid : bool)];
  [%expect
    {|
    (rollback (failed true) (fns_before 1) (fns_after 1) (globals_before 1)
     (globals_after 1) (subsequent_gid 1) (fns_final 3) (all_fns_valid true))
    |}]
;;

let%expect_test "Or patterns share exact frame slots across branches" =
  let lower, find_top, _ =
    check_and_lower
      {|
type choice =
  | Left of int * string
  | Right of string * int

let extract (c : choice) : int =
  match c with
  | Left (n, s) | Right (s, n) -> Int.add n (String.length s)
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "extract") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name extract) (params ((slot s0+8 p0+2)))
       (frame ((scalar_bytes 40) (ptr_words 6))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Match
         ((scrutinee_slot (slot s8+8 p2+2)) (scrutinee_ty choice)
          (scrutinee (Local (slot s0+8 p0+2)))
          (cases
           (((Or
              (Construct
               ((case 0)
                (discriminant
                 (Niche (leaf (Ptr (ptr_index 0))) (untagged 0) (first 1)
                  (last 1) (start 0)))
                (args
                 (((place s0+8 p0+0) (Bind (slot s16+8 p0+0)))
                  ((place s8+0 p0+1) (Bind (slot s0+0 p4+1)))))))
              (Construct
               ((case 1)
                (discriminant
                 (Niche (leaf (Ptr (ptr_index 0))) (untagged 0) (first 1)
                  (last 1) (start 0)))
                (args
                 (((place s8+0 p1+1) (Bind (slot s0+0 p4+1)))
                  ((place s0+8 p2+0) (Bind (slot s16+8 p0+0))))))))
             =>
             (Prim_op
              ((op Int_add) (arg_slots ((slot s24+8 p0+0) (slot s32+8 p0+0)))
               (args
                ((Local (slot s16+8 p0+0))
                 (Prim_op
                  ((op String_length) (arg_slots ((slot s0+0 p5+1)))
                   (args ((Local (slot s0+0 p4+1)))))))))))))
          (exception_cases ())))))))
    (globals
     (((id 0) (name extract) (layout ((s 0) (p 1))) (ty "choice -> int")
       (kind (Function 0)))))
    |}]
;;

let%expect_test "Indirect over-application stays binary Call_indirect" =
  let lower, find_top, _ =
    check_and_lower
      {|
let make_f (u : unit) : int -> int -> int =
  fun (a : int) ->
    let _ = print_int a in
    fun (b : int) -> Int.add a b

let indirect_over (f : int -> int -> int) : int =
  f (let _ = print_int 1 in 10) (let _ = print_int 2 in 20)

let dynamic_over (u : unit) : int =
  (make_f ()) (let _ = print_int 1 in 10) (let _ = print_int 2 in 20)
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "indirect_over") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "dynamic_over") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name indirect_over) (params ((slot s0+0 p0+1)))
       (frame ((scalar_bytes 32) (ptr_words 3))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Call_indirect
         ((fn_slot (slot s0+0 p2+1))
          (fn
           (Call_indirect
            ((fn_slot (slot s0+0 p1+1)) (fn (Local (slot s0+0 p0+1)))
             (arg_slot (slot s0+8 p0+0))
             (arg
              (Let
               ((slot (slot s0+0 p0+0))
                (body (Prim_call ((prim print_int) (args ((Lit (Int 1)))))))
                (in_ (Lit (Int 10))))))
             (tail false))))
          (arg_slot (slot s16+8 p0+0))
          (arg
           (Let
            ((slot (slot s0+0 p0+0))
             (body (Prim_call ((prim print_int) (args ((Lit (Int 2)))))))
             (in_ (Lit (Int 20))))))
          (tail true)))))
      ((id 1) (name dynamic_over) (params ((slot s0+0 p0+0)))
       (frame ((scalar_bytes 32) (ptr_words 1))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Call_indirect
         ((fn_slot (slot s0+0 p0+1))
          (fn
           (Call_direct
            ((fn 2) (tail false) (arg_slots ((slot s0+0 p0+0) (slot s0+8 p0+0)))
             (args
              ((Lit Unit)
               (Let
                ((slot (slot s0+0 p0+0))
                 (body (Prim_call ((prim print_int) (args ((Lit (Int 1)))))))
                 (in_ (Lit (Int 10))))))))))
          (arg_slot (slot s16+8 p0+0))
          (arg
           (Let
            ((slot (slot s0+0 p0+0))
             (body (Prim_call ((prim print_int) (args ((Lit (Int 2)))))))
             (in_ (Lit (Int 20))))))
          (tail true)))))
      ((id 2) (name make_f) (params ((slot s0+0 p0+0) (slot s0+8 p0+0)))
       (frame ((scalar_bytes 16) (ptr_words 0))) (env ((s 0) (p 0)))
       (partials (Heap)) (result ((s 0) (p 1))) (result_ty "int -> int")
       (body
        (Let
         ((slot (slot s0+0 p0+0))
          (body (Prim_call ((prim print_int) (args ((Local (slot s0+8 p0+0)))))))
          (in_
           (Make_closure
            ((fn 3) (placement Heap) (env ((s 8) (p 0)))
             (captures
              (((src (Local (slot s0+8 p0+0))) (dst (place s0+8 p0+0))))))))))))
      ((id 3) (name <fun>) (params ((slot s0+8 p0+0)))
       (frame ((scalar_bytes 24) (ptr_words 0))) (env ((s 8) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Prim_op
         ((op Int_add) (arg_slots ((slot s8+8 p0+0) (slot s16+8 p0+0)))
          (args ((Capture (place s0+8 p0+0)) (Local (slot s0+8 p0+0))))))))))
    (globals
     (((id 0) (name indirect_over) (layout ((s 0) (p 1)))
       (ty "(int -> int -> int) -> int") (kind (Function 0)))
      ((id 1) (name dynamic_over) (layout ((s 0) (p 1))) (ty "unit -> int")
       (kind (Function 1)))))
    |}]
;;

let%expect_test "Region records contiguous clear_ptrs excluding outer locals" =
  let lower, find_top, _ =
    check_and_lower
      {|
let region_clear (s : string) : int =
  let outer_box = Box.make s in
  let n =
    region_ (
      let r1 = stack_ (Box.make "temp1") in
      let r2 = stack_ (Box.make "temp2") in
      Int.add (String.length r1.*) (String.length r2.*)
    )
  in
  Int.add n (String.length outer_box.*)
|}
  in
  ignore
    (Lower.specialize_toplevel lower ~ident:(find_top "region_clear") ()
     |> Or_error.ok_exn
     : Compiled.Global_id.t);
  print_lower lower;
  [%expect
    {|
    (fns
     (((id 0) (name region_clear) (params ((slot s0+0 p0+1)))
       (frame ((scalar_bytes 40) (ptr_words 13))) (env ((s 0) (p 0)))
       (partials ()) (result ((s 8) (p 0))) (result_ty int)
       (body
        (Let
         ((slot (slot s0+0 p1+1))
          (body
           (Box_make
            ((placement Region) (content ((s 0) (p 1)))
             (value (Local (slot s0+0 p0+1))))))
          (in_
           (Let
            ((slot (slot s0+8 p0+0))
             (body
              (Region
               ((clear_ptrs (3 8))
                (body
                 (Let
                  ((slot (slot s0+0 p3+1))
                   (body
                    (Box_make
                     ((placement Region) (content ((s 0) (p 1)))
                      (value (Lit (String temp1))))))
                   (in_
                    (Let
                     ((slot (slot s0+0 p5+1))
                      (body
                       (Box_make
                        ((placement Region) (content ((s 0) (p 1)))
                         (value (Lit (String temp2))))))
                      (in_
                       (Prim_op
                        ((op Int_add)
                         (arg_slots ((slot s8+8 p0+0) (slot s16+8 p0+0)))
                         (args
                          ((Prim_op
                            ((op String_length) (arg_slots ((slot s0+0 p7+1)))
                             (args
                              ((Deref
                                ((content ((s 0) (p 1)))
                                 (ptr (Local (slot s0+0 p3+1)))))))))
                           (Prim_op
                            ((op String_length) (arg_slots ((slot s0+0 p9+1)))
                             (args
                              ((Deref
                                ((content ((s 0) (p 1)))
                                 (ptr (Local (slot s0+0 p5+1)))))))))))))))))))))))
             (in_
              (Prim_op
               ((op Int_add) (arg_slots ((slot s24+8 p0+0) (slot s32+8 p0+0)))
                (args
                 ((Local (slot s0+8 p0+0))
                  (Prim_op
                   ((op String_length) (arg_slots ((slot s0+0 p11+1)))
                    (args
                     ((Deref
                       ((content ((s 0) (p 1))) (ptr (Local (slot s0+0 p1+1)))))))))))))))))))))))
    (globals
     (((id 0) (name region_clear) (layout ((s 0) (p 1))) (ty "string -> int")
       (kind (Function 0)))))
    |}]
;;
