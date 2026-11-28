open! Import
open Source_helpers

module Recording = struct
  module Value = Treewalk.Value

  module Event = struct
    type t =
      | Type_decl of
          { name : string
          ; decl : Signature.Type_decl.t
          }
      | Scc of
          { members : string list
          ; allocations : Typed.Allocation.t list
          }
      | Run
      | Splice
  end

  module Log_entry = struct
    type t =
      | Type_decl of string
      | Scc of string list
      | Run
      | Splice
    [@@deriving sexp_of]

    let of_event : Event.t -> t = function
      | Type_decl { name; decl = _ } -> Type_decl name
      | Scc { members; allocations = _ } -> Scc members
      | Run -> Run
      | Splice -> Splice
    ;;
  end

  type t =
    { inner : Treewalk.t
    ; events : Event.t Queue.t
    }

  let type_id : t Type_equal.Id.t =
    Type_equal.Id.create ~name:"Recording_interpreter" sexp_of_opaque
  ;;

  let create ~stamps () =
    { inner = Treewalk.create ~stamps (); events = Queue.create () }
  ;;

  let set_output t output = Treewalk.set_output t.inner output
  let set_argv t argv = Treewalk.set_argv t.inner argv
  let with_output t output ~f = Treewalk.with_output t.inner output ~f
  let set_limits t limits = Treewalk.set_limits t.inner limits

  let add_ident_info t ~ident ~info =
    Treewalk.add_ident_info t.inner ~ident ~info
  ;;

  let add_type_decl t ~name ~decl =
    Queue.enqueue t.events (Type_decl { name; decl });
    Treewalk.add_type_decl t.inner ~name ~decl
  ;;

  let add_scc t (scc : Typed.Scc.t) ~allocations =
    let members =
      List.map scc.members ~f:(fun (m : Typed.Toplevel.t) -> m.ident.name)
    in
    Queue.enqueue t.events (Scc { members; allocations });
    Treewalk.add_scc t.inner scc ~allocations
  ;;

  let splice t ~code ~level ~entry ~reflect =
    Queue.enqueue t.events Splice;
    Treewalk.splice t.inner ~code ~level ~entry ~reflect
  ;;

  let run t expr ~level ~reflect =
    Queue.enqueue t.events Run;
    Treewalk.run t.inner expr ~level ~reflect
  ;;

  let global_info t ~ident = Treewalk.global_info t.inner ~ident

  let run_expect_test t test ~on_expect =
    Treewalk.run_expect_test t.inner test ~on_expect
  ;;

  let eval_expr ?bindings t expr = Treewalk.eval_expr ?bindings t.inner expr
  let eval_toplevel t ~ident = Treewalk.eval_toplevel t.inner ~ident
  let find t ~ident = Treewalk.find t.inner ~ident

  let type_decls t =
    Queue.to_list t.events
    |> List.filter_map ~f:(function
      | Event.Type_decl { name; decl } -> Some (name, decl)
      | Scc _ | Run | Splice -> None)
  ;;

  let sccs t =
    Queue.to_list t.events
    |> List.filter_map ~f:(function
      | Event.Scc { members; allocations } -> Some (members, allocations)
      | Type_decl _ | Run | Splice -> None)
  ;;

  let log t = Queue.to_list t.events |> List.map ~f:Log_entry.of_event

  let builtin_names =
    String.Set.of_list
      [ "int"; "bool"; "string"; "char"; "unit"; "exn"; "ptr"; "<error>" ]
  ;;

  let assert_invariants t =
    let seen = String.Hash_set.create () in
    List.iter (type_decls t) ~f:(fun (name, _) ->
      if Set.mem builtin_names name
      then raise_s [%message "Builtin type was pushed" (name : string)];
      match Hash_set.strict_add seen name with
      | Ok -> ()
      | Duplicate ->
        raise_s
          [%message "Type declaration pushed more than once" (name : string)])
  ;;
end

let interpreter = (module Recording : Interpreter_intf.S)

let check_recording_result source =
  let stamps = Stamp_counter.create () in
  let interp = Interpreter.create interpreter ~stamps in
  Interpreter.set_limits interp limits;
  let tc = Typecheck.create ~stamps ~interpreter:interp () in
  let result =
    Typecheck.typecheck tc ~program:(program ~prelude:false source)
  in
  let state = Interpreter.state interp Recording.type_id |> Option.value_exn in
  Recording.assert_invariants state;
  result, state
;;

let check_recording source =
  match check_recording_result source with
  | Error failed, _ ->
    raise_s
      [%message "Typechecking failed" (failed.errors : Typecheck_error.t list)]
  | Ok checked, state -> checked, state
;;

let%expect_test "program types are pushed with canonical names, never builtins" =
  let _checked, state =
    check_recording
      {|
type color = Red | Green | Blue
type point = { x : int; y : int }
module M = struct
  type tree = Leaf of int | Node of ptr[forest]
  type forest = Empty | More of tree * ptr[forest]
end
let use
  (c : color)
  (p : point)
  (t : M.tree)
  (i : int)
  (b : bool)
  (s : string)
  (ch : char)
  (u : unit)
  (e : exn)
  (r : ptr[int])
  =
  (c, p, t, i, b, s, ch, u, e, r)
|}
  in
  let decls =
    Recording.type_decls state
    |> List.map ~f:(fun (name, (decl : Signature.Type_decl.t)) ->
      let kind =
        match decl.kind with
        | Variant cs ->
          sprintf
            "Variant(%s)"
            (String.concat ~sep:"," (List.map cs ~f:(fun c -> c.name)))
        | Record fs ->
          sprintf
            "Record(%s)"
            (String.concat ~sep:"," (List.map fs ~f:(fun f -> f.name)))
        | Alias _ -> "Alias"
        | Abstract -> "Abstract"
      in
      name, decl.name, kind)
  in
  print_s [%sexp (decls : (string * string * string) list)];
  print_s [%sexp (Recording.log state : Recording.Log_entry.t list)];
  [%expect
    {|
    ((color color "Variant(Red,Green,Blue)") (point point "Record(x,y)")
     (M.tree tree "Variant(Leaf,Node)") (M.forest forest "Variant(Empty,More)"))
    ((Type_decl color) (Type_decl point) (Type_decl M.tree) (Type_decl M.forest)
     (Scc (use)))
    |}]
;;

let%expect_test "prelude types from prims and intrinsics are pushed before \
                 compile-time evaluation"
  =
  let _checked, state =
    check_recording
      {|
let a = .~(let _ = String.concat in .<1>.)
let b = .~(let _ = Stag.Code.erase .<1>. in .<2>.)
let c _ = read_line ()
|}
  in
  print_s [%sexp (Recording.log state : Recording.Log_entry.t list)];
  [%expect
    {|
    ((Type_decl prelude.list) (Type_decl prelude.box) Splice (Scc (a))
     (Type_decl prelude.Stag.Expr.t) (Type_decl prelude.Stag.Lit.t)
     (Type_decl prelude.Stag.Value_ref.t) (Type_decl prelude.Stag.Path.t)
     (Type_decl prelude.Stag.Type_expr.t) (Type_decl prelude.Stag.Type_ref.t)
     (Type_decl prelude.Stag.Type_expr.Arg.t)
     (Type_decl prelude.Stag.Type_expr.Row.t)
     (Type_decl prelude.Stag.Mode_expr.t)
     (Type_decl prelude.Stag.Mode_expr.Atom.t)
     (Type_decl prelude.Stag.Mode_expr.Constraint.t)
     (Type_decl prelude.Stag.Mode_expr.Term.t)
     (Type_decl prelude.Stag.Mode_expr.Axis.t) (Type_decl prelude.option)
     (Type_decl prelude.Stag.Constructor_ref.t)
     (Type_decl prelude.Stag.Field_ref.t) (Type_decl prelude.Stag.Pattern.t)
     (Type_decl prelude.Stag.Pattern.Rest.t) (Type_decl prelude.Stag.Toplevel.t)
     (Type_decl prelude.Stag.Type_decl.Param.t)
     (Type_decl prelude.Stag.Type_decl.Kind.t) (Scc (prelude.Stag.Code.erase))
     Splice (Scc (b)) (Scc (c)))
    |}]
;;

let%expect_test "type only in annotation inside a compile-time splice is \
                 pushed before the splice runs"
  =
  let _checked, state =
    check_recording
      {|
let x = .~(let _ : option[b] = None in .<1>.)
type b = B of c
type c = C of int
|}
  in
  print_s [%sexp (Recording.log state : Recording.Log_entry.t list)];
  [%expect
    {| ((Type_decl prelude.option) (Type_decl b) (Type_decl c) Splice (Scc (x))) |}]
;;

let%expect_test "syntax splices push types before Interpreter.run" =
  let _checked, state =
    check_recording
      {|
let x = .~(let _ : option[d] = None in Stag.Expr.Lit (Stag.Lit.Int 42))
type d = D of int
|}
  in
  print_s [%sexp (Recording.log state : Recording.Log_entry.t list)];
  [%expect
    {|
    ((Type_decl prelude.option) (Type_decl prelude.Stag.Expr.t)
     (Type_decl prelude.Stag.Lit.t) (Type_decl prelude.Stag.Value_ref.t)
     (Type_decl prelude.Stag.Path.t) (Type_decl prelude.Stag.Type_expr.t)
     (Type_decl prelude.Stag.Type_ref.t) (Type_decl prelude.Stag.Type_expr.Arg.t)
     (Type_decl prelude.Stag.Type_expr.Row.t)
     (Type_decl prelude.Stag.Mode_expr.t)
     (Type_decl prelude.Stag.Mode_expr.Atom.t) (Type_decl prelude.list)
     (Type_decl prelude.box) (Type_decl prelude.Stag.Mode_expr.Constraint.t)
     (Type_decl prelude.Stag.Mode_expr.Term.t)
     (Type_decl prelude.Stag.Mode_expr.Axis.t)
     (Type_decl prelude.Stag.Constructor_ref.t)
     (Type_decl prelude.Stag.Field_ref.t) (Type_decl prelude.Stag.Pattern.t)
     (Type_decl prelude.Stag.Pattern.Rest.t) (Type_decl prelude.Stag.Toplevel.t)
     (Type_decl prelude.Stag.Type_decl.Param.t)
     (Type_decl prelude.Stag.Type_decl.Kind.t) (Type_decl d) Run (Scc (x)))
    |}]
;;

let%expect_test "add_scc receives per-SCC allocations matching \
                 program.allocations"
  =
  let checked, state =
    check_recording
      {|
let mk (x : int) = Box.make x
let cell (x : int) = Ptr.make x
let plain (x : int) = Int.add x 1
let pair (x : int) = (Box.make x, Ptr.make x)
|}
  in
  let program_allocations = checked.program.allocations in
  let scc_records = Recording.sccs state in
  let flattened = List.concat_map scc_records ~f:snd in
  assert (
    [%equal: Sexp.t]
      [%sexp (flattened : Typed.Allocation.t list)]
      [%sexp (program_allocations : Typed.Allocation.t list)]);
  List.iter scc_records ~f:(fun (members, allocations) ->
    let kinds =
      List.map allocations ~f:(fun (a : Typed.Allocation.t) ->
        a.kind, a.placement)
    in
    print_s
      [%sexp
        (members : string list)
        , (kinds
           : (Typed.Allocation.Kind.t * Typed.Allocation.Placement.t) list)]);
  [%expect
    {|
    ((prelude.Box.make) ((Box Heap)))
    ((mk) ((Box Heap)))
    ((prelude.Ptr.make) ((Pointer Heap)))
    ((cell) ((Pointer Heap)))
    ((plain) ())
    ((pair) ((Box Heap) (Pointer Heap)))
    |}]
;;

let%expect_test "expect tests and benches push types referenced only in them" =
  let _checked, expect_state =
    check_recording {|
let%expect_test "t" =
  let _ = read_line in
  ()
|}
  in
  let _checked, bench_body_state =
    check_recording {|
let%bench "b1" =
  let _ = String.concat in
  ()
|}
  in
  let _checked, bench_param_state =
    check_recording {|
let%bench "b2" f in [read_line] =
  let _ = f in
  ()
|}
  in
  print_s
    [%message
      "expect test"
        ~log:(Recording.log expect_state : Recording.Log_entry.t list)];
  print_s
    [%message
      "bench body"
        ~log:(Recording.log bench_body_state : Recording.Log_entry.t list)];
  print_s
    [%message
      "bench param"
        ~log:(Recording.log bench_param_state : Recording.Log_entry.t list)];
  [%expect
    {|
    ("expect test" (log ((Type_decl prelude.option))))
    ("bench body" (log ((Type_decl prelude.list) (Type_decl prelude.box))))
    ("bench param" (log ((Type_decl prelude.option))))
    |}]
;;

let%expect_test "inline type splices are not forced early during SCC finish or \
                 splices"
  =
  let _checked, state1 =
    check_recording
      {|
let c = let _ = d in 1
let d (u : unit) = let _ : list[t] = Nil in 1
type t = T of .~(let _ = c in Stag.Type_expr.Con (Stag.Path.Name "int", Nil))
|}
  in
  let _checked, state2 =
    check_recording
      {|
let c = .~(let _ : list[t] = Nil in .<1>.)
type t = T of .~(let _ = c in Stag.Type_expr.Con (Stag.Path.Name "int", Nil))
|}
  in
  let _checked, state3 =
    check_recording
      {|
let c = let _ = d in 1
let d (u : unit) = let _ : list[wrap] = Nil in 1
type wrap = Wrap of t
type t = T of .~(let _ = c in Stag.Type_expr.Con (Stag.Path.Name "int", Nil))
|}
  in
  print_s
    [%message "scc" ~log:(Recording.log state1 : Recording.Log_entry.t list)];
  print_s
    [%message "splice" ~log:(Recording.log state2 : Recording.Log_entry.t list)];
  print_s
    [%message
      "transitive by-value"
        ~log:(Recording.log state3 : Recording.Log_entry.t list)];
  [%expect
    {|
    (scc
     (log
      ((Type_decl prelude.list) (Type_decl prelude.box) (Scc (d)) (Scc (c))
       (Type_decl prelude.Stag.Type_expr.t) (Type_decl prelude.Stag.Path.t)
       (Type_decl prelude.Stag.Type_ref.t)
       (Type_decl prelude.Stag.Type_expr.Arg.t)
       (Type_decl prelude.Stag.Type_expr.Row.t)
       (Type_decl prelude.Stag.Mode_expr.t)
       (Type_decl prelude.Stag.Mode_expr.Atom.t) (Type_decl prelude.Stag.Expr.t)
       (Type_decl prelude.Stag.Lit.t) (Type_decl prelude.Stag.Value_ref.t)
       (Type_decl prelude.option) (Type_decl prelude.Stag.Constructor_ref.t)
       (Type_decl prelude.Stag.Field_ref.t) (Type_decl prelude.Stag.Pattern.t)
       (Type_decl prelude.Stag.Pattern.Rest.t)
       (Type_decl prelude.Stag.Toplevel.t)
       (Type_decl prelude.Stag.Type_decl.Param.t)
       (Type_decl prelude.Stag.Type_decl.Kind.t)
       (Type_decl prelude.Stag.Mode_expr.Constraint.t)
       (Type_decl prelude.Stag.Mode_expr.Term.t)
       (Type_decl prelude.Stag.Mode_expr.Axis.t) Run (Type_decl t))))
    (splice
     (log
      ((Type_decl prelude.list) (Type_decl prelude.box) Splice (Scc (c))
       (Type_decl prelude.Stag.Type_expr.t) (Type_decl prelude.Stag.Path.t)
       (Type_decl prelude.Stag.Type_ref.t)
       (Type_decl prelude.Stag.Type_expr.Arg.t)
       (Type_decl prelude.Stag.Type_expr.Row.t)
       (Type_decl prelude.Stag.Mode_expr.t)
       (Type_decl prelude.Stag.Mode_expr.Atom.t) (Type_decl prelude.Stag.Expr.t)
       (Type_decl prelude.Stag.Lit.t) (Type_decl prelude.Stag.Value_ref.t)
       (Type_decl prelude.option) (Type_decl prelude.Stag.Constructor_ref.t)
       (Type_decl prelude.Stag.Field_ref.t) (Type_decl prelude.Stag.Pattern.t)
       (Type_decl prelude.Stag.Pattern.Rest.t)
       (Type_decl prelude.Stag.Toplevel.t)
       (Type_decl prelude.Stag.Type_decl.Param.t)
       (Type_decl prelude.Stag.Type_decl.Kind.t)
       (Type_decl prelude.Stag.Mode_expr.Constraint.t)
       (Type_decl prelude.Stag.Mode_expr.Term.t)
       (Type_decl prelude.Stag.Mode_expr.Axis.t) Run (Type_decl t))))
    ("transitive by-value"
     (log
      ((Type_decl prelude.list) (Type_decl prelude.box) (Scc (d)) (Scc (c))
       (Type_decl prelude.Stag.Type_expr.t) (Type_decl prelude.Stag.Path.t)
       (Type_decl prelude.Stag.Type_ref.t)
       (Type_decl prelude.Stag.Type_expr.Arg.t)
       (Type_decl prelude.Stag.Type_expr.Row.t)
       (Type_decl prelude.Stag.Mode_expr.t)
       (Type_decl prelude.Stag.Mode_expr.Atom.t) (Type_decl prelude.Stag.Expr.t)
       (Type_decl prelude.Stag.Lit.t) (Type_decl prelude.Stag.Value_ref.t)
       (Type_decl prelude.option) (Type_decl prelude.Stag.Constructor_ref.t)
       (Type_decl prelude.Stag.Field_ref.t) (Type_decl prelude.Stag.Pattern.t)
       (Type_decl prelude.Stag.Pattern.Rest.t)
       (Type_decl prelude.Stag.Toplevel.t)
       (Type_decl prelude.Stag.Type_decl.Param.t)
       (Type_decl prelude.Stag.Type_decl.Kind.t)
       (Type_decl prelude.Stag.Mode_expr.Constraint.t)
       (Type_decl prelude.Stag.Mode_expr.Term.t)
       (Type_decl prelude.Stag.Mode_expr.Axis.t) Run (Type_decl wrap)
       (Type_decl t))))
    |}]
;;

let%expect_test "splice-defined type declaration is pushed when checked" =
  let _checked, state =
    check_recording
      {|
let kind =
  Stag.Type_decl.Kind.Variant
    (Cons (Box.make (("A", Nil), Cons (Box.make (("B", Nil), Nil)))))
type t = .~kind
let x = A
|}
  in
  print_s [%sexp (Recording.log state : Recording.Log_entry.t list)];
  [%expect
    {|
    ((Type_decl prelude.Stag.Type_decl.Kind.t) (Type_decl prelude.list)
     (Type_decl prelude.box) (Scc (prelude.Box.make))
     (Type_decl prelude.Stag.Type_expr.t) (Type_decl prelude.Stag.Type_ref.t)
     (Type_decl prelude.Stag.Path.t) (Type_decl prelude.Stag.Type_expr.Arg.t)
     (Type_decl prelude.Stag.Type_expr.Row.t)
     (Type_decl prelude.Stag.Mode_expr.t)
     (Type_decl prelude.Stag.Mode_expr.Atom.t) (Type_decl prelude.Stag.Expr.t)
     (Type_decl prelude.Stag.Lit.t) (Type_decl prelude.Stag.Value_ref.t)
     (Type_decl prelude.option) (Type_decl prelude.Stag.Constructor_ref.t)
     (Type_decl prelude.Stag.Field_ref.t) (Type_decl prelude.Stag.Pattern.t)
     (Type_decl prelude.Stag.Pattern.Rest.t) (Type_decl prelude.Stag.Toplevel.t)
     (Type_decl prelude.Stag.Type_decl.Param.t)
     (Type_decl prelude.Stag.Mode_expr.Constraint.t)
     (Type_decl prelude.Stag.Mode_expr.Term.t)
     (Type_decl prelude.Stag.Mode_expr.Axis.t) (Scc (kind)) Run (Type_decl t)
     (Scc (x)))
    |}]
;;

let%expect_test "failed type declaration is not pushed and still reports error" =
  let result, state =
    check_recording_result
      {|
type good = Good of int
type bad = Bad of missing_type
let x = Good 1
|}
  in
  let errors =
    match result with
    | Ok _ -> raise_s [%message "expected typecheck error"]
    | Error failed -> failed.errors
  in
  print_s [%message (errors : Typecheck_error.t list)];
  print_s [%sexp (Recording.log state : Recording.Log_entry.t list)];
  [%expect
    {|
    (errors (((Unbound_type missing_type) @ t.stag:3:18-30)))
    ((Type_decl good) (Scc (x)))
    |}]
;;

let%expect_test "exception payload types are pushed" =
  let _checked, state =
    check_recording
      {|
type payload = { code : int; detail : extra }
type extra = Extra of string
exception E of payload
let f _ =
  match raise (E { code = 1; detail = Extra "hi" }) with
  | exception E _ -> 0
  | x -> x
|}
  in
  print_s [%sexp (Recording.log state : Recording.Log_entry.t list)];
  [%expect
    {| ((Type_decl payload) (Type_decl extra) (Scc (prelude.raise)) (Scc (f))) |}]
;;

let%expect_test "reflect pushes reflected type and Stag.Type.Decl.t closure" =
  let _checked, state =
    check_recording
      {|
[@@@warning "-non-exhaustive"]
type item = Item of extra
type extra = Extra of int
let head ty =
  match Stag.Type.expr ty with
  | Stag.Type_expr.Con (Stag.Path.Ref r, _) -> r
let x = .~(let _ = Stag.Type.decl (head (type item)) in .<1>.)
|}
  in
  let _checked, reentrant_state =
    check_recording
      {|
[@@@warning "-non-exhaustive"]
let c = 1
let ptr_target ty =
  match Stag.Type.expr ty with
  | Stag.Type_expr.Con
      (_, Cons * (Stag.Type_expr.Arg.Ty (Stag.Type_expr.Con (Stag.Path.Ref r, _)), _)) ->
    r
let x = .~(let _ = Stag.Type.decl (ptr_target (type ptr[spliced])) in .<1>.)
type spliced = S of .~(let _ = c in Stag.Type_expr.Con (Stag.Path.Name "int", Nil))
|}
  in
  print_s
    [%message "direct" ~log:(Recording.log state : Recording.Log_entry.t list)];
  print_s
    [%message
      "reentrant"
        ~log:(Recording.log reentrant_state : Recording.Log_entry.t list)];
  [%expect
    {|
    (direct
     (log
      ((Type_decl item) (Type_decl extra) (Type_decl prelude.Stag.Type.t)
       (Type_decl prelude.Stag.Type_expr.t) (Type_decl prelude.Stag.Type_ref.t)
       (Type_decl prelude.Stag.Path.t) (Type_decl prelude.Stag.Type_expr.Arg.t)
       (Type_decl prelude.Stag.Type_expr.Row.t)
       (Type_decl prelude.Stag.Mode_expr.t)
       (Type_decl prelude.Stag.Mode_expr.Atom.t) (Type_decl prelude.list)
       (Type_decl prelude.box) (Type_decl prelude.Stag.Expr.t)
       (Type_decl prelude.Stag.Lit.t) (Type_decl prelude.Stag.Value_ref.t)
       (Type_decl prelude.option) (Type_decl prelude.Stag.Constructor_ref.t)
       (Type_decl prelude.Stag.Field_ref.t) (Type_decl prelude.Stag.Pattern.t)
       (Type_decl prelude.Stag.Pattern.Rest.t)
       (Type_decl prelude.Stag.Toplevel.t)
       (Type_decl prelude.Stag.Type_decl.Param.t)
       (Type_decl prelude.Stag.Type_decl.Kind.t)
       (Type_decl prelude.Stag.Mode_expr.Constraint.t)
       (Type_decl prelude.Stag.Mode_expr.Term.t)
       (Type_decl prelude.Stag.Mode_expr.Axis.t) (Scc (prelude.Stag.Type.expr))
       (Scc (head)) (Type_decl prelude.Stag.Type.Decl.t)
       (Type_decl prelude.Stag.Type.Kind.t) (Scc (prelude.Stag.Type.decl)) Splice
       (Scc (x)))))
    (reentrant
     (log
      ((Scc (c)) (Type_decl prelude.Stag.Type.t)
       (Type_decl prelude.Stag.Type_expr.t) (Type_decl prelude.Stag.Type_ref.t)
       (Type_decl prelude.Stag.Path.t) (Type_decl prelude.Stag.Type_expr.Arg.t)
       (Type_decl prelude.Stag.Type_expr.Row.t)
       (Type_decl prelude.Stag.Mode_expr.t)
       (Type_decl prelude.Stag.Mode_expr.Atom.t) (Type_decl prelude.list)
       (Type_decl prelude.box) (Type_decl prelude.Stag.Expr.t)
       (Type_decl prelude.Stag.Lit.t) (Type_decl prelude.Stag.Value_ref.t)
       (Type_decl prelude.option) (Type_decl prelude.Stag.Constructor_ref.t)
       (Type_decl prelude.Stag.Field_ref.t) (Type_decl prelude.Stag.Pattern.t)
       (Type_decl prelude.Stag.Pattern.Rest.t)
       (Type_decl prelude.Stag.Toplevel.t)
       (Type_decl prelude.Stag.Type_decl.Param.t)
       (Type_decl prelude.Stag.Type_decl.Kind.t)
       (Type_decl prelude.Stag.Mode_expr.Constraint.t)
       (Type_decl prelude.Stag.Mode_expr.Term.t)
       (Type_decl prelude.Stag.Mode_expr.Axis.t) (Scc (prelude.Stag.Type.expr))
       (Scc (ptr_target)) (Type_decl prelude.Stag.Type.Decl.t)
       (Type_decl prelude.Stag.Type.Kind.t) (Scc (prelude.Stag.Type.decl)) Splice
       Run (Type_decl spliced) (Scc (x)))))
    |}]
;;
