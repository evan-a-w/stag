open! Import
module Value = Interp_value

type handle

external create_handle
  :  Debug_intf.Gc_stress.t
  -> handle
  = "stag_rust_interp_create"

external add_ident_info
  :  handle
  -> Typed.Ident.t
  -> Typed.Ident_info.t
  -> unit
  = "stag_rust_interp_add_ident_info"

external add_scc : handle -> Typed.Scc.t -> unit = "stag_rust_interp_add_scc"

external splice
  :  handle
  -> Stamp_counter.t
  -> Typed.Expr.t
  -> int
  -> Typed.Expr.t Value.Outcome.t
  = "stag_rust_interp_splice"

external run
  :  handle
  -> Typed.Expr.t
  -> int
  -> Value.t Value.Outcome.t
  = "stag_rust_interp_run"

external global_info
  :  handle
  -> Typed.Ident.t
  -> Typed.Ident_info.t option
  = "stag_rust_interp_global_info"

external eval : handle -> unit Value.Outcome.t = "stag_rust_interp_eval"

external run_expect_test
  :  handle
  -> Typed.Expect_test.t
  -> unit Value.Outcome.t
  = "stag_rust_interp_run_expect_test"

external eval_expr
  :  handle
  -> (Typed.Ident.t * Value.t) list
  -> Typed.Expr.t
  -> Value.t Value.Outcome.t
  = "stag_rust_interp_eval_expr"

external eval_toplevel
  :  handle
  -> Typed.Ident.t
  -> unit Value.Outcome.t
  = "stag_rust_interp_eval_toplevel"

external summary : handle -> string = "stag_rust_interp_summary"

type t =
  { handle : handle
  ; stamps : Stamp_counter.t
  ; mutable output : string -> unit
  }

exception Reached_error [@@deriving sexp_of]

let create_with_gc_stress ~stamps gc_stress =
  { handle = create_handle gc_stress; stamps; output = print_string }
;;

let create ~stamps () = create_with_gc_stress ~stamps Off
let set_output t output = t.output <- output

let with_output t output ~f =
  let previous = t.output in
  t.output <- output;
  Exn.protect ~f ~finally:(fun () -> t.output <- previous)
;;

let set_limits (_ : t) (_ : Limits.t) = ()
let add_ident_info t ~ident ~info = add_ident_info t.handle ident info
let add_scc t scc = add_scc t.handle scc

let splice t ~code ~level ~entry:_ ~reflect:_ =
  splice t.handle t.stamps code level |> Value.Outcome.to_or_error
;;

let run t expr ~level ~reflect:_ =
  run t.handle expr level |> Value.Outcome.to_or_error
;;

let global_info t ~ident = global_info t.handle ident
let eval t = eval t.handle |> Value.Outcome.to_or_error

let run_expect_test t test ~on_expect:_ =
  run_expect_test t.handle test |> Value.Outcome.to_or_error
;;

let eval_expr ?(bindings = []) t expr =
  eval_expr t.handle bindings expr |> Value.Outcome.to_or_error
;;

let eval_toplevel t ~ident =
  eval_toplevel t.handle ident |> Value.Outcome.to_or_error
;;

let summary t = summary t.handle
