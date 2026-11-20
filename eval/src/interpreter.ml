open! Import

module Value = struct
  type t = T : (module Interpreter_intf.Value with type t = 'a) * 'a -> t

  let sexp_of_t (T ((module V), value)) = V.sexp_of_t value

  let to_string_hum ?max_length (T ((module V), value)) =
    V.to_string_hum ?max_length value
  ;;

  let to_syntax (T ((module V), value)) syntax ~loc =
    V.to_syntax syntax value ~loc
  ;;
end

type t = T : (module Interpreter_intf.S with type t = 'a) * 'a -> t

let create (module I : Interpreter_intf.S) ~stamps =
  T ((module I), I.create ~stamps ())
;;

let set_output (T ((module I), t)) output = I.set_output t output
let with_output (T ((module I), t)) output ~f = I.with_output t output ~f
let set_limits (T ((module I), t)) limits = I.set_limits t limits

let add_ident_info (T ((module I), t)) ~ident ~info =
  I.add_ident_info t ~ident ~info
;;

let add_scc (T ((module I), t)) scc = I.add_scc t scc

let splice (T ((module I), t)) ~code ~level ~entry ~reflect =
  I.splice t ~code ~level ~entry ~reflect
;;

let run (T ((module I), t)) expr ~level ~reflect =
  I.run t expr ~level ~reflect
  |> Or_error.map ~f:(fun value -> Value.T ((module I.Value), value))
;;

let global_info (T ((module I), t)) ~ident = I.global_info t ~ident
let eval (T ((module I), t)) = I.eval t

let run_expect_test (T ((module I), t)) test ~on_expect =
  I.run_expect_test t test ~on_expect
;;

let eval_toplevel (T ((module I), t)) ~ident = I.eval_toplevel t ~ident

let find (T ((module I), t)) ~ident =
  I.find t ~ident
  |> Or_error.map ~f:(fun value -> Value.T ((module I.Value), value))
;;

let state (type a) (T ((module I), t)) (id : a Type_equal.Id.t) : a option =
  match Type_equal.Id.same_witness I.type_id id with
  | Some T -> Some t
  | None -> None
;;
