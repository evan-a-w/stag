open! Import

type t =
  { loc : Loc.t
  ; message : string
  ; raised : Value.t option
  }
[@@deriving sexp_of]

let create ~loc message = { loc; message; raised = None }

let raise_ ~loc exn =
  { loc
  ; message = "Uncaught exception " ^ Value.to_string_hum exn
  ; raised = Some exn
  }
;;

exception Runtime_error of t

let () =
  Sexplib0.Sexp_conv.Exn_converter.add
    [%extension_constructor Runtime_error]
    (function
    | Runtime_error { loc; message; raised = _ } ->
      Sexp.List [ Atom message; Atom "@"; Loc.sexp_of_t loc ]
    | _ -> assert false)
;;

let to_error t = Error.of_exn (Runtime_error t)

let of_error error =
  match Error.to_exn error with
  | Runtime_error t -> Some t
  | _ -> None
;;

let raised error = Option.bind (of_error error) ~f:(fun t -> t.raised)
