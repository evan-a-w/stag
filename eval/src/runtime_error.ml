open! Import

type t =
  { loc : Loc.t
  ; message : string
  }
[@@deriving sexp_of]

exception Runtime_error of t

let () =
  Sexplib0.Sexp_conv.Exn_converter.add
    [%extension_constructor Runtime_error]
    (function
    | Runtime_error { loc; message } ->
      Sexp.List [ Atom message; Atom "@"; Loc.sexp_of_t loc ]
    | _ -> assert false)
;;

let to_error t = Error.of_exn (Runtime_error t)

let of_error error =
  match Error.to_exn error with
  | Runtime_error t -> Some t
  | _ -> None
;;
