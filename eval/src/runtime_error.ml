open! Import

module Kind = struct
  type t =
    | Message of string
    | Raised of Value.t
  [@@deriving sexp_of]
end

type t =
  { loc : Loc.t
  ; kind : Kind.t
  }
[@@deriving sexp_of]

let create ~loc message = { loc; kind = Message message }
let raise_exn ~loc exn = { loc; kind = Raised exn }

let message t =
  match t.kind with
  | Message message -> message
  | Raised exn -> "Uncaught exception " ^ Value.to_string_for_message exn
;;

exception Runtime_error of t

let () =
  Sexplib0.Sexp_conv.Exn_converter.add
    [%extension_constructor Runtime_error]
    (function
    | Runtime_error t ->
      Sexp.List [ Atom (message t); Atom "@"; Loc.sexp_of_t t.loc ]
    | _ -> assert false)
;;

let to_error t = Error.of_exn (Runtime_error t)

let of_error error =
  match Error.to_exn error with
  | Runtime_error t -> Some t
  | _ -> None
;;

let raised error =
  match of_error error with
  | Some { kind = Raised exn; loc = _ } -> Some exn
  | Some { kind = Message _; loc = _ } | None -> None
;;
