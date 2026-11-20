open! Import

module Raised = struct
  module Kind = struct
    type 'value t =
      { id : 'value Type_equal.Id.t
      ; to_string_for_message : 'value -> string
      }

    let create ~name ~sexp_of ~to_string_for_message =
      { id = Type_equal.Id.create ~name sexp_of; to_string_for_message }
    ;;
  end

  type t = T : 'value Kind.t * 'value -> t

  let create kind value = T (kind, value)
  let sexp_of_t (T (kind, value)) = Type_equal.Id.to_sexp kind.id value
  let to_string_for_message (T (kind, value)) = kind.to_string_for_message value

  let value (type value) (T (kind', value)) (kind : value Kind.t) : value option
    =
    match Type_equal.Id.same_witness kind'.id kind.id with
    | Some T -> Some value
    | None -> None
  ;;
end

module Kind = struct
  type t =
    | Message of string
    | Raised of Raised.t
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
  | Raised exn -> "Uncaught exception " ^ Raised.to_string_for_message exn
;;

exception Runtime_error of t

let () =
  Sexplib0.Sexp_conv.Exn_converter.add
    [%extension_constructor Runtime_error]
    (Basement.Stdlib_shim.Obj.magic_portable (function
      | Runtime_error t ->
        Sexp.List [ Atom (message t); Atom "@"; Loc.sexp_of_t t.loc ]
      | _ -> assert false))
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
