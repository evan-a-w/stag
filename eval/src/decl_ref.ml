open! Import

module Kind = struct
  type t =
    | Type
    | Constructor
    | Field
  [@@deriving sexp_of, equal]

  let to_string = function
    | Type -> "type"
    | Constructor -> "constructor"
    | Field -> "field"
  ;;
end

type t =
  { kind : Kind.t
  ; name : string
  }
[@@deriving equal]

let sexp_of_t { kind; name } =
  Sexp.List [ Atom (Kind.to_string kind); Atom (Prelude_path.to_string_hum name) ]
;;

let short_name t = List.last_exn (String.split t.name ~on:'.')

let to_string_hum { kind; name } =
  sprintf "<%s %s>" (Kind.to_string kind) (Prelude_path.to_string_hum name)
;;
