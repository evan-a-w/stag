open! Import

module Kind = struct
  type t =
    | Type
    | Constructor
    | Field
    | Value

  let to_string = function
    | Type -> "type"
    | Constructor -> "constructor"
    | Field -> "field"
    | Value -> "value"
  ;;
end

type t =
  { kind : Kind.t
  ; name : string
  }

let sexp_of_t { kind; name } =
  Sexp.List
    [ Atom (Kind.to_string kind); Atom (Prelude_path.to_string_hum name) ]
;;

let short_name t = Prelude_path.short_name t.name

let root_path t =
  let names path = String.split path ~on:'.' in
  match Prelude_path.relative t.name with
  | Some _ -> `Prelude (names (Prelude_path.root_path t.name))
  | None when not (String.mem t.name '.') -> `Prelude [ t.name ]
  | None -> `Program (names t.name)
;;

let to_string_hum { kind; name } =
  sprintf "<%s %s>" (Kind.to_string kind) (Prelude_path.to_string_hum name)
;;
