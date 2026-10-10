open! Core

module Level = struct
  type t =
    | Any
    | Level of int
  [@@deriving sexp, compare, equal]
end

module Type_check_state = struct
  type t =
    | To_be_checked
    | In_check
    | Done
  [@@deriving sexp, compare, equal]
end

module Entry = struct
  type t =
    { mutable scheme : Type.ty
    ; args : (string * Type.ty) list
    ; mutable type_check_state : Type_check_state.t
    ; level : Level.t
    }
  [@@deriving sexp, compare, equal]
end

type t = { entries : Entry.t list String.Table.t } [@@deriving sexp]

let create () = { entries = String.Table.create () }

let lookup t name =
  Hashtbl.find t.entries name |> Option.value ~default:[] |> List.hd
;;
