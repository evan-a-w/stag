open! Core

module Level = struct
  type t =
    | Any
    | Level of int
  [@@deriving sexp, compare, equal]
end

module Counts = struct
  module Ro = struct
    type t =
      { level : Level.t
      ; rank : Type.rank
      }
    [@@deriving sexp, compare, equal, fields]
  end

  type t =
    { mutable level : Level.t
    ; mutable rank : Type.rank
    }
  [@@deriving fields, sexp]

  let ro { level; rank } : Ro.t = { level; rank }
  let of_ro ({ level; rank } : Ro.t) = { level; rank }
  let create () = { level = Any; rank = 0 }

  let incr_level t =
    match t.level with
    | Any -> t.level <- Level 0
    | Level n -> t.level <- Level (n + 1)
  ;;

  let decr_level t =
    match t.level with
    | Any -> failwith "decr_level: level is already Any"
    | Level n when n = 0 -> t.level <- Any
    | Level n -> t.level <- Level (n - 1)
  ;;
end

module Type_check_state = struct
  type t =
    | To_be_checked of
        { args : (string * Type.ty) list
        ; expr : Syntax.Expr.t
        }
    | In_check
    | Done
  [@@deriving sexp, compare, equal]
end

module Entry = struct
  type t =
    { mutable scheme : Type.ty
    ; mutable type_check_state : Type_check_state.t
    ; counts : Counts.Ro.t
    }
  [@@deriving sexp, compare, equal]
end

type t = { entries : Entry.t list String.Table.t } [@@deriving sexp]

let create () = { entries = String.Table.create () }

let lookup t name =
  Hashtbl.find t.entries name |> Option.value ~default:[] |> List.hd
;;
