open! Core

module Level = struct
  type t = int [@@deriving sexp, compare, equal]

  let is_allowed_at t ~used_at ~which =
    match which with
    | `Any_greater -> used_at >= t
    | `Exactly_the_same -> used_at = t
  ;;
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
  let create () = { level = 0; rank = 0 }
  let incr_level t = t.level <- t.level + 1
  let decr_level t = t.level <- t.level - 1
end

module Type_check_state = struct
  type t =
    | To_be_checked of
        { args : (string * Type.ty) list
        ; expr : Syntax.Expr.t
        ; mask : String.Set.t
        }
    | In_check
    | Done
  [@@deriving sexp, compare, equal]
end

module Entry = struct
  type t =
    { mutable scheme : Type.ty
    ; mutable type_check_state : Type_check_state.t
    ; mutable bypasses_value_restriction : bool
    ; counts : Counts.Ro.t
    }
  [@@deriving sexp, compare, equal]
end

type t = { entries : Entry.t list String.Table.t } [@@deriving sexp]

let create () = { entries = String.Table.create () }

let lookup t name =
  Hashtbl.find t.entries name |> Option.value ~default:[] |> List.hd
;;
