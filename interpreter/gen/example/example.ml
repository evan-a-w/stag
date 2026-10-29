open! Core

type id = int [@@deriving rust]

module Pos = struct
  type t =
    { line : int
    ; col : int
    }
  [@@deriving rust]
end

type 'a located =
  { txt : 'a
  ; pos : Pos.t
  }
[@@deriving rust]

module Name = struct
  module T = struct
    type t = string located [@@deriving rust]
  end

  include T
end

module Record_type = struct
  type t =
    { name : string
    ; fields : string array
    ; ignored : unit -> unit [@rust.skip]
    ; arity : int
    }
  [@@deriving rust] [@@rust.shared]
end

type 'a var =
  { id : id
  ; mutable link : 'a option
  }

and ty =
  | Int
  | Arrow of ty * ty
  | Var of ty var
  | Record of
      { record : Record_type.t
      ; skipped : int -> int [@rust.skip]
      ; fields : (string * ty) list
      }
  | Unit
  | Tuple of (ty * Name.t)
  | Chars of char * bool array
[@@deriving rust] [@@rust.shared]

module Expr = struct
  module Desc = struct
    type 'e t =
      | Lit of int
      | App of 'e * 'e
      | Let of
          { name : Name.t
          ; body : 'e
          ; in_ : 'e
          }
    [@@deriving rust]
  end

  type t =
    { desc : t Desc.t
    ; ty : ty
    ; env : ty String.Map.t
    ; free : String.Set.t
    ; ids : int Core.Int.Map.t
    ; next : t option
    }
  [@@deriving rust]

  type nonrec ty = ty [@@deriving rust]
end

module Defs = struct
  type expr = expr_desc located

  and expr_desc =
    | Const of char * bool
    | Pair of expr * expr option
    | Never of Core.Nothing.t
    | Absurd of Nothing.t list
  [@@deriving rust]
end

open Defs

module Re = struct
  type t = expr [@@deriving rust]
end

module Var = struct
  type 'a t = 'a var [@@deriving rust]
end

module Chain = struct
  type 'a t = 'a Var.t [@@deriving rust]
end

type chained = { var : ty Chain.t } [@@deriving rust]

let () =
  Ppx_rust_runtime.register_rebuild Record_type.rust_t (fun t ->
    { t with ignored = (fun () -> ()) })
;;

let () =
  Ppx_rust_runtime.register_rebuild rust_ty (function
    | Record r -> Record { r with skipped = Fn.id }
    | t -> t)
;;
