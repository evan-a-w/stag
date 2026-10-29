open! Core

type id = int [@@deriving rust, sexp_of]

module Pos = struct
  type t =
    { line : int
    ; col : int
    }
  [@@deriving rust, sexp_of]
end

type 'a located =
  { txt : 'a
  ; pos : Pos.t
  }
[@@deriving rust, sexp_of]

module Name = struct
  module T = struct
    type t = string located [@@deriving rust, sexp_of]
  end

  include T
end

module Record_type = struct
  type t =
    { name : string
    ; fields : string array
    ; ignored : unit -> unit [@rust.skip] [@sexp.opaque]
    ; arity : int
    }
  [@@deriving rust, sexp_of] [@@rust.shared]
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
      ; skipped : int -> int [@rust.skip] [@sexp.opaque]
      ; fields : (string * ty) list
      }
  | Unit
  | Tuple of (ty * Name.t)
  | Chars of char * bool array
[@@deriving rust, sexp_of] [@@rust.shared]

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
    [@@deriving rust, sexp_of]
  end

  type t =
    { desc : t Desc.t
    ; ty : ty
    ; env : ty String.Map.t
    ; free : String.Set.t
    ; ids : int Core.Int.Map.t
    ; next : t option
    }
  [@@deriving rust, sexp_of]

  type nonrec ty = ty [@@deriving rust, sexp_of]
end

module Defs = struct
  (* The sexp_of of [Never] is unreachable. *)
  [@@@warning "-56"]

  type expr = expr_desc located

  and expr_desc =
    | Const of char * bool
    | Pair of expr * expr option
    | Never of Core.Nothing.t
    | Absurd of Nothing.t list
  [@@deriving rust, sexp_of]
end

open Defs

module Re = struct
  type t = expr [@@deriving rust, sexp_of]
end

module Var = struct
  type 'a t = 'a var [@@deriving rust, sexp_of]
end

module Chain = struct
  type 'a t = 'a Var.t [@@deriving rust, sexp_of]
end

type chained = { var : ty Chain.t } [@@boxed] [@@deriving rust, sexp_of]

let () =
  Ppx_rust_runtime.register_rebuild Record_type.rust_t (fun t ->
    if String.equal t.name "raise" then failwith "rebuilding raise";
    { t with ignored = (fun () -> ()) })
;;

let () =
  Ppx_rust_runtime.register_rebuild rust_ty (function
    | Record r -> Record { r with skipped = Fn.id }
    | t -> t)
;;
