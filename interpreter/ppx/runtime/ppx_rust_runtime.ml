open Sexplib0.Sexp_conv

module Key = struct
  type t = string list [@@deriving sexp_of]
end

module Witness = struct
  type 'a t = { key : Key.t }
end

module Type_expr = struct
  type t =
    | Var of string
    | Decl of Key.t * t list
    | Tuple of t list
    | Int
    | Bool
    | Char
    | Unit
    | String
    | Nothing
    | List of t
    | Array of t
    | Option of t
    | Map of t * t
    | Set of t
  [@@deriving sexp_of]
end

module Field = struct
  type t =
    { name : string
    ; ty : Type_expr.t
    ; mutable_ : bool
    ; skip : bool
    }
  [@@deriving sexp_of]
end

module Constructor = struct
  module Args = struct
    type t =
      | Tuple of Type_expr.t list
      | Record of Field.t list
    [@@deriving sexp_of]
  end

  type t =
    { name : string
    ; args : Args.t
    }
  [@@deriving sexp_of]
end

module Kind = struct
  type t =
    | Alias of Type_expr.t
    | Record of Field.t list
    | Variant of Constructor.t list
  [@@deriving sexp_of]
end

module Decl = struct
  type t =
    { key : Key.t
    ; params : string list
    ; kind : Kind.t
    ; shared : bool
    }
  [@@deriving sexp_of]
end

let registered = ref []
let register decls = registered := List.rev_append decls !registered
let all () = List.rev !registered
