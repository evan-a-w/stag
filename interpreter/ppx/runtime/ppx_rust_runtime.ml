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

  let key_name = function
    | Decl (key, []) -> Some (String.concat "." key)
    | Int -> Some "int"
    | String -> Some "string"
    | Char -> Some "char"
    | Bool -> Some "bool"
    | Var _
    | Decl _
    | Tuple _
    | Unit
    | Nothing
    | List _
    | Array _
    | Option _
    | Map _
    | Set _ -> None
  ;;
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

module Callback_name = struct
  let map name = "ppx_rust.map." ^ name
  let set name = "ppx_rust.set." ^ name
  let rebuild key = "ppx_rust.rebuild." ^ String.concat "." key
end

let key_name ty =
  match Type_expr.key_name ty with
  | Some name -> name
  | None -> invalid_arg "Ppx_rust_runtime: unsupported map key or set element"
;;

let register_map ~key of_alist =
  Callback.register (Callback_name.map (key_name key)) of_alist
;;

let register_set ~elt of_list =
  Callback.register (Callback_name.set (key_name elt)) of_list
;;

let rebuilds = ref []

let register_rebuild (witness : _ Witness.t) f =
  rebuilds := witness.key :: !rebuilds;
  Callback.register (Callback_name.rebuild witness.key) f
;;

let rebuilt () = List.rev !rebuilds
