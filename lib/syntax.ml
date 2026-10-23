open! Core

module Lit = struct
  type t =
    | Int of string
    | Bool of bool
    | String of string
    | Char of char
    | Unit
  [@@deriving sexp, compare, equal, variants]

  let ty = function
    | Int _ -> Type.int
    | Bool _ -> Type.bool
    | String _ -> Type.string
    | Char _ -> Type.char
    | Unit -> Type.unit
  ;;
end

module Path : sig
  (** Non-empty. Rendered as its names joined with [.]. *)
  type t = private string list [@@deriving sexp, compare, equal]

  val of_list_exn : string list -> t
  val name : string -> t

  (** Splits on [.]. *)
  val of_string : string -> t

  val to_string : t -> string
end = struct
  type t = string list [@@deriving compare, equal]

  let of_list_exn = function
    | [] -> raise_s [%message "Path.of_list_exn: empty path"]
    | names -> names
  ;;

  let name name = [ name ]
  let of_string string = String.split string ~on:'.' |> of_list_exn
  let to_string t = String.concat t ~sep:"."
  let sexp_of_t t = Sexp.Atom (to_string t)
  let t_of_sexp sexp = of_string (string_of_sexp sexp)
end

module Type_expr = struct
  type t =
    | Var of string
    | Con of Path.t * arg list
    | Arrow of t * t
    | Tuple of t list
    | Code of
        { inner : t
        ; context : context
        }

  and context =
    | Empty
    | Extend of string * t * context
    | Var of string

  and arg =
    | Ty of t
    | Context of context
  [@@deriving sexp, compare, equal]
end

module Type_decl = struct
  module Param = struct
    type t =
      | Ty of string
      | Context of string
    [@@deriving sexp, compare, equal]

    let name (Ty name | Context name) = name
  end

  module Kind = struct
    type t =
      | Variant of (string * Type_expr.t list) list
      | Record of (string * Type_expr.t) list
      | Alias of Type_expr.t
    [@@deriving sexp, compare, equal]
  end

  type t =
    { name : string
    ; params : Param.t list
    ; kind : Kind.t
    }
  [@@deriving sexp, compare, equal]
end

module Binding = struct
  type t = string * Type_expr.t option [@@deriving sexp, compare, equal]
end

module Pattern = struct
  type t =
    | Any
    | Var of string
    | Lit of Lit.t
    | Tuple of t list
    | Construct of Path.t * t list
    | Record of (Path.t * t) list
  [@@deriving sexp, compare, equal]
end

module Expr = struct
  type t =
    | Lit of Lit.t
    | Var of Path.t * Type_expr.t option
    | App of t * t
    | Let of string * Binding.t list * Type_expr.t option * t * t
    | Splice of
        { expr : t
        ; subs :
            (string * t option) list
              option (* [None] is all in scope, so is diff to [] *)
        }
    | Quote of t
    | Tuple of t list
    | Construct of Path.t * t list
    | Record of (Path.t * t) list
    | Field of t * Path.t
    | Match of t * (Pattern.t * t) list
  [@@deriving sexp, compare, equal, variants]

  (** Syntactic values, which may be generalized (the relaxed value
      restriction). *)
  let rec is_value = function
    | Lit _ | Var _ | Quote _ -> true
    | Let (_, args, _, equal, in_) ->
      ((not (List.is_empty args)) || is_value equal) && is_value in_
    | App _ | Splice _ -> false
    | Tuple exprs | Construct (_, exprs) -> List.for_all exprs ~f:is_value
    | Record fields -> List.for_all fields ~f:(fun (_, expr) -> is_value expr)
    | Field (expr, _) -> is_value expr
    | Match (expr, cases) ->
      is_value expr && List.for_all cases ~f:(fun (_, expr) -> is_value expr)
  ;;
end

module Toplevel = struct
  type t =
    | Let of string * Binding.t list * Type_expr.t option * Expr.t
    | Type of Type_decl.t
    | Module of string * t list
  [@@deriving sexp, compare, equal, variants]
end

module Program = struct
  type t = Toplevel.t list [@@deriving sexp, compare, equal]
end
