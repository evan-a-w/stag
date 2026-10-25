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
        ; context : row
        }

  and row =
    | Empty
    | Extend of string * t * row
    | Var of string

  and arg =
    | Ty of t
    | Row of row
  [@@deriving sexp, compare, equal]
end

module Type_decl = struct
  module Param = struct
    type t =
      | Ty of string
      | Row of string
    [@@deriving sexp, compare, equal]

    let name (Ty name | Row name) = name
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

  let rec binds t name =
    match t with
    | Any | Lit _ -> false
    | Var var -> String.equal var name
    | Tuple patterns | Construct (_, patterns) ->
      List.exists patterns ~f:(fun pattern -> binds pattern name)
    | Record fields ->
      List.exists fields ~f:(fun (_, pattern) -> binds pattern name)
  ;;
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

  let sub_exprs = function
    | Lit _ | Var _ -> []
    | App (f, x) -> [ f; x ]
    | Let (_, _, _, equal, in_) -> [ equal; in_ ]
    | Splice { expr; subs } ->
      expr
      :: List.filter_map (Option.value subs ~default:[]) ~f:(fun (_, expr) ->
        expr)
    | Quote expr | Field (expr, _) -> [ expr ]
    | Tuple exprs | Construct (_, exprs) -> exprs
    | Record fields -> List.map fields ~f:snd
    | Match (expr, cases) -> expr :: List.map cases ~f:snd
  ;;

  (** Over-approximates: a splice with implicit substitutions may refer to any
      name in scope. *)
  let rec refers_to t name =
    match t with
    | Lit _ -> false
    | Var (path, _) -> [%equal: Path.t] path (Path.name name)
    | Let (bound, args, _, equal, in_) ->
      (not (String.equal bound name))
      && (refers_to in_ name
          || ((not
                 (List.exists args ~f:(fun (arg, _) -> String.equal arg name)))
              && refers_to equal name))
    | Splice { subs = None; _ } -> true
    | Splice { expr; subs = Some subs } ->
      refers_to expr name
      || List.exists subs ~f:(fun (label, expr) ->
        match expr with
        | None -> [%equal: Path.t] (Path.of_string label) (Path.name name)
        | Some expr -> refers_to expr name)
    | Match (expr, cases) ->
      refers_to expr name
      || List.exists cases ~f:(fun (pattern, body) ->
        (not (Pattern.binds pattern name)) && refers_to body name)
    | App _ | Quote _ | Tuple _ | Construct _ | Record _ | Field _ ->
      List.exists (sub_exprs t) ~f:(fun expr -> refers_to expr name)
  ;;

  (** Syntactic values, which may be generalized (the relaxed value
      restriction). *)
  let rec is_value = function
    | Lit _ | Var _ -> true
    | Quote expr -> splices_are_values expr ~depth:1
    | Let (name, args, _, equal, in_) ->
      binding_is_value name ~args ~equal && is_value in_
    | App _ | Splice _ -> false
    | Tuple exprs | Construct (_, exprs) -> List.for_all exprs ~f:is_value
    | Record fields -> List.for_all fields ~f:(fun (_, expr) -> is_value expr)
    | Field (expr, _) -> is_value expr
    | Match (expr, cases) ->
      is_value expr && List.for_all cases ~f:(fun (_, expr) -> is_value expr)

  (* Whether the splices run when the enclosing quote [depth] levels out is
     built are values. *)
  and splices_are_values t ~depth =
    match t with
    | Splice { expr; subs } ->
      (if depth = 1
       then is_value expr
       else splices_are_values expr ~depth:(depth - 1))
      && List.for_all (Option.value subs ~default:[]) ~f:(fun (_, expr) ->
        Option.for_all expr ~f:(splices_are_values ~depth))
    | Quote expr -> splices_are_values expr ~depth:(depth + 1)
    | Lit _
    | Var _
    | App _
    | Let _
    | Tuple _
    | Construct _
    | Record _
    | Field _
    | Match _ -> List.for_all (sub_exprs t) ~f:(splices_are_values ~depth)

  (** Local lets are recursive, and a recursive non-function is not a
      value. *)
  and binding_is_value name ~args ~equal =
    (not (List.is_empty args)) || (is_value equal && not (refers_to equal name))
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
