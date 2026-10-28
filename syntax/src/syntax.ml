open! Import

module Lit = struct
  type t =
    | Int of int
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

(** What names a declared type, constructor, field or toplevel value: a path
    as written, resolved where it is, or from the root scope ([.M.x], which
    nothing local shadows), or a canonical name, which reflection and
    [Stag.Code.erase] give (see [Stag.Type_ref] in the prelude) and the parser
    never makes, naming the same declaration wherever it is. *)
module Decl_path = struct
  type t =
    | Path of Path.t
    | Root of Path.t
    | Resolved of string
  [@@deriving compare, equal]

  (** As parsed, and a [Resolved] as [<name>]. *)
  let to_string = function
    | Path path -> Path.to_string path
    | Root path -> "." ^ Path.to_string path
    | Resolved name -> sprintf "<%s>" (Prelude_path.to_string_hum name)
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)

  (** The source naming the declaration of canonical name [name] from
      anywhere, given the program's root modules: its root path, unless the
      first name of a prelude path is a root module (or it is an intrinsic,
      named only within the prelude), where nothing names it, so as
      [<prelude.P>], which does not parse, [P] its public path. *)
  let resolved_source ~root_modules name =
    let path = Prelude_path.root_path name in
    let unnamed =
      Option.is_some (Prelude_path.relative name)
      &&
      match String.split path ~on:'.' with
      | first :: _ :: _ ->
        Set.mem root_modules first || String.equal first "Intrinsic"
      | _ -> false
    in
    if unnamed then sprintf "<%s>" (Prelude_path.canonical path) else "." ^ path
  ;;
end

(** A name as written, eg. a binder or a label. *)
module Name = struct
  type t = string Located.t [@@deriving sexp_of, compare, equal]
end

module Type_expr = struct
  type t = desc Located.t

  and desc =
    | Var of string
    | Con of Decl_path.t Located.t * arg list
    | Arrow of t * t
    | Tuple of t list
    | Code of
        { inner : t
        ; context : row
        }
    | Record of row
    | Variant of row (** Each entry is a tag and its argument type. *)

  and row = row_desc Located.t

  and row_desc =
    | Empty
    | Extend of Name.t * t * row
    | Var of Name.t

  and arg =
    | Ty of t
    | Row of row
  [@@deriving sexp_of, compare, equal]
end

module Type_decl = struct
  module Param = struct
    type t =
      | Ty of string
      | Row of string
    [@@deriving sexp_of, compare, equal]

    let name (Ty name | Row name) = name
  end

  module Kind = struct
    type t =
      | Variant of (Name.t * Type_expr.t list) list
      | Record of (Name.t * Type_expr.t) list
      | Alias of Type_expr.t
      | Abstract
    [@@deriving sexp_of, compare, equal]
  end

  type t =
    { name : Name.t
    ; params : Param.t Located.t list
    ; kind : Kind.t
    }
  [@@deriving sexp_of, compare, equal]
end

module Binding = struct
  type t = Name.t * Type_expr.t option [@@deriving sexp_of, compare, equal]
end

module Pattern = struct
  (** What a polymorphic record pattern says about the fields it does not
      list. *)
  module Rest = struct
    type t =
      | Closed (** There are none. *)
      | Open (** There may be some. *)
      | Bind of Name.t (** Binds the record without the listed fields. *)
    [@@deriving sexp_of, compare, equal]
  end

  type t = desc Located.t

  and desc =
    | Any
    | Var of string
    | Lit of Lit.t
    | Tuple of t list
    | Construct of Decl_path.t Located.t * t list
    | Record of (Decl_path.t Located.t * t) list
    | Poly_variant of Name.t * t option (** [None] matches a [unit] argument. *)
    | Poly_record of
        { fields : (Name.t * t) list
        ; rest : Rest.t
        }
    | Exception of t
    (** [exception p], at the top of a match arm: matches an exception the
        scrutinee raises. *)
  [@@deriving sexp_of, compare, equal]
end

module Expr = struct
  type t = desc Located.t

  and desc =
    | Lit of Lit.t
    | Var of Decl_path.t * Type_expr.t option
    | App of t * t
    | Let of Name.t * Binding.t list * Type_expr.t option * t * t
    | Fun of Binding.t list * t (** [fun x (y : t) -> e], of some params. *)
    | Splice of
        { expr : t
        ; subs :
            (Name.t * t option) list
              option (* [None] is all in scope, so is diff to [] *)
        }
    | Quote of t
    | Tuple of t list
    | Construct of Decl_path.t Located.t * t list
    | Record of (Decl_path.t Located.t * t) list
    | Field of t * Decl_path.t Located.t
    | Record_update of t * (Decl_path.t Located.t * t) list
    (** [{e with f = v; ...}] *)
    | Type of Type_expr.t (** [type t]: the type as a value. *)
    | Match of t * (Pattern.t * t) list
    | Poly_record of (Name.t * t) list
    | Poly_field of t * Name.t
    | Poly_extend of (Name.t * t) list * t (** [{x = v; ... | e}] *)
    | Poly_restrict of t * Name.t (** [e \ x] *)
    | Poly_update of t * (Name.t * t) list (** [{e with x = v; ...}] *)
    | Poly_variant of Name.t * t option (** [None] is a [unit] argument. *)
    | Expect of string Located.t
    (** [[%expect {|...|}]], located at the string, or zero width after
        [[%expect] if there is none. *)
    | Error (** In place of what did not parse. *)
  [@@deriving sexp_of, compare, equal]
end

(** [n in [a; b]] of [let%bench "name" n in [a; b] = e]. *)
module Bench_param = struct
  type t =
    { name : Name.t
    ; args : Expr.t list (** Non-empty. *)
    }
  [@@deriving sexp_of, compare, equal]
end

module Toplevel = struct
  type t = desc Located.t

  and desc =
    | Let of Name.t * Binding.t list * Type_expr.t option * Expr.t
    | Type of Type_decl.t
    | Exception of Name.t * Type_expr.t list (** [exception E of t1 * t2]. *)
    | Module of Name.t * t list
    | Module_splice of Name.t * Expr.t * t list Located.t option
    (** [module M = .~e], or with its expansion
        [module%inline M = .~e ... end%inline]. *)
    | Include of Expr.t * t list Located.t option
    (** [include .~e], or with its expansion
        [include%inline .~e ... end%inline]. An expansion is the declarations
        written between the splice and [end%inline], located from the end of
        the splice to the start of [end%inline]. It is only checked against
        what the splice generates, which is what is declared. *)
    | Expect_test of
        { name : string Located.t
        ; body : Expr.t
        }
    | Bench of
        { name : string Located.t
        ; param : Bench_param.t option
        ; body : Expr.t
        }
  [@@deriving sexp_of, compare, equal]
end

module Program = struct
  type t = Toplevel.t list [@@deriving sexp_of, compare, equal]
end
