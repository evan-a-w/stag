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

(* The syntax's types are mutually recursive, as splices in type positions
   hold expressions and struct quotes hold declarations; each is re-exported
   by its module below. *)
[@@@warning "-30"]

type type_expr = type_expr_desc Located.t

and type_expr_desc =
  | Var of string
  | Con of Decl_path.t Located.t * type_arg list
  | Arrow of type_expr * type_expr
  | Tuple of type_expr list
  | Code of
      { inner : type_expr
      ; context : row
      }
  | Record of row
  | Variant of row (** Each entry is a tag and its argument type. *)
  | Splice of expr
  (** [.~e]: the type [e : Stag.Type_expr.t] evaluates to at compile time, or
      an antiquote in a struct quote. *)

and row = row_desc Located.t

and row_desc =
  | Empty
  | Extend of Name.t * type_expr * row
  | Var of Name.t

and type_arg =
  | Ty of type_expr
  | Row of row

and type_decl_param =
  | Ty of string
  | Row of string

and type_decl_kind =
  | Variant of (Name.t * type_expr list) list
  | Record of (Name.t * type_expr) list
  | Alias of type_expr
  | Abstract
  | Splice of expr
  (** [type t = .~e]: the definition [e : Stag.Type_decl.Kind.t] evaluates to
      at compile time, declared as an include of it, or an antiquote in a
      struct quote. *)

and type_decl =
  { name : Name.t
  ; params : type_decl_param Located.t list
  ; kind : type_decl_kind
  }

and binding = Name.t * type_expr option

and pattern_rest =
  | Closed (** There are none. *)
  | Open (** There may be some. *)
  | Bind of Name.t (** Binds the record without the listed fields. *)

and pattern = pattern_desc Located.t

and pattern_desc =
  | Any
  | Var of string
  | Lit of Lit.t
  | Tuple of pattern list
  | Construct of Decl_path.t Located.t * pattern list
  | Record of (Decl_path.t Located.t * pattern) list
  | Poly_variant of Name.t * pattern option
  (** [None] matches a [unit] argument. *)
  | Poly_record of
      { fields : (Name.t * pattern) list
      ; rest : pattern_rest
      }
  | Exception of pattern
  (** [exception p], at the top of a match arm: matches an exception the
      scrutinee raises. *)

and expr = expr_desc Located.t

and expr_desc =
  | Lit of Lit.t
  | Var of Decl_path.t * type_expr option
  | App of expr * expr
  | Let of Name.t * binding list * type_expr option * expr * expr
  | Fun of binding list * expr (** [fun x (y : t) -> e], of some params. *)
  | Splice of
      { expr : expr
      ; subs :
          (Name.t * expr option) list
            option (* [None] is all in scope, so is diff to [] *)
      }
  | Quote of expr
  | Tuple of expr list
  | Construct of Decl_path.t Located.t * expr list
  | Record of (Decl_path.t Located.t * expr) list
  | Field of expr * Decl_path.t Located.t
  | Record_update of expr * (Decl_path.t Located.t * expr) list
  (** [{e with f = v; ...}] *)
  | Type of type_expr (** [type t]: the type as a value. *)
  | Match of expr * (pattern * expr) list
  | Poly_record of (Name.t * expr) list
  | Poly_field of expr * Name.t
  | Poly_extend of (Name.t * expr) list * expr (** [{x = v; ... | e}] *)
  | Poly_restrict of expr * Name.t (** [e \ x] *)
  | Poly_update of expr * (Name.t * expr) list (** [{e with x = v; ...}] *)
  | Poly_variant of Name.t * expr option
  (** [None] is a [unit] argument. *)
  | Expect of string Located.t
  (** [[%expect {|...|}]], located at the string, or zero width after
      [[%expect] if there is none. *)
  | Struct_quote of toplevel list
  (** [.< struct ... end >.]: the declarations as a
      [list[Stag.Toplevel.t]], with antiquotes. *)

and bench_param =
  { name : Name.t
  ; args : expr list (** Non-empty. *)
  }

and toplevel = toplevel_desc Located.t

and toplevel_desc =
  | Let of Name.t * binding list * type_expr option * expr
  | Type of type_decl
  | Exception of Name.t * type_expr list (** [exception E of t1 * t2]. *)
  | Module of Name.t * toplevel list
  | Module_splice of Name.t * expr * toplevel list Located.t option
  (** [module M = .~e], or with its expansion
      [module%inline M = .~e ... end%inline]. *)
  | Include of expr * toplevel list Located.t option
  (** [include .~e], or with its expansion
      [include%inline .~e ... end%inline]. An expansion is the declarations
      written between the splice and [end%inline], located from the end of
      the splice to the start of [end%inline]. It is only checked against
      what the splice generates, which is what is declared. *)
  | Expect_test of
      { name : string Located.t
      ; body : expr
      }
  | Bench of
      { name : string Located.t
      ; param : bench_param option
      ; body : expr
      }
[@@deriving sexp_of, compare, equal]

[@@@warning "+30"]

module Type_expr = struct
  type nonrec row = row

  type arg = type_arg =
    | Ty of type_expr
    | Row of row

  type t = type_expr

  type desc = type_expr_desc =
    | Var of string
    | Con of Decl_path.t Located.t * arg list
    | Arrow of t * t
    | Tuple of t list
    | Code of
        { inner : t
        ; context : row
        }
    | Record of row
    | Variant of row
    | Splice of expr

  type nonrec row_desc = row_desc =
    | Empty
    | Extend of Name.t * t * row
    | Var of Name.t

  let sexp_of_t = sexp_of_type_expr
  let compare = compare_type_expr
  let equal = equal_type_expr
end

module Type_decl = struct
  module Param = struct
    type t = type_decl_param =
      | Ty of string
      | Row of string

    let sexp_of_t = sexp_of_type_decl_param
    let compare = compare_type_decl_param
    let equal = equal_type_decl_param
    let name (Ty name | Row name) = name
  end

  module Kind = struct
    type t = type_decl_kind =
      | Variant of (Name.t * Type_expr.t list) list
      | Record of (Name.t * Type_expr.t) list
      | Alias of Type_expr.t
      | Abstract
      | Splice of expr

    let sexp_of_t = sexp_of_type_decl_kind
    let compare = compare_type_decl_kind
    let equal = equal_type_decl_kind
  end

  type t = type_decl =
    { name : Name.t
    ; params : Param.t Located.t list
    ; kind : Kind.t
    }

  let sexp_of_t = sexp_of_type_decl
  let compare = compare_type_decl
  let equal = equal_type_decl
end

module Binding = struct
  type t = binding

  let sexp_of_t = sexp_of_binding
  let compare = compare_binding
  let equal = equal_binding
end

module Pattern = struct
  (** What a polymorphic record pattern says about the fields it does not
      list. *)
  module Rest = struct
    type t = pattern_rest =
      | Closed
      | Open
      | Bind of Name.t

    let sexp_of_t = sexp_of_pattern_rest
    let compare = compare_pattern_rest
    let equal = equal_pattern_rest
  end

  type t = pattern

  type desc = pattern_desc =
    | Any
    | Var of string
    | Lit of Lit.t
    | Tuple of t list
    | Construct of Decl_path.t Located.t * t list
    | Record of (Decl_path.t Located.t * t) list
    | Poly_variant of Name.t * t option
    | Poly_record of
        { fields : (Name.t * t) list
        ; rest : Rest.t
        }
    | Exception of t

  let sexp_of_t = sexp_of_pattern
  let compare = compare_pattern
  let equal = equal_pattern
end

module Expr = struct
  type t = expr

  type desc = expr_desc =
    | Lit of Lit.t
    | Var of Decl_path.t * Type_expr.t option
    | App of t * t
    | Let of Name.t * Binding.t list * Type_expr.t option * t * t
    | Fun of Binding.t list * t
    | Splice of
        { expr : t
        ; subs : (Name.t * t option) list option
        }
    | Quote of t
    | Tuple of t list
    | Construct of Decl_path.t Located.t * t list
    | Record of (Decl_path.t Located.t * t) list
    | Field of t * Decl_path.t Located.t
    | Record_update of t * (Decl_path.t Located.t * t) list
    | Type of Type_expr.t
    | Match of t * (Pattern.t * t) list
    | Poly_record of (Name.t * t) list
    | Poly_field of t * Name.t
    | Poly_extend of (Name.t * t) list * t
    | Poly_restrict of t * Name.t
    | Poly_update of t * (Name.t * t) list
    | Poly_variant of Name.t * t option
    | Expect of string Located.t
    | Struct_quote of toplevel list

  let sexp_of_t = sexp_of_expr
  let compare = compare_expr
  let equal = equal_expr
end

(** [n in [a; b]] of [let%bench "name" n in [a; b] = e]. *)
module Bench_param = struct
  type t = bench_param =
    { name : Name.t
    ; args : Expr.t list
    }

  let sexp_of_t = sexp_of_bench_param
  let compare = compare_bench_param
  let equal = equal_bench_param
end

module Toplevel = struct
  type t = toplevel

  type desc = toplevel_desc =
    | Let of Name.t * Binding.t list * Type_expr.t option * Expr.t
    | Type of Type_decl.t
    | Exception of Name.t * Type_expr.t list
    | Module of Name.t * t list
    | Module_splice of Name.t * Expr.t * t list Located.t option
    | Include of Expr.t * t list Located.t option
    | Expect_test of
        { name : string Located.t
        ; body : Expr.t
        }
    | Bench of
        { name : string Located.t
        ; param : Bench_param.t option
        ; body : Expr.t
        }

  let sexp_of_t = sexp_of_toplevel
  let compare = compare_toplevel
  let equal = equal_toplevel
end

module Program = struct
  type t = Toplevel.t list [@@deriving sexp_of, compare, equal]
end
