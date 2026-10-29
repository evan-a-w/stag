open! Import

(** The typed tree the typechecker produces, shaped like [Syntax] with names
    resolved. Types are those of the typechecker, only final once the whole
    program is checked (so read them through [Type.canonical_ty]), and may be
    cyclic.

    Sexps render types with [Type.Printer], one printer per toplevel (or per
    outermost value converted), so variables are named consistently within
    it. A node is rendered as its constructor followed by [: type]. *)

module Ident : sig
  (** A binder, local or toplevel. Stamps are unique among the idents made by
      one [Stamp_counter.t]. *)
  type t =
    { name : string
    ; stamp : int
    }

  (** As [name/stamp]. *)
  val sexp_of_t : t -> Sexp.t

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t

  val fresh : Stamp_counter.t -> string -> t
end

module Binder : sig
  type t =
    | Local of Ident.t
    | Toplevel of Ident.t (** Named by its canonical name. *)
  [@@deriving sexp_of, equal]
end

(** What a variable refers to. *)
module Var_ref : sig
  type t =
    | Bound of Binder.t
    (** Bound in the same quote, or outside any quote and usable at this
        level (so a persistent toplevel used at level 0 is [Bound]). *)
    | Persistent of Binder.t
    (** A persistent binder from outside the enclosing quote: its value is
        embedded in the code. *)
    | Free of string
    (** A free variable of the enclosing quote: its context entry of this
        name. *)
    | Free_toplevel of string
    (** A toplevel, by canonical name, as a free variable of the enclosing
        quote (see modes.md): its context entry of that name. *)
    | Prim of Prim.t (** Usable at any level, like a persistent toplevel. *)
    | Intrinsic of Intrinsic.t (** As [Prim]. *)
  [@@deriving sexp_of]
end

(** A constructor or field of a declared type, with its index in the
    declaration. Names are canonical. *)
module Constructor_ref : sig
  type t =
    { type_name : string
    ; name : string
    ; index : int
    }
  [@@deriving sexp_of]

  (** The exception [name], a canonical name. Exceptions are of type [exn]
      and share an index, so are told apart by name. *)
  val exception_ : string -> t

  val is_exception : t -> bool
end

(** A nominal record type: its canonical name, and its fields' canonical
    names by index. One per declaration, shared by its fields' refs and its
    values. *)
module Record_type : sig
  type t =
    { name : string
    ; fields : string array
    }

  (** As its name. *)
  val sexp_of_t : t -> Sexp.t
end

module Field_ref : sig
  type t =
    { record : Record_type.t
    ; index : int
    }

  val name : t -> string

  (** As [((type_name T) (name f) (index i))]. *)
  val sexp_of_t : t -> Sexp.t
end

(** [ty] is the type of the values the node matches. *)
module Pattern : sig
  module Rest : sig
    type t =
      | Closed
      | Open
      | Bind of Ident.t
    [@@deriving sexp_of]
  end

  module Desc : sig
    type 'p t =
      | Any
      | Var of Ident.t
      | Lit of Syntax.Lit.t
      | Tuple of 'p list
      | Construct of Constructor_ref.t * 'p list
      | Record of (Field_ref.t * 'p) list
      | Poly_variant of string * 'p option
      | Poly_record of
          { fields : (string * 'p) list
          ; rest : Rest.t
          }
      | Error of 'p list
      (** What failed to check, of its parts that did. Matching it raises
          [Reached_error]. *)
    [@@deriving sexp_of]
  end

  type t =
    { desc : t Desc.t
    ; ty : Type.ty
    ; loc : Loc.t (** Of the syntax it was inferred from. *)
    }
  [@@deriving sexp_of]

  (** Nominal records and constructors are refutable, even when their type has
      only one constructor. *)
  val is_irrefutable : t -> bool

  (** The variables it binds, in order. *)
  val idents : t -> Ident.t list

  (** Of [Type.error]. *)
  val error : t list -> loc:Loc.t -> t

  val has_error : t -> bool
end

(** [ty] is the type of the node, instantiated where it is a variable. *)
module Expr : sig
  module Subs : sig
    type 'e t =
      | Explicit of (string * 'e) list
      (** Each entry, by name, is the code it is bound to, at the splice's
          level. A substitution without an expression is a [Var]. *)
      | Implicit of Binder.t String.Map.t
      (** The binders at the splice that may provide the code's entries of
          their name. The other entries of the code become entries of the
          enclosing quote's context. *)
    [@@deriving sexp_of]
  end

  module Desc : sig
    type 'e t =
      | Lit of Syntax.Lit.t
      | Var of Var_ref.t
      | App of 'e * 'e
      | Let of
          { ident : Ident.t
          ; args : Ident.t list
          ; annots : (Ident.t * Syntax.Type_expr.t) list
            (** The annotated [args], their annotations as written but for
                type names, which are canonical ([Resolved]). Likewise
                [result_annot] and [Fun]'s and [Annotated]'s. *)
          ; result_annot : Syntax.Type_expr.t option
          ; generalized : bool
          ; body : 'e (** [ident] is bound in [body]. *)
          ; in_ : 'e
          }
      | Fun of
          { args : Ident.t list (** Non-empty. *)
          ; annots : (Ident.t * Syntax.Type_expr.t) list
          ; body : 'e
          }
      | Annotated of 'e * Syntax.Type_expr.t (** [(x : t)], of a [Var]. *)
      | Splice of
          { code : 'e (** At the level outside the splice. *)
          ; subs : 'e Subs.t
          } (** Within a quote. *)
      | Toplevel_splice of
          { code : 'e (** At the level below, evaluated while checking. *)
          ; expansion : 'e
            (** The code [code] evaluated to, instantiated and inferred here,
              within lets binding its explicit substitutions. *)
          } (** A splice outside any quote. *)
      | Quote of 'e
      | Tuple of 'e list
      | Construct of Constructor_ref.t * 'e list
      | Record of (Field_ref.t * 'e) list
      | Field of 'e * Field_ref.t
      | Record_update of 'e * (Field_ref.t * 'e) list
      (** [{e with f = v; ...}], of [e]'s type but for the parameters only the
          updated fields' types contain. *)
      | Match of
          { scrutinee : 'e
          ; cases : (Pattern.t * 'e) list
          ; exception_cases : (Pattern.t * 'e) list
            (** The cases [exception p], as [p]: they match what the scrutinee
              raises. *)
          }
      | Poly_record of (string * 'e) list
      | Poly_field of 'e * string
      | Poly_extend of (string * 'e) list * 'e
      | Poly_restrict of 'e * string
      | Poly_update of 'e * (string * 'e) list
      | Poly_variant of string * 'e option
      | Expect of string Located.t
      (** Checks the output so far against the string, see
          [Expect_test_runner]. *)
      | Type of Syntax.Type_expr.t
      (** [type t], its types named by canonical names ([Resolved]). *)
      | Error of 'e list
      (** In place of what did not parse or check, with the parts of it that
          did. *)
    [@@deriving sexp_of]
  end

  type t =
    { desc : t Desc.t
    ; ty : Type.ty
    ; loc : Loc.t (** Of the syntax it was inferred from. *)
    }
  [@@deriving sexp_of, rust]

  (** The children of [t], in order. *)
  val sub_exprs : t -> t list

  (** Whether [t] has an [Error] node or pattern: what failed to check. *)
  val has_error : t -> bool

  (** Whether [t] refers to a binder satisfying [f], by a [Bound] or
      [Persistent] variable or an [Implicit] substitution. *)
  val refers_to : t -> f:(Binder.t -> bool) -> bool

  (** Syntactic values, which may be generalized (the relaxed value
      restriction). A toplevel splice is judged by its expansion. *)
  val is_value : t -> bool

  (** Whether a [Let] of [ident] may be generalized: a function, or a value
      that doesn't refer to [ident]. *)
  val binding_is_value : ident:Ident.t -> args:Ident.t list -> body:t -> bool
end

module Toplevel : sig
  type t =
    { ident : Ident.t
    ; args : Ident.t list
    ; scheme : Type.ty
    ; body : Expr.t
    }
  [@@deriving sexp_of]
end

(** A strongly connected component of toplevels, in the order they were
    reached. Components may be larger than needed, as references to functions
    from within quotes count. [recursive] if the members need a recursive
    binding: a member refers to a member by a [Bound] or [Persistent]
    variable, or by an [Implicit] substitution. *)
module Scc : sig
  type t =
    { members : Toplevel.t list
    ; recursive : bool
    }
  [@@deriving sexp_of]
end

module Ident_info : sig
  type t =
    { ty : Type.ty
    ; level : int
    }
  [@@deriving sexp_of]
end

module Expect_test : sig
  type t =
    { name : string Located.t
    ; body : Expr.t
    ; loc : Loc.t
    ; generated : bool (** By a declaration splice, so located at the splice. *)
    }
  [@@deriving sexp_of]
end

(** [let%bench "name" = body], or [let%bench "name" x in [a; b] = body]. *)
module Bench : sig
  type t =
    { name : string Located.t
    ; param : (Ident.t * Expr.t list) option
      (** The parameter bound in [body], and its arguments, at level 0. *)
    ; body : Expr.t (** At level 0, of any type. *)
    ; loc : Loc.t
    ; generated : bool (** By a declaration splice, so located at the splice. *)
    }
  [@@deriving sexp_of]
end

module Program : sig
  type t =
    { sccs : Scc.t list (** Each component after the ones it refers to. *)
    ; expect_tests : Expect_test.t list (** In program order. *)
    ; benches : Bench.t list (** In program order. *)
    ; declaration_splices : Expr.t list
      (** The expression of each declaration splice that ran, at level -1. *)
    ; ident_info : Ident_info.t Ident.Map.t
    }
  [@@deriving sexp_of]

  (** Whether any of its expressions [Expr.has_error]. *)
  val has_error : t -> bool
end
