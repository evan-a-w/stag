open! Core

(** The typed tree the typechecker produces, shaped like [Syntax] with names
    resolved. Types are those of the typechecker, only final once the whole
    program is checked (so read them through [Type.canonical_ty]), and may be
    cyclic.

    Sexps render types with [Type.Printer], one printer per toplevel (or per
    outermost value converted), so variables are named consistently within
    it. A node is rendered as its constructor followed by [: type]. *)

module Ident : sig
  (** A local binder. Stamps are unique among the binders of one program. *)
  type t =
    { name : string
    ; stamp : int
    }
  [@@deriving compare, equal, hash]

  (** As [name/stamp]. *)
  val sexp_of_t : t -> Sexp.t

  include Comparable.S_plain with type t := t
  include Hashable.S_plain with type t := t
end

module Binder : sig
  type t =
    | Local of Ident.t
    | Toplevel of string (** By canonical name. *)
  [@@deriving compare, equal, sexp_of]
end

(** What a variable refers to. *)
module Var_ref : sig
  type t =
    | Bound of Binder.t
    (** Bound at the level the variable is used at, and in the same quote if
        there is one. *)
    | Persistent of Binder.t
    (** A persistent binder from outside the enclosing quote: its value is
        embedded in the code. *)
    | Free of string
    (** A free variable of the enclosing quote: its context entry of this
        name. *)
  [@@deriving compare, equal, sexp_of]
end

(** A constructor or field of a declared type, with its index in the
    declaration. Names are canonical. *)
module Constructor_ref : sig
  type t =
    { type_name : string
    ; name : string
    ; index : int
    }
  [@@deriving compare, equal, sexp_of]
end

module Field_ref : sig
  type t =
    { type_name : string
    ; name : string
    ; index : int
    }
  [@@deriving compare, equal, sexp_of]
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
    [@@deriving sexp_of]
  end

  type t =
    { desc : t Desc.t
    ; ty : Type.ty
    }
  [@@deriving sexp_of]
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
          ; generalized : bool
          ; body : 'e (** [ident] is bound in [body]. *)
          ; in_ : 'e
          }
      | Splice of
          { code : 'e (** At the level outside the splice. *)
          ; subs : 'e Subs.t
          }
      | Quote of 'e
      | Tuple of 'e list
      | Construct of Constructor_ref.t * 'e list
      | Record of (Field_ref.t * 'e) list
      | Field of 'e * Field_ref.t
      | Match of 'e * (Pattern.t * 'e) list
      | Poly_record of (string * 'e) list
      | Poly_field of 'e * string
      | Poly_extend of (string * 'e) list * 'e
      | Poly_restrict of 'e * string
      | Poly_update of 'e * (string * 'e) list
      | Poly_variant of string * 'e option
    [@@deriving sexp_of]
  end

  type t =
    { desc : t Desc.t
    ; ty : Type.ty
    }
  [@@deriving sexp_of]
end

module Toplevel : sig
  type t =
    { name : string (** Canonical. *)
    ; args : Ident.t list
    ; scheme : Type.ty
    ; body : Expr.t
    }
  [@@deriving sexp_of]
end

(** A strongly connected component of toplevels, in the order they were
    reached. [recursive] if a member refers to a member. *)
module Scc : sig
  type t =
    { members : Toplevel.t list
    ; recursive : bool
    }
  [@@deriving sexp_of]
end

module Program : sig
  type t =
    { sccs : Scc.t list (** Each component after the ones it refers to. *)
    ; idents : Type.ty Ident.Map.t
      (** The type of each binder: its scheme for a generalized [Let]. *)
    }
  [@@deriving sexp_of]
end
