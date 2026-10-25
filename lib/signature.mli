open! Core

(** A checked program: its module tree, with items in program order. Items are
    named unqualified, while types ([Type.TCon]) name declared types
    canonically: by the full module path joined with [.]. *)

module Value : sig
  type t =
    { name : string
    ; scheme : Type.ty
    ; persistent : bool
    }
  [@@deriving sexp_of]
end

module Constructor : sig
  type t =
    { name : string
    ; args : Type.ty list
    }
  [@@deriving sexp_of]
end

module Field : sig
  type t =
    { name : string
    ; ty : Type.ty
    }
  [@@deriving sexp_of]
end

module Type_decl : sig
  module Kind : sig
    type t =
      | Variant of Constructor.t list
      | Record of Field.t list
      | Alias of Type.ty (** Expanded. *)
      | Abstract
    [@@deriving sexp_of]
  end

  (** The [params] are [Generic], and shared with [kind]. *)
  type t =
    { name : string
    ; params : Type.arg list
    ; kind : Kind.t
    }
  [@@deriving sexp_of]
end

module Item : sig
  type t =
    | Value of Value.t
    | Type of Type_decl.t
    | Module of
        { name : string
        ; items : t list
        }
  [@@deriving sexp_of]
end

type t = Item.t list [@@deriving sexp_of]

(** Renders [t] like an OCaml signature, one item per line, types as by
    [Type.to_string_hum]. Variables are named per item.

    A type constructor is named relative to the module of the item it is in:
    by the shortest suffix of its canonical name that resolves back to it
    lexically from there, e.g. [M.N.t] is [t] inside [M.N], [N.t] inside [M]
    and [M.N.t] at the root. If no suffix does, as for a root type shadowed by
    a type of the same name in an enclosing module, it is the canonical name
    after a leading [.], which stands for the root: [.t], [.M.t]. *)
val to_string_hum : t -> string
