open! Import

(** The names of a file's syntax, classified by where they are written. *)

module Kind : sig
  type t =
    | Module
    | Type (** A declared type, in a type expression or declaration. *)
    | Constructor (** Also of an exception. *)
    | Field
    | Poly_label (** A polymorphic record field or variant tag. *)
    | Value (** A variable used. *)
    | Let of { is_function : bool } (** A [let]'s name. *)
    | Parameter
    | Pattern_var
  [@@deriving sexp_of, equal]
end

module Occurrence : sig
  type t =
    { loc : Loc.t
      (** Of the whole path, eg. [M.N.x], whose last name is of [kind] and the
          others modules. *)
    ; kind : Kind.t
    ; path : string list
    ; declaration : bool
    ; module_path : string list
      (** Of the module it is in, eg. [["Foo"; "A"]] in [a.stag] of library
          [foo], to resolve [path] lexically. *)
    }
  [@@deriving sexp_of]
end

(** The occurrences in [toplevels], within the module [module_path], in no
    particular order. *)
val of_toplevels
  :  module_path:string list
  -> Syntax.Toplevel.t list
  -> Occurrence.t list
