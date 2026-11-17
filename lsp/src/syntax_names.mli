open! Import

(** The names of a file's syntax, classified by where they are written. *)

module Kind : sig
  type t =
    | Module
    | Type (** A declared type, in a type expression or declaration. *)
    | Constructor (** Also of an exception. *)
    | Field
    | Poly_field (** Of a polymorphic record. *)
    | Poly_tag (** Of a polymorphic variant. *)
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
    (** Of the module it is in, eg. [["Foo"; "A"]] in [a.stag] of library [foo],
        to resolve [path] lexically: [[]] for a root path. *)
    ; quoted : bool
    (** In a struct quote, so resolved where the declarations are spliced, not
        here. *)
    }
  [@@deriving sexp_of]
end

(** The occurrences in [toplevels], within the module [module_path], in no
    particular order, and in the inline expansions [walk_expansion] accepts (by
    default, all). *)
val of_toplevels
  :  ?walk_expansion:(Syntax.Toplevel.t list Located.t -> bool)
  -> module_path:string list
  -> Syntax.Toplevel.t list
  -> Occurrence.t list
