(** Descriptions of OCaml type declarations, as [[@@deriving rust]] makes
    them, from which [Rust_bindgen] generates the Rust types and their
    conversions from OCaml values.

    It depends only on the standard library and [Sexplib0], as every library
    deriving [rust] links it. *)

(** A declaration's module path and name, eg. [["Typed"; "Expr"; "t"]].
    Modules named [T] are left out, as their parent includes them (the
    [module T = struct ... end include T] idiom). *)
module Key : sig
  type t = string list [@@deriving sexp_of]
end

(** [rust_u : u Witness.t] is defined next to each derived type [u], so a
    declaration's references to [u] are resolved by OCaml itself: by looking
    up [rust_u] where [u] is written, checked to be of type [u Witness.t]. *)
module Witness : sig
  type 'a t = { key : Key.t }
end

(** Type expressions, their names resolved. *)
module Type_expr : sig
  type t =
    | Var of string (** Without the quote. *)
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
    | Map of t * t (** A Base map, of its key and data. *)
    | Set of t (** A Base set. *)
  [@@deriving sexp_of]

  (** The name of a map key or set element type, [None] for those that
      cannot be: [Decl]s without arguments, [int], [string], [char] and
      [bool]. *)
  val key_name : t -> string option
end

module Field : sig
  type t =
    { name : string
    ; ty : Type_expr.t
    ; mutable_ : bool
    ; skip : bool (** [[@rust.skip]]: left out of the Rust type. *)
    }
  [@@deriving sexp_of]
end

module Constructor : sig
  module Args : sig
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

module Kind : sig
  type t =
    | Alias of Type_expr.t
    | Record of Field.t list
    | Variant of Constructor.t list
  [@@deriving sexp_of]
end

module Decl : sig
  type t =
    { key : Key.t
    ; params : string list
    ; kind : Kind.t
    ; shared : bool
      (** [[@@rust.shared]]: each OCaml value is converted once, into an
          arena. *)
    }
  [@@deriving sexp_of]
end

(** Records a group of declarations, at module initialisation. *)
val register : Decl.t list -> unit

(** Every declaration registered so far, in order. *)
val all : unit -> Decl.t list

(** The names of the OCaml callbacks ([Callback.register]) that Rust code
    building OCaml values calls. *)
module Callback_name : sig
  (** Builds the map of an association list of a key type, by its
      [Type_expr.key_name]. *)
  val map : string -> string

  (** Builds the set of a list of an element type. *)
  val set : string -> string

  (** Rebuilds a value of a declaration with [[@rust.skip]] fields. *)
  val rebuild : Key.t -> string
end

(** [[@@deriving rust]] registers, with these, how to build the maps and sets
    its declarations contain, from their comparators: the name of a key type
    stands for one comparator. *)
val register_map : key:Type_expr.t -> (Obj.t -> Obj.t) -> unit

val register_set : elt:Type_expr.t -> (Obj.t -> Obj.t) -> unit

(** [register_rebuild rust_u f] registers how to rebuild a [u] whose
    [[@rust.skip]] fields are [()], as values built from Rust are, eg. by
    looking it up by name. Declarations with [[@rust.skip]] fields need one,
    which may not read those fields. *)
val register_rebuild : 'a Witness.t -> ('a -> 'a) -> unit

(** The declarations [register_rebuild] was called for. *)
val rebuilt : unit -> Key.t list
