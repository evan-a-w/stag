open! Import

(** How to build the representation of syntax as the prelude's [Stag] types. *)
module type Builder = sig
  type t

  (** A constructor of the prelude, by its path in it, eg. ["Stag.Lit.Int"]. *)
  val con : string -> t list -> t

  val int : int -> t
  val bool : bool -> t
  val string : string -> t
  val char : char -> t
  val tuple : t list -> t

  (** A polymorphic record. *)
  val record : (string * t) list -> t

  (** A [Stag.Path.Ref]'s ref, of the declaration of canonical name [name]. *)
  val ref_ : Decl_ref.Kind.t -> string -> t

  (** The concatenation of two lists, for an antiquoted item. *)
  val append : t -> t -> t
end

module type S = sig
  type t

  val lit : Syntax.Lit.t -> t
  val type_expr : Syntax.Type_expr.t -> t
  val type_param : Syntax.Type_decl.Param.t Located.t -> t
  val type_decl : Syntax.Type_decl.t -> t
  val type_decl_kind : Syntax.Type_decl.Kind.t -> t
  val pattern : Syntax.Pattern.t -> t
  val expr : Syntax.Expr.t -> t
  val toplevel : Syntax.Toplevel.t -> t

  (** [antiquote] replaces a splice in the position of the kind, unless
      [None]: an item's by the list of declarations it stands for. *)
  val program
    :  ?antiquote:(Antiquote.Kind.t -> Syntax.Expr.t -> t option)
    -> Syntax.Program.t
    -> t

  val list : ('a -> t) -> 'a list -> t
  val option : ('a -> t) -> 'a option -> t
end

module type Lift = sig
  (** The representation of syntax as values of the prelude's [Stag] types
      ([Stag.Expr.t] etc.), built by a [Builder]: as values, or as expressions
      building them. The expansion of a [module%inline] or [include%inline] is
      dropped. *)

  module type Builder = Builder
  module type S = S

  module Make (B : Builder) : S with type t := B.t
end
