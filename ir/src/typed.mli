open! Import

(** The typed tree the typechecker produces, shaped like [Syntax] with names
    resolved. Types are those of the typechecker, only final once the whole
    program is checked (so read them through [Type.canonical_ty]), and may be
    cyclic.

    Sexps render types with [Type.Printer], one printer per toplevel (or per
    outermost value converted), so variables are named consistently within it. A
    node is rendered as its constructor followed by [: type]. *)

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
    (** Bound in the same quote, or outside any quote and usable at this level
        (so a persistent toplevel used at level 0 is [Bound]). *)
    | Persistent of Binder.t
    (** A persistent binder from outside the enclosing quote: its value is
        embedded in the code. *)
    | Free of string
    (** A free variable of the enclosing quote: its context entry of this name. *)
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

  (** The exception [name], a canonical name. Exceptions are of type [exn] and
      share an index, so are told apart by name. *)
  val exception_ : string -> t

  val is_exception : t -> bool
end

(** A nominal record type: its canonical name, and its fields' canonical names
    by index. One per declaration, shared by its fields' refs and its values. *)
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
      | Deref of 'p
      | Construct of Constructor_ref.t * 'p list
      | Record of (Field_ref.t * 'p) list
      | Poly_variant of string * 'p option
      | Poly_record of
          { fields : (string * 'p) list
          ; rest : Rest.t
          }
      | Error of 'p list
      (** What failed to check, of its parts that did. Matching it is a
          [Reached_error] runtime error. *)
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
end

(** The locality markers of expressions: [stack_ e], [exclave_ e], [region_ e]
    and [e [@nontail]]. *)
module Marker : sig
  type t =
    | Stack
    | Exclave
    | Region
    | Nontail
  [@@deriving sexp_of, equal]
end

(** An allocation in run time code, and where a compiler makes it. The modes of
    a function's type are chosen by each instance of it; a function whose
    allocations are made differently in some instances than in others is
    compiled once for each way. *)
module Allocation : sig
  (** Names an allocation site, or a [let] owning some: unique in the program,
      so each copy of spliced code has its own. *)
  module Id : sig
    type t [@@deriving sexp_of, compare, equal, hash]

    include Hashable.S_plain with type t := t

    val of_int : int -> t
  end

  module Kind : sig
    type t =
      | Closure
      | Partial_application
      (** The closure of a function given some of its arguments: made by the
          caller, as the result of its call. *)
      | Code
      | Pointer (** [Ptr.make] *)
      | Box (** [Box.make] *)
      | Cell (** Of a [let mut]. *)
    [@@deriving sexp_of, compare, equal]
  end

  module Placement : sig
    type t =
      | Heap
      | Region (** The function's (or [region_]'s) own. *)
      | Caller_region
      (** The one around it: returned by the function (or [region_]), or under
          [exclave_]. Of a partial application, its caller's own. *)
    [@@deriving sexp_of, compare, equal]
  end

  (** A side of an arrow in a type: the path down to the arrow, then which side. *)
  module Side : sig
    module Step : sig
      type t =
        | Arg
        | Res
        | Component of int (** Of a tuple, from 0, as printed. *)
        | Type_arg of int (** Of a type constructor, from 0, as printed. *)
        | Field of string (** Of a structural record, or a variant's tag. *)
        | Code (** What the code computes. *)
      [@@deriving sexp_of, compare, equal]
    end

    type t =
      { steps : Step.t list
      ; side : [ `Arg | `Res ]
      }
    [@@deriving sexp_of, compare, equal]

    (** As [res], [arg.res], [component 0.res]: the steps, then the side. *)
    val to_string : t -> string
  end

  (** The function whose instances decide where it is made: a toplevel, or a
      function bound by a [let] in it (generalized, so with instances of its
      own), by each [let] down to it. *)
  module Owner : sig
    module Let : sig
      type t =
        { id : Id.t (** As the [let]'s [Expr.Made.owns]. *)
        ; name : string
        ; loc : Loc.t
        }
      [@@deriving sexp_of]
    end

    type t =
      { toplevel : Ident.t
      ; lets : Let.t list (** Outermost first. *)
      }
    [@@deriving sexp_of]

    (** By the toplevel and the ids of the lets. *)
    val equal : t -> t -> bool

    (** As [f.g]. *)
    val to_string : t -> string
  end

  (** An allocation made more locally in the instances of an owner (its own, or
      one around it) where a side of that owner's type is [local]. *)
  module Follows : sig
    type t =
      { owner : Owner.t
      ; side : Side.t
      (** In [owner]'s type: of the site's owner, or one around it. *)
      ; if_local : Placement.t
      }
    [@@deriving sexp_of]
  end

  type t =
    { id : Id.t (** As the [Expr.Made] of the expression that makes it. *)
    ; owner : Owner.t
    ; loc : Loc.t
    ; kind : Kind.t
    ; placement : Placement.t
    (** Where it is made in every instance of its owner: as locally as its least
        instance, with every mode of its owners' types as low as it may be,
        allows, or on the heap where anything it is bounded by may yet be
        constrained by code outside its component. *)
    ; follows : Follows.t option
    (** Where it is made instead in the instances where that side is [local]
        (with every other side, for all it relies on, at its least). A site that
        needs several sides [local] to be made more locally is made at
        [placement]. *)
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
      (** The binders at the splice that may provide the code's entries of their
          name. The other entries of the code become entries of the enclosing
          quote's context. *)
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
          ; annots : (Ident.t * Syntax.Annot.t) list
          (** The annotated [args], their annotations as written but for type
              names, which are canonical ([Resolved]). Likewise [result_annot]
              and [Fun]'s and [Annotated]'s. *)
          ; result_annot : Syntax.Type_expr.t option
          ; mode : Syntax.Mode_expr.t option
          (** As written: of the result of a function, else of the value. *)
          ; generalized : bool
          ; body : 'e (** [ident] is bound in [body]. *)
          ; in_ : 'e
          }
      | Seq of 'e * 'e (** [a; b]: [a]'s value, of any type, is dropped. *)
      | Let_mut of
          { ident : Ident.t
          ; annot : Syntax.Type_expr.t option
          ; mode : Syntax.Mode_expr.t option (** Of the values it holds. *)
          ; body : 'e (** [ident] is not bound in [body]. *)
          ; in_ : 'e
          }
      | Assign of Var_ref.t * 'e
      (** Of a [let mut] bound in the same quote, or a mutable toplevel. *)
      | Fun of
          { args : Ident.t Nonempty_list.t
          ; annots : (Ident.t * Syntax.Annot.t) list
          ; body : 'e
          }
      | Annotated of 'e * Syntax.Annot.t
      | Marked of Marker.t * 'e
      (** [(x : t @ m)], of a [Var], its type resolved as [Let]'s. *)
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
      | Deref of 'e
      | Construct of Constructor_ref.t * 'e list
      | Record of
          { record : Record_type.t
          ; fields : (Field_ref.t * 'e) list
          }
      | Field of 'e * Field_ref.t
      | Record_update of
          { record : Record_type.t
          ; expr : 'e
          ; fields : (Field_ref.t * 'e) list
          }
      (** [{e with f = v; ...}], of [e]'s type but for the parameters only the
          updated fields' types contain. *)
      | Store of 'e * 'e
      (** [e <- v], [e] a place: a [Deref], or a field of one. *)
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
      (** Checks the output so far against the string, see [Expect_test_runner]. *)
      | External of
          { symbol : string
          ; signature : C_signature.t
          ; at_compile_time : Compile_time_c_calls.Policy.t
          }
      (** The body of [external f : t = "symbol"]: the C function [symbol],
          called once given [C_signature.arity signature] arguments. *)
      | Type of Syntax.Type_expr.t
      (** [type t], its types named by canonical names ([Resolved]). *)
      | Error of 'e list
      (** In place of what did not parse or check, with the parts of it that
          did. *)
    [@@deriving sexp_of]
  end

  (** The allocation sites an expression in run time code makes, by
      [Allocation.Id]. *)
  module Made : sig
    type t =
      { made : Allocation.Id.t option
      (** What evaluating it allocates: a closure, code, a [Box.make] or
          [Ptr.make] block, a cell, or a partial application of a prim (where it
          is applied, or what applying it makes where it is named as a value). *)
      ; partials : Allocation.Id.t list
      (** Of a function, the partial applications given 1, 2, ... of its
          arguments. *)
      ; owns : Allocation.Id.t option
      (** Of a [let], naming it in the [Allocation.Owner]s it is in. *)
      }
    [@@deriving sexp_of]

    val none : t
  end

  type t =
    { desc : t Desc.t
    ; ty : Type.ty
    ; loc : Loc.t (** Of the syntax it was inferred from. *)
    ; made : Made.t
    }
  [@@deriving sexp_of]

  (** How a child's scope differs from its parent's. *)
  module Child : sig
    type t =
      { depth : int
      (** Quotes in, relative to its parent: [1] in a quote, [-1] in a splice's
          code. *)
      ; binds : Ident.t list (** The locals its parent binds in it. *)
      }
    [@@deriving sexp_of]
  end

  (** The one enumeration of an expression's children, which every walker over
      expressions shares. The children are what the expression evaluates, in the
      order it does: not a [Toplevel_splice]'s [code], which ran while checking,
      so is no part of what it means. *)
  module Traverse (M : Local_monad.S) : sig
    (** [t] with [f] applied to each of its children, in order. *)
    val map_children : t -> f:(Child.t -> t -> t M.t) @ local -> t M.t
  end

  val map_children : t -> f:(Child.t -> t -> t) @ local -> t
  val iter_children : t -> f:(Child.t -> t -> unit) @ local -> unit
  val fold_children : t -> init:'a -> f:('a -> Child.t -> t -> 'a) @ local -> 'a

  (** The children of [t], in order. *)
  val sub_exprs : t -> t list

  (** Whether [t] has an [Error] node or pattern: what failed to check. *)
  val has_error : t -> bool

  module Ref : sig
    type t =
      | Binder of Binder.t
      | Free of string
  end

  (** What [t] itself, not its children, refers to: the variable it reads or
      assigns, or the providers of its [Implicit] substitution. *)
  val iter_own_refs : t -> f:(Ref.t -> unit) @ local -> unit

  (** Whether [t] refers to a binder satisfying [f], by a [Bound] or
      [Persistent] variable, an [Assign] or an [Implicit] substitution. *)
  val refers_to : t -> f:(Binder.t -> bool) @ local -> bool
end

module Toplevel : sig
  module Kind : sig
    type t =
      | Value
      | Mut (** [let mut]. *)
      | Function of Ident.t Nonempty_list.t (** Of these arguments. *)
    [@@deriving sexp_of]
  end

  type t =
    { ident : Ident.t
    ; kind : Kind.t
    ; scheme : Type.ty
    ; body : Expr.t
    ; partials : Allocation.Id.t list (** As [Expr.Made.partials]. *)
    }
  [@@deriving sexp_of]
end

(** A strongly connected component of toplevels, in the order they were reached.
    Components may be larger than needed, as references to functions from within
    quotes count. *)
module Scc : sig
  type t = { members : Toplevel.t list } [@@deriving sexp_of]
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
  module Item : sig
    type t =
      | Toplevel of Ident.t
      | Module of string * t list
    [@@deriving sexp_of]

    (** In order. *)
    val toplevels : t -> Ident.t list
  end

  type t =
    { sccs : Scc.t list (** Each component after the ones it refers to. *)
    ; items : Item.t list
    (** The toplevel bindings and modules of the program, in program order, with
        what a splice or include generated in its place: the order toplevels are
        evaluated in, but for [Driver] ordering files by dependency. *)
    ; expect_tests : Expect_test.t list (** In program order. *)
    ; benches : Bench.t list (** In program order. *)
    ; declaration_splices : Expr.t list
    (** The expression of each declaration splice that ran, at level -1. *)
    ; ident_info : Ident_info.t Ident.Map.t
    ; allocations : Allocation.t list (** In program order. *)
    }
  [@@deriving sexp_of]

  (** Whether any of its expressions [Expr.has_error]. *)
  val has_error : t -> bool
end
