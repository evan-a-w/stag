open! Import

type t =
  | Int of int
  | Bool of bool
  | String of string
  | Char of char
  | Unit
  | Tuple of t list
  | Ptr of t ref
  | Box of t (** Immutable: what [Box.make] makes. *)
  | Construct of Typed.Constructor_ref.t * t list
  | Record of Typed.Record_type.t * t array (** By field index. *)
  | Poly_record of t String.Map.t
  | Poly_variant of string * t (** A tag without an argument has [Unit]. *)
  | Function of
      { args : Typed.Ident.t list (** Those not yet applied, non-empty. *)
      ; body : Typed.Expr.t
      ; env : t Env.t
      ; level : int
      }
  | Code of
      { expr : Typed.Expr.t
      ; persistent : t Typed.Ident.Map.t
      (** The values of the persistent binders [expr] embeds. *)
      }
  | Prim of
      { prim : Prim.t
      ; applied : t list (** Fewer than its arity, latest first. *)
      }
  | External of
      { symbol : string
      ; signature : C_signature.t
      ; at_compile_time : Compile_time_c_calls.Policy.t
      ; applied : t list (** As [Prim]'s. *)
      }
  | Intrinsic of Intrinsic.t
  | Type of Syntax.Type_expr.t
  (** A [Stag.Type.t], its types named by canonical names. *)
  | Ref of Decl_ref.t
  (** A [Stag.Type_ref.t], [Stag.Constructor_ref.t], [Stag.Field_ref.t] or
      [Stag.Value_ref.t]. *)

val to_prim : t -> Prim.Value.t or_null
val of_lit : Syntax.Lit.t -> t

(** Pointers expose their payload as [(Ptr v)], with [<cycle>] on a back-edge,
    and prelude lists are [(Cons x xs)]. Deep or excessively large values are
    bounded with [<truncated>]. Functions are [<fun>], as their envs may contain
    themselves. Here and in [to_string_hum], constructors of the prelude are
    named as within it ([Prelude_path.to_string_hum]). *)
val sexp_of_t : t -> Sexp.t

(** As stag syntax, eg. [C (1, `A)] and [Ptr.make (1, Nil)], for messages.
    Cycles and excessive depth/size use [<cycle>] and [<truncated>]. Functions
    are [<fun>], code [<code>]. If longer than [max_length], it is cut there and
    followed by [...]. Sharing is printed as repeated values; reparsing a
    printed pointer value does not preserve its identity. *)
val to_string_hum : ?max_length:int -> t -> string

(** [to_string_hum], cut at 1000 characters, followed by [...]. *)
val to_string_for_message : t -> string
