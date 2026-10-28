open! Import

type t =
  | Int of int
  | Bool of bool
  | String of string
  | Char of char
  | Unit
  | Tuple of t list
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
  | Intrinsic of Intrinsic.t
  | Type of Syntax.Type_expr.t
  (** A [Stag.Type.t], its types named by canonical names. *)
  | Ref of Decl_ref.t
  (** A [Stag.Type_ref.t], [Stag.Constructor_ref.t] or [Stag.Field_ref.t]. *)

val of_prim : Prim.Value.t -> t
val to_prim : t -> Prim.Value.t option
val of_lit : Syntax.Lit.t -> t

(** Functions are [<fun>], as their envs may contain themselves. Here and in
    [to_string_hum], constructors of the prelude are named as within it
    ([Prelude_path.to_string_hum]). *)
val sexp_of_t : t -> Sexp.t

(** As stag syntax, eg. [C (1, `A)], for messages. Functions are [<fun>],
    code [<code>] and nominal records [<record>]. *)
val to_string_hum : t -> string
