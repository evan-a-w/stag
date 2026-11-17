open! Import
open Syntax

(** List literals [[a; b; c]], which are sugar for chains of the prelude's
    [Cons] ending in its [Nil], both named [Decl_path.Resolved] so that nothing
    local changes what a literal means. *)

val cons : Decl_path.t
val nil : Decl_path.t

(** The literal of [items], each given with the [[] or [;] before it, closed
    at [close], the [\]] (or the whole [[]] if there are no items). A [Cons] is
    located from the token before its item to [close], and its constructor at
    that token; the [Nil] and its constructor at [close]. *)
val expr : items:(Loc.t * Expr.t) list -> close:Loc.t -> Expr.t

val pattern : items:(Loc.t * Pattern.t) list -> close:Loc.t -> Pattern.t

(** The items of a list literal, if this is one. *)
val expr_items : Expr.t -> Expr.t list option

val pattern_items : Pattern.t -> Pattern.t list option

(** A list literal with [f] applied to each of its items, keeping its [Cons]es
    and [Nil] as they are, if this is one. *)
val map_expr_items : Expr.t -> f:(Expr.t -> Expr.t) -> Expr.t option

val map_pattern_items
  :  Pattern.t
  -> f:(Pattern.t -> Pattern.t)
  -> Pattern.t option
