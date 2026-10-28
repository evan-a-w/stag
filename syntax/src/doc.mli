open! Import

(** Documents laid out to a width, as Wadler's "prettier printer": a [group]
    is printed flat (its [line]s as spaces) if it fits on the rest of the line,
    and otherwise broken (its [line]s as newlines, its inner groups deciding for
    themselves). *)
type t

val empty : t

(** Printed as is. A text containing a newline never fits flat; the lines
    after the first are printed verbatim, without indentation. *)
val text : string -> t

val ( ^^ ) : t -> t -> t
val concat : t list -> t
val separate : t -> t list -> t

(** A space, or a newline. *)
val line : t

(** A space, unless at the start of a line or after one. *)
val space : t

(** Nothing, or a newline. *)
val cut : t

(** Always a newline, so its groups are broken. *)
val hardline : t

(** A newline, unless nothing but indentation precedes it on its line (which
    is then indented as the newline would be). Never fits flat. *)
val fresh_line : t

(** The next text that is not just spaces starts a new line (without its
    leading spaces), unless a newline comes first. Fits, as the end of a
    line. *)
val end_line : t

(** Ends the line, unless nothing but indentation is on it, and leaves a blank
    line before the next text, unless one is already there or nothing has been
    printed. Never fits flat. *)
val blank_line : t

(** [t], but at the start of a group, before the group instead, so that what
    it breaks (eg. a comment on its own line) is not the group. *)
val leading : t -> t

(** The text, only when its group is broken. *)
val if_break : string -> t

(** Lines in [t] are indented by [n] more. *)
val nest : int -> t -> t

(** Lines in [t] are indented to the column [t] starts at. *)
val align : t -> t

val group : t -> t

(** [a] if it fits, otherwise [b]. In a flat group, [a] (flat). In a broken
    one, [a] fits if, laid out with its groups flat and its other [line]s
    broken, each of its lines fits, as does the rest of the line after it.
    Then [a]'s groups decide for themselves as usual. Deciding looks at no
    more of [a] than a group does, per line of [a] outside its groups. *)
val alt : t -> t -> t

module Leaf : sig
  type t =
    | Text of string
    | If_break of string
end

(** [t] with each [text] and [if_break], in order, replaced by [f]'s doc. Both
    alternatives of an [alt] are mapped starting from the same [acc], and the
    first's is kept. *)
val fold_map_leaves
  :  t
  -> init:'acc
  -> f:('acc -> Leaf.t -> 'acc * t)
  -> 'acc * t

(** Whether [t] fits on one line of [width], flat. *)
val fits_flat : width:int -> t -> bool

(** Lines end without trailing spaces (but those of texts). *)
val render : ?width:int -> t -> string
