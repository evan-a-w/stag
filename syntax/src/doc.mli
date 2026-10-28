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

(** Nothing, or a newline. *)
val cut : t

(** Always a newline, so its groups are broken. *)
val hardline : t

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

(** Whether [t] fits on one line of [width], flat. *)
val fits_flat : width:int -> t -> bool

(** Lines end without trailing spaces (but those of texts). *)
val render : ?width:int -> t -> string
