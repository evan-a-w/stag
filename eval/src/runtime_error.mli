open! Import

module Kind : sig
  type t =
    | Message of string
    | Raised of Value.t
    (** An exception, of type [exn], which a [match] may handle. Other errors
        can't be handled. *)
  [@@deriving sexp_of]
end

(** An error raised by the program being evaluated, eg. an exception it does
    not handle or an exceeded limit, at the expression that raised it. *)
type t =
  { loc : Loc.t
  ; kind : Kind.t
  }
[@@deriving sexp_of]

val create : loc:Loc.t -> string -> t
val raise_exn : loc:Loc.t -> Value.t -> t

(** Built when shown, so raising a big value is cheap. For [Raised exn],
    [Uncaught exception E args]. *)
val message : t -> string

(** Sexps as [(message @ loc)]. *)
val to_error : t -> Error.t

(** The [t] an error was made from by [to_error], if any. *)
val of_error : Error.t -> t option

(** The exception an error raises, if it is one. *)
val raised : Error.t -> Value.t option
