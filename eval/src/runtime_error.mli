open! Import

(** An exception raised by a program, a value of the interpreter evaluating it,
    which only that interpreter can inspect. *)
module Raised : sig
  type t [@@deriving sexp_of]

  (** An interpreter's type of values, made once per interpreter. *)
  module Kind : sig
    type 'value t

    val create
      :  name:string
      -> sexp_of:('value -> Sexp.t)
      -> to_string_for_message:('value -> string)
      -> 'value t
  end

  val create : 'value Kind.t -> 'value -> t

  (** The exception, if it is a value of [kind]. *)
  val value : t -> 'value Kind.t -> 'value option

  val to_string_for_message : t -> string
end

module Kind : sig
  type t =
    | Message of string
    | Raised of Raised.t
    (** An exception, of type [exn], which a [match] may handle. Other errors
        can't be handled. *)
  [@@deriving sexp_of]
end

(** An error raised by the program being evaluated, eg. an exception it does not
    handle or an exceeded limit, at the expression that raised it. *)
type t =
  { loc : Loc.t
  ; kind : Kind.t
  }
[@@deriving sexp_of]

val create : loc:Loc.t -> string -> t
val raise_exn : loc:Loc.t -> Raised.t -> t

(** Built when shown, so raising a big value is cheap. For [Raised exn],
    [Uncaught exception E args]. *)
val message : t -> string

(** Sexps as [(message @ loc)]. *)
val to_error : t -> Error.t

(** The [t] an error was made from by [to_error], if any. *)
val of_error : Error.t -> t option

(** The exception an error raises, if it is one. *)
val raised : Error.t -> Raised.t option
