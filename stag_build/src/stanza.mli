open! Import

(** The stanza of a [stag] file, eg. [(library (name foo) (libraries bar))]. *)

module Kind : sig
  type t =
    | Library
    | Executable
    | Test
  [@@deriving sexp_of, equal]

  (** As written in a stanza: [library], [executable] or [test]. *)
  val to_string : t -> string
end

type t =
  { kind : Kind.t
  ; name : string Located.t
  ; libraries : string Located.t list
  ; loc : Loc.t
  }
[@@deriving sexp_of]

(** The module of library [foo] is [Foo]. *)
val module_name : t -> string

(** The stanza of the [stag] file at [path], if it has one, or every error in
    it. *)
val parse : path:string -> string -> (t option, Diagnostic.t list) Result.t
