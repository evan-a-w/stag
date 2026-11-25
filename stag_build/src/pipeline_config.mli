open! Import

(** How a program is checked and run, resolved the same way by [stag],
    [stag-build] and the language server, so that they agree. *)

module Prelude_choice : sig
  (** The sources of the [Stdlib_checkout] that all the program's files are in,
      else the embedded prelude. *)
  type t =
    | Embedded
    | Checkout of Stdlib_checkout.t

  (** Of a program of files at [paths], absolute or relative to the current
      directory. *)
  val of_paths : string list -> t
end

(** A [Prelude_choice.t], loaded. *)
module Loaded_prelude : sig
  type t =
    | Embedded
    | Parsed of Syntax.Toplevel.t list (** A checkout's sources. *)

  (** The contents of a file are [contents path] if some, else read from disk. *)
  val load
    :  ?contents:(string -> string option)
    -> Prelude_choice.t
    -> (t, Diagnostic.t) Result.t

  (** As [Driver.check]'s [prelude]. *)
  val toplevels : t -> Syntax.Toplevel.t list option
end

type t =
  { prelude : Prelude_choice.t
  ; limits : Limits.t (** Of each phase: compile time, evaluation, each test. *)
  }

(** Of a program of files at [paths], as [Prelude_choice.of_paths]. *)
val create : limits:Limits.t -> string list -> t

(** [-fuel], [-max-depth] and [-time-limit], [default] (unlimited by default)
    where not given. *)
val limits_param : ?default:Limits.t -> unit -> Limits.t Command.Param.t

(** Says that limits apply to each of [phases] on its own. *)
val limits_readme : phases:string -> string
