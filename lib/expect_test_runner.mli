open! Core

(** Runs a program's [let%expect_test]s, as ppx_expect does OCaml's. *)

module Mismatch : sig
  type t =
    { loc : Loc.t
      (** The string of the [[%expect]] (zero width after [[%expect] if it has
          none), or for trailing output, zero width at the end of the test. *)
    ; expected : string option (** [None] for trailing output. *)
    ; actual : string (** Normalized. *)
    }
  [@@deriving sexp_of]
end

module Test_result : sig
  type t =
    { name : string Located.t
    ; mismatches : Mismatch.t list
    ; error : Error.t option
      (** A runtime error, which stops the test, so later [[%expect]]s and
          trailing output are not checked. *)
    }
  [@@deriving sexp_of]

  val passed : t -> bool
end

(** Runs the tests of [driver]'s program in order, after [Driver.eval], each
    with [limits] of its own, unlimited by default. Each [[%expect]] checks the
    output since the previous one, and the output after the last must be
    empty. Outputs are compared after [normalize]. *)
val run : ?limits:Limits.t -> Driver.t -> Test_result.t list

(** Without leading and trailing blank lines, trailing whitespace on each line
    and the indentation common to all lines. *)
val normalize : string -> string

(** [contents] of the file [path] with the mismatches in it corrected, if any,
    formatted as ppx_expect does: [{| 5 |}] for one line, else an indented
    block. An [[%expect]] reached more than once gets its first mismatch. *)
val corrected
  :  path:string
  -> contents:string
  -> Test_result.t list
  -> string option
