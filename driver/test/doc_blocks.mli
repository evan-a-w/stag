open! Import

(** The stag code blocks of a markdown document, and the claims their comments
    make about the code, for tests that keep a document's examples honest. *)

module Kind : sig
  type t =
    | Program (** [```stag]: checks and runs. *)
    | Error (** [```stag-error]: fails to check or to run. *)
    | Project
    (** [```stag-project]: files of a stag-build project, each after a line
        [-- PATH]. *)
  [@@deriving sexp_of]
end

module Block : sig
  type t =
    { kind : Kind.t
    ; modes : bool (** [```stag modes]: print signatures with their modes. *)
    ; line : int (** Of the opening fence, from 1. *)
    ; code : string
    (** Preceded by as many blank lines as precede it in the document, so its
        line numbers are the document's. *)
    }
  [@@deriving sexp_of]
end

(** Every block fenced by [```stag...] (others are skipped). Raises on an
    unknown info string or an unclosed fence. *)
val parse : string -> Block.t list

(** [-- PATH] separated files of a [Project] block. *)
val project_files : string -> (string * string) list

module Claim : sig
  (** A comment [(* val x : t = v *)] claims the signature item [val x : t] and
      the value [x = v] ([= v] is optional), [(* type ... *)] and
      [(* exception ... *)] a signature item, [(* prints: s *)] output
      containing [s], and [(* Error: s *)] an error message containing [s]. *)
  type t =
    | Item of string
    | Value of
        { name : string
        ; value : string
        }
    | Prints of string
    | Error of string
  [@@deriving sexp_of]
end

(** The claims of the comments of [code], with their whitespace collapsed. *)
val claims : string -> Claim.t list

(** Collapses each run of whitespace into one space, and trims. *)
val normalize : string -> string

(** The signature items of [signature] (as [Signature.to_string_hum] prints it),
    each with its continuation lines, normalized. Modules give their
    [module M : sig] line and [end]. *)
val signature_items : string -> string list
