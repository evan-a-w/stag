open! Import

(** Formatting source files with [Stag_syntax.Printer.format]. *)

(** [file]'s contents formatted, or its parse error. *)
val file : Source_file.t -> (string, Diagnostic.t) Result.t

module Mode : sig
  type t =
    | Check (** Writes nothing. *)
    | Inplace (** Rewrites each unformatted file with its formatted version. *)
end

module Unformatted : sig
  type t =
    { file : Source_file.t
    ; formatted : string
    }

  (** From [file.contents] to [formatted], both named [file.path]. *)
  val diff : t -> string
end

module Summary : sig
  type t =
    { unformatted : Unformatted.t list (** In the order of the files. *)
    ; errors : Diagnostic.t list
      (** Parse errors, and failures to rewrite a file. *)
    }
end

(** Formats each file, rewriting it at [root/path] (or [path] without [root])
    in [Inplace] mode if it changes. A file that doesn't parse is left as it
    is, the others still formatted. *)
val run : ?root:string -> mode:Mode.t -> Source_file.t list -> Summary.t
