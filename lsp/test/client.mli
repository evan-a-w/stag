open! Import

(** A client driving a server in process, over files in a temporary
    directory, printing what the server sends readably: paths relative to the
    directory, diagnostics one per line. *)

type t

(** Initializes a server for a client of [capabilities] over the [files]
    (relative path, contents), and removes them after [f]. *)
val with_server
  :  ?limits:Limits.t
  -> ?capabilities:Yojson.Safe.t
  -> ?files:(string * string) list
  -> (t -> unit)
  -> unit

val server : t -> Server.t
val path : t -> string -> string
val uri : t -> string -> string

(** Sends a notification and prints what the server sends, after checking
    the documents changed unless not [check]. *)
val notify : ?check:bool -> t -> string -> Yojson.Safe.t -> unit

val check_pending : t -> unit

(** Sends a request and prints what the server sends before the response.
    The response's result, or prints its error. *)
val request : t -> string -> Yojson.Safe.t -> Yojson.Safe.t option

val did_open : t -> string -> unit

(** Replaces [path]'s whole contents. *)
val did_change : ?check:bool -> t -> string -> version:int -> string -> unit

(** [{"textDocument": {"uri": ...}}] of [path], with [extra] fields. *)
val text_document
  :  ?extra:(string * Yojson.Safe.t) list
  -> t
  -> string
  -> Yojson.Safe.t

val position : line:int -> character:int -> Yojson.Safe.t

(** Replaces the directory's URI in a string by nothing. *)
val relativize : t -> string -> string

(** Prints JSON with paths relative to the directory. *)
val print_json : t -> Yojson.Safe.t -> unit

(** The semantic tokens of [path], one per line, with their text. *)
val print_semantic_tokens : t -> string -> unit

(** Prints the hover at a position of [file], after the position. *)
val print_hover : t -> string -> line:int -> character:int -> unit

(** Prints the definitions of what is at a position of [file], after the
    position. *)
val print_definition : t -> string -> line:int -> character:int -> unit
