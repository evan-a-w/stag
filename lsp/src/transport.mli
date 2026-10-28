open! Import

(** JSON-RPC messages framed by [Content-Length] headers, over file
    descriptors. *)

type t

(** Reads [read_size] bytes at a time. *)
val create
  :  ?read_size:int
  -> input:Core_unix.File_descr.t
  -> output:Core_unix.File_descr.t
  -> unit
  -> t

(** Over stdin and stdout, sending to stderr whatever else the process writes
    to stdout, so it can't corrupt the messages. *)
val of_stdio : unit -> t

(** The next message, blocking for it, or [None] at the end of the input. A
    message that is not JSON is answered with a [ParseError], and one that
    is not JSON-RPC with an [InvalidRequest], and is an [Error]. A
    ["params": null] counts as no params. *)
val read : t -> (Jsonrpc.Packet.t, string) Result.t option

(** Whether [read] would not block (a message, or the start of one, is there)
    or input arrives [within] (default: zero). *)
val has_pending_input : ?within:Time_ns.Span.t -> t -> bool

val write : t -> Jsonrpc.Packet.t -> unit

(** The framing of one message, for tests. *)
val frame : Jsonrpc.Packet.t -> string
