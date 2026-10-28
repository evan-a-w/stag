open! Import

(** JSON-RPC messages framed by [Content-Length] headers, over file
    descriptors. *)

type t

val create : input:Core_unix.File_descr.t -> output:Core_unix.File_descr.t -> t

(** The next message, blocking for it, or [None] at the end of the input. A
    message that is not valid JSON-RPC is an [Error]. *)
val read : t -> (Jsonrpc.Packet.t, string) Result.t option

(** Whether [read] would not block: a message, or the start of one, is
    already there. *)
val has_pending_input : t -> bool

val write : t -> Jsonrpc.Packet.t -> unit

(** The framing of one message, for tests. *)
val frame : Jsonrpc.Packet.t -> string
