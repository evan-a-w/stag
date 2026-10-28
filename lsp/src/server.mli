open! Import

(** The language server: a synchronous loop over the client's messages.
    Documents are checked when the client has nothing more to say, so a burst
    of changes is checked once, at its last version. *)

type t

(** Compile-time evaluation of checks is within [limits]. Messages to the
    client go to [send]. *)
val create : limits:Limits.t -> send:(Jsonrpc.Packet.t -> unit) -> t

(** Handles a message from the client, but checks no changed documents yet,
    unless a request needs them checked. *)
val handle : t -> Jsonrpc.Packet.t -> unit

(** Checks the documents changed since their last check. *)
val check_pending : t -> unit

(** After the [exit] notification: 0 if [shutdown] came first, else 1. *)
val exit_code : t -> int option

(** Serves the client on [transport] until [exit] or the end of its input,
    returning the exit code. *)
val run : limits:Limits.t -> Transport.t -> int

(** Limits for checks: a bad splice must not hang the editor. *)
val default_limits : Limits.t
