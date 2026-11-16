open! Import

(** The language server: a synchronous loop over the client's messages.
    Documents are checked when the client has nothing more to say, so a burst of
    changes is checked once, at its last version. *)

type t

(** Compile-time evaluation of checks is within [limits]. Messages to the client
    go to [send]. Definitions in the prelude are in [stdlib]'s sources if found
    nowhere nearer (see [Checker.check]); [run]'s is the checkout its executable
    is in, if any. *)
val create
  :  ?stdlib:Stdlib_checkout.t
  -> limits:Limits.t
  -> send:(Jsonrpc.Packet.t -> unit)
  -> unit
  -> t

(** Handles a message from the client, but checks no changed documents yet,
    unless a request needs them checked. *)
val handle : t -> Jsonrpc.Packet.t -> unit

(** Checks the documents changed since their last check. *)
val check_pending : t -> unit

(** After the [exit] notification: 0 if [shutdown] came first, else 1. *)
val exit_code : t -> int option

(** Serves the client on [transport] until [exit] or the end of its input,
    returning the exit code. Changed documents are checked once the client has
    sent nothing for [debounce] (default: 150ms). *)
val run : ?debounce:Time_ns.Span.t -> limits:Limits.t -> Transport.t -> int

(** Limits for checks: a bad splice must not hang the editor. *)
val default_limits : Limits.t
