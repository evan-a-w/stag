open! Import

(** A document's contents, with positions converted between byte offsets
    (stag's) and LSP positions: lines split at [\n], characters counted in code
    units of the negotiated encoding. Invalid UTF-8 counts one unit per byte. *)

module Encoding : sig
  type t =
    | Utf_8
    | Utf_16
  [@@deriving sexp_of]

  (** UTF-8 if the client offers it, else UTF-16, which all clients support. *)
  val negotiate : Lsp_types.PositionEncodingKind.t list -> t

  val to_lsp : t -> Lsp_types.PositionEncodingKind.t
end

type t

val create : encoding:Encoding.t -> string -> t
val contents : t -> string

(** Clamped to the line, whose [\r\n] or [\n] is not in it, and to the document.
    A position inside a character's code units is the character's start. *)
val offset_of_position : t -> Lsp_types.Position.t -> int

(** Clamped to the document. An offset inside a UTF-8 sequence is the sequence's
    start. *)
val position_of_offset : t -> int -> Lsp_types.Position.t

(** The span of [loc]'s offsets. *)
val range_of_loc : t -> Loc.t -> Lsp_types.Range.t

val range_of_offsets : t -> start:int -> stop:int -> Lsp_types.Range.t

(** The whole document, eg. to replace it. *)
val full_range : t -> Lsp_types.Range.t

(** Code units of the bytes [pos, pos + len). *)
val length : t -> pos:int -> len:int -> int

(** The text with a change of [textDocument/didChange] applied. *)
val apply_change : t -> Lsp_types.TextDocumentContentChangeEvent.t -> t

(** The bytes [start, stop) split into the pieces on each line, without line
    terminators, empty pieces dropped. *)
val split_lines : t -> start:int -> stop:int -> (int * int) list
