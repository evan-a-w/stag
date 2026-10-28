open! Import

(** A document's contents, with positions converted between byte offsets
    (stag's) and LSP positions: lines split at [\n], characters counted in
    UTF-16 code units. Invalid UTF-8 counts one unit per byte. *)

type t

val create : string -> t
val contents : t -> string

(** Clamped to the line, whose [\r\n] or [\n] is not in it, and to the
    document. A character inside a surrogate pair is the pair's start. *)
val offset_of_position : t -> Lsp_types.Position.t -> int

(** Clamped to the document. An offset inside a UTF-8 sequence is the
    sequence's start. *)
val position_of_offset : t -> int -> Lsp_types.Position.t

(** The span of [loc]'s offsets. *)
val range_of_loc : t -> Loc.t -> Lsp_types.Range.t

val range_of_offsets : t -> start:int -> stop:int -> Lsp_types.Range.t

(** The whole document, eg. to replace it. *)
val full_range : t -> Lsp_types.Range.t

(** UTF-16 code units of the bytes [pos, pos + len). *)
val utf16_length : t -> pos:int -> len:int -> int

(** The text with a change of [textDocument/didChange] applied. *)
val apply_change : t -> Lsp_types.TextDocumentContentChangeEvent.t -> t
