open! Import

(** [textDocument/documentSymbol]: the outline of a file's toplevels. *)
val of_toplevels
  :  Text.t
  -> Syntax.Toplevel.t list
  -> Lsp_types.DocumentSymbol.t list
