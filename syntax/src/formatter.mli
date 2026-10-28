open! Import

(** A file's contents formatted: its syntax printed as [Printer.program], to
    [width] (80 by default), keeping its comments, in order, and the blank lines
    around them and between one line toplevels.

    Each comment is put in the gap between the printed tokens matching those
    around it in the source (by [Alignment] of the tokens). One on its own line
    stays so, at the indentation of what follows; one ending a line of code
    stays at its end (breaking the line after it); others stay inline before
    what follows. Formatting what this returns gives it back. *)
val format
  :  ?width:int
  -> file:string
  -> string
  -> (string, Parser.Error.t) Result.t
