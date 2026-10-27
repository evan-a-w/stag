open! Core

(** A unified diff of [old] and [new_] by lines, with 3 lines of context, as
    [diff -u] without timestamps, or [None] if they are equal. *)
val unified
  :  old_name:string
  -> new_name:string
  -> old:string
  -> new_:string
  -> string option
