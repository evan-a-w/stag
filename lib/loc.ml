open! Core

module Pos = struct
  type t =
    { line : int
    ; col : int
    ; offset : int
    }
  [@@deriving sexp_of, compare, equal]

  let start = { line = 1; col = 0; offset = 0 }
end

type t =
  { file : string
  ; start : Pos.t
  ; stop : Pos.t
  }
[@@deriving compare, equal]

let none = { file = "_none_"; start = Pos.start; stop = Pos.start }
let is_none t = equal t none

let merge a b =
  if is_none a
  then b
  else if is_none b
  then a
  else (
    if not (String.equal a.file b.file)
    then
      raise_s
        [%message
          "Loc.merge: different files" (a.file : string) (b.file : string)];
    { file = a.file
    ; start = (if Pos.compare a.start b.start <= 0 then a.start else b.start)
    ; stop = (if Pos.compare a.stop b.stop >= 0 then a.stop else b.stop)
    })
;;

let to_string t =
  if is_none t
  then t.file
  else if t.start.line = t.stop.line
  then sprintf "%s:%d:%d-%d" t.file t.start.line t.start.col t.stop.col
  else
    sprintf
      "%s:%d:%d-%d:%d"
      t.file
      t.start.line
      t.start.col
      t.stop.line
      t.stop.col
;;

let sexp_of_t t = Sexp.Atom (to_string t)
