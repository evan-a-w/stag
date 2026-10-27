open! Core

type t =
  { checked : Typecheck.Checked.t
  ; interpreter : Treewalk.t
  }
[@@deriving fields ~getters]

let check program =
  let interpreter = Treewalk.create ~stamps:(Stamp_counter.create ()) () in
  let%map.Result checked =
    Typecheck.typecheck (Typecheck.create ~interpreter ()) ~program
  in
  { checked; interpreter }
;;

let eval t = Treewalk.eval t.interpreter
