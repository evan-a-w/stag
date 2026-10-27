open! Core

type t =
  { checked : Typecheck.Checked.t
  ; interpreter : Treewalk.t
  }
[@@deriving fields ~getters]

let check ?(limits = Limits.unlimited) ?output program =
  let interpreter = Treewalk.create ~stamps:(Stamp_counter.create ()) () in
  Option.iter output ~f:(Treewalk.set_output interpreter);
  Treewalk.set_limits interpreter limits;
  let%map.Result checked =
    Typecheck.typecheck (Typecheck.create ~interpreter ()) ~program
  in
  { checked; interpreter }
;;

let eval ?(limits = Limits.unlimited) t =
  Treewalk.set_limits t.interpreter limits;
  Treewalk.eval t.interpreter
;;
