open! Import
include Treewalk

let interpreter = (module Treewalk : Interpreter_intf.S)
let to_interpreter t = Interpreter.T ((module Treewalk), t)

let of_interpreter interpreter =
  match Interpreter.state interpreter type_id with
  | Some t -> t
  | None -> raise_s [%message "Stag_treewalk.of_interpreter: not a treewalk"]
;;
