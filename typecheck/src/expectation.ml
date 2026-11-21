open! Import

type t =
  | Required
  | Hint
  | Code_hint of t

let is_required = function
  | Required -> true
  | Hint | Code_hint _ -> false
;;

let quoted = function
  | Code_hint t -> t
  | (Required | Hint) as t -> t
;;
