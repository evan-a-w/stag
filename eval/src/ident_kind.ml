open! Import

type t =
  | Lower
  | Binder
  | Upper
  | Label
  | Tyvar
[@@deriving sexp_of]

let describe = function
  | Lower -> "a lowercase identifier"
  | Binder -> "a lowercase identifier or _"
  | Upper -> "a capitalized identifier"
  | Label -> "a label"
  | Tyvar -> "a type variable name"
;;

let accepts t text =
  let prefix =
    match t with
    | Lower | Binder | Upper -> ""
    | Label -> "`"
    | Tyvar -> "'"
  in
  let written = prefix ^ text in
  match Lexer.tokenize ~file:"" written with
  | Ok [| { token; loc }; { token = Eof; _ } |] ->
    loc.start.offset = 0
    && loc.stop.offset = String.length written
    &&
      (match t, token with
      | (Lower | Binder), Lident _ | Binder, Underscore -> true
      | Upper, Uident _ | Label, Label _ | Tyvar, Tyvar _ -> true
      | _ -> false)
  | Ok _ | Error _ -> false
;;
