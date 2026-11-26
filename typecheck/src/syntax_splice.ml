open! Import

type t =
  { splice : Loc.t
  ; generated : Syntax.Expr.t
  }

let wrap_error t (error : Typecheck_error.t) =
  match error.kind with
  | In_syntax_splice _ -> error
  | _ when not (Loc.equal error.loc t.splice) -> error
  | kind ->
    { error with
      kind = In_syntax_splice { generated = t.generated; error = kind }
    }
;;
