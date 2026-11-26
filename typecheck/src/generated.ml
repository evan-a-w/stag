open! Import

type t =
  { splice : Loc.t
  ; declaration : string
  ; generated : Syntax.Toplevel.t [@sexp.opaque]
  }
[@@deriving sexp_of]

let create splice (toplevel : Syntax.Toplevel.t) =
  let declaration =
    match toplevel.txt with
    | Let (name, _, _, _) -> "let " ^ name.txt
    | Let_nonrec (name, _, _, _) -> "let nonrec " ^ name.txt
    | Let_mut (name, _, _) -> "let mut " ^ name.txt
    | Type decl ->
      (if decl.nonrec_ then "type nonrec " else "type ") ^ decl.name.txt
    | Exception (name, _) | Malformed_exception (name, _) ->
      "exception " ^ name.txt
    | External { name; _ } -> "external " ^ name.txt
    | Module (name, _) | Module_splice (name, _, _) -> "module " ^ name.txt
    | Include _ -> "include"
    | Expect_test { name; _ } -> sprintf "let%%expect_test %S" name.txt
    | Bench { name; _ } -> sprintf "let%%bench %S" name.txt
  in
  { splice = Module_scope.Splice.loc splice; declaration; generated = toplevel }
;;

let wrap_error t (error : Typecheck_error.t) =
  match error.kind with
  | In_generated_declaration _ -> error
  | _ when not (Loc.equal error.loc t.splice) -> error
  | kind ->
    { error with
      loc = t.splice
    ; kind =
        In_generated_declaration
          { declaration = t.declaration; generated = t.generated; error = kind }
    }
;;
