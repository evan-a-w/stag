open! Import

type t =
  { splice : Loc.t
  ; declaration : string
  ; generated : Syntax.Toplevel.t [@sexp.opaque]
  }
[@@deriving sexp_of]

let create splice (toplevel : Syntax.Toplevel.t) =
  let rec declaration (toplevel : Syntax.Toplevel.t) =
    match toplevel.txt with
    | Attributed (toplevel, _) -> declaration toplevel
    | Attribute attribute -> sprintf "[@@@%s]" attribute.name.txt
    | Let (name, _, _, _) -> "let " ^ name.txt
    | Let_mut (name, _, _) -> "let mut " ^ name.txt
    | Type decl -> "type " ^ decl.name.txt
    | Exception (name, _) | Malformed_exception (name, _) ->
      "exception " ^ name.txt
    | External { name; _ } -> "external " ^ name.txt
    | Module (name, _) | Module_splice (name, _, _) -> "module " ^ name.txt
    | Include _ -> "include"
    | Expect_test { name; _ } -> sprintf "let%%expect_test %S" name.txt
    | Bench { name; _ } -> sprintf "let%%bench %S" name.txt
  in
  let declaration = declaration toplevel in
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
