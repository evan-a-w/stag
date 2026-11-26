open! Import

type t =
  { component : Component.t
  ; module_ : Module_scope.t
  ; syntax_splice : Syntax_splice.t option
  ; generated : Generated.t option
  }

let wrap_error t error =
  Module_scope.within
    t.module_
    (let error =
       match t.syntax_splice with
       | None -> error
       | Some splice -> Syntax_splice.wrap_error splice error
     in
     match t.generated with
     | None -> error
     | Some generated -> Generated.wrap_error generated error)
;;
