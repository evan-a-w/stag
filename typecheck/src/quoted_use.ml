open! Import

module Via = struct
  type t =
    { toplevel : string
    ; loc : Loc.t
    }
end

type t =
  { awaited : Typed.Ident.t
  ; ty : Type.ty
  ; loc : Loc.t
  ; via : Via.t list
  ; reporter : Reporter.t
  ; failed : bool ref
  ; templates : bool ref list
  }

let sexp_of_t { awaited; loc; _ } =
  [%sexp (awaited : Typed.Ident.t), (loc : Loc.t)]
;;

let create ~awaited ~ty ~loc ~reporter =
  { awaited; ty; loc; via = []; reporter; failed = ref false; templates = [] }
;;

let in_instance t = not (List.is_empty t.via)

let instance t ~toplevel ~ty ~loc ~via ~reporter =
  { t with
    ty
  ; loc
  ; reporter
  ; via = via @ ({ Via.toplevel; loc = t.loc } :: t.via)
  ; failed = ref false
  }
;;

let local_instance t ~ty ~loc ~reporter =
  { t with
    ty
  ; loc
  ; reporter
  ; failed = ref false
  ; templates = t.failed :: t.templates
  }
;;

let fail t = t.failed := true
let template_failed t = List.exists t.templates ~f:( ! )

let error t error =
  let error = Typecheck_error.unification_error ~loc:t.loc error in
  List.fold (List.rev t.via) ~init:error ~f:(fun error { Via.toplevel; loc } ->
    { error with
      kind = Via_quote { toplevel; quote = loc; error = error.kind }
    })
;;
