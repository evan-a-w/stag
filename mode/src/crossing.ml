open! Import

module When = struct
  type t =
    | Never
    | Always
    | While_pointer_free of Graph.Layout.t

  let sexp_of_t = function
    | Never -> Sexp.Atom "Never"
    | Always -> Atom "Always"
    | While_pointer_free layout ->
      [%sexp While_pointer_free (Graph.Layout.id layout : int)]
  ;;
end

type t =
  { stage : When.t
  ; provenance : When.t
  ; locality : When.t
  }
[@@deriving sexp_of]

let none = { stage = Never; provenance = Never; locality = Never }
let immediate = { stage = Always; provenance = Always; locality = Always }

let while_pointer_free layout =
  let when_ = When.While_pointer_free layout in
  { stage = when_; provenance = when_; locality = when_ }
;;
