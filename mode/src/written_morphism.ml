open! Import

type t =
  | Plain of Axis.Any.t
  | Meet of Axis.Any.t
  | Release
  | Static_if_const
  | Const_if_static
[@@deriving sexp_of]

let all =
  List.concat_map Axis.Any.all ~f:(fun axis -> [ Plain axis; Meet axis ])
  @ [ Release; Static_if_const; Const_if_static ]
;;

let middle axis = Axis.Any.size axis / 2

let morphism = function
  | Plain axis -> Morphism.identity axis
  | Meet axis -> Morphism.meet_with axis (middle axis)
  | Release -> Morphism.release
  | Static_if_const -> Morphism.const_is_static
  | Const_if_static -> Morphism.runtime_is_gc
;;

let is_trivial m =
  List.for_all
    (List.init (Axis.Any.size (Morphism.dst m)) ~f:Fn.id)
    ~f:(fun x -> Morphism.left m x = 0)
;;
