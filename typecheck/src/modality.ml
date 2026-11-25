open! Import

module Step = struct
  type t =
    | Meet of Mode.Const.t
    | Param of Mode_arg.t
    | Cell of Mode_arg.t
  [@@deriving sexp_of]
end

type t = Step.t list [@@deriving sexp_of]

let id = []
let of_step step = [ step ]
let of_cell held = [ Step.Cell held ]
let compose outer inner = outer @ inner

let is_global t =
  List.exists t ~f:(function
    | Step.Meet { locality = Global; _ } | Cell _ -> true
    | Meet _ | Param _ -> false)
;;

let from_last_cell t =
  List.fold t ~init:[] ~f:(fun acc (step : Step.t) ->
    match step with
    | Cell _ -> [ step ]
    | Meet _ | Param _ -> acc @ [ step ])
;;
