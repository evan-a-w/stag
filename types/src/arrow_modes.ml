open! Import

module Side = struct
  type t =
    | Fixed of Mode.Const.t
    | Var of Mode.Var.t
    | Var_at of
        { var : Mode.Var.t
        ; locality : Mode.Locality.t
        }

  let var = function
    | Fixed _ -> None
    | Var var | Var_at { var; _ } -> Some var
  ;;

  module On_axis = struct
    type 'a t =
      | Point of 'a
      | Var of Mode.Var.t
  end

  let on_axis (type a) t (axis : a Mode.Axis.t) : a On_axis.t =
    match t, axis with
    | Fixed const, _ -> Point (Mode.Const.get const axis)
    | Var var, _ -> Var var
    | Var_at { locality; _ }, Locality -> Point locality
    | Var_at { var; _ }, (Stage | Provenance) -> Var var
  ;;
end

type t =
  { arg : Side.t
  ; res : Side.t
  ; curry : Side.t
  }

let default_const : Mode.Const.t =
  { stage = Nonportable; provenance = Gc; locality = Global }
;;

let default_curry_const : Mode.Const.t =
  { default_const with locality = Regional }
;;

let default_curry : Side.t = Fixed default_curry_const

let default =
  { arg = Fixed default_const
  ; res = Fixed default_const
  ; curry = default_curry
  }
;;

let fresh ids ~rank =
  { arg = Var (Mode.Var.create ids ~rank)
  ; res = Var (Mode.Var.create ids ~rank)
  ; curry = Var (Mode.Var.create ids ~rank)
  }
;;

let reason = Mode.Reason.create "the function types are unified"

let fixed_reason =
  Mode.Reason.create
    "a function type fixes it, as the function types are unified"
;;

let curry_reason =
  Mode.Reason.create
    "the function types are unified, including whether their results may take \
     arguments after the region ends (not printed)"
;;

let equate_side (side1 : Side.t) (side2 : Side.t) ~reason =
  match side1, side2 with
  | Var var, Fixed const | Fixed const, Var var ->
    let%bind.Result () = Mode.Var.at_least_const var const fixed_reason in
    Mode.Var.at_most_const var const fixed_reason
  | Var var1, Var var2 -> Mode.Var.equate var1 var2 reason
  | (Fixed _ | Var_at _), _ | Var _, Var_at _ ->
    let on (type a) (axis : a Mode.Axis.t) =
      match Side.on_axis side1 axis, Side.on_axis side2 axis with
      | Point p1, Point p2 -> Mode.Error.equal_on axis p1 p2 reason
      | Var var, Point p | Point p, Var var ->
        let%bind.Result () = Mode.Var.at_least var axis p fixed_reason in
        Mode.Var.at_most var axis p fixed_reason
      | Var var1, Var var2 ->
        let%bind.Result () = Mode.Var.submode_on var1 var2 axis reason in
        Mode.Var.submode_on var2 var1 axis reason
    in
    let%bind.Result () = on Stage in
    let%bind.Result () = on Provenance in
    on Locality
;;

let equate t1 t2 =
  let%bind.Result () = equate_side t1.arg t2.arg ~reason in
  let%bind.Result () = equate_side t1.res t2.res ~reason in
  equate_side t1.curry t2.curry ~reason:curry_reason
;;

let iter_side (side : Side.t) ~(local_ f) =
  match Side.var side with
  | Some var -> f var [@nontail]
  | None -> Ok ()
;;

let iter_result t ~(local_ f) =
  match iter_side t.arg ~f with
  | Error _ as error -> error
  | Ok () ->
    (match iter_side t.res ~f with
     | Error _ as error -> error
     | Ok () -> iter_side t.curry ~f [@nontail])
;;

let exists t ~(local_ f) =
  let side (side : Side.t) =
    match Side.var side with
    | Some var -> f var
    | None -> false
  in
  side t.arg || side t.res || (side t.curry [@nontail])
;;

let map_side (side : Side.t) ~f : Side.t =
  match side with
  | Fixed _ -> side
  | Var var ->
    let var' = f var in
    if phys_equal var var' then side else Var var'
  | Var_at { var; locality } ->
    let var' = f var in
    if phys_equal var var' then side else Var_at { var = var'; locality }
;;

let map t ~f =
  let arg = map_side t.arg ~f in
  let res = map_side t.res ~f in
  let curry = map_side t.curry ~f in
  if phys_equal arg t.arg && phys_equal res t.res && phys_equal curry t.curry
  then t
  else { arg; res; curry }
;;

let same_side (a : Side.t) (b : Side.t) =
  match a, b with
  | Fixed a, Fixed b -> Mode.Const.equal a b
  | Var a, Var b -> Mode.Var.id a = Mode.Var.id b
  | Var_at a, Var_at b ->
    Mode.Var.id a.var = Mode.Var.id b.var
    && Mode.Locality.equal a.locality b.locality
  | (Fixed _ | Var _ | Var_at _), _ -> false
;;

let same t t' =
  same_side t.arg t'.arg && same_side t.res t'.res && same_side t.curry t'.curry
;;

let sub_reason =
  Mode.Reason.create "a function is used at a type with other modes"
;;

let sub_fixed_reason =
  Mode.Reason.create
    "a function type fixes it, as a function is used at a type with other modes"
;;

let sub_side (lower : Side.t) (upper : Side.t) =
  match lower, upper with
  | Var var, Fixed const -> Mode.Var.at_most_const var const sub_fixed_reason
  | Fixed const, Var var -> Mode.Var.at_least_const var const sub_fixed_reason
  | Var lower, Var upper -> Mode.Var.submode lower upper sub_reason
  | (Fixed _ | Var_at _), _ | Var _, Var_at _ ->
    let on (type a) (axis : a Mode.Axis.t) =
      match Side.on_axis lower axis, Side.on_axis upper axis with
      | Point lower, Point upper ->
        Mode.Error.le_on axis ~lower ~upper sub_reason
      | Var var, Point p -> Mode.Var.at_most var axis p sub_fixed_reason
      | Point p, Var var -> Mode.Var.at_least var axis p sub_fixed_reason
      | Var lower, Var upper -> Mode.Var.submode_on lower upper axis sub_reason
    in
    let%bind.Result () = on Stage in
    let%bind.Result () = on Provenance in
    on Locality
;;

let sub ~lower ~upper =
  let%bind.Result () = sub_side upper.arg lower.arg in
  let%bind.Result () = sub_side lower.res upper.res in
  equate_side lower.curry upper.curry ~reason:curry_reason
;;
