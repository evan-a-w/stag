open! Import

module Side = struct
  type t =
    | Fixed of Mode.Const.t
    | Var of Mode.Var.t
end

type t =
  { arg : Side.t
  ; res : Side.t
  ; curry : Side.t
  }

let default_const : Mode.Const.t =
  { stage = Nonportable; provenance = Unknown; locality = Global }
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

let fixed_reason = Mode.Reason.create "a function type fixes it"

let side_var ids ~rank : Side.t -> Mode.Var.t = function
  | Var var -> var
  | Fixed const ->
    let var = Mode.Var.create ids ~rank in
    (match
       let%bind.Result () = Mode.Var.at_least_const var const fixed_reason in
       Mode.Var.at_most_const var const fixed_reason
     with
     | Ok () -> var
     | Error error -> raise_s [%sexp (error : Mode.Error.t)])
;;

let reason = Mode.Reason.create "the function types are unified"

let equate_side ids (side1 : Side.t) (side2 : Side.t) =
  match side1, side2 with
  | Fixed const1, Fixed const2 when Mode.Const.equal const1 const2 -> Ok ()
  | _ ->
    Mode.Var.equate
      (side_var ids ~rank:0 side1)
      (side_var ids ~rank:0 side2)
      reason
;;

let equate ids t1 t2 =
  let%bind.Result () = equate_side ids t1.arg t2.arg in
  let%bind.Result () = equate_side ids t1.res t2.res in
  equate_side ids t1.curry t2.curry
;;

let side_vars : Side.t -> _ = function
  | Var var -> [ var ]
  | Fixed _ -> []
;;

let vars t = side_vars t.arg @ side_vars t.res @ side_vars t.curry

let map_side (side : Side.t) ~f : Side.t =
  match side with
  | Fixed _ -> side
  | Var var ->
    let var' = f var in
    if phys_equal var var' then side else Var var'
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
  | Fixed _, Var _ | Var _, Fixed _ -> false
;;

let same t t' =
  same_side t.arg t'.arg && same_side t.res t'.res && same_side t.curry t'.curry
;;
