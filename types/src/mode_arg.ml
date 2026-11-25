open! Import

module Const = struct
  type t =
    { stage : Mode.Stage.t
    ; provenance : Mode.Provenance.t
    }
  [@@deriving sexp_of, equal]

  let default = { stage = Nonportable; provenance = Gc }

  let of_mode ({ stage; provenance; locality = _ } : Mode.Const.t) =
    { stage; provenance }
  ;;
end

type t =
  | Fixed of Const.t
  | Var of Mode.Var.t

let gc = Fixed Const.default
let reason = Mode.Reason.create "the types are unified"

let fix var ({ stage; provenance } : Const.t) =
  let on (type a) (axis : a Mode.Axis.t) (point : a) =
    let%bind.Result () = Mode.Var.at_least var axis point reason in
    Mode.Var.at_most var axis point reason
  in
  let%bind.Result () = on Stage stage in
  on Provenance provenance
;;

let equate t1 t2 =
  match t1, t2 with
  | Fixed c1, Fixed c2 ->
    if not (Mode.Stage.equal c1.stage c2.stage)
    then Error (Mode.Error.unequal Stage c1.stage c2.stage reason)
    else if not (Mode.Provenance.equal c1.provenance c2.provenance)
    then
      Error (Mode.Error.unequal Provenance c1.provenance c2.provenance reason)
    else Ok ()
  | Var var, Fixed const | Fixed const, Var var -> fix var const
  | Var var1, Var var2 ->
    let on axis =
      let%bind.Result () = Mode.Var.submode_on var1 var2 axis reason in
      Mode.Var.submode_on var2 var1 axis reason
    in
    let%bind.Result () = on Mode.Axis.Stage in
    on Provenance
;;

let iter_result t ~(local_ f) =
  match t with
  | Var var -> f var [@nontail]
  | Fixed _ -> Ok ()
;;

let exists t ~(local_ f) =
  match t with
  | Var var -> f var [@nontail]
  | Fixed _ -> false
;;

let map t ~f =
  match t with
  | Fixed _ -> t
  | Var var ->
    let var' = f var in
    if phys_equal var var' then t else Var var'
;;

let same t1 t2 =
  match t1, t2 with
  | Fixed c1, Fixed c2 -> Const.equal c1 c2
  | Var v1, Var v2 -> Mode.Var.id v1 = Mode.Var.id v2
  | Fixed _, Var _ | Var _, Fixed _ -> false
;;

let known t =
  match t with
  | Fixed const -> Some const
  | Var var ->
    let lower = Const.of_mode (Mode.Var.lower var) in
    Option.some_if
      (Const.equal lower (Const.of_mode (Mode.Var.upper var)))
      lower
;;

let sexp_of_t t =
  match t with
  | Fixed const -> [%sexp (const : Const.t)]
  | Var var -> [%sexp Var (Mode.Var.id var : int)]
;;
