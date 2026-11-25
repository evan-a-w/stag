open! Import

type t =
  | Fixed of Mode.Const.t
  | Var of Mode.Var.t

let default : Mode.Const.t =
  { stage = Nonportable; provenance = Gc; locality = Global }
;;

let gc = Fixed default
let reason = Mode.Reason.create "the types are unified"

(* On stage and provenance: a mode argument has no locality. *)
let fix var (const : Mode.Const.t) =
  let on (type a) (axis : a Mode.Axis.t) =
    let point = Mode.Const.get const axis in
    let%bind.Result () = Mode.Var.at_least var axis point reason in
    Mode.Var.at_most var axis point reason
  in
  let%bind.Result () = on Stage in
  on Provenance
;;

let same_const (c1 : Mode.Const.t) (c2 : Mode.Const.t) =
  Mode.Stage.equal c1.stage c2.stage
  && Mode.Provenance.equal c1.provenance c2.provenance
;;

let equate ids t1 t2 =
  match t1, t2 with
  | Fixed c1, Fixed c2 when same_const c1 c2 -> Ok ()
  | Fixed c1, Fixed c2 ->
    let var = Mode.Var.create ids ~rank:0 in
    let%bind.Result () = fix var c1 in
    fix var c2
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
  | Fixed c1, Fixed c2 -> same_const c1 c2
  | Var v1, Var v2 -> Mode.Var.id v1 = Mode.Var.id v2
  | Fixed _, Var _ | Var _, Fixed _ -> false
;;

let known t =
  match t with
  | Fixed const -> Some const
  | Var var ->
    let lower = Mode.Var.lower var in
    let upper = Mode.Var.upper var in
    Option.some_if
      (same_const lower upper)
      { lower with locality = default.locality }
;;

let sexp_of_t t =
  match t with
  | Fixed const -> [%sexp (const : Mode.Const.t)]
  | Var var -> [%sexp Var (Mode.Var.id var : int)]
;;

let sub_reason = Mode.Reason.create "a type is used at one with another mode"

let sub ids ~lower ~upper =
  let var t =
    match t with
    | Var var -> Ok var
    | Fixed const ->
      let var = Mode.Var.create ids ~rank:0 in
      let%map.Result () = fix var const in
      var
  in
  match lower, upper with
  | Fixed c1, Fixed c2
    when Mode.Stage.le c1.stage c2.stage
         && Mode.Provenance.le c1.provenance c2.provenance -> Ok ()
  | _ ->
    let%bind.Result lower = var lower in
    let%bind.Result upper = var upper in
    let%bind.Result () =
      Mode.Var.submode_on lower upper Mode.Axis.Stage sub_reason
    in
    Mode.Var.submode_on lower upper Provenance sub_reason
;;
