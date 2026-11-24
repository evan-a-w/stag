open! Import

type t =
  | Fixed of Mode.Const.t
  | Var of Mode.Var.t

let default : Mode.Const.t =
  { stage = Nonportable; provenance = Gc; locality = Global }
;;

let gc = Fixed default
let fresh ids ~rank = Var (Mode.Var.create ids ~rank)
let reason = Mode.Reason.create "the types are unified"

let fix var const =
  let%bind.Result () = Mode.Var.at_least_const var const reason in
  Mode.Var.at_most_const var const reason
;;

let var ids t =
  match t with
  | Var var -> var
  | Fixed const ->
    let var = Mode.Var.create ids ~rank:0 in
    Mode.Error.ok_exn (fix var const);
    var
;;

let equate ids t1 t2 =
  match t1, t2 with
  | Fixed c1, Fixed c2 when Mode.Const.equal c1 c2 -> Ok ()
  | Fixed _, Fixed const -> fix (var ids t1) const
  | Var var, Fixed const | Fixed const, Var var -> fix var const
  | Var var1, Var var2 -> Mode.Var.equate var1 var2 reason
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
  | Fixed c1, Fixed c2 -> Mode.Const.equal c1 c2
  | Var v1, Var v2 -> Mode.Var.id v1 = Mode.Var.id v2
  | Fixed _, Var _ | Var _, Fixed _ -> false
;;

let known t =
  match t with
  | Fixed const -> Some const
  | Var var ->
    let lower = Mode.Var.lower var in
    Option.some_if (Mode.Const.equal lower (Mode.Var.upper var)) lower
;;

let sexp_of_t t =
  match t with
  | Fixed const -> [%sexp (const : Mode.Const.t)]
  | Var var -> [%sexp Var (Mode.Var.id var : int)]
;;
