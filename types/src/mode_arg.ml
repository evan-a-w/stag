open! Import

type t =
  | Fixed of Mode.Provenance.t
  | Var of Mode.Var.t

let gc = Fixed Gc
let fresh ids ~rank = Var (Mode.Var.create ids ~rank)
let reason = Mode.Reason.create "the pointer types are unified"

let fix var provenance =
  let%bind.Result () = Mode.Var.at_least var Provenance provenance reason in
  Mode.Var.at_most var Provenance provenance reason
;;

let var ids t =
  match t with
  | Var var -> var
  | Fixed provenance ->
    let var = Mode.Var.create ids ~rank:0 in
    Mode.Error.ok_exn (fix var provenance);
    var
;;

let equate ids t1 t2 =
  match t1, t2 with
  | Fixed p1, Fixed p2 when Mode.Provenance.equal p1 p2 -> Ok ()
  | Fixed _, Fixed provenance -> fix (var ids t1) provenance
  | Var var, Fixed provenance | Fixed provenance, Var var -> fix var provenance
  | Var var1, Var var2 ->
    let%bind.Result () = Mode.Var.submode_on var1 var2 Provenance reason in
    Mode.Var.submode_on var2 var1 Provenance reason
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
  | Fixed p1, Fixed p2 -> Mode.Provenance.equal p1 p2
  | Var v1, Var v2 -> Mode.Var.id v1 = Mode.Var.id v2
  | Fixed _, Var _ | Var _, Fixed _ -> false
;;

let known t =
  match t with
  | Fixed provenance -> Some provenance
  | Var var ->
    let lower = (Mode.Var.lower var).provenance in
    Option.some_if
      (Mode.Provenance.equal lower (Mode.Var.upper var).provenance)
      lower
;;

let sexp_of_t t =
  match t with
  | Fixed provenance -> [%sexp (provenance : Mode.Provenance.t)]
  | Var var -> [%sexp Var (Mode.Var.id var : int)]
;;
