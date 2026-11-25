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

module On_axis = struct
  type 'e t =
    { f : 'a. 'a Mode.Axis.t -> get:(Const.t -> 'a) -> (unit, 'e) Result.t }
end

let on_axes ({ f } : _ On_axis.t) =
  let%bind.Result () = f Stage ~get:(fun c -> c.stage) in
  f Provenance ~get:(fun c -> c.provenance)
;;

type t =
  | Fixed of Const.t
  | Var of Mode.Var.t

let gc = Fixed Const.default
let reason = Mode.Reason.create "the types are unified"

let equate t1 t2 =
  on_axes
    { f =
        (fun axis ~get ->
          match t1, t2 with
          | Fixed c1, Fixed c2 ->
            Mode.Error.equal_on axis (get c1) (get c2) reason
          | Var var, Fixed const | Fixed const, Var var ->
            let%bind.Result () =
              Mode.Var.at_least var axis (get const) reason
            in
            Mode.Var.at_most var axis (get const) reason
          | Var var1, Var var2 ->
            let%bind.Result () = Mode.Var.submode_on var1 var2 axis reason in
            Mode.Var.submode_on var2 var1 axis reason)
    }
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
