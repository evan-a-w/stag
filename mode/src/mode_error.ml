open! Import

type t =
  | Mismatch :
      { var : int
      ; axis : 'a Axis.t
      ; lower : 'a
      ; upper : 'a
      ; lower_because : Reason.t list
      ; upper_because : Reason.t list
      }
      -> t
  | Unequal :
      { axis : 'a Axis.t
      ; first : 'a
      ; second : 'a
      ; because : Reason.t
      }
      -> t

let sexp_of_t = function
  | Mismatch { var; axis; lower; upper; lower_because; upper_because } ->
    let module C = (val Axis.chain axis) in
    let var = sprintf "m%d" var in
    [%message
      "Mode mismatch"
        (var : string)
        ~axis:(Axis.Any.T axis : Axis.Any.t)
        (lower : C.t)
        (upper : C.t)
        (lower_because : Reason.t list)
        (upper_because : Reason.t list)]
  | Unequal { axis; first; second; because } ->
    let module C = (val Axis.chain axis) in
    [%message
      "Modes unequal"
        ~axis:(Axis.Any.T axis : Axis.Any.t)
        (first : C.t)
        (second : C.t)
        (because : Reason.t)]
;;

let equal_on (type a) (axis : a Axis.t) (first : a) (second : a) because =
  let module C = (val Axis.chain axis) in
  if C.equal first second
  then Ok ()
  else Error (Unequal { axis; first; second; because })
;;

let ok_exn = function
  | Ok () -> ()
  | Error t -> raise_s (sexp_of_t t)
;;
