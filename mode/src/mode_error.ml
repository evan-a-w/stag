open! Import

type t =
  | Mismatch :
      { var : int option
      ; axis : 'a Axis.t
      ; lower : 'a
      ; upper : 'a
      ; lower_because : Reason.t list
      ; upper_because : Reason.t list
      }
      -> t

let sexp_of_t
  (Mismatch { var; axis; lower; upper; lower_because; upper_because })
  =
  let module C = (val Axis.chain axis) in
  let var = Option.map var ~f:(sprintf "m%d") in
  [%message
    "Mode mismatch"
      (var : (string option[@sexp.option]))
      ~axis:(Axis.Any.T axis : Axis.Any.t)
      (lower : C.t)
      (upper : C.t)
      (lower_because : Reason.t list)
      (upper_because : Reason.t list)]
;;

let unequal (type a) (axis : a Axis.t) (a : a) (b : a) reason =
  let module C = (val Axis.chain axis) in
  Mismatch
    { var = None
    ; axis
    ; lower = C.join a b
    ; upper = C.meet a b
    ; lower_because = [ reason ]
    ; upper_because = [ reason ]
    }
;;

let ok_exn = function
  | Ok () -> ()
  | Error t -> raise_s (sexp_of_t t)
;;
