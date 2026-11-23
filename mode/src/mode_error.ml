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

let sexp_of_t
  (Mismatch { var; axis; lower; upper; lower_because; upper_because })
  =
  let module C = (val Axis.chain axis) in
  [%message
    "Mode mismatch"
      ~var:(sprintf "m%d" var)
      ~axis:(Axis.Any.T axis : Axis.Any.t)
      (lower : C.t)
      (upper : C.t)
      (lower_because : Reason.t list)
      (upper_because : Reason.t list)]
;;

let ok_exn = function
  | Ok () -> ()
  | Error t -> raise_s (sexp_of_t t)
;;
