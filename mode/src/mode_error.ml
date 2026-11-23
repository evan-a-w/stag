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

let create
  ~var
  ~axis:(Axis.Any.T axis)
  ~lower
  ~upper
  ~lower_because
  ~upper_because
  =
  let module C = (val Axis.chain axis) in
  Mismatch
    { var
    ; axis
    ; lower = C.of_index_exn lower
    ; upper = C.of_index_exn upper
    ; lower_because
    ; upper_because
    }
;;

let ok_exn = function
  | Ok () -> ()
  | Error t -> raise_s (sexp_of_t t)
;;
