open! Import
include Stag_eval.Runtime_error

let kind =
  Raised.Kind.create
    ~name:"Treewalk.Value.t"
    ~sexp_of:Value.sexp_of_t
    ~to_string_for_message:Value.to_string_for_message
;;

let raise_exn ~loc exn = raise_exn ~loc (Raised.create kind exn)

let raised error =
  Option.bind (raised error) ~f:(fun raised -> Raised.value raised kind)
;;
