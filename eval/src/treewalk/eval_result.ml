open! Import

type 'a t = ('a Global.t, Error.t Global.t) Local_result.t

let return x = exclave_ Ok { Global.global = x }
let error error = exclave_ Error { Global.global = error }
let error_s sexp = exclave_ error (Error.create_s sexp)

let of_or_error : 'a Or_error.t -> 'a t @ local = function
  | Ok x -> exclave_ return x
  | Error e -> exclave_ error e
;;

let to_or_error : 'a t @ local -> 'a Or_error.t = function
  | Ok { global = x } -> Ok x
  | Error { global = error } -> Error error
;;

(* [go] returns on the heap, so each [f]'s result is freed by its tail call. *)
module List = struct
  let map list ~f = exclave_
    let rec go acc list : _ Or_error.t =
      match list with
      | [] -> Ok (List.rev acc)
      | x :: list ->
        (match f x with
         | Ok { Global.global = y } -> go (y :: acc) list
         | Error { global = error } -> Error error)
    in
    of_or_error (go [] list)
  ;;

  let iter list ~f = exclave_
    let rec go list : unit Or_error.t =
      match list with
      | [] -> Ok ()
      | x :: list ->
        (match f x with
         | Ok { Global.global = () } -> go list
         | Error { global = error } -> Error error)
    in
    of_or_error (go list)
  ;;
end
