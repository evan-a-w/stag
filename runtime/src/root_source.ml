open! Import

module type S = sig
  type t

  val name : string
  val holds_locals : bool
  val iter : t -> f:(Memory.Addr.t -> unit) @ local -> unit [@@zero_alloc]
end

type t = T : (module S with type t = 'a) * 'a -> t

let create m x = T (m, x)

let name (T (m, _)) =
  let module M = (val m) in
  M.name
;;

let holds_locals (T (m, _)) =
  let module M = (val m) in
  M.holds_locals
;;

(* The checker can't see through the indirect call, but every [S.iter] is
   checked zero-alloc when it is made a source. *)
let[@zero_alloc] iter (T (m, x)) ~f =
  let module M = (val m) in
  (M.iter [@zero_alloc assume]) x ~f
;;
