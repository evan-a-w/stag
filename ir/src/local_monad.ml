(** A monad whose [bind] takes a local function, so traversals can take local
    ones. *)
module type S = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) @ local -> 'b t
end
