open! Import

(** [Result.t], bound at local mode: [let%bindl.Local_result] and
    [let%mapl_fun.Local_result] on a result returned with [exclave_]. *)
type ('a, 'e) t = ('a, 'e) Result.t

val return : 'a @ local -> ('a, 'e) t @ local

val bind
  :  ('a, 'e) t @ local
  -> f:('a @ local -> ('b, 'e) t @ local) @ local
  -> ('b, 'e) t @ local

val map
  :  ('a, 'e) t @ local
  -> f:('a @ local -> 'b @ local) @ local
  -> ('b, 'e) t @ local

module Let_syntax : sig
  val return : 'a @ local -> ('a, 'e) t @ local

  module Let_syntax : sig
    val return : 'a @ local -> ('a, 'e) t @ local

    val bind
      :  ('a, 'e) t @ local
      -> f:('a @ local -> ('b, 'e) t @ local) @ local
      -> ('b, 'e) t @ local

    val map
      :  ('a, 'e) t @ local
      -> f:('a @ local -> 'b @ local) @ local
      -> ('b, 'e) t @ local
  end
end
