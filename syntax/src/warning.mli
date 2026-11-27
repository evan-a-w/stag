open! Import

(** Warnings, by name, and their configuration by warning specs. *)

module Name : sig
  type t =
    | Non_exhaustive
    | Unused_arm
  [@@deriving sexp, compare, equal, enumerate]

  (** As written in a spec, eg. ["non-exhaustive"]. *)
  val to_string : t -> string

  val of_string : string -> t option
end

module Level : sig
  type t =
    | Off
    | Warn
    | Error
  [@@deriving sexp, compare, equal]
end

(** A warning spec, as in [[@warning "-unused-arm +non-exhaustive"]]: items
    separated by whitespace or commas, each a sign ([-] off, [+] a warning, [@]
    an error) then a warning's name, or [all] for every warning. *)
module Spec : sig
  module Item : sig
    module Target : sig
      type t =
        | All
        | Warning of Name.t
      [@@deriving sexp, compare, equal]
    end

    type t =
      { level : Level.t
      ; target : Target.t
      }
    [@@deriving sexp, compare, equal]
  end

  type t = Item.t list [@@deriving sexp, compare, equal]

  val of_string : string -> (t, string) Result.t
  val to_string : t -> string
end

module Settings : sig
  type t [@@deriving sexp_of]

  val default : t

  (** Every warning [Off]. *)
  val off : t

  (** Later items of the spec win. *)
  val apply : t -> Spec.t -> t

  val level : t -> Name.t -> Level.t
end
