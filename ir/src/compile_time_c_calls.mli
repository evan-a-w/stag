open! Import

(** Which [external]s may be called at compile time: those of the libraries that
    allow it, and the prelude's. Calls at runtime are always allowed. *)

module Policy : sig
  (** Of one external. *)
  type t =
    | Allowed
    | Denied of
        { external_ : string (** Its canonical name. *)
        ; library : string option
        (** The library declaring it, if it is in a project. *)
        }
  [@@deriving sexp_of, equal]

  (** Why a call at compile time of an external [Denied] it fails. *)
  val denial : external_:string -> library:string option -> string
end

module Library : sig
  type t =
    { name : string
    ; allowed : bool
    }
end

type t

(** Only the prelude's externals (core's included). *)
val none : t

val all : t

(** The prelude's externals, and those of each root module (eg. [Foo] for
    [Foo.f] or [Foo.M.f]) of [libraries] as its library says. *)
val of_libraries : Library.t String.Map.t -> t

(** Of the external of canonical name [external_]. *)
val policy : t -> external_:string -> Policy.t
