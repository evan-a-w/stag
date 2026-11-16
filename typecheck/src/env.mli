open! Import

module Level : sig
  type t = int [@@deriving sexp, compare, equal]
end

module Entry : sig
  type t =
    { scheme : Type.ty
    ; level : Level.t
    ; quote_id : int option (** The innermost quote the entry is bound in. *)
    ; persistent : bool
    (** Persistent entries (generalized syntactic functions) may also be used at
        any later level, from within any quote. *)
    ; binder : Typed.Binder.t
    }
  [@@deriving sexp_of]

  val toplevel : ident:Typed.Ident.t -> scheme:Type.ty -> persistent:bool -> t

  (** Non-persistent entries may only be used at their own level, within the
      quote they were bound in. *)
  val is_allowed_at : t -> used_at:Level.t -> quote_id:int option -> bool
end

(** Local bindings, each name mapping to its stack of shadowed entries. *)
type t [@@deriving sexp_of]

val create : unit -> t
val find : t -> string -> Entry.t option

(** The entry of [name] binding [binder], even if shadowed. *)
val find_binder : t -> Typed.Binder.t -> name:string -> Entry.t option

(** The innermost entry of each bound name, sorted by name. *)
val visible : t -> (string * Entry.t) list

(** Binds [bindings] (later ones shadowing earlier ones) for the duration of
    [f]. *)
val with_bindings : t -> (string * Entry.t) list -> f:(unit -> 'a) -> 'a
