open! Import

module Level : sig
  type t = int [@@deriving sexp, compare, equal]
end

(** What a use of a variable bound outside it crosses (Fitch-style, as in
    OxCaml): the variable's mode is constrained by each lock between its binding
    and its use. *)
module Lock : sig
  module Kind : sig
    type t =
      | Closure of Mode.Var.t
      (** A function body: what it captures is at most the closure's mode. *)
      | Quote of Mode.Var.t
      (** A quote: what it embeds from outside is [portable], and at most the
          code's mode. *)
  end

  type t =
    { id : int
    ; kind : Kind.t
    }
end

module Entry : sig
  type t =
    { scheme : Type.ty
    ; level : Level.t
    ; quote_id : int option (** The innermost quote the entry is bound in. *)
    ; mode : Mode.Var.t
    ; locks : Lock.t list (** The locks enclosing its binding, innermost first. *)
    ; binder : Typed.Binder.t
    }
  [@@deriving sexp_of]

  val toplevel : ident:Typed.Ident.t -> scheme:Type.ty -> mode:Mode.Var.t -> t

  (** The locks of [locks], innermost first, that a use under them crosses. *)
  val crossed : t -> locks:Lock.t list -> Lock.t list
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
val with_bindings : t -> (string * Entry.t) list -> f:(unit -> 'a) @ local -> 'a
