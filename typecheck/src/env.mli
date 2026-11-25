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
      | Splice
      (** A splice in a quote: it leaves the quote, so what is outside the quote
          is used across neither the splice nor the locks between it and the
          quote. *)
      | Exclave
      (** [exclave_ e]: [e] is in the caller's region, after the function's has
          ended, so what is local to the function's is gone. *)
      | Region
      (** [region_ e]: [e] is in a region of its own, so what is local outside
          it outlives it, as for a closure's body, but nothing is captured. *)
  end

  type t =
    { id : int
    ; kind : Kind.t
    }

  (** [locks] (innermost first) without the splices, nor the quotes they leave
      and the locks between them: those a use under [locks] crosses. *)
  val cancel_splices : t list -> t list
end

module Entry : sig
  type t =
    { scheme : Type.ty
    ; level : Level.t
    ; quote_id : int option (** The innermost quote the entry is bound in. *)
    ; mode : Binding.t (** Of the variable, or of a [let mut]'s cell. *)
    ; held : Binding.t option (** Of the values a [let mut] holds. *)
    ; shape : Mode_shape.t option
    (** The modes of its value's by-value components, where known: each is at
        least that component's. *)
    ; locks : Lock.t list
    (** The locks enclosing its binding, innermost first. *)
    ; binder : Typed.Binder.t
    ; defining : bool
    (** Used in its own definition, so its mode isn't final: nothing is decided
        by its current bounds. *)
    }
  [@@deriving sexp_of]

  val toplevel
    :  defining:bool
    -> ident:Typed.Ident.t
    -> scheme:Type.ty
    -> mode:Binding.t
    -> held:Binding.t option
    -> t

  (** The locks of [locks], innermost first, that a use under them crosses:
      never a [Splice]. *)
  val crossed : t -> locks:Lock.t list -> Lock.t list
end

module Lookup : sig
  type t =
    | Bound of Entry.t
    | Forgotten (** By [forget], so neither a local nor a toplevel. *)
    | Unbound (** No local: maybe a toplevel. *)
end

(** Local bindings, each name mapping to its stack of shadowed entries. *)
type t [@@deriving sexp_of]

val create : unit -> t

(** The innermost binding of [name] seen at [level]. *)
val lookup : t -> string -> level:int -> Lookup.t

(** The entry of [name] binding [binder], even if shadowed. *)
val find_binder : t -> Typed.Binder.t -> name:string -> Entry.t option

(** The innermost entry of each name bound at [level], sorted by name. *)
val visible : t -> level:int -> (string * Entry.t) list

(** Binds [bindings] (later ones shadowing earlier ones) for the duration of
    [f]. *)
val with_bindings : t -> (string * Entry.t) list -> f:(unit -> 'a) @ local -> 'a

(** Hides [names]' bindings, local and toplevel, for the duration of [f], at
    [level] and the quotes in it, but not in splices back to an earlier level. *)
val forgetting : t -> string list -> level:int -> f:(unit -> 'a) @ local -> 'a
