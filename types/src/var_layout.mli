open! Import

(** The layouts of type, row and presence variables: whether their values have
    pointers. Binding a variable decides its layout, or ties it to the layouts
    of the variables of what it is bound to. Generalizing an unbound variable
    decides it has pointers unless annotated pointer-free: a scheme's instances
    all have their own layouts, deciding the constraints of their uses, so the
    scheme keeps none that a layout of its own guards. *)

module Layout_of : sig
  (** Of a type. *)
  type t =
    | Pointer_free
    | Has_pointers
    | Depends of Mode.Layout.t list
    (** Has pointers if any of these do, else is pointer-free. *)
end

type t

(** [layout_of] says what a variable is bound to is. *)
val create : layout_of:(Type.ty -> Layout_of.t) -> t

(** Of a type, by the [layout_of] it was created with. *)
val layout_of : t -> Type.ty -> Layout_of.t

(** Made once needed. *)
val of_ty : t -> (Type.ty, _) Type.Var.t -> Mode.Layout.t

val of_row : t -> (Type.row, _) Type.Var.t -> Mode.Layout.t

(** Whether an entry of the presence is there: has pointers once it is
    [Present]. *)
val of_presence
  :  t
  -> (Type.presence, Type.presence_bound) Type.Var.t
  -> Mode.Layout.t

(** A layout of its own, for what has pointers if any of [lowers] do. *)
val above : t -> Mode.Layout.t list -> Mode.Layout.t

(** A layout of its own, for what has pointers once [layout] and any of [lowers]
    do: a field that may be present, of an entry with those layouts. *)
val both : t -> Mode.Layout.t -> Mode.Layout.t list -> Mode.Layout.t

(** The variables made while checking a scope: those numbered from [first_id]
    on, until one within it begins. *)
module Frame : sig
  type t
end

(** [f] within a frame: what it doesn't [decide] is left to the frame around it,
    unless [f] raises, abandoning what it checked. *)
val within : t -> first_id:Type.id -> (Frame.t -> 'a) -> 'a

(** Until [check] says it is done, it is run as frames are decided, after their
    layouts are: in that of the innermost scope now, or around it. [final] once
    the layouts it waits on won't be decided by generalizing: those of what a
    toplevel doesn't generalize, or of earlier toplevels. *)
val wait : t -> (final:bool -> bool) -> unit

(** Runs the [wait]ing checks of all frames, as before running code: what is
    known then is checked. *)
val check_waiting : t -> unit

(** Decides, as generalizing does, the layouts not known of the frame's
    variables still unbound of rank greater than [rank]. As its scope ends,
    those are the ones its type doesn't reach, which nothing can bind: a
    variable shared with an outer scope has the rank of its scope. Not those
    [except] keeps, which the scope's type reaches but it doesn't generalize.
    Then runs the frame's [wait]ing checks, [final] if [final]. *)
val decide
  :  ?except:(Type.id -> bool)
  -> ?final:bool
  -> Frame.t
  -> rank:Type.rank
  -> Mode.Error.t list
