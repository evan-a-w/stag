open! Import

(** An [include%inline] or [module%inline] whose splice ran. *)

module Status : sig
  type t =
    | Up_to_date
    | Stale of { generated : string }
    (** The declarations generated, printed as their expansion should be
        written, at column 0. *)
  [@@deriving sexp_of]
end

type t =
  { loc : Loc.t (** The splice's: its toplevel up to the end of [.~e]. *)
  ; expansion : Syntax.Toplevel.t list Located.t
  ; status : Status.t
  }
[@@deriving sexp_of]

(** Whether [written] is how [generated] is printed, by a toplevel at
    [column], with each [Decl_path.Resolved] name printed as the path [path]
    gives, or [<name>] if none. *)
val status
  :  written:Syntax.Toplevel.t list
  -> Syntax.Toplevel.t list
  -> column:int
  -> path:
       (Decl_paths.Namespace.t
        -> locals:String.Set.t
        -> string
        -> Syntax.Decl_path.t option)
  -> Status.t
