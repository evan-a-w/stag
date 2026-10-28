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
    [column], with each [Decl_path.Root] and [Decl_path.Resolved] path printed
    as the path [path] gives (given where it is used, as for
    [Decl_paths.map]), else a root path as is, and a resolved name [name] as
    [<name>]. *)
val status
  :  written:Syntax.Toplevel.t list
  -> Syntax.Toplevel.t list
  -> column:int
  -> path:
       (Decl_paths.Namespace.t
        -> modules:string list
        -> locals:String.Set.t
        -> Syntax.Decl_path.t
        -> Syntax.Decl_path.t option)
  -> Status.t
