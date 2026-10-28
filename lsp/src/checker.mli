open! Import

(** Checking a file as [stag-build] does, within its project's target, or as
    [stag check] does if it is in no project or a source of the prelude in a
    [Stdlib_checkout]. Paths are absolute. *)

module Result : sig
  type t =
    { diagnostics : Diagnostic.t list String.Map.t
      (** By file, for each file checked (or that the project failed to
          load because of), empty if fine. Locations name files by absolute
          path. *)
    ; index : Index.t option
      (** If it typechecked. Locations name files by absolute path. *)
    }
end

(** Checks [path], with the contents of each file as [contents] (eg. an
    editor's buffers) if some, else on disk. Compile-time evaluation is
    within [limits], and its output is dropped. Definitions in the prelude are
    in the sources of the checkout [path] is in, else of [stdlib], if those
    are the prelude's embedded. *)
val check
  :  ?stdlib:Stdlib_checkout.t
  -> limits:Limits.t
  -> contents:(string -> string option)
  -> string
  -> Result.t
