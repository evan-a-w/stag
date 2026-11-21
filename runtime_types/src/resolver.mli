open! Import

(** The declarations that [Type.TCon] names, by canonical name. A program's
    types are only in it if its signature is added, and the prelude's if the
    prelude's is. *)
type t

val empty : t

(** [add t ~prefix signature] adds the declarations of [signature], their
    canonical names under [prefix] ([""] for a program's, [Prelude_path.root]
    for the prelude's). The first declaration of a name added wins, so add a
    program's before the prelude's. *)
val add : t -> prefix:string -> Signature.t -> t

val of_signatures : (string * Signature.t) list -> t

(** By canonical name. *)
val find : t -> string -> Signature.Type_decl.t option
