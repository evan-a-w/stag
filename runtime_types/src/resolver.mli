open! Import

(** The declarations that [Type.TCon] names, by canonical name. A program's
    types are only in it if its signature is added, and the prelude's if the
    prelude's is.

    Every signature and declaration added must come from types made by one
    [Fresh.t]: a declaration's parameters and structural types are identified by
    their ids. *)
type t

val empty : t

(** [add_decl t ~name ~decl] adds [decl] under canonical [name]. The first
    declaration of a name added wins. *)
val add_decl : t -> name:string -> decl:Signature.Type_decl.t -> t

(** [add t ~prefix signature] adds the declarations of [signature], their
    canonical names under [prefix] ([""] for a program's, [Prelude_path.root]
    for the prelude's). The first declaration of a name added wins, so add a
    program's before the prelude's. *)
val add : t -> prefix:string -> Signature.t -> t

val of_signatures : (string * Signature.t) list -> t

(** By canonical name. *)
val find : t -> string -> Signature.Type_decl.t option
