open! Core

(** A file of a program, which is the module named after it, as in OCaml:
    [dir/foo_bar.stag] is [Foo_bar]. *)
type t =
  { path : string (** As given, which locations name. *)
  ; contents : string
  }
[@@deriving sexp_of]

(** [Foo_bar] for [dir/foo_bar.stag] or [dir/Foo_bar.stag]. Errs unless the
    basename is [.stag] after a letter followed by letters, digits, [_] and
    [']. *)
val module_name : string -> (string, string) Result.t

module Error : sig
  type t =
    | Parse of Parser.Error.t
    | Invalid_file_name of
        { path : string
        ; message : string
        }
  [@@deriving sexp_of]

  (** [Invalid_file_name] is at the start of the file. *)
  val loc : t -> Loc.t
end

(** The module of the file: [Toplevel.Module], named at the start of the file,
    of its toplevels. *)
val parse : t -> (Syntax.Toplevel.t, Error.t) Result.t
