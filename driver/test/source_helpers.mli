open! Import

(** Helpers for tests written as stag source, going through the real parser
    and [Driver], printing as the DSL of [Helpers] does.

    {[
      let%expect_test "tags" =
        check
          {|
let a = `A 1
let f v = match v with `A n -> n | `B -> 0
|};
        [%expect {| ... |}]
      ;;
    ]}

    Source is in the file [t.stag], so locations print as
    [t.stag:LINE:COL-COL], with lines counted from the start of the string (so
    from 2 if it starts with a newline, as above) and columns from 0. Write
    source at column 0, so columns are those of the source as written.

    A source string is checked by [Driver.check] as a program of toplevels (not
    wrapped in a module) after [prelude], unless [~prelude:false]. The prelude
    is left out of everything printed. The files of [check_files] are checked
    by [Driver.check_files], without the prelude.

    Everything runs within [Helpers.limits], unless given other [limits].

    Failures print rather than raise: parse errors as [Parser.Error.to_string],
    type errors as the sexp of [Typecheck_error.t] (of [Driver.Error.t] for
    files), runtime errors as the sexp of [Error.t]. *)

(** A checked program. *)
type t

(** ["t.stag"], the file of source strings. *)
val file : string

(** The source of [Helpers.prims], which parses to the same toplevels, so
    stamps and type variable ids are as with the DSL:

    {v
let add (x : int) (y : int) : int = x
let eq (x : int) (y : int) : bool = true
let cond (c : bool) x y = cond c y x; x
let id x = x
    v}

    [cond c a b] gives [a] and [b] the same type, as there is no [if]. *)
val prelude : string

(** The toplevels of [source], in [file] unless given, raising on a parse
    error. *)
val parse_exn : ?file:string -> string -> Syntax.Program.t

(** [parse_exn source] after the prelude, unless [~prelude:false], for tests
    calling [Typecheck] or [Driver] themselves. *)
val program : ?prelude:bool -> string -> Syntax.Program.t

(** Checks [source], printing the error if it fails to parse or check. Toplevel
    splices run, printing to stdout. *)
val typecheck : ?limits:Limits.t -> ?prelude:bool -> string -> t option

(** As [typecheck], for toplevels built otherwise, eg. by the DSL for what
    source can't express. *)
val typecheck_program
  :  ?limits:Limits.t
  -> ?prelude:bool
  -> Syntax.Program.t
  -> t option

(** Checks the files, given as [(path, contents)], printing the error if it
    fails. *)
val check_files : ?limits:Limits.t -> (string * string) list -> t option

val driver : t -> Driver.t
val checked : t -> Typecheck.Checked.t

(** The checked toplevels, except the prelude's, in the order of their
    components. The prelude's are told apart by name, so a toplevel shadowing
    one of its names is left out too. *)
val members : t -> Typed.Toplevel.t list

(** The signature, without the prelude. *)
val print_signature : t -> unit

(** [(ident body)] for each of [members] (only those named in [only], by
    canonical name like ["M.x"], if given). Toplevel splices show their code
    and expansion. *)
val print_bodies : ?only:string list -> t -> unit

(** [(name value)] for each of [members] evaluated so far: after [typecheck],
    those evaluated at compile time. *)
val print_values : t -> unit

(** Evaluates the program ([Driver.eval]), then [print_values], then the
    runtime error, if any. *)
val eval_and_print_values : ?limits:Limits.t -> t -> unit

(** Each component of the typed program, but those of only the prelude, and
    with [~idents] the type and level of each binder, but the prelude's. *)
val print_typed : ?idents:bool -> t -> unit

(** [print_signature], then the bodies of [bodies], then:

    {v
-- evaluated at compile time
<print_values>
-- evaluated
<eval_and_print_values>
    v}

    leaving out the last part if [~eval:false]. *)
val print_run
  :  ?limits:Limits.t
  -> ?bodies:string list
  -> ?eval:bool
  -> t
  -> unit

(** {2 Shorthands}

    Each with a [_program] version taking toplevels, as [typecheck_program]. *)

(** [typecheck], then [print_signature]. *)
val check : ?limits:Limits.t -> ?prelude:bool -> string -> unit

val check_program
  :  ?limits:Limits.t
  -> ?prelude:bool
  -> Syntax.Program.t
  -> unit

(** [typecheck], then [print_run], with [limits] for both. *)
val run
  :  ?limits:Limits.t
  -> ?prelude:bool
  -> ?bodies:string list
  -> ?eval:bool
  -> string
  -> unit

val run_program
  :  ?limits:Limits.t
  -> ?prelude:bool
  -> ?bodies:string list
  -> ?eval:bool
  -> Syntax.Program.t
  -> unit
