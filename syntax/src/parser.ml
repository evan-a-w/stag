open! Import
open Syntax

module Error = struct
  type t =
    | Lex of Lexer.Error.t
    | Syntax of
        { loc : Loc.t
        ; message : string
        }
  [@@deriving sexp_of]

  let loc = function
    | Lex error -> error.loc
    | Syntax { loc; _ } -> loc
  ;;

  let message = function
    | Lex error -> Lexer.Error.message error
    | Syntax { message; loc = _ } -> message
  ;;

  let to_string = function
    | Lex error -> Lexer.Error.to_string error
    | Syntax { loc; message } -> sprintf "%s: %s" (Loc.to_string loc) message
  ;;
end

exception Syntax_error of Loc.t * string

type t =
  { tokens : Lexer.Located.t array
  ; comments : Comment.t array
  ; mutable pos : int
  ; mutable bare_tuple_end : int
  (** The [pos] after the last [)] whose contents were a tuple without
      parentheses of its own, as in [(a, b)] but not [((a, b))]. *)
  ; errors : Error.t Queue.t option
  (** Where to record errors, to keep parsing toplevels after them, if any. *)
  ; mutable salvage : (Expr.t -> Toplevel.desc) option
  (** What of the toplevel being parsed parsed, given an [Expr.Error] for the
      rest. *)
  ; lex_errors : int array
  (** The offsets of the lexemes skipped by lexer errors, in order, if
      recovering. *)
  ; mutable limit : int
  (** When recovering, the index of the next token that ends the toplevel being
      parsed (it appears as [Eof]), else that of [Eof]. *)
  ; mutable col : int (** That of the toplevel being parsed, if recovering. *)
  }

let steps = ref 0

let peek_at t n =
  incr steps;
  let i = t.pos + n in
  t.tokens.(if i >= t.limit then Array.length t.tokens - 1 else i).token
;;

let peek t = peek_at t 0
let loc t = t.tokens.(t.pos).loc
let prev_loc t = t.tokens.(t.pos - 1).loc
let is t token = Token.equal (peek t) token

let advance t =
  match peek t with
  | Eof -> ()
  | _ -> t.pos <- t.pos + 1
;;

let located ~loc txt = { Located.txt; loc }

(** Located from [start] to the last token consumed. *)
let since t start txt = located ~loc:(Loc.merge start (prev_loc t)) txt

let describe : Token.t -> string = function
  | Eof -> "end of file"
  | Label label -> sprintf "the label `%s" label
  | token -> sprintf "`%s`" (Token.to_string token)
;;

let fail loc message = raise (Syntax_error (loc, message))

(* The token at [pos], even past [limit]. *)
let found t = t.tokens.(t.pos).token

let expected t what =
  match found t with
  | At_at -> fail (loc t) "`@@` is reserved (for modalities)"
  | token ->
    fail (loc t) (sprintf "expected %s, found %s" what (describe token))
;;

let expect t token =
  if is t token then advance t else expected t (describe token)
;;

(** Expects [close], matching the [opener] at [open_loc]. *)
let close t token ~opener ~open_loc =
  if is t token
  then advance t
  else
    fail
      (loc t)
      (sprintf
         "expected %s to close this %s (opened at %d:%d), found %s"
         (describe token)
         (describe opener)
         open_loc.Loc.start.#line
         open_loc.start.#col
         (describe (found t)))
;;

(** Expects [token], which goes with the [opener] at [open_loc]. *)
let expect_for t token ~opener ~open_loc =
  if is t token
  then advance t
  else
    fail
      (loc t)
      (sprintf
         "expected %s for the %s at %d:%d, found %s"
         (describe token)
         (describe opener)
         open_loc.Loc.start.#line
         open_loc.start.#col
         (describe (found t)))
;;

(** Parses [opener items close], with [item]s separated by [sep] and an optional
    trailing [sep]. *)
let delimited t ~opener ~close:closer ~sep ~item =
  let open_loc = loc t in
  expect t opener;
  let rec loop acc =
    if is t closer
    then List.rev acc
    else (
      let acc = item t :: acc in
      if is t sep
      then (
        advance t;
        loop acc)
      else List.rev acc)
  in
  let items = loop [] in
  close t closer ~opener ~open_loc;
  items
;;

(** [[a; b]], with an optional trailing [;]: the items, each with the [[] or
    [;] before it, and the [\]], or the whole [[]] if there are none. *)
let list_items t ~item =
  let open_loc = loc t in
  expect t Lbracket;
  let rec loop before acc =
    if is t Rbracket
    then List.rev acc
    else (
      let acc = (before, item t) :: acc in
      if is t Semi
      then (
        let semi = loc t in
        advance t;
        loop semi acc)
      else List.rev acc)
  in
  let items = loop open_loc [] in
  let close_loc = loc t in
  close t Rbracket ~opener:Lbracket ~open_loc;
  items, if List.is_empty items then Loc.merge open_loc close_loc else close_loc
;;

let lident t ~what =
  match peek t with
  | Lident name ->
    let loc = loc t in
    advance t;
    located ~loc name
  | _ -> expected t what
;;

let uident t ~what =
  match peek t with
  | Uident name ->
    let loc = loc t in
    advance t;
    located ~loc name
  | _ -> expected t what
;;

let label t ~what =
  match peek t with
  | Label name ->
    let loc = loc t in
    advance t;
    located ~loc name
  | _ -> expected t what
;;

let tyvar t =
  match peek t with
  | Tyvar name ->
    let loc = loc t in
    advance t;
    located ~loc name
  | Weak_tyvar name ->
    fail
      (loc t)
      (sprintf
         "weak type variables like `'_%s` cannot be written, they are only \
          printed"
         name)
  | _ -> expected t "a type variable like `'a`"
;;

(** A path [M.N.x] or [M.N.C], or a root path [.M.N.x], and whether its last
    name is capitalized. *)
let path t ~what =
  let start = loc t in
  let root = is t Root in
  if root then advance t;
  let rec go acc =
    match peek t, peek_at t 1, peek_at t 2 with
    | Uident name, Dot, (Uident _ | Lident _) ->
      advance t;
      advance t;
      go (name :: acc)
    | Uident name, _, _ ->
      advance t;
      List.rev (name :: acc), true
    | Lident name, _, _ ->
      advance t;
      List.rev (name :: acc), false
    | _ -> expected t what
  in
  let names, capitalized = go [] in
  let path = Path.of_list_exn names in
  ( since t start (if root then Decl_path.Root path else Decl_path.Path path)
  , capitalized )
;;

let lowercase_path t ~what =
  match path t ~what with
  | path, false -> path
  | _, true ->
    t.pos <- t.pos - 1;
    expected t what
;;

let uppercase_path t ~what =
  match path t ~what with
  | path, true -> path
  | _, false ->
    t.pos <- t.pos - 1;
    expected t what
;;

(* Types hold splices of expressions, and expressions struct quotes of
   declarations, which are parsed after them. *)
let spliced_expr : (t -> Expr.t) ref = ref (fun _ -> assert false)
let struct_toplevels : (t -> Toplevel.t list) ref = ref (fun _ -> assert false)

(* Types *)

let starts_type : Token.t -> bool = function
  | Tyvar _
  | Weak_tyvar _
  | Lident _
  | Uident _
  | Root
  | Lparen
  | Lbrace
  | Lbracket
  | Less
  | Splice -> true
  | _ -> false
;;

(** [@ 'm] or [@ local portable], after the [@]. *)
let mode_expr t : Mode_expr.t =
  let start = loc t in
  match peek t with
  | Tyvar _ | Weak_tyvar _ ->
    let var = tyvar t in
    located ~loc:var.loc (Mode_expr.Var var.txt)
  | Lident _ ->
    let rec loop acc =
      match peek t with
      | Lident "and" when not (List.is_empty acc) -> List.rev acc
      | Lident name ->
        (match Mode_expr.Atom.of_string name with
         | None ->
           fail
             (loc t)
             (sprintf
                "unknown mode `%s`, expected one of %s"
                name
                (String.concat
                   ~sep:", "
                   (List.map Mode_expr.Atom.all ~f:(fun atom ->
                      "`" ^ Mode_expr.Atom.to_string atom ^ "`"))))
         | Some atom ->
           (match
              List.find acc ~f:(fun other ->
                Mode_expr.Axis.equal
                  (Mode_expr.Atom.axis other)
                  (Mode_expr.Atom.axis atom))
            with
            | Some other ->
              fail
                (loc t)
                (sprintf
                   "`%s` and `%s` are both %s modes"
                   (Mode_expr.Atom.to_string other)
                   name
                   (Mode_expr.Axis.to_string (Mode_expr.Atom.axis atom)))
            | None ->
              advance t;
              loop (atom :: acc)))
      | _ -> List.rev acc
    in
    let atoms = loop [] in
    since t start (Mode_expr.Atoms atoms)
  | _ -> expected t "a mode like `local`, or a mode variable like `'m`"
;;

(** [with 'm <= 'n and ...], after the [with]. *)
let mode_constraints t =
  let rec loop acc =
    let lower = mode_expr t in
    if not (is t Less && Token.equal (peek_at t 1) Equal) then expected t "`<=`";
    advance t;
    advance t;
    let upper = mode_expr t in
    (match lower.txt, upper.txt with
     | Atoms _, Atoms _ ->
       fail
         (Loc.merge lower.loc upper.loc)
         "a mode constraint relates a mode variable, as in `'m <= 'n` or `'m \
          <= portable`"
     | _ -> ());
    let acc = { Mode_expr.Constraint.lower; upper } :: acc in
    match peek t with
    | Lident "and" ->
      advance t;
      loop acc
    | _ -> List.rev acc
  in
  loop []
;;

let misplaced_mode =
  "a mode is only written on a function type's argument or result, as in `t @ \
   local -> u`, or on an annotation, as in `(x : t @ local)`"
;;

(* A type that is not [t @ m]. *)
let rec type_expr t : Type_expr.t =
  let ty = moded_type_expr t in
  match ty.txt with
  | Moded (_, mode) -> fail mode.loc misplaced_mode
  | _ -> ty

(* A type, or [t @ m] that is an annotation's type and mode. *)
and moded_type_expr t : Type_expr.t =
  let domain = arrow_operand t in
  if is t Arrow
  then (
    advance t;
    let codomain = moded_type_expr t in
    located
      ~loc:(Loc.merge domain.loc codomain.loc)
      (Type_expr.Arrow (domain, codomain)))
  else domain

and arrow_operand t : Type_expr.t =
  let ty = type_tuple t in
  if is t At
  then (
    advance t;
    let mode = mode_expr t in
    if is t At
    then
      fail
        (loc t)
        "a type has one mode: write its modes together, as in `t @ local \
         portable`";
    located ~loc:(Loc.merge ty.loc mode.loc) (Type_expr.Moded (ty, mode)))
  else ty

and type_tuple t : Type_expr.t =
  let first = type_app t in
  if is t Star
  then (
    let rec loop acc =
      if is t Star
      then (
        advance t;
        loop (type_app t :: acc))
      else List.rev acc
    in
    let tys = loop [ first ] in
    located
      ~loc:(Loc.merge first.loc (List.last_exn tys).loc)
      (Type_expr.Tuple tys))
  else first

and type_app t : Type_expr.t =
  let ty = type_atom t in
  let ty =
    if is t Lbracket
    then (
      match ty.txt with
      | Con (path, []) ->
        let args =
          delimited t ~opener:Lbracket ~close:Rbracket ~sep:Comma ~item:type_arg
        in
        if List.is_empty args
        then fail (prev_loc t) "expected a type argument, found `]`";
        if is t Lbracket then fail (loc t) "type arguments are written once";
        since t ty.loc (Type_expr.Con (path, args))
      | _ ->
        fail (loc t) "only type constructors like `list` take type arguments")
    else ty
  in
  (match peek t with
   | Lident _ | Uident _ ->
     fail
       (loc t)
       "type arguments are written after the name, in brackets: `list[int]`"
   | _ -> ());
  ty

and type_arg t : Type_expr.arg =
  match peek t with
  | Lbrace -> Row (record_row t)
  | _ -> Ty (type_expr t)

and record_row t =
  row
    t
    ~entry_label:(fun t ->
      match peek t with
      | Lident name ->
        fail
          (loc t)
          (sprintf
             "polymorphic record labels are written with a backtick: `%s"
             name)
      | _ -> label t ~what:"a polymorphic record label like `x")
    ~what:"a polymorphic record label like `x"

and type_atom t : Type_expr.t =
  let start = loc t in
  match peek t with
  | Tyvar _ | Weak_tyvar _ ->
    let var = tyvar t in
    located ~loc:var.loc (Var var.txt : Type_expr.desc)
  | Lident _ | Uident _ | Root ->
    let path = lowercase_path t ~what:"a type name" in
    located ~loc:path.loc (Type_expr.Con (path, []))
  | Lparen ->
    advance t;
    if is t Rparen
    then (
      advance t;
      since t start (Type_expr.Tuple []))
    else (
      let ty = type_expr t in
      close t Rparen ~opener:Lparen ~open_loc:start;
      since t start ty.txt)
  | Lbrace ->
    let row = record_row t in
    since t start (Type_expr.Record row)
  | Lbracket -> variant t
  | Less ->
    advance t;
    let inner = type_expr t in
    close t Greater ~opener:Less ~open_loc:start;
    if not (is t Lbrace)
    then expected t "the code type's context, like `{..'r}`";
    let context =
      row
        t
        ~entry_label:(fun t ->
          match peek t with
          | Label name ->
            fail
              (loc t)
              (sprintf
                 "the variables of a code context are written without a \
                  backtick: %s"
                 name)
          | _ -> lident t ~what:"a variable name")
        ~what:"a variable name"
    in
    since t start (Type_expr.Code { inner; context })
  | Splice ->
    advance t;
    let e = !spliced_expr t in
    since t start (Type_expr.Splice e)
  | _ -> expected t "a type"

(** A zero width [Empty] row, before the current token. *)
and empty_row t : Type_expr.row =
  let loc = loc t in
  located ~loc:{ loc with stop = loc.start } Type_expr.Empty

and extend_row name ty (rest : Type_expr.row) : Type_expr.row =
  let last =
    match rest.txt with
    | Empty -> ty.Located.loc
    | Extend _ | Var _ -> rest.loc
  in
  located
    ~loc:(Loc.merge name.Located.loc last)
    (Type_expr.Extend (name, ty, rest))

(** [..'r], at [..]. *)
and row_tail t : Type_expr.row =
  let start = loc t in
  expect t Dotdot;
  let var = tyvar t in
  since t start (Type_expr.Var var)

(** [{l : t; ...; ..'r}], located with its braces. An entry's type may be
    [mut t] or [immut t], for a context: [immut] is a keyword only there, before
    a type. *)
and row t ~entry_label ~what : Type_expr.row =
  let open_loc = loc t in
  expect t Lbrace;
  let rec loop () : Type_expr.row =
    match peek t with
    | Rbrace -> empty_row t
    | Dotdot -> row_tail t
    | Label _ | Lident _ ->
      let name = entry_label t in
      expect t Colon;
      let ty =
        match peek t with
        | Mut ->
          let start = loc t in
          advance t;
          let ty = type_expr t in
          since t start (Type_expr.Mut ty)
        | Lident "immut" when starts_type (peek_at t 1) ->
          let start = loc t in
          advance t;
          let ty = type_expr t in
          since t start (Type_expr.Immut ty)
        | _ -> type_expr t
      in
      let rest =
        match peek t with
        | Semi ->
          advance t;
          loop ()
        | _ -> empty_row t
      in
      extend_row name ty rest
    | _ -> expected t what
  in
  let row = loop () in
  close t Rbrace ~opener:Lbrace ~open_loc;
  since t open_loc row.txt

(** [[`A of t | `B; ..'r]]. *)
and variant t : Type_expr.t =
  let start = loc t in
  expect t Lbracket;
  let leading_bar = is t Bar in
  if leading_bar then advance t;
  let tail () : Type_expr.row =
    match peek t with
    | Dotdot -> row_tail t
    | _ -> expected t "a row variable like `..'r` after `;`"
  in
  let rec entries ~first : Type_expr.row =
    match peek t with
    | Label _ ->
      let tag = label t ~what:"" in
      let ty =
        if is t Of
        then (
          advance t;
          type_expr t)
        else
          located
            ~loc:tag.loc
            (Type_expr.Con
               (located ~loc:tag.loc (Decl_path.Root (Path.name "unit")), []))
      in
      let rest : Type_expr.row =
        match peek t with
        | Bar ->
          advance t;
          entries ~first:false
        | Semi ->
          advance t;
          tail ()
        | _ -> empty_row t
      in
      extend_row tag ty rest
    | Dotdot when first && not leading_bar -> row_tail t
    | Rbracket when first && not leading_bar -> empty_row t
    | _ -> expected t "a polymorphic variant tag like `A"
  in
  let row = entries ~first:true in
  close t Rbracket ~opener:Lbracket ~open_loc:start;
  let row = since t start row.txt in
  since t start (Type_expr.Variant row)
;;

(* Patterns *)

let starts_pattern_atom : Token.t -> bool = function
  | Underscore
  | Star
  | Lident _
  | Uident _
  | Root
  | Label _
  | Int _
  | Char _
  | String _
  | True
  | False
  | Lparen
  | Lbracket
  | Lbrace -> true
  | _ -> false
;;

(** [C (a, b)] has two arguments (a tuple, if [C] is declared with one, which
    the typechecker decides), and [C ((a, b))] one; [arg] was just parsed. *)
let construct_args t (arg : _ Located.t) ~tuple_items =
  match tuple_items arg.txt with
  | Some items when t.bare_tuple_end = t.pos -> items
  | _ -> [ arg ]
;;

(** Closes the parentheses around [contents], recording if [contents] is a tuple
    without parentheses of its own: one starting where its first item does, as
    parentheses are part of a location. *)
let close_parens t ~open_loc (contents : _ Located.t) ~tuple_items =
  close t Rparen ~opener:Lparen ~open_loc;
  match tuple_items contents.txt with
  | Some ((first : _ Located.t) :: _)
    when Loc.Pos.equal first.loc.start contents.loc.start ->
    t.bare_tuple_end <- t.pos
  | _ -> ()
;;

let rec pattern t : Pattern.t =
  let first = pattern_construct t in
  if is t Comma
  then (
    let rec loop acc =
      if is t Comma
      then (
        advance t;
        loop (pattern_construct t :: acc))
      else List.rev acc
    in
    let items = loop [ first ] in
    located
      ~loc:(Loc.merge first.loc (List.last_exn items).loc)
      (Pattern.Tuple items))
  else first

and pattern_construct t : Pattern.t =
  let start = loc t in
  match peek t with
  | Star ->
    advance t;
    let inner = pattern_construct t in
    since t start (Pattern.Deref inner)
  | Uident _ | Root ->
    let path = uppercase_path t ~what:"a constructor" in
    let args =
      if starts_pattern_atom (peek t)
      then (
        let arg = pattern_construct t in
        construct_args t arg ~tuple_items:(function
          | Pattern.Tuple items -> Some items
          | _ -> None))
      else []
    in
    since t start (Pattern.Construct (path, args))
  | Label tag ->
    let tag = located ~loc:start tag in
    advance t;
    let arg =
      if starts_pattern_atom (peek t) then Some (pattern_construct t) else None
    in
    since t start (Pattern.Poly_variant (tag, arg))
  | Exception ->
    advance t;
    let pattern = pattern_construct t in
    since t start (Pattern.Exception pattern)
  | _ -> pattern_atom t

and pattern_atom t : Pattern.t =
  let start = loc t in
  let lit lit =
    advance t;
    since t start (Pattern.Lit lit)
  in
  match peek t with
  | Underscore ->
    advance t;
    since t start Pattern.Any
  | Lident name ->
    advance t;
    since t start (Pattern.Var name)
  | Int n -> lit (Int n)
  | Char c -> lit (Char c)
  | String s -> lit (String s)
  | True -> lit (Bool true)
  | False -> lit (Bool false)
  | Lparen ->
    advance t;
    if is t Rparen
    then (
      advance t;
      since t start (Pattern.Lit Unit))
    else (
      let pattern = pattern t in
      if is t Colon
      then fail (loc t) "type annotations on patterns are not supported";
      close_parens t ~open_loc:start pattern ~tuple_items:(function
        | Pattern.Tuple items -> Some items
        | _ -> None);
      since t start pattern.txt)
  | Lbrace -> record_pattern t
  | Lbracket ->
    let items, close = list_items t ~item:pattern in
    List_literal.pattern ~items ~close
  | _ -> expected t "a pattern"

and record_pattern t : Pattern.t =
  let start = loc t in
  match peek_at t 1 with
  | Lident _ | Uident _ | Root ->
    let fields =
      delimited t ~opener:Lbrace ~close:Rbrace ~sep:Semi ~item:(fun t ->
        let path = lowercase_path t ~what:"a field name" in
        expect t Equal;
        path, pattern t)
    in
    since t start (Pattern.Record fields)
  | _ ->
    advance t;
    let rec loop acc =
      match peek t with
      | Rbrace -> List.rev acc, Pattern.Rest.Closed
      | Dotdot ->
        advance t;
        (match peek t with
         | Lident _ -> List.rev acc, Bind (lident t ~what:"")
         | _ -> List.rev acc, Open)
      | Label _ ->
        let name = label t ~what:"" in
        expect t Equal;
        let acc = (name, pattern t) :: acc in
        if is t Semi
        then (
          advance t;
          loop acc)
        else List.rev acc, Closed
      | _ -> expected t "a polymorphic record label like `x, or `..`"
    in
    let fields, rest = loop [] in
    close t Rbrace ~opener:Lbrace ~open_loc:start;
    since t start (Pattern.Poly_record { fields; rest })
;;

(* Expressions *)

let expr_at ~loc (desc : Expr.desc) : Expr.t = located ~loc desc
let expr_since t start (desc : Expr.desc) : Expr.t = since t start desc

let starts_atom : Token.t -> bool = function
  | Int _
  | Star
  | Char _
  | String _
  | Lident _
  | Uident _
  | Root
  | Label _
  | True
  | False
  | Lparen
  | Lbracket
  | Lbrace
  | Quote_open
  | Expect_open
  | Splice -> true
  | _ -> false
;;

(** At [{], whether it starts a nominal record: [{x = ...] or [{M.x = ...]. *)
let nominal_record_ahead t =
  let rec go i =
    match peek_at t i, peek_at t (i + 1) with
    | Uident _, Dot -> go (i + 2)
    | Lident _, Equal -> true
    | _ -> false
  in
  go (if Token.equal (peek_at t 1) Root then 2 else 1)
;;

let pattern_parameter =
  "patterns are not allowed as parameters, only `x`, `_`, `(x : t)` or `(x @ \
   m)`; use `match`"
;;

let attribute_ahead t = is t Lbracket && Token.equal (peek_at t 1) At

(** Parses [[@nontail]], returning its location. *)
let nontail_attribute t =
  let open_loc = loc t in
  advance t;
  advance t;
  let name = lident t ~what:"an attribute, `nontail`" in
  if not (String.equal name.txt "nontail")
  then
    fail
      name.loc
      (sprintf "unknown attribute `[@%s]`, only `[@nontail]` exists" name.txt);
  close t Rbracket ~opener:Lbracket ~open_loc;
  Loc.merge open_loc (prev_loc t)
;;

let nontail_without_application_message =
  "`[@nontail]` is only written after an application, as in `f x [@nontail]`"
;;

let nontail_without_application t =
  fail (nontail_attribute t) nontail_without_application_message
;;

(** [: t], [: t @ m] or [@ m], if any. *)
let annot t : Annot.t =
  match peek t with
  | Colon ->
    advance t;
    let ty = moded_type_expr t in
    let ty, mode =
      match ty.txt with
      | Moded (ty, mode) -> ty, Some mode
      | _ -> ty, None
    in
    let ty =
      if is t With
      then (
        advance t;
        let constraints = mode_constraints t in
        let last = List.last_exn constraints in
        located
          ~loc:(Loc.merge ty.loc last.upper.loc)
          (Type_expr.Constrained (ty, constraints)))
      else ty
    in
    { ty = Some ty; mode }
  | At ->
    advance t;
    let mode = mode_expr t in
    if is t With
    then
      fail
        (loc t)
        "mode constraints follow an annotation's type, as in `(x : t @ 'm with \
         'm <= portable)`";
    { ty = None; mode = Some mode }
  | _ -> Annot.none
;;

(** [x], [_], [(x : t)], [(x @ m)] or [(x : t @ m)]. *)
let param t : Binding.t =
  let binder t =
    match peek t with
    | Underscore ->
      let loc = loc t in
      advance t;
      located ~loc "_"
    | _ -> lident t ~what:"a parameter"
  in
  match peek t with
  | Lparen ->
    let open_loc = loc t in
    (match peek_at t 1, peek_at t 2 with
     | (Lident _ | Underscore), (Colon | At) -> ()
     | _ -> fail open_loc pattern_parameter);
    advance t;
    let name = binder t in
    let annot = annot t in
    close t Rparen ~opener:Lparen ~open_loc;
    name, annot
  | _ -> binder t, Annot.none
;;

let params t =
  let rec loop acc =
    match peek t with
    | Lident _ | Underscore | Lparen -> loop (param t :: acc)
    | _ -> List.rev acc
  in
  loop []
;;

(* With [~seq:false], as in a [{...}] entry, [e1; e2] is not parsed (also in the
   bodies of [let] and [match]), leaving the [;] as the separator. *)
let rec expr ?(seq = true) t : Expr.t =
  match peek t with
  | Let -> let_ t ~seq
  | Match -> match_ t ~seq
  | Fun -> fun_ t ~seq
  | Stack_ | Exclave_ -> region t ~seq
  | _ ->
    let first = assignment t ~seq in
    if seq && is t Semi
    then (
      let semi = loc t in
      advance t;
      let second = expr t in
      expr_at
        ~loc:(Loc.merge first.loc second.loc)
        (Let (located ~loc:semi "_", [], Annot.none, first, second)))
    else first

and binding_name t =
  if is t Rec then fail (loc t) "lets are always recursive; remove `rec`";
  match peek t with
  | Underscore ->
    let loc = loc t in
    advance t;
    located ~loc "_"
  | _ -> lident t ~what:"a name to bind"

(** [name params : t @ m = e], after [let]. *)
and binding t =
  let name = binding_name t in
  let params = params t in
  let annot = annot t in
  expect t Equal;
  name, params, annot, expr t

(** [name : t = e], after [let mut]. *)
and mut_binding t =
  expect t Mut;
  let name = binding_name t in
  if not (List.is_empty (params t))
  then fail name.loc "a mutable variable has no parameters";
  let annot = annot t in
  expect t Equal;
  name, annot, expr t

and let_ t ~seq : Expr.t =
  let start = loc t in
  expect t Let;
  match peek t with
  | Mut ->
    let name, annot, equal = mut_binding t in
    expect_for t In ~opener:Let ~open_loc:start;
    let body = expr t ~seq in
    expr_at
      ~loc:(Loc.merge start body.loc)
      (Expr.Let_mut (name, annot, equal, body))
  | _ ->
    let name, params, annot, equal = binding t in
    expect_for t In ~opener:Let ~open_loc:start;
    let body = expr t ~seq in
    expr_at
      ~loc:(Loc.merge start body.loc)
      (Expr.Let (name, params, annot, equal, body))

(** [x <- e], right associative, looser than [,]. *)
and assignment t ~seq : Expr.t =
  let target = tuple t ~seq in
  match peek t with
  | Larrow ->
    let store = store_target target in
    advance t;
    let value = assignment t ~seq in
    expr_at ~loc:(Loc.merge target.loc value.loc) (store value)
  | _ -> target

and store_target (target : Expr.t) : Expr.t -> Expr.desc =
  let rec under_deref (e : Expr.t) =
    match e.txt with
    | Deref _ -> true
    | Field (inner, _) | Poly_field (inner, _) -> under_deref inner
    | _ -> false
  in
  match target.txt with
  | Var (path, annot) when Annot.is_none annot ->
    fun value -> Assign (located ~loc:target.loc path, value)
  | (Deref _ | Field _ | Poly_field _) when under_deref target ->
    fun value -> Store (target, value)
  | Field _ | Poly_field _ ->
    fail
      target.loc
      "only a field of a dereference is mutable, as in [p.*.x <- v]: records \
       are values, and are mutated through pointers"
  | _ -> fail target.loc "only a variable or a dereference can be assigned to"

and fun_ t ~seq : Expr.t =
  let start = loc t in
  expect t Fun;
  let params = params t in
  if List.is_empty params
  then (
    match peek t with
    | Uident _
    | Label _
    | Int _
    | Char _
    | String _
    | True
    | False
    | Lbrace
    | Lbracket -> fail (loc t) pattern_parameter
    | _ -> expected t "a parameter");
  expect t Arrow;
  let body = expr t ~seq in
  expr_at ~loc:(Loc.merge start body.loc) (Expr.Fun (params, body))

and match_ t ~seq : Expr.t =
  let start = loc t in
  expect t Match;
  let scrutinee = expr t in
  expect_for t With ~opener:Match ~open_loc:start;
  if is t Bar then advance t;
  let rec arms acc =
    let pattern = pattern t in
    expect t Arrow;
    let body = expr t ~seq in
    let acc = (pattern, body) :: acc in
    if is t Bar
    then (
      advance t;
      arms acc)
    else List.rev acc, body
  in
  let arms, last = arms [] in
  expr_at ~loc:(Loc.merge start last.loc) (Expr.Match (scrutinee, arms))

and tuple t ~seq : Expr.t =
  let first = tuple_item t ~seq in
  if is t Comma
  then (
    let rec loop acc =
      if is t Comma
      then (
        advance t;
        loop (tuple_item t ~seq :: acc))
      else List.rev acc
    in
    let items = loop [ first ] in
    expr_at ~loc:(Loc.merge first.loc (List.last_exn items).loc) (Tuple items))
  else first

and tuple_item t ~seq : Expr.t =
  match peek t with
  | Let -> let_ t ~seq
  | Match -> match_ t ~seq
  | Fun -> fun_ t ~seq
  | Stack_ | Exclave_ -> region t ~seq
  | _ -> construct t

(** [local_ e] or [exclave_ e]. *)
and region t ~seq : Expr.t =
  let start = loc t in
  let keyword = peek t in
  advance t;
  let body = expr t ~seq in
  expr_at
    ~loc:(Loc.merge start body.loc)
    (match keyword with
     | Exclave_ -> Exclave body
     | _ -> Stack body)

(** A value in [{...}]. *)
and entry_value t = assignment t ~seq:false

and construct ?(in_argument = false) t : Expr.t =
  let start = loc t in
  let saved = t.pos in
  match peek t with
  | Uident _ | Root ->
    (match path t ~what:"" with
     | path, true ->
       let args =
         if attribute_ahead t
         then nontail_without_application t
         else if starts_atom (peek t)
         then (
           let arg = construct ~in_argument:true t in
           construct_args t arg ~tuple_items:(function
             | Expr.Tuple items -> Some items
             | _ -> None))
         else []
       in
       expr_since t start (Construct (path, args))
     | _, false ->
       t.pos <- saved;
       app ~in_argument t)
  | Label tag ->
    let tag = located ~loc:start tag in
    advance t;
    let arg =
      if attribute_ahead t
      then nontail_without_application t
      else if starts_atom (peek t)
      then Some (construct ~in_argument:true t)
      else None
    in
    expr_since t start (Poly_variant (tag, arg))
  | Type ->
    advance t;
    expr_since t start (Type (type_app t))
  | _ -> app ~in_argument t

(* [in_argument]: [t] is a constructor's argument, which [[@nontail]] can't
   follow, as it would read as applying to the construction. *)
and app ?(in_argument = false) t : Expr.t =
  let rec loop (f : Expr.t) =
    if starts_atom (peek t) && not (attribute_ahead t)
    then (
      let arg = splice t in
      loop (expr_at ~loc:(Loc.merge f.loc arg.loc) (Expr.App (f, arg))))
    else f
  in
  let e = loop (splice t) in
  if attribute_ahead t
  then (
    let attribute_loc = nontail_attribute t in
    match e.txt with
    | App _ when in_argument ->
      fail
        attribute_loc
        "`[@nontail]` can't follow a constructor's argument: parenthesize the \
         application, as in `C (f x [@nontail])`"
    | App _ -> expr_since t e.loc (Nontail e)
    | _ -> fail attribute_loc nontail_without_application_message)
  else e

and splice t : Expr.t =
  match peek t with
  | Star ->
    let start = loc t in
    advance t;
    let inner = splice t in
    expr_since t start (Deref inner)
  | Splice ->
    let start = loc t in
    advance t;
    let expr =
      match peek t with
      | Splice -> splice t
      | _ -> postfix t
    in
    let subs =
      if is t Dot_lbrace
      then
        Some
          (delimited
             t
             ~opener:Dot_lbrace
             ~close:Rbrace
             ~sep:Semi
             ~item:(fun t ->
               let name = lident t ~what:"a variable to provide" in
               if is t Equal
               then (
                 advance t;
                 name, Some (entry_value t))
               else name, None))
      else None
    in
    expr_since t start (Splice { expr; subs })
  | _ ->
    let e = postfix t in
    if is t Dot_lbrace
    then
      fail
        (loc t)
        "substitutions `.{...}` are only written after a splice `.~e`";
    e

and postfix t : Expr.t =
  let rec loop (e : Expr.t) =
    match peek t with
    | Dot_star ->
      advance t;
      loop (expr_since t e.loc (Deref e))
    | Dot ->
      advance t;
      (match peek t with
       | Label _ ->
         let label = label t ~what:"" in
         loop (expr_since t e.loc (Poly_field (e, label)))
       | Lparen ->
         let open_loc = loc t in
         advance t;
         if not (is t Root)
         then expected t "a root path like `.M.x`: `e.(.M.x)`";
         let field = lowercase_path t ~what:"a field name" in
         close t Rparen ~opener:Lparen ~open_loc;
         loop (expr_since t e.loc (Field (e, field)))
       | _ ->
         let field = lowercase_path t ~what:"a field name" in
         loop (expr_since t e.loc (Field (e, field))))
    | Backslash ->
      advance t;
      let label = label t ~what:"a polymorphic record label like `x" in
      loop (expr_since t e.loc (Poly_restrict (e, label)))
    | _ -> e
  in
  loop (atom t)

and atom t : Expr.t =
  let start = loc t in
  let lit lit =
    advance t;
    expr_since t start (Expr.Lit lit)
  in
  match peek t with
  | Int n -> lit (Int n)
  | Char c -> lit (Char c)
  | String s -> lit (String s)
  | True -> lit (Bool true)
  | False -> lit (Bool false)
  | Lident _ | Uident _ | Root ->
    (match path t ~what:"" with
     | path, true -> expr_since t start (Construct (path, []))
     | path, false -> expr_since t start (Var (path.txt, Annot.none)))
  | Label tag ->
    advance t;
    expr_since t start (Poly_variant (located ~loc:start tag, None))
  | Lparen ->
    advance t;
    if is t Rparen
    then (
      advance t;
      expr_since t start (Lit Unit))
    else (
      let e = expr t in
      match peek t with
      | Colon | At ->
        (match e.txt with
         | Var (path, existing) when Annot.is_none existing ->
           let annot = annot t in
           close t Rparen ~opener:Lparen ~open_loc:start;
           expr_since t start (Var (path, annot))
         | _ ->
           fail
             e.loc
             "annotations are only supported on variables, as in `(x : t)` or \
              `(x @ m)`")
      | _ ->
        close_parens t ~open_loc:start e ~tuple_items:(function
          | Expr.Tuple items -> Some items
          | _ -> None);
        expr_since t start e.txt)
  | Lbrace -> record t
  | Quote_open when [%equal: Token.t] (peek_at t 1) Struct ->
    advance t;
    let struct_loc = loc t in
    advance t;
    let toplevels = !struct_toplevels t in
    close t End ~opener:Struct ~open_loc:struct_loc;
    close t Quote_close ~opener:Quote_open ~open_loc:start;
    expr_since t start (Struct_quote toplevels)
  | Lbracket ->
    let items, close = list_items t ~item:entry_value in
    List_literal.expr ~items ~close
  | Quote_open ->
    advance t;
    let e = expr t in
    close t Quote_close ~opener:Quote_open ~open_loc:start;
    expr_since t start (Quote e)
  | Expect_open ->
    advance t;
    let expected =
      match peek t with
      | String s ->
        let loc = loc t in
        advance t;
        located ~loc s
      | _ -> located ~loc:{ start with start = start.stop } ""
    in
    close t Rbracket ~opener:Expect_open ~open_loc:start;
    expr_since t start (Expect expected)
  | _ -> expected t "an expression"

and poly_field t =
  let name = label t ~what:"a polymorphic record label like `x" in
  expect t Equal;
  name, entry_value t

and nominal_field t =
  let path = lowercase_path t ~what:"a field name" in
  expect t Equal;
  path, entry_value t

and record t : Expr.t =
  let start = loc t in
  match peek_at t 1 with
  | Rbrace ->
    advance t;
    advance t;
    expr_since t start (Poly_record [])
  | Label _ ->
    advance t;
    let rec loop acc =
      let acc = poly_field t :: acc in
      match peek t with
      | Semi ->
        advance t;
        (match peek t with
         | Rbrace -> List.rev acc, None
         | _ -> loop acc)
      | Bar ->
        advance t;
        List.rev acc, Some (entry_value t)
      | _ -> List.rev acc, None
    in
    let fields, base = loop [] in
    close t Rbrace ~opener:Lbrace ~open_loc:start;
    (match base with
     | None -> expr_since t start (Poly_record fields)
     | Some base -> expr_since t start (Poly_extend (fields, base)))
  | _ when nominal_record_ahead t ->
    let fields =
      delimited t ~opener:Lbrace ~close:Rbrace ~sep:Semi ~item:nominal_field
    in
    expr_since t start (Record fields)
  | _ ->
    advance t;
    let base = splice t in
    if not (is t With) then expected t "`with` or a record field";
    advance t;
    let rec loop acc ~field =
      let acc = field t :: acc in
      if is t Semi
      then (
        advance t;
        if is t Rbrace then List.rev acc else loop acc ~field)
      else List.rev acc
    in
    (match peek t with
     | Label _ ->
       let fields = loop [] ~field:poly_field in
       close t Rbrace ~opener:Lbrace ~open_loc:start;
       expr_since t start (Poly_update (base, fields))
     | _ ->
       let fields = loop [] ~field:nominal_field in
       close t Rbrace ~opener:Lbrace ~open_loc:start;
       expr_since t start (Record_update (base, fields)))
;;

(* Declarations *)

let salvage t f = t.salvage <- Some f

let type_param t : Type_decl.Param.t Located.t =
  let start = loc t in
  match peek t with
  | Tyvar _ | Weak_tyvar _ ->
    let var = tyvar t in
    located ~loc:var.loc (Type_decl.Param.Ty var.txt)
  | Lbrace ->
    advance t;
    expect t Dotdot;
    let var = tyvar t in
    close t Rbrace ~opener:Lbrace ~open_loc:start;
    since t start (Type_decl.Param.Row var.txt)
  | _ -> expected t "a type parameter like `'a` or `{..'r}`"
;;

(** [global_ t], with [t] parsed by [ty], or [t]. *)
let maybe_global t ~ty : Type_expr.t =
  match peek t with
  | Global_ ->
    let start = loc t in
    advance t;
    let inner = ty t in
    since t start (Type_expr.Global inner)
  | _ -> ty t
;;

(* [parsed] is told the arguments parsed so far, as they are parsed. An
   exception's arguments are always global, so are not written [global_]. *)
let constructor_args ?(parsed = ignore) ~exception_ t =
  if is t Of
  then (
    advance t;
    let rec args acc =
      if exception_ && is t Global_
      then
        fail
          (loc t)
          "an exception's arguments are always global: remove `global_`";
      let acc = maybe_global t ~ty:type_app :: acc in
      parsed (List.rev acc);
      match peek t with
      | At ->
        fail
          (loc t)
          (if exception_
           then "an exception's arguments have no mode, they are always global"
           else
             "a constructor's argument has no mode: to make it always global, \
              write `global_ t`")
      | Star ->
        advance t;
        args acc
      | Arrow ->
        fail
          (loc t)
          "a constructor argument of function type must be parenthesized: `C \
           of (a -> b)`"
      | _ -> List.rev acc
    in
    args [])
  else []
;;

(* [parsed] is told the members parsed so far, including the one being parsed
   once it is named, as they are parsed. *)
let type_kind t ~(parsed : Type_decl.Kind.t -> unit) : Type_decl.Kind.t =
  match peek t, peek_at t 1 with
  | Splice, _ ->
    let start = loc t in
    advance t;
    let e = postfix t in
    (match peek t with
     | Star | Arrow ->
       fail
         (loc t)
         (sprintf
            "a type splice `.~e` defines the whole type: to splice part of an \
             alias, write it in parentheses, as in `type t = (.~e * u)`")
     | _ -> ());
    Splice (since t start e)
  | Uident _, Dot -> Alias (type_expr t)
  | (Bar | Uident _), _ ->
    let leading_bar = is t Bar in
    if leading_bar then advance t;
    (match peek t with
     | Uident _ ->
       let rec loop acc =
         let name = uident t ~what:"a constructor name" in
         let parsed_args args =
           parsed (Variant (List.rev ((name, args) :: acc)))
         in
         parsed_args [];
         let acc =
           (name, constructor_args t ~exception_:false ~parsed:parsed_args)
           :: acc
         in
         parsed (Variant (List.rev acc));
         if is t Bar
         then (
           advance t;
           loop acc)
         else List.rev acc
       in
       Variant (loop [])
     | _ -> Variant [])
  | Lbrace, Lident _ ->
    let fields = ref [] in
    Record
      (delimited t ~opener:Lbrace ~close:Rbrace ~sep:Semi ~item:(fun t ->
         let name = lident t ~what:"a field name" in
         (* For the name, whose type is never looked at. *)
         let missing = located ~loc:name.loc (Type_expr.Tuple []) in
         parsed (Record (List.rev ((name, missing) :: !fields)));
         expect t Colon;
         let field = name, maybe_global t ~ty:type_expr in
         fields := field :: !fields;
         parsed (Record (List.rev !fields));
         field))
  | _ -> Alias (type_expr t)
;;

(* [salvage] is told what parsed of it so far, as it is parsed. *)
let type_decl t ~salvage : Type_decl.t =
  let name = lident t ~what:"a type name" in
  let salvage params kind =
    salvage { Type_decl.name; params; kind = Malformed kind }
  in
  salvage [] Abstract;
  let params =
    if is t Lbracket
    then (
      let params =
        delimited t ~opener:Lbracket ~close:Rbracket ~sep:Comma ~item:type_param
      in
      if List.is_empty params
      then fail (prev_loc t) "expected a type parameter, found `]`";
      params)
    else []
  in
  salvage params Abstract;
  let kind =
    if is t Equal
    then (
      advance t;
      type_kind t ~parsed:(salvage params))
    else Abstract
  in
  salvage params kind;
  { name; params; kind }
;;

(* What follows a skipped lexeme in a toplevel parsed as if it were not there,
   so the toplevel is salvaged as if it were a syntax error. Those in a [struct]
   are in the toplevels in it. *)
let with_lex_errors t (toplevel : Toplevel.t) ~start =
  let start = t.tokens.(start).loc.start.#offset in
  let stop = (loc t).start.#offset in
  match toplevel.txt, t.salvage with
  | (Module _ | Module_splice (_, _, Some _) | Include (_, Some _)), _ | _, None
    -> toplevel
  | _, Some desc ->
    if Array.binary_search
         t.lex_errors
         ~compare:Int.compare
         `First_greater_than_or_equal_to
         start
       |> function
       | None -> false
       | Some i -> t.lex_errors.(i) < stop
    then { toplevel with txt = desc (expr_at ~loc:toplevel.loc Error) }
    else toplevel
;;

let first_on_line t i =
  i = 0 || t.tokens.(i).loc.start.#line > t.tokens.(i - 1).loc.stop.#line
;;

(* Whether the token at [i] is a toplevel keyword first on its line and not
   indented more than [col]. *)
let starts_toplevel_at t i ~col =
  match t.tokens.(i).token with
  | Let
  | Type
  | Exception
  | Module
  | Module_inline
  | Include
  | Include_inline
  | Let_expect_test
  | Let_bench -> first_on_line t i && t.tokens.(i).loc.start.#col <= col
  | _ -> false
;;

(* The index of the first token from [from] that starts a toplevel not indented
   more than [col], or of [Eof]. *)
let next_toplevel t ~from ~col =
  let rec go i =
    incr steps;
    match t.tokens.(i).token with
    | Eof -> i
    | _ when starts_toplevel_at t i ~col -> i
    | _ -> go (i + 1)
  in
  go from
;;

let closer : Token.t -> Token.t option = function
  | Lparen -> Some Rparen
  | Lbracket | Expect_open -> Some Rbracket
  | Lbrace | Dot_lbrace -> Some Rbrace
  | Quote_open -> Some Quote_close
  | Struct -> Some End
  | Module_inline | Include_inline -> Some End_inline
  | _ -> None
;;

(* The innermost bracket opened from [start] and not closed before [stop], with
   its closer, if any. *)
let unclosed t ~start ~stop =
  let rec go i open_ =
    incr steps;
    if i >= stop
    then List.hd open_
    else (
      let ({ token; loc } : Lexer.Located.t) = t.tokens.(i) in
      match closer token, token with
      | Some closer, _ -> go (i + 1) ((token, loc, closer) :: open_)
      | None, (Rparen | Rbracket | Rbrace | Quote_close | End | End_inline) ->
        go (i + 1) (List.tl open_ |> Option.value ~default:[])
      | None, _ -> go (i + 1) open_)
  in
  go start []
;;

let rec toplevel t : Toplevel.t =
  let start = loc t in
  match peek t with
  | Let when Token.equal (peek_at t 1) Mut ->
    advance t;
    let name, annot, equal = mut_binding t in
    if is t In
    then
      fail
        (loc t)
        "unexpected `in`: a toplevel `let` is a declaration, without `in`";
    since t start (Toplevel.Let_mut (name, annot, equal))
  | Let ->
    advance t;
    let name = binding_name t in
    salvage t (fun error -> Let (name, [], Annot.none, error));
    let params = params t in
    salvage t (fun error -> Let (name, params, Annot.none, error));
    let annot = annot t in
    salvage t (fun error -> Let (name, params, annot, error));
    expect t Equal;
    let equal = expr t in
    if is t In
    then (
      salvage t (fun _ -> Let (name, params, annot, equal));
      fail
        (loc t)
        "unexpected `in`: a toplevel `let` is a declaration, without `in`");
    since t start (Toplevel.Let (name, params, annot, equal))
  | Type ->
    advance t;
    let decl =
      type_decl t ~salvage:(fun decl -> salvage t (fun _ -> Type decl))
    in
    since t start (Toplevel.Type decl)
  | Exception ->
    advance t;
    let name = uident t ~what:"a constructor name" in
    let parsed args = salvage t (fun _ -> Malformed_exception (name, args)) in
    parsed [];
    let args = constructor_args t ~exception_:true ~parsed in
    since t start (Toplevel.Exception (name, args))
  | (Module | Module_inline) as keyword ->
    advance t;
    let name = uident t ~what:"a module name" in
    salvage t (fun error -> Module_splice (name, error, None));
    expect t Equal;
    (match peek t, keyword with
     | Splice, _ ->
       let expr = declaration_splice t in
       let expansion = expansion t ~keyword ~open_loc:start in
       since t start (Toplevel.Module_splice (name, expr, expansion))
     | _, Module ->
       let struct_loc = loc t in
       expect t Struct;
       let toplevels = toplevels t ~in_struct:true in
       close_recovering t Token.End ~opener:Token.Struct ~open_loc:struct_loc;
       since t start (Toplevel.Module (name, toplevels))
     | _ -> expected t "a declaration splice `.~e` after `module%inline M =`")
  | (Include | Include_inline) as keyword ->
    advance t;
    salvage t (fun error -> Include (error, None));
    (match peek t with
     | Splice ->
       let expr = declaration_splice t in
       let expansion = expansion t ~keyword ~open_loc:start in
       since t start (Toplevel.Include (expr, expansion))
     | _ ->
       expected
         t
         (sprintf "a declaration splice `.~e` after %s" (describe keyword)))
  | Let_expect_test ->
    advance t;
    let name =
      match peek t with
      | String s ->
        let loc = loc t in
        advance t;
        located ~loc s
      | _ -> expected t "the name of the test, a string"
    in
    salvage t (fun body -> Expect_test { name; body });
    expect t Equal;
    let body = expr t in
    since t start (Toplevel.Expect_test { name; body })
  | Let_bench ->
    advance t;
    let name =
      match peek t with
      | String s ->
        let loc = loc t in
        advance t;
        located ~loc s
      | _ -> expected t "the name of the benchmark, a string"
    in
    salvage t (fun body -> Bench { name; param = None; body });
    let param =
      match peek t with
      | Lident _ ->
        let name = lident t ~what:"a parameter" in
        expect t In;
        let open_loc = loc t in
        (match List.map (fst (list_items t ~item:entry_value)) ~f:snd with
         | [] -> fail open_loc "a parameterised benchmark needs an argument"
         | args -> Some { Bench_param.name; args })
      | _ -> None
    in
    salvage t (fun body -> Bench { name; param; body });
    expect t Equal;
    let body = expr t in
    since t start (Toplevel.Bench { name; param; body })
  | _ ->
    expected
      t
      "a declaration (`let`, `type`, `exception`, `module` or `include`)"

(* The next toplevel after the broken one at [start]: the next toplevel keyword
   first on its line and not indented more than the broken one, or, in a
   [struct], the [end] closing it, by nesting or by being less indented.
   Unmatched closing brackets are skipped. *)
and resync t ~start ~in_struct =
  let col = t.tokens.(start).loc.start.#col in
  let rec go i ~depth =
    incr steps;
    let ({ token; loc } : Lexer.Located.t) = t.tokens.(i) in
    match token with
    | Eof -> i
    | _ when starts_toplevel_at t i ~col -> i
    | (End | End_inline)
      when in_struct && first_on_line t i && loc.start.#col < col -> i
    | token when Option.is_some (closer token) -> go (i + 1) ~depth:(depth + 1)
    | (End | End_inline) when in_struct && depth = 0 -> i
    | Rparen | Rbracket | Rbrace | End | End_inline | Quote_close ->
      go (i + 1) ~depth:(Int.max 0 (depth - 1))
    | _ -> go (i + 1) ~depth
  in
  go (start + 1) ~depth:0

(* [.~e], the declarations [e] generates. *)
and declaration_splice t =
  advance t;
  postfix t

(* After [include%inline] or [module%inline], the declarations up to
   [end%inline], from the end of the splice (and comments on its line) to the
   start of [end%inline]. *)
and expansion t ~(keyword : Token.t) ~open_loc =
  match keyword with
  | Module_inline | Include_inline ->
    let splice_end = (prev_loc t).stop in
    let last_comment =
      Array.fold t.comments ~init:None ~f:(fun last (comment : Comment.t) ->
        if comment.next_token = t.pos
           && comment.loc.start.#line = splice_end.#line
           && comment.loc.start.#offset >= splice_end.#offset
        then Some comment
        else last)
    in
    let start =
      match last_comment with
      | Some comment -> comment.loc.stop
      | None -> splice_end
    in
    let toplevels = toplevels t ~in_struct:true in
    let end_loc = loc t in
    close_recovering t End_inline ~opener:keyword ~open_loc;
    Some (located ~loc:{ end_loc with start; stop = end_loc.start } toplevels)
  | _ -> None

and close_recovering t (token : Token.t) ~(opener : Token.t) ~open_loc =
  match t.errors with
  | None -> close t token ~opener ~open_loc
  | Some errors ->
    (try close t token ~opener ~open_loc with
     | Syntax_error (loc, message) ->
       Queue.enqueue errors (Syntax { loc; message }))

and toplevels t ~in_struct =
  let stop () =
    match peek t with
    | Eof -> true
    | End | End_inline -> in_struct || Option.is_none t.errors
    | _ -> false
  in
  match t.errors with
  | None ->
    let rec loop acc =
      if stop () then List.rev acc else loop (toplevel t :: acc)
    in
    loop []
  | Some errors ->
    let no_limit = Array.length t.tokens - 1 in
    let outer_limit = t.limit in
    let outer_col = t.col in
    (* Unless the declarations in it are not indented more than it. *)
    let limit = if t.limit = t.pos then no_limit else t.limit in
    let rec loop acc =
      t.limit <- limit;
      if stop ()
      then List.rev acc
      else (
        let start = t.pos in
        t.salvage <- None;
        t.col <- t.tokens.(start).loc.start.#col;
        t.limit <- Int.min limit (next_toplevel t ~from:(start + 1) ~col:t.col);
        match
          match peek t with
          | (Rparen | Rbracket | Rbrace | Quote_close | End | End_inline) as
            token -> fail (loc t) (sprintf "unmatched %s" (describe token))
          | _ -> toplevel t
        with
        | toplevel -> loop (with_lex_errors t toplevel ~start :: acc)
        | exception Syntax_error (loc, message) ->
          let message =
            match
              t.pos = t.limit
              && t.limit < Array.length t.tokens - 1
              && Loc.equal loc t.tokens.(t.limit).loc
            with
            | false -> message
            | true ->
              (match unclosed t ~start ~stop:t.limit with
               | None -> message
               | Some (opener, open_loc, closer) ->
                 sprintf
                   "expected %s to close this %s (opened at %d:%d), found %s"
                   (describe closer)
                   (describe opener)
                   open_loc.start.#line
                   open_loc.start.#col
                   (describe (found t)))
          in
          Queue.enqueue errors (Syntax { loc; message });
          let salvage = t.salvage in
          t.pos <- resync t ~start ~in_struct;
          let loc = Loc.merge t.tokens.(start).loc (prev_loc t) in
          (match salvage with
           | None -> loop acc
           | Some desc -> loop (located ~loc (desc (expr_at ~loc Error)) :: acc)))
    in
    let toplevels = loop [] in
    t.col <- outer_col;
    t.limit
    <- (if outer_limit = no_limit || t.pos <= outer_limit
        then outer_limit
        else next_toplevel t ~from:t.pos ~col:outer_col);
    toplevels
;;

let run_with_comments ~file source f =
  match Lexer.tokenize_with_comments ~file source with
  | Error error -> Result.Error (Error.Lex error)
  | Ok (tokens, comments) ->
    let t =
      { tokens
      ; comments = Array.of_list comments
      ; pos = 0
      ; bare_tuple_end = -1
      ; errors = None
      ; salvage = None
      ; lex_errors = [||]
      ; limit = Array.length tokens - 1
      ; col = 0
      }
    in
    (match
       let result = f t in
       (match peek t with
        | Eof -> ()
        | (Rparen | Rbracket | Rbrace | Quote_close | Greater | End | End_inline)
          as token -> fail (loc t) (sprintf "unmatched %s" (describe token))
        | _ -> expected t "end of file");
       result
     with
     | result -> Ok (result, tokens, comments)
     | exception Syntax_error (loc, message) -> Error (Syntax { loc; message }))
;;

let run ~file source f =
  Result.map (run_with_comments ~file source f) ~f:(fun (result, _, _) ->
    result)
;;

let () =
  spliced_expr := postfix;
  struct_toplevels := toplevels ~in_struct:true
;;

let program ~file source = run ~file source (toplevels ~in_struct:false)

let program_with_comments ~file source =
  run_with_comments ~file source (toplevels ~in_struct:false)
;;

let expr ~file source = run ~file source expr
let pattern ~file source = run ~file source pattern
let type_expr ~file source = run ~file source type_expr
let steps () = !steps

let program_recovering ~file source =
  let { Lexer.Recovered.tokens; comments; errors = lex_errors } =
    Lexer.tokenize_recovering ~file source
  in
  let skipped =
    List.filter lex_errors ~f:Lexer.Error.skipped
    |> List.map ~f:(fun (error : Lexer.Error.t) -> error.loc)
  in
  match lex_errors, program ~file source with
  | [], Ok program -> program, []
  | _ ->
    let errors = Queue.create () in
    let t =
      { tokens
      ; comments = Array.of_list comments
      ; pos = 0
      ; bare_tuple_end = -1
      ; errors = Some errors
      ; salvage = None
      ; lex_errors =
          List.map skipped ~f:(fun loc -> loc.start.#offset) |> Array.of_list
      ; limit = Array.length tokens - 1
      ; col = 0
      }
    in
    let program = toplevels t ~in_struct:false in
    (* A syntax error at the token after a skipped lexeme is likely due to it. *)
    let after_skipped =
      List.filter_map skipped ~f:(fun loc ->
        Array.binary_search
          tokens
          ~compare:(fun (token : Lexer.Located.t) offset ->
            Int.compare token.loc.start.#offset offset)
          `First_greater_than_or_equal_to
          loc.stop.#offset
        |> Option.globalize Int.globalize
        |> Option.map ~f:(fun i -> tokens.(i).loc.start.#offset))
      |> Int.Set.of_list
    in
    let start (error : Error.t) = (Error.loc error).start.#offset in
    let errors =
      List.map lex_errors ~f:(fun error -> Error.Lex error)
      @ List.filter (Queue.to_list errors) ~f:(fun error ->
        not (Set.mem after_skipped (start error)))
      |> List.stable_sort ~compare:(Comparable.lift Int.compare ~f:start)
      |> List.remove_consecutive_duplicates
           ~which_to_keep:`First
           ~equal:(fun a b -> start a = start b)
    in
    program, errors
;;
