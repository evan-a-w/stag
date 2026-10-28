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
  ; mutable pos : int
  ; mutable bare_tuple_end : int
    (** The [pos] after the last [)] whose contents were a tuple without
          parentheses of its own, as in [(a, b)] but not [((a, b))]. *)
  }

let peek_at t n =
  t.tokens.(Int.min (t.pos + n) (Array.length t.tokens - 1)).token
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

let decl_path (path : Path.t Located.t) =
  { path with txt = Decl_path.Path path.txt }
;;

(** Located from [start] to the last token consumed. *)
let since t start txt = located ~loc:(Loc.merge start (prev_loc t)) txt

let describe : Token.t -> string = function
  | Eof -> "end of file"
  | Label label -> sprintf "the label `%s" label
  | token -> sprintf "`%s`" (Token.to_string token)
;;

let fail loc message = raise (Syntax_error (loc, message))

let expected t what =
  match peek t with
  | At | At_at -> fail (loc t) "`@` is reserved (for modes)"
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
         open_loc.Loc.start.line
         open_loc.start.col
         (describe (peek t)))
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
         open_loc.Loc.start.line
         open_loc.start.col
         (describe (peek t)))
;;

(** Parses [opener items close], with [item]s separated by [sep] and an
    optional trailing [sep]. *)
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

(** A path [M.N.x] or [M.N.C], and whether its last name is capitalized. *)
let path t ~what =
  let start = loc t in
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
  since t start (Path.of_list_exn names), capitalized
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

(* Types *)

let rec type_expr t : Type_expr.t =
  let domain = type_tuple t in
  if is t Arrow
  then (
    advance t;
    let codomain = type_expr t in
    located
      ~loc:(Loc.merge domain.loc codomain.loc)
      (Type_expr.Arrow (domain, codomain)))
  else domain

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
  | Lident _ | Uident _ ->
    let path = lowercase_path t ~what:"a type name" in
    located ~loc:path.loc (Type_expr.Con (decl_path path, []))
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

(** [{l : t; ...; ..'r}], located with its braces. *)
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
      let ty = type_expr t in
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
               (located ~loc:tag.loc (Decl_path.Path (Path.name "unit")), []))
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
  | Lident _
  | Uident _
  | Label _
  | Int _
  | Char _
  | String _
  | True
  | False
  | Lparen
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

(** Closes the parentheses around [contents], recording if [contents] is a
    tuple without parentheses of its own: one starting where its first item
    does, as parentheses are part of a location. *)
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
  | Uident _ ->
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
    since t start (Pattern.Construct (decl_path path, args))
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
  | _ -> expected t "a pattern"

and record_pattern t : Pattern.t =
  let start = loc t in
  match peek_at t 1 with
  | Lident _ | Uident _ ->
    let fields =
      delimited t ~opener:Lbrace ~close:Rbrace ~sep:Semi ~item:(fun t ->
        let path = lowercase_path t ~what:"a field name" in
        expect t Equal;
        decl_path path, pattern t)
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
  | Char _
  | String _
  | Lident _
  | Uident _
  | Label _
  | True
  | False
  | Lparen
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
  go 1
;;

let pattern_parameter =
  "patterns are not allowed as parameters, only `x`, `_` or `(x : t)`; use \
   `match`"
;;

(** [x], [_] or [(x : t)]. *)
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
     | (Lident _ | Underscore), Colon -> ()
     | _ -> fail open_loc pattern_parameter);
    advance t;
    let name = binder t in
    expect t Colon;
    let ty = type_expr t in
    close t Rparen ~opener:Lparen ~open_loc;
    name, Some ty
  | _ -> binder t, None
;;

let params t =
  let rec loop acc =
    match peek t with
    | Lident _ | Underscore | Lparen -> loop (param t :: acc)
    | _ -> List.rev acc
  in
  loop []
;;

(* With [~seq:false], as in a [{...}] entry, [e1; e2] is not parsed (also in
   the bodies of [let] and [match]), leaving the [;] as the separator. *)
let rec expr ?(seq = true) t : Expr.t =
  match peek t with
  | Let -> let_ t ~seq
  | Match -> match_ t ~seq
  | Fun -> fun_ t ~seq
  | _ ->
    let first = tuple t ~seq in
    if seq && is t Semi
    then (
      let semi = loc t in
      advance t;
      let second = expr t in
      expr_at
        ~loc:(Loc.merge first.loc second.loc)
        (Let (located ~loc:semi "_", [], None, first, second)))
    else first

(** [name params : t = e], after [let]. *)
and binding t =
  if is t Rec then fail (loc t) "lets are always recursive; remove `rec`";
  let name =
    match peek t with
    | Underscore ->
      let loc = loc t in
      advance t;
      located ~loc "_"
    | _ -> lident t ~what:"a name to bind"
  in
  let params = params t in
  let annot =
    if is t Colon
    then (
      advance t;
      Some (type_expr t))
    else None
  in
  expect t Equal;
  name, params, annot, expr t

and let_ t ~seq : Expr.t =
  let start = loc t in
  expect t Let;
  let name, params, annot, equal = binding t in
  expect_for t In ~opener:Let ~open_loc:start;
  let body = expr t ~seq in
  expr_at
    ~loc:(Loc.merge start body.loc)
    (Expr.Let (name, params, annot, equal, body))

and fun_ t ~seq : Expr.t =
  let start = loc t in
  expect t Fun;
  let params = params t in
  if List.is_empty params
  then (
    match peek t with
    | Uident _ | Label _ | Int _ | Char _ | String _ | True | False | Lbrace ->
      fail (loc t) pattern_parameter
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
  | _ -> construct t

(** A value in [{...}]. *)
and entry_value t = tuple t ~seq:false

and construct t : Expr.t =
  let start = loc t in
  let saved = t.pos in
  match peek t with
  | Uident _ ->
    (match path t ~what:"" with
     | path, true ->
       let args =
         if starts_atom (peek t)
         then (
           let arg = construct t in
           construct_args t arg ~tuple_items:(function
             | Expr.Tuple items -> Some items
             | _ -> None))
         else []
       in
       expr_since t start (Construct (decl_path path, args))
     | _, false ->
       t.pos <- saved;
       app t)
  | Label tag ->
    let tag = located ~loc:start tag in
    advance t;
    let arg = if starts_atom (peek t) then Some (construct t) else None in
    expr_since t start (Poly_variant (tag, arg))
  | Type ->
    advance t;
    expr_since t start (Type (type_app t))
  | _ -> app t

and app t : Expr.t =
  let rec loop (f : Expr.t) =
    if starts_atom (peek t)
    then (
      let arg = splice t in
      loop (expr_at ~loc:(Loc.merge f.loc arg.loc) (Expr.App (f, arg))))
    else f
  in
  loop (splice t)

and splice t : Expr.t =
  match peek t with
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
    | Dot ->
      advance t;
      (match peek t with
       | Label _ ->
         let label = label t ~what:"" in
         loop (expr_since t e.loc (Poly_field (e, label)))
       | _ ->
         let field = lowercase_path t ~what:"a field name" in
         loop (expr_since t e.loc (Field (e, decl_path field))))
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
  | Lident _ | Uident _ ->
    (match path t ~what:"" with
     | path, true -> expr_since t start (Construct (decl_path path, []))
     | path, false -> expr_since t start (Var (path.txt, None)))
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
      | Colon ->
        (match e.txt with
         | Var (path, None) ->
           advance t;
           let ty = type_expr t in
           close t Rparen ~opener:Lparen ~open_loc:start;
           expr_since t start (Var (path, Some ty))
         | _ ->
           fail
             e.loc
             "type annotations are only supported on variables, as in `(x : t)`")
      | _ ->
        close_parens t ~open_loc:start e ~tuple_items:(function
          | Expr.Tuple items -> Some items
          | _ -> None);
        expr_since t start e.txt)
  | Lbrace -> record t
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
  decl_path path, entry_value t

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

let constructor_decl t =
  let name = uident t ~what:"a constructor name" in
  if is t Of
  then (
    advance t;
    let rec args acc =
      let acc = type_app t :: acc in
      match peek t with
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
    name, args [])
  else name, []
;;

let type_kind t : Type_decl.Kind.t =
  match peek t, peek_at t 1 with
  | Uident _, Dot -> Alias (type_expr t)
  | (Bar | Uident _), _ ->
    let leading_bar = is t Bar in
    if leading_bar then advance t;
    (match peek t with
     | Uident _ ->
       let rec loop acc =
         let acc = constructor_decl t :: acc in
         if is t Bar
         then (
           advance t;
           loop acc)
         else List.rev acc
       in
       Variant (loop [])
     | _ -> Variant [])
  | Lbrace, Lident _ ->
    Record
      (delimited t ~opener:Lbrace ~close:Rbrace ~sep:Semi ~item:(fun t ->
         let name = lident t ~what:"a field name" in
         expect t Colon;
         name, type_expr t))
  | _ -> Alias (type_expr t)
;;

let type_decl t : Type_decl.t =
  let name = lident t ~what:"a type name" in
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
  let kind =
    if is t Equal
    then (
      advance t;
      type_kind t)
    else Abstract
  in
  { name; params; kind }
;;

let rec toplevel t : Toplevel.t =
  let start = loc t in
  match peek t with
  | Let ->
    advance t;
    let name, params, annot, equal = binding t in
    if is t In
    then
      fail
        (loc t)
        "unexpected `in`: a toplevel `let` is a declaration, without `in`";
    since t start (Toplevel.Let (name, params, annot, equal))
  | Type ->
    advance t;
    since t start (Toplevel.Type (type_decl t))
  | Exception ->
    advance t;
    let name, args = constructor_decl t in
    since t start (Toplevel.Exception (name, args))
  | Module ->
    advance t;
    let name = uident t ~what:"a module name" in
    expect t Equal;
    (match peek t with
     | Splice ->
       since t start (Toplevel.Module_splice (name, declaration_splice t))
     | _ ->
       let struct_loc = loc t in
       expect t Struct;
       let toplevels = toplevels t ~stop:Token.End in
       close t End ~opener:Struct ~open_loc:struct_loc;
       since t start (Toplevel.Module (name, toplevels)))
  | Include ->
    advance t;
    (match peek t with
     | Splice -> since t start (Toplevel.Include (declaration_splice t))
     | _ -> expected t "a declaration splice `.~e` after `include`")
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
    expect t Equal;
    let body = expr t in
    since t start (Toplevel.Expect_test { name; body })
  | _ ->
    expected
      t
      "a declaration (`let`, `type`, `exception`, `module` or `include`)"

(* [.~e], the declarations [e] generates. *)
and declaration_splice t =
  advance t;
  postfix t

and toplevels t ~stop =
  let rec loop acc =
    if is t stop || is t Eof then List.rev acc else loop (toplevel t :: acc)
  in
  loop []
;;

let run_with_comments ~file source f =
  match Lexer.tokenize_with_comments ~file source with
  | Error error -> Error (Error.Lex error)
  | Ok (tokens, comments) ->
    let t = { tokens; pos = 0; bare_tuple_end = -1 } in
    (match
       let result = f t in
       (match peek t with
        | Eof -> ()
        | (Rparen | Rbracket | Rbrace | Quote_close | Greater | End) as token ->
          fail (loc t) (sprintf "unmatched %s" (describe token))
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

let program ~file source = run ~file source (toplevels ~stop:Token.Eof)

let program_with_comments ~file source =
  run_with_comments ~file source (toplevels ~stop:Token.Eof)
;;

let expr ~file source = run ~file source expr
let pattern ~file source = run ~file source pattern
let type_expr ~file source = run ~file source type_expr
