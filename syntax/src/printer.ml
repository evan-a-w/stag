open! Import
open Syntax
open Doc

let path p = text (Path.to_string p)
let located_path (p : Path.t Located.t) = path p.txt
let parens t = text "(" ^^ align t ^^ text ")"
let paren_if b t = if b then parens t else t
let space_separated ts = separate (text " ") ts

(* The commonest escapes, or [\ddd] for the other control characters, and for
   all non-ASCII bytes unless [s] is valid UTF-8. *)
let string_literal s =
  let utf_8 = Stdlib.String.is_valid_utf_8 s in
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter s ~f:(fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\t' -> Buffer.add_string buf "\\t"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\b' -> Buffer.add_string buf "\\b"
    | c
      when Char.to_int c < 32
           || Char.to_int c = 127
           || (Char.to_int c > 127 && not utf_8) ->
      Buffer.add_string buf (sprintf "\\%03d" (Char.to_int c))
    | c -> Buffer.add_char buf c);
  Buffer.add_char buf '"';
  Buffer.contents buf
;;

let quoted_string s =
  let id =
    Sequence.unfold ~init:0 ~f:(fun n -> Some (String.make n 'x', n + 1))
    |> Sequence.find_exn ~f:(fun id ->
      not (String.is_substring s ~substring:("|" ^ id ^ "}")))
  in
  sprintf "{%s|%s|%s}" id s id
;;

let lit : Lit.t -> Doc.t = function
  | Int n -> text (Int.to_string n)
  | Bool b -> text (Bool.to_string b)
  | String s -> text (string_literal s)
  | Char c -> text (sprintf "'%s'" (Char.escaped c))
  | Unit -> text "()"
;;

(* [{a; b}], or broken with each item on its own line, followed by a [;]
   unless [~trailing:false]. *)
let braces ?(opener = "{") ?(trailing = true) ?tail items =
  match items, tail with
  | [], None -> text (opener ^ "}")
  | _ ->
    let tail =
      match tail with
      | None -> empty
      | Some tail -> line ^^ tail
    in
    group
      (text opener
       ^^ nest
            2
            (cut
             ^^ separate (text ";" ^^ line) items
             ^^ (if trailing then if_break ";" else empty)
             ^^ tail)
       ^^ cut
       ^^ text "}")
;;

(* [lhs = rhs], with [rhs] on the next line if it does not fit, unless it is
   [docked] (a record, which breaks within its braces). *)
let definition ?(docked = false) ?(sep = " =") lhs rhs =
  if docked
  then lhs ^^ text sep ^^ text " " ^^ rhs
  else group (lhs ^^ text sep ^^ nest 2 (line ^^ rhs))
;;

(* Types *)

let tyvar name = text ("'" ^ name)

let rec type_expr ~prec (t : Type_expr.t) =
  match t.txt with
  | Var name -> tyvar name
  | Con (p, []) -> located_path p
  | Con (p, args) ->
    located_path p
    ^^ text "["
    ^^ align (group (separate (text "," ^^ line) (List.map args ~f:type_arg)))
    ^^ text "]"
  | Arrow _ ->
    let rec arrows (t : Type_expr.t) =
      match t.txt with
      | Arrow (domain, codomain) -> domain :: arrows codomain
      | _ -> [ t ]
    in
    let tys = arrows t in
    paren_if
      (prec > 0)
      (group
         (separate
            (text " ->" ^^ line)
            (List.mapi tys ~f:(fun i ty ->
               type_expr ~prec:(if i = List.length tys - 1 then 0 else 1) ty))))
  | Tuple [] -> text "()"
  | Tuple [ t ] -> type_expr ~prec t
  | Tuple tys ->
    paren_if
      (prec > 1)
      (group
         (separate (text " *" ^^ line) (List.map tys ~f:(type_expr ~prec:2))))
  | Code { inner; context } ->
    text "<" ^^ type_expr ~prec:0 inner ^^ text ">" ^^ row ~label:Fn.id context
  | Record r -> row ~label:(fun name -> "`" ^ name) r
  | Variant r -> variant r

(* A [{...}] argument is a row, so a type argument starting with a record type
   is parenthesized. *)
and type_arg : Type_expr.arg -> Doc.t = function
  | Ty t ->
    let doc = type_expr ~prec:0 t in
    if String.is_prefix (render doc) ~prefix:"{" then parens doc else doc
  | Row r -> row ~label:(fun name -> "`" ^ name) r

and row_entries (r : Type_expr.row) =
  match r.txt with
  | Empty -> [], None
  | Var name -> [], Some (text ("..'" ^ name.txt))
  | Extend (name, t, rest) ->
    let entries, tail = row_entries rest in
    (name, t) :: entries, tail

and row ~label r =
  let entries, tail = row_entries r in
  let entries =
    List.map entries ~f:(fun ((name : Name.t), t) ->
      definition ~sep:" :" (text (label name.txt)) (type_expr ~prec:0 t))
  in
  text "{"
  ^^ align (group (separate (text ";" ^^ line) (entries @ Option.to_list tail)))
  ^^ text "}"

and variant r =
  let entries, tail = row_entries r in
  let tags =
    List.map entries ~f:(fun ((tag : Name.t), (t : Type_expr.t)) ->
      match t.txt with
      | Con ({ txt = p; _ }, []) when Path.equal p (Path.name "unit") ->
        text ("`" ^ tag.txt)
      | _ -> definition ~sep:" of" (text ("`" ^ tag.txt)) (type_expr ~prec:0 t))
  in
  let tags = separate (text " |" ^^ line) tags in
  let body =
    match entries, tail with
    | _, None -> tags
    | [], Some tail -> tail
    | _ :: _, Some tail -> tags ^^ text ";" ^^ line ^^ tail
  in
  text "[" ^^ align (group body) ^^ text "]"
;;

let type_expr' = type_expr ~prec:0

(* Patterns: [prec] 0 is a whole pattern, 1 a tuple's component, 2 a
   constructor's argument. *)

let rec pattern ~prec (p : Pattern.t) =
  match p.txt with
  | Any -> text "_"
  | Var name -> text name
  | Lit (Int n as l) when n < 0 -> paren_if (prec > 1) (lit l)
  | Lit l -> lit l
  | Tuple [] -> text "()"
  | Tuple [ p ] -> pattern ~prec p
  | Tuple ps ->
    paren_if
      (prec > 0)
      (group (separate (text "," ^^ line) (List.map ps ~f:(pattern ~prec:1))))
  | Construct (c, []) -> located_path c
  | Construct (c, [ arg ]) -> located_path c ^^ text " " ^^ pattern ~prec:2 arg
  | Construct (c, args) ->
    located_path c
    ^^ text " "
    ^^ parens
         (group
            (separate (text "," ^^ line) (List.map args ~f:(pattern ~prec:1))))
  | Poly_variant (tag, None) -> text ("`" ^ tag.txt)
  | Poly_variant (tag, Some arg) ->
    text ("`" ^ tag.txt ^ " ") ^^ pattern ~prec:2 arg
  | Record fields ->
    braces
      (List.map fields ~f:(fun (p, value) ->
         definition (located_path p) (pattern ~prec:0 value)))
  | Poly_record { fields; rest } ->
    let fields =
      List.map fields ~f:(fun (label, value) ->
        definition (text ("`" ^ label.txt)) (pattern ~prec:0 value))
    in
    (match rest with
     | Closed -> braces fields
     | Open -> braces ~trailing:false (fields @ [ text ".." ])
     | Bind name -> braces ~trailing:false (fields @ [ text (".." ^ name.txt) ]))
;;

let pattern' = pattern ~prec:0

(* Expressions *)

(** How tightly an expression binds, loosest first: the expressions that may be
    written at each, without parentheses. *)
module Level = struct
  type t =
    | Seq
    | Tuple
    | Tuple_item (** And [let] and [match]. *)
    | Construct
    | App
    | Splice
    | Postfix
    | Atom
  [@@deriving compare]
end

(** What follows an expression, which a [let] or [match] ending it would take
    as its own. *)
module After = struct
  type t =
    | Nothing
    | Comma
    | Semi
    | Bar
end

module Ctx = struct
  type t =
    { level : Level.t
    ; seq : bool (** Whether the bodies of [let]s and [match]es are [Seq]s. *)
    ; after : After.t
    }

  let top = { level = Seq; seq = true; after = Nothing }
  let at level = { level; seq = true; after = Nothing }

  (* The level of the bodies of [let]s and [match]es. *)
  let body t = if t.seq then Level.Seq else Tuple
end

let rec unwrap (e : Expr.t) =
  match e.txt with
  | Tuple [ e ] -> unwrap e
  | _ -> e
;;

let seq (e : Expr.t) =
  match e.txt with
  | Let ({ txt = "_"; _ }, [], None, first, second) -> Some (first, second)
  | _ -> None
;;

let level (e : Expr.t) : Level.t =
  let e = unwrap e in
  match e.txt with
  | Let _ when Option.is_some (seq e) -> Seq
  | Let _ | Match _ -> Tuple_item
  | Tuple (_ :: _ :: _) -> Tuple
  | Lit (Int n) when n < 0 -> Tuple_item
  | Construct (_, _ :: _) | Poly_variant (_, Some _) -> Construct
  | App _ -> App
  | Splice _ -> Splice
  | Field _ | Poly_field _ | Poly_restrict _ -> Postfix
  | Lit _ | Var _ | Tuple _
  | Construct (_, [])
  | Poly_variant (_, None)
  | Quote _
  | Record _
  | Poly_record _
  | Poly_extend _
  | Poly_update _
  | Expect _ -> Atom
;;

let bare_at level' e = Level.compare (level e) level' >= 0

(* For [e] written without parentheses: whether it ends with a [let] or
   [match], which takes a following [,] (or [;]) as its own... *)
let rec is_open e =
  match (unwrap e).txt with
  | Let _ | Match _ -> true
  | Tuple (_ :: _ :: _ as es) ->
    let last = List.last_exn es in
    bare_at Tuple_item last && is_open last
  | _ -> false
;;

(* ...and whether it ends with a [match], which takes a following [|]. *)
let rec ends_with_match ~seq:in_seq e =
  let e = unwrap e in
  match e.txt, seq e with
  | Match _, _ -> true
  | Let _, Some (_, second) -> ends_with_match ~seq:true second
  | Let (_, _, _, _, body), None ->
    bare_at (Ctx.body { Ctx.top with seq = in_seq }) body
    && ends_with_match ~seq:in_seq body
  | Tuple (_ :: _ :: _ as es), _ ->
    let last = List.last_exn es in
    bare_at Tuple_item last && ends_with_match ~seq:in_seq last
  | _ -> false
;;

let takes_after (ctx : Ctx.t) e =
  match ctx.after with
  | Nothing -> false
  | Comma | Semi -> is_open e
  | Bar -> ends_with_match ~seq:ctx.seq e
;;

(* Postfix operators would extend these to paths, or just look wrong. *)
let needs_parens_before_postfix e =
  match (unwrap e).txt with
  | Construct (_, []) | Poly_variant (_, None) | Quote _ -> true
  | _ -> false
;;

let is_record e =
  match (unwrap e).txt with
  | Record _ | Poly_record _ | Poly_extend _ | Poly_update _ -> true
  | _ -> false
;;

let binding ((name : Name.t), annot) =
  match annot with
  | None -> text name.txt
  | Some t -> parens (text (name.txt ^ " : ") ^^ type_expr' t)
;;

let let_lhs keyword (name : Name.t) params annot =
  text (keyword ^ " ")
  ^^ space_separated (text name.txt :: List.map params ~f:binding)
  ^^
  match annot with
  | None -> empty
  | Some t -> text " : " ^^ type_expr' t
;;

let rec expr ?(parenthesize = false) (ctx : Ctx.t) e =
  let e = unwrap e in
  if parenthesize || (not (bare_at ctx.level e)) || takes_after ctx e
  then parens (bare Ctx.top e)
  else bare ctx e

and bare (ctx : Ctx.t) (e : Expr.t) =
  match e.txt, seq e with
  | Let _, Some (first, second) ->
    expr { level = Tuple; seq = true; after = Semi } first
    ^^ text ";"
    ^^ hardline
    ^^ expr { ctx with level = Seq } second
  | Let (name, params, annot, rhs, body), None ->
    let lhs = let_lhs "let" name params annot in
    let rhs_doc = expr Ctx.top rhs in
    (if is_record rhs
     then lhs ^^ text " = " ^^ rhs_doc ^^ text " in"
     else
       group (lhs ^^ text " =" ^^ nest 2 (line ^^ rhs_doc) ^^ line ^^ text "in"))
    ^^ hardline
    ^^ expr { ctx with level = Ctx.body ctx } body
  | Match (scrutinee, arms), _ ->
    let last = List.length arms - 1 in
    let arm i (p, body) =
      let after : After.t = if i = last then ctx.after else Bar in
      group
        (align (pattern' p)
         ^^ text " ->"
         ^^ nest 2 (line ^^ expr { ctx with level = Ctx.body ctx; after } body)
        )
    in
    let head =
      group
        (text "match"
         ^^ nest 2 (line ^^ expr Ctx.top scrutinee)
         ^^ line
         ^^ text "with")
    in
    (match arms with
     | [ only ] -> group (head ^^ line ^^ if_break "| " ^^ arm 0 only)
     | arms ->
       head
       ^^ concat
            (List.mapi arms ~f:(fun i a -> hardline ^^ text "| " ^^ arm i a)))
  | Tuple (_ :: _ :: _ as es), _ -> tuple ctx es
  | Tuple _, _ -> text "()"
  | Lit l, _ -> lit l
  | Var (p, None), _ -> path p
  | Var (p, Some t), _ ->
    parens (text (Path.to_string p ^ " : ") ^^ type_expr' t)
  | App _, _ ->
    let rec spine (e : Expr.t) args =
      match e.txt with
      | App (f, arg) -> spine f (arg :: args)
      | _ -> e, args
    in
    let f, args = spine e [] in
    let f =
      match (unwrap f).txt with
      | Construct (_, []) | Poly_variant (_, None) ->
        expr ~parenthesize:true (Ctx.at App) f
      | _ -> expr (Ctx.at App) f
    in
    group
      (f
       ^^ nest
            2
            (concat
               (List.map args ~f:(fun arg -> line ^^ expr (Ctx.at Splice) arg)))
      )
  | Construct (c, []), _ -> located_path c
  | Construct (c, [ arg ]), _ ->
    group (located_path c ^^ nest 2 (line ^^ expr (Ctx.at Construct) arg))
  | Construct (c, args), _ ->
    group (located_path c ^^ nest 2 (line ^^ parens (tuple Ctx.top args)))
  | Poly_variant (tag, None), _ -> text ("`" ^ tag.txt)
  | Poly_variant (tag, Some arg), _ ->
    group (text ("`" ^ tag.txt) ^^ nest 2 (line ^^ expr (Ctx.at Construct) arg))
  | Splice { expr = inner; subs }, _ ->
    let inner =
      match (unwrap inner).txt, subs with
      | Splice { subs = None; _ }, Some _ ->
        expr ~parenthesize:true Ctx.top inner
      | Splice _, _ -> bare Ctx.top (unwrap inner)
      | _ -> expr (Ctx.at Postfix) inner
    in
    let subs =
      match subs with
      | None -> empty
      | Some subs ->
        braces
          ~opener:".{"
          (List.map subs ~f:(fun ((name : Name.t), value) ->
             match value with
             | None -> text name.txt
             | Some value -> field (text name.txt) value))
    in
    text ".~" ^^ inner ^^ subs
  | Quote e, _ ->
    group (text ".<" ^^ nest 2 (line ^^ expr Ctx.top e) ^^ line ^^ text ">.")
  | Field (record, p), _ -> postfix record ^^ text "." ^^ located_path p
  | Poly_field (record, label), _ -> postfix record ^^ text (".`" ^ label.txt)
  | Poly_restrict (record, label), _ ->
    postfix record ^^ text (" \\ `" ^ label.txt)
  | Record fields, _ ->
    braces (List.map fields ~f:(fun (p, value) -> field (located_path p) value))
  | Poly_record fields, _ -> braces (poly_fields fields)
  | Poly_extend (fields, record), _ ->
    braces
      ~trailing:false
      ~tail:(text "| " ^^ entry record)
      (poly_fields ~last_after:After.Bar fields)
  | Poly_update (record, fields), _ ->
    let record =
      let parenthesize =
        match (unwrap record).txt with
        | Poly_variant _ -> true
        | _ -> false
      in
      expr ~parenthesize (Ctx.at Splice) record
    in
    group
      (text "{"
       ^^ record
       ^^ text " with"
       ^^ nest
            2
            (line
             ^^ separate (text ";" ^^ line) (poly_fields fields)
             ^^ if_break ";")
       ^^ cut
       ^^ text "}")
  | Expect { txt = ""; _ }, _ -> text "[%expect]"
  | Expect s, _ -> text "[%expect " ^^ text (quoted_string s.txt) ^^ text "]"

and postfix record =
  expr
    ~parenthesize:(needs_parens_before_postfix record)
    (Ctx.at Postfix)
    record

and tuple (ctx : Ctx.t) es =
  let last = List.length es - 1 in
  group
    (separate
       (text "," ^^ line)
       (List.mapi es ~f:(fun i e ->
          let after : After.t = if i = last then ctx.after else Comma in
          expr { ctx with level = Tuple_item; after } e)))

(* A value in [{...}]. *)
and entry ?(after = After.Nothing) e =
  expr { level = Tuple; seq = false; after } e

and field ?after lhs value =
  definition ~docked:(is_record value) lhs (entry ?after value)

and poly_fields ?(last_after = After.Nothing) fields =
  let last = List.length fields - 1 in
  List.mapi fields ~f:(fun i ((label : Name.t), value) ->
    let after = if i = last then last_after else Nothing in
    field ~after (text ("`" ^ label.txt)) value)
;;

let expr' = expr Ctx.top

(* Declarations *)

let type_param (param : Type_decl.Param.t Located.t) =
  match param.txt with
  | Ty name -> tyvar name
  | Row name -> text ("{..'" ^ name ^ "}")
;;

let type_decl ({ name; params; kind } : Type_decl.t) =
  let lhs =
    text ("type " ^ name.txt)
    ^^
    match params with
    | [] -> empty
    | params ->
      text "["
      ^^ separate (text ", ") (List.map params ~f:type_param)
      ^^ text "]"
  in
  match kind with
  | Abstract -> lhs
  | Alias t -> definition lhs (type_expr' t)
  | Variant [] -> lhs ^^ text " = |"
  | Variant constructors ->
    let constructor ((name : Name.t), args) =
      match args with
      | [] -> text name.txt
      | args ->
        text (name.txt ^ " of ")
        ^^ align
             (group
                (separate
                   (text " *" ^^ line)
                   (List.map args ~f:(type_expr ~prec:2))))
    in
    group
      (lhs
       ^^ text " ="
       ^^ nest
            2
            (concat
               (List.mapi constructors ~f:(fun i c ->
                  line
                  ^^ (if i = 0 then if_break "| " else text "| ")
                  ^^ constructor c))))
  | Record fields ->
    lhs
    ^^ text " = "
    ^^ braces
         (List.map fields ~f:(fun ((name : Name.t), t) ->
            definition ~sep:" :" (text name.txt) (type_expr' t)))
;;

module Kind = struct
  type t =
    | Let
    | Type
    | Other
  [@@deriving equal]

  let of_toplevel (t : Toplevel.t) =
    match t.txt with
    | Let _ -> Let
    | Type _ -> Type
    | Module _ | Expect_test _ -> Other
  ;;
end

let rec toplevel ~width ~indent (t : Toplevel.t) =
  match t.txt with
  | Let (name, params, annot, rhs) ->
    definition
      ~docked:(is_record rhs)
      (let_lhs "let" name params annot)
      (expr' rhs)
  | Type decl -> type_decl decl
  | Module (name, []) -> text ("module " ^ name.txt ^ " = struct end")
  | Module (name, toplevels') ->
    text ("module " ^ name.txt ^ " = struct")
    ^^ nest 2 (hardline ^^ toplevels ~width ~indent:(indent + 2) toplevels')
    ^^ hardline
    ^^ text "end"
  | Expect_test { name; body } ->
    definition
      (text ("let%expect_test " ^ string_literal name.txt))
      (expr' body)

and toplevels ~width ~indent ts =
  let one_line doc =
    not (String.mem (render ~width:(width - indent) doc) '\n')
  in
  let docs =
    List.map ts ~f:(fun t ->
      let doc = toplevel ~width ~indent t in
      Kind.of_toplevel t, one_line doc, doc)
  in
  match docs with
  | [] -> empty
  | (_, _, first) :: _ ->
    List.fold
      (List.zip_exn (List.drop_last_exn docs) (List.tl_exn docs))
      ~init:first
      ~f:(fun acc ((kind, one_line, _), (kind', one_line', doc)) ->
        let compact =
          one_line
          && one_line'
          && Kind.equal kind kind'
          && not (Kind.equal kind Other)
        in
        acc ^^ hardline ^^ (if compact then empty else hardline) ^^ doc)
;;

let type_expr ?width t = render ?width (type_expr' t)
let pattern ?width p = render ?width (pattern' p)
let expr ?width e = render ?width (expr' e)
let toplevel ?(width = 80) t = render ~width (toplevel ~width ~indent:0 t)

let program ?(width = 80) ts =
  match ts with
  | [] -> ""
  | ts -> render ~width (toplevels ~width ~indent:0 ts ^^ hardline)
;;
