open! Import
open Syntax
open Doc

let path p = text (Path.to_string p)

let decl_path (p : Decl_path.t Located.t) =
  match p.txt with
  | Path p -> path p
  | Resolved name -> text (Prelude_path.to_string_hum name)
;;

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
   unless [~trailing:false], then the [tail]. *)
let braces ?(opener = "{") ?(trailing = true) ?tail items =
  match items, tail with
  | [], None -> text (opener ^ "}")
  | _ ->
    let tail =
      match tail with
      | None -> empty
      | Some tail -> tail
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

(* [lhs = rhs ending], with [rhs] on the next line if it does not fit, or a
   [record] docked, breaking within its braces if it does not fit on the next
   line either. *)
let definition ?(record = false) ?(sep = " =") ?ending lhs rhs =
  let ending space =
    match ending with
    | None -> empty
    | Some ending -> space ^^ text ending
  in
  let next_line = nest 2 (line ^^ rhs) ^^ ending line in
  group
    (lhs
     ^^ text sep
     ^^
     if record
     then alt next_line (text " " ^^ rhs ^^ ending (text " "))
     else next_line)
;;

let tyvar name = text ("'" ^ name)

let rec type_expr ~prec (t : Type_expr.t) =
  match t.txt with
  | Var name -> tyvar name
  | Con (p, []) -> decl_path p
  | Con (p, args) ->
    decl_path p
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
    if starts_with_record t then parens doc else doc
  | Row r -> row ~label:(fun name -> "`" ^ name) r

(* Whether [t], printed at precedence 0, starts with a record type. *)
and starts_with_record (t : Type_expr.t) =
  match t.txt with
  | Record _ -> true
  | Arrow (first, _) | Tuple (first :: _) -> starts_with_record first
  | Var _ | Con _ | Tuple [] | Code _ | Variant _ -> false

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
      | Con ({ txt = Path p; _ }, []) when Path.equal p (Path.name "unit") ->
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

let type_expr_doc = type_expr ~prec:0

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
  | Construct (c, []) -> decl_path c
  | Construct (c, args) ->
    let rec unwrap (p : Pattern.t) =
      match p.txt with
      | Tuple [ p ] -> unwrap p
      | _ -> p
    in
    let args =
      match List.map args ~f:unwrap with
      | [ ({ txt = Tuple (_ :: _ :: _); _ } as arg) ] ->
        parens (pattern ~prec:2 arg)
      | [ arg ] -> pattern ~prec:2 arg
      | args ->
        parens
          (group
             (separate (text "," ^^ line) (List.map args ~f:(pattern ~prec:1))))
    in
    paren_if (prec > 1) (decl_path c ^^ text " " ^^ args)
  | Poly_variant (tag, None) -> text ("`" ^ tag.txt)
  | Poly_variant (tag, Some arg) ->
    paren_if (prec > 1) (text ("`" ^ tag.txt ^ " ") ^^ pattern ~prec:2 arg)
  | Record fields ->
    braces
      (List.map fields ~f:(fun (p, value) ->
         definition (decl_path p) (pattern ~prec:1 value)))
  | Poly_record { fields; rest } ->
    let fields =
      List.map fields ~f:(fun (label, value) ->
        definition (text ("`" ^ label.txt)) (pattern ~prec:1 value))
    in
    (match rest with
     | Closed -> braces fields
     | Open -> braces ~trailing:false (fields @ [ text ".." ])
     | Bind name -> braces ~trailing:false (fields @ [ text (".." ^ name.txt) ]))
  | Exception p -> paren_if (prec > 1) (text "exception " ^^ pattern ~prec:1 p)
;;

let pattern_doc = pattern ~prec:0

(** How tightly an expression binds, loosest first: the expressions that may be
    written at each, without parentheses. *)
module Level = struct
  type t =
    | Seq
    | Item
    (** A tuple's item, or a value in [{...}]: also [let], [match], [fun] and
        negative literals. *)
    | Construct
    | App
    | Splice
    | Postfix
    | Atom
  [@@deriving compare]
end

(** What follows an expression, which a [let], [match] or [fun] ending it
    would take as its own. *)
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
    ; seq : bool
      (** Whether the bodies of [let]s, [match]es and [fun]s are [Seq]s. *)
    ; after : After.t
    ; flat : bool
      (** Whether [let]s and sequences may stay on one line, as in parentheses,
        rather than always breaking. *)
    }

  let top = { level = Seq; seq = true; after = Nothing; flat = false }
  let at level = { top with level }
  let item ?(seq = true) after = { level = Item; seq; after; flat = true }

  (* The level of the bodies of [let]s, [match]es and [fun]s. *)
  let body t = if t.seq then Level.Seq else Item
end

let rec unwrap (e : Expr.t) =
  match e.txt with
  | Tuple [ e ] | Fun ([], e) -> unwrap e
  | _ -> e
;;

let seq (e : Expr.t) =
  match e.txt with
  | Let ({ txt = "_"; _ }, [], None, first, second) -> Some (first, second)
  | _ -> None
;;

let is_construct (e : Expr.t) =
  match e.txt with
  | Construct (_, _ :: _) | Poly_variant (_, Some _) -> true
  | _ -> false
;;

let level (e : Expr.t) : Level.t =
  let e = unwrap e in
  match e.txt with
  | Let _ when Option.is_some (seq e) -> Seq
  | Let _ | Match _ | Fun _ -> Item
  | Lit (Int n) when n < 0 -> Item
  | _ when is_construct e -> Construct
  | App _ -> App
  | Splice _ -> Splice
  | Type _ -> Construct
  | Field _ | Poly_field _ | Poly_restrict _ -> Postfix
  | Lit _
  | Var _
  | Tuple _
  | Construct _
  | Poly_variant _
  | Quote _
  | Record _
  | Poly_record _
  | Poly_extend _
  | Poly_update _
  | Record_update _
  | Expect _
  | Error -> Atom
;;

let bare_at at e = Level.compare (level e) at >= 0

let extends_right e =
  match (unwrap e).txt with
  | Let _ | Match _ | Fun _ -> true
  | _ -> false
;;

(* For [e] written without parentheses: whether it ends with a [match], which
   takes a following [|]. *)
let rec ends_with_match ~seq:in_seq e =
  let e = unwrap e in
  match e.txt, seq e with
  | Match _, _ -> true
  | Let _, Some (_, second) -> ends_with_match ~seq:true second
  | (Let (_, _, _, _, body) | Fun (_, body)), None ->
    bare_at (Ctx.body { Ctx.top with seq = in_seq }) body
    && ends_with_match ~seq:in_seq body
  | _ -> false
;;

let takes_after (ctx : Ctx.t) e =
  match ctx.after with
  | Nothing -> false
  | Comma | Semi -> extends_right e
  | Bar -> ends_with_match ~seq:ctx.seq e
;;

(* Postfix operators would extend these to paths, or just look wrong. *)
let needs_parens_before_postfix e =
  match (unwrap e).txt with
  | Construct (_, []) | Poly_variant (_, None) | Quote _ | Poly_restrict _ ->
    true
  | _ -> false
;;

let is_record e =
  match (unwrap e).txt with
  | Record _ | Record_update _ | Poly_record _ | Poly_extend _ | Poly_update _
    -> true
  | _ -> false
;;

let binding ((name : Name.t), annot) =
  match annot with
  | None -> text name.txt
  | Some t -> parens (text (name.txt ^ " : ") ^^ type_expr_doc t)
;;

let let_lhs keyword (name : Name.t) params annot =
  let annot =
    match annot with
    | None -> empty
    | Some t -> text " : " ^^ type_expr_doc t
  in
  group
    (text (keyword ^ " " ^ name.txt)
     ^^ nest
          4
          (concat (List.map params ~f:(fun p -> line ^^ binding p)) ^^ annot))
;;

let rec expr ?(parenthesize = false) (ctx : Ctx.t) e =
  let e = unwrap e in
  if parenthesize || (not (bare_at ctx.level e)) || takes_after ctx e
  then parens (flat e)
  else bare ctx e

and flat e = group (bare { Ctx.top with flat = true } e)

and bare (ctx : Ctx.t) (e : Expr.t) =
  match e.txt, seq e with
  | Let _, Some (first, second) ->
    expr (Ctx.item Semi) first
    ^^ text ";"
    ^^ (if ctx.flat then line else hardline)
    ^^ expr { ctx with level = Seq } second
  | Let (name, params, annot, rhs, body), None ->
    definition
      ~record:(is_record rhs)
      ~ending:"in"
      (let_lhs "let" name params annot)
      (expr Ctx.top rhs)
    ^^ (if ctx.flat then line else hardline)
    ^^ expr { ctx with level = Ctx.body ctx } body
  | Fun (params, body), _ ->
    group
      (group
         (text "fun"
          ^^ nest 4 (concat (List.map params ~f:(fun p -> line ^^ binding p)))
          ^^ text " ->")
       ^^ nest 2 (line ^^ expr { ctx with level = Ctx.body ctx } body))
  | Match (scrutinee, arms), _ ->
    let last = List.length arms - 1 in
    let arm i (p, body) =
      let after : After.t = if i = last then ctx.after else Bar in
      let doc =
        align (pattern_doc p)
        ^^ text " ->"
        ^^ nest 2 (line ^^ expr { ctx with level = Ctx.body ctx; after } body)
      in
      (* A flat [let] or sequence is only kept on the line of a flat match. *)
      match ctx.flat, (unwrap body).txt with
      | true, Let _ -> doc
      | _ -> group doc
    in
    let head =
      group
        (text "match"
         ^^ nest
              2
              (line
               ^^ expr ~parenthesize:(extends_right scrutinee) Ctx.top scrutinee
              )
         ^^ line
         ^^ text "with")
    in
    (match arms with
     | [ only ] -> group (head ^^ line ^^ if_break "| " ^^ arm 0 only)
     | arms ->
       head
       ^^ concat
            (List.mapi arms ~f:(fun i a -> hardline ^^ text "| " ^^ arm i a)))
  | Tuple (_ :: _ :: _ as es), _ -> parens (tuple_items es)
  | Tuple _, _ -> text "()"
  | Lit l, _ -> lit l
  | Var (p, None), _ -> path p
  | Var (p, Some t), _ ->
    parens (text (Path.to_string p ^ " : ") ^^ type_expr_doc t)
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
               (List.map args ~f:(fun arg ->
                  let parenthesize =
                    match (unwrap arg).txt with
                    | Poly_restrict _ -> true
                    | _ -> false
                  in
                  line ^^ expr ~parenthesize (Ctx.at Splice) arg))))
  | Construct (c, []), _ -> decl_path c
  | Poly_variant (tag, None), _ -> text ("`" ^ tag.txt)
  | (Construct _ | Poly_variant _), _ -> construct e
  | Splice { expr = inner; subs }, _ ->
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
    spliced inner ^^ subs
  | Type t, _ -> text "type " ^^ type_expr ~prec:2 t
  | Quote e, _ ->
    group (text ".<" ^^ nest 2 (line ^^ flat (unwrap e)) ^^ line ^^ text ">.")
  | Field (record, p), _ -> postfix record ^^ text "." ^^ decl_path p
  | Poly_field (record, label), _ -> postfix record ^^ text (".`" ^ label.txt)
  | Poly_restrict (record, label), _ ->
    postfix record ^^ text (" \\ `" ^ label.txt)
  | Record fields, _ ->
    braces (List.map fields ~f:(fun (p, value) -> field (decl_path p) value))
  | Poly_record fields, _ -> braces (poly_fields fields)
  | Poly_extend (fields, record), _ ->
    braces
      ~trailing:false
      ~tail:(group (text " |" ^^ nest 2 (line ^^ entry record)))
      (poly_fields ~last_after:After.Bar fields)
  | Poly_update (record, fields), _ -> update record (poly_fields fields)
  | Record_update (record, fields), _ ->
    update
      record
      (List.map fields ~f:(fun (p, value) -> field (decl_path p) value))
  | Expect { txt = ""; _ }, _ -> text "[%expect]"
  | Expect s, _ -> text "[%expect " ^^ text (quoted_string s.txt) ^^ text "]"
  | Error, _ -> text "<error>"

and update record fields =
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
     ^^ nest 2 (line ^^ separate (text ";" ^^ line) fields ^^ if_break ";")
     ^^ cut
     ^^ text "}")

(* [C1 (C2 (... Cn args))], with the heads on one line, as each would otherwise
   be indented further than the last, quadratically in deep chains. *)
and construct e =
  let head_and_args (e : Expr.t) =
    match e.txt with
    | Construct (c, args) -> decl_path c, args
    | Poly_variant (tag, arg) -> text ("`" ^ tag.txt), Option.to_list arg
    | _ -> assert false
  in
  let rec chain e heads =
    let head, args = head_and_args e in
    match args with
    | [ arg ] when is_construct (unwrap arg) ->
      chain (unwrap arg) (head :: heads)
    | _ -> List.rev heads, head, args
  in
  let heads, head, args = chain e [] in
  let args =
    match args with
    | [ arg ] ->
      (match (unwrap arg).txt with
       | Tuple (_ :: _ :: _) -> parens (expr Ctx.top arg)
       | _ -> expr (Ctx.at Splice) arg)
    | args -> parens (tuple_items args)
  in
  group
    (concat (List.map heads ~f:(fun head -> head ^^ text " ("))
     ^^ head
     ^^ nest 2 (line ^^ args)
     ^^ text (String.make (List.length heads) ')'))

and spliced inner =
  let inner =
    match (unwrap inner).txt with
    | Splice _ | Quote _ -> expr ~parenthesize:true Ctx.top inner
    | _ -> expr (Ctx.at Postfix) inner
  in
  text ".~" ^^ inner

and postfix record =
  expr
    ~parenthesize:(needs_parens_before_postfix record)
    (Ctx.at Postfix)
    record

and tuple_items es =
  let last = List.length es - 1 in
  group
    (separate
       (text "," ^^ line)
       (List.mapi es ~f:(fun i e ->
          let after : After.t = if i = last then Nothing else Comma in
          expr (Ctx.item after) e)))

(* A value in [{...}]. *)
and entry ?(after = After.Nothing) e = expr (Ctx.item ~seq:false after) e

and field ?after lhs value =
  definition ~record:(is_record value) lhs (entry ?after value)

and poly_fields ?(last_after = After.Nothing) fields =
  let last = List.length fields - 1 in
  List.mapi fields ~f:(fun i ((label : Name.t), value) ->
    let after = if i = last then last_after else Nothing in
    field ~after (text ("`" ^ label.txt)) value)
;;

let expr_doc = expr Ctx.top

let type_param (param : Type_decl.Param.t Located.t) =
  match param.txt with
  | Ty name -> tyvar name
  | Row name -> text ("{..'" ^ name ^ "}")
;;

let constructor_decl ((name : Name.t), args) =
  match args with
  | [] -> text name.txt
  | args ->
    text (name.txt ^ " of ")
    ^^ align
         (group
            (separate
               (text " *" ^^ line)
               (List.map args ~f:(type_expr ~prec:2))))
;;

let type_decl ({ name; params; kind } : Type_decl.t) =
  let lhs =
    text ("type " ^ name.txt)
    ^^
    match params with
    | [] -> empty
    | params ->
      text "["
      ^^ align
           (group (separate (text "," ^^ line) (List.map params ~f:type_param)))
      ^^ text "]"
  in
  match kind with
  | Abstract -> lhs
  | Alias t -> definition lhs (type_expr_doc t)
  | Variant [] -> lhs ^^ text " = |"
  | Variant constructors ->
    group
      (lhs
       ^^ text " ="
       ^^ nest
            2
            (concat
               (List.mapi constructors ~f:(fun i c ->
                  line
                  ^^ (if i = 0 then if_break "| " else text "| ")
                  ^^ constructor_decl c))))
  | Record fields ->
    lhs
    ^^ text " = "
    ^^ braces
         (List.map fields ~f:(fun ((name : Name.t), t) ->
            definition ~sep:" :" (text name.txt) (type_expr_doc t)))
;;

module Kind = struct
  type t =
    | Let
    | Type
    | Module_splice
    | Include
    | Other
  [@@deriving equal]

  let of_toplevel (t : Toplevel.t) =
    match t.txt with
    | Let _ -> Let
    | Type _ | Exception _ -> Type
    | Module_splice _ -> Module_splice
    | Include _ -> Include
    | Module _ | Expect_test _ | Bench _ -> Other
  ;;
end

let rec toplevel ~width ~indent ~blank_between (t : Toplevel.t) =
  match t.txt with
  | Let (name, params, annot, rhs) ->
    definition
      ~record:(is_record rhs)
      (let_lhs "let" name params annot)
      (expr_doc rhs)
  | Type decl -> type_decl decl
  | Exception (name, args) -> text "exception " ^^ constructor_decl (name, args)
  | Module (name, []) -> text ("module " ^ name.txt ^ " = struct end")
  | Module (name, members) ->
    text ("module " ^ name.txt ^ " = struct")
    ^^ nest
         2
         (hardline
          ^^ toplevels ~width ~indent:(indent + 2) ~blank_between members)
    ^^ hardline
    ^^ text "end"
  | Module_splice (name, e) -> text ("module " ^ name.txt ^ " = ") ^^ spliced e
  | Include e -> text "include " ^^ spliced e
  | Expect_test { name; body } ->
    definition
      (text ("let%expect_test " ^ string_literal name.txt))
      (expr_doc body)
  | Bench { name; param; body } ->
    let lhs = text ("let%bench " ^ string_literal name.txt) in
    let lhs =
      match param with
      | None -> lhs
      | Some { name = x; args } ->
        let last = List.length args - 1 in
        lhs
        ^^ text (" " ^ x.txt ^ " in ")
        ^^ group
             (text "["
              ^^ nest
                   2
                   (cut
                    ^^ separate
                         (text ";" ^^ line)
                         (List.mapi args ~f:(fun i arg ->
                            entry
                              ~after:(if i = last then Nothing else Semi)
                              arg)))
              ^^ cut
              ^^ text "]")
    in
    definition lhs (expr_doc body)

and toplevels ~width ~indent ~blank_between ts =
  let one_line doc = fits_flat ~width:(width - indent) doc in
  let docs =
    List.map ts ~f:(fun (t : Toplevel.t) ->
      let doc = toplevel ~width ~indent ~blank_between t in
      (Kind.of_toplevel t, one_line doc, t.loc), doc)
  in
  match docs with
  | [] -> empty
  | (_, first) :: _ ->
    List.fold
      (List.zip_exn (List.drop_last_exn docs) (List.tl_exn docs))
      ~init:first
      ~f:
        (fun
          acc
          ( ((prev_kind, prev_one_line, prev_loc), _)
          , ((kind, one_line, loc), doc) ) ->
        let compact =
          prev_one_line
          && one_line
          && Kind.equal prev_kind kind
          && (not (Kind.equal prev_kind Other))
          && not (blank_between prev_loc loc)
        in
        acc ^^ hardline ^^ (if compact then empty else hardline) ^^ doc)
;;

let type_expr ?width t = render ?width (type_expr_doc t)
let pattern ?width p = render ?width (pattern_doc p)
let expr ?width e = render ?width (expr_doc e)

let toplevel ?(width = 80) t =
  render ~width (toplevel ~width ~indent:0 ~blank_between:(fun _ _ -> false) t)
;;

let program_doc ~width ~blank_between ts =
  toplevels ~width ~indent:0 ~blank_between ts
;;

let program ?(width = 80) ts =
  match ts with
  | [] -> ""
  | ts ->
    render
      ~width
      (program_doc ~width ~blank_between:(fun _ _ -> false) ts ^^ hardline)
;;
