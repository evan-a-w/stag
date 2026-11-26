open! Import

module Kind = struct
  type t =
    | Module
    | Type
    | Constructor
    | Field
    | Poly_field
    | Poly_tag
    | Value
    | Let of { is_function : bool }
    | Parameter
    | Pattern_var
  [@@deriving sexp_of, equal]
end

module Occurrence = struct
  type t =
    { loc : Loc.t
    ; kind : Kind.t
    ; path : string list
    ; declaration : bool
    ; quoted : bool
    }
  [@@deriving sexp_of]
end

type add = ?declaration:bool -> kind:Kind.t -> loc:Loc.t -> string list -> unit

let decl_path ~(add : add) ~kind ~loc : Syntax.Decl_path.t -> unit = function
  | Path path | Root path -> add ~kind ~loc (path :> string list)
  | Resolved _ -> ()
;;

let of_toplevels ?(walk_expansion = Fn.const true) toplevels =
  let occurrences = Queue.create () in
  let add ?(declaration = false) ~quoted ~kind ~loc path =
    if not (Loc.is_none loc)
    then
      Queue.enqueue
        occurrences
        { Occurrence.loc; kind; path; declaration; quoted }
  in
  let rec walk_toplevels ?(quoted = false) toplevels =
    List.iter toplevels ~f:(walk_toplevel ~quoted)
  and walk_toplevel ~quoted (toplevel : Syntax.Toplevel.t) =
    let add = add ~quoted in
    let name ?declaration kind (name : Syntax.Name.t) =
      add ?declaration ~kind ~loc:name.loc [ name.txt ]
    in
    (* Declarations in a struct quote are not of the module. *)
    let toplevels = walk_toplevels ~quoted:true in
    let rec type_expr ty = walk_type_expr ~add ~expr ty
    and expr e = walk_expr ~add ~type_expr ~toplevels e in
    match toplevel.txt with
    | Let (binder, bindings, annot, body) ->
      let_ ~add ~type_expr binder bindings annot body;
      expr body
    | Let_mut (binder, annot, body) ->
      let_ ~add ~type_expr binder [] annot body;
      expr body
    | Type { name = type_name; params = _; kind } ->
      name ~declaration:true Type type_name;
      let rec decl_kind : Syntax.Type_decl.Kind.t -> unit = function
        | Variant constructors ->
          List.iter constructors ~f:(fun (constructor, args) ->
            name ~declaration:true Constructor constructor;
            List.iter args ~f:type_expr)
        | Record fields ->
          List.iter fields ~f:(fun (field, ty) ->
            name ~declaration:true Field field;
            type_expr ty)
        | Alias ty -> type_expr ty
        | Splice e -> expr e.txt
        | Abstract -> ()
        | Malformed kind -> decl_kind kind
      in
      decl_kind kind
    | Exception (exn, args) | Malformed_exception (exn, args) ->
      name ~declaration:true Constructor exn;
      List.iter args ~f:type_expr
    | External { name = ext; ty; symbol = _ } ->
      add
        ~declaration:true
        ~kind:(Kind.Let { is_function = true })
        ~loc:ext.loc
        [ ext.txt ];
      type_expr ty
    | Module (module_name, toplevels) ->
      name ~declaration:true Module module_name;
      walk_toplevels ~quoted toplevels
    | Module_splice (module_name, body, expansion) ->
      name ~declaration:true Module module_name;
      expr body;
      expansion_toplevels expansion
    | Include (body, expansion) ->
      expr body;
      expansion_toplevels expansion
    | Expect_test { name = _; body } -> expr body
    | Bench { name = _; param; body } ->
      Option.iter param ~f:(fun { name = param; args } ->
        name ~declaration:true Kind.Parameter param;
        List.iter args ~f:expr);
      expr body
    | Attributed (toplevel, _) -> walk_toplevel ~quoted toplevel
    | Attribute _ -> ()
  and expansion_toplevels = function
    | Some (expansion : Syntax.Toplevel.t list Located.t)
      when walk_expansion expansion -> walk_toplevels expansion.txt
    | Some _ | None -> ()
  and let_ ~(add : add) ~type_expr (binder : Syntax.Name.t) bindings annot body =
    let is_function =
      (not (List.is_empty bindings))
      ||
      match (body : Syntax.Expr.t).txt with
      | Fun _ -> true
      | _ -> false
    in
    add
      ~declaration:true
      ~kind:(Kind.Let { is_function })
      ~loc:binder.loc
      [ binder.txt ];
    walk_bindings ~add ~type_expr bindings;
    Option.iter annot.Syntax.Annot.ty ~f:type_expr
  and walk_bindings ~(add : add) ~type_expr bindings =
    List.iter bindings ~f:(fun ((name : Syntax.Name.t), annot) ->
      add ~declaration:true ~kind:Kind.Parameter ~loc:name.loc [ name.txt ];
      Option.iter annot.Syntax.Annot.ty ~f:type_expr)
  and walk_type_expr ~(add : add) ~expr (ty : Syntax.Type_expr.t) =
    let recurse = walk_type_expr ~add ~expr in
    match ty.txt with
    | Var _ -> ()
    | Con (path, args) ->
      decl_path ~add ~kind:Kind.Type ~loc:path.loc path.txt;
      List.iter args ~f:(function
        | Ty ty -> recurse ty
        | Row row -> walk_row ~add ~expr ~kind:Kind.Poly_field row
        | Mode _ -> ())
    | Arrow (a, b) ->
      recurse a;
      recurse b
    | Tuple tys -> List.iter tys ~f:recurse
    | Code { inner; context } ->
      recurse inner;
      walk_row ~add ~expr ~kind:Kind.Poly_field context
    | Record row -> walk_row ~add ~expr ~kind:Kind.Poly_field row
    | Variant row -> walk_row ~add ~expr ~kind:Kind.Poly_tag row
    | Mut ty | Immut ty | Moded (ty, _) | Constrained (ty, _) | Modality (ty, _)
      -> recurse ty
    | Splice e -> expr e
  and walk_row ~(add : add) ~expr ~kind (row : Syntax.Type_expr.row) =
    match row.txt with
    | Empty | Var _ -> ()
    | Extend (label, ty, rest) ->
      add ~kind ~loc:label.loc [ label.txt ];
      walk_type_expr ~add ~expr ty;
      walk_row ~add ~expr ~kind rest
  and walk_pattern ~(add : add) (pattern : Syntax.Pattern.t) =
    let recurse = walk_pattern ~add in
    match pattern.txt with
    | Any | Lit _ -> ()
    | Var name ->
      add ~declaration:true ~kind:Kind.Pattern_var ~loc:pattern.loc [ name ]
    | Tuple patterns -> List.iter patterns ~f:recurse
    | Deref pattern -> recurse pattern
    | Construct (path, args) ->
      decl_path ~add ~kind:Kind.Constructor ~loc:path.loc path.txt;
      List.iter args ~f:recurse
    | Record fields ->
      List.iter fields ~f:(fun (path, pattern) ->
        decl_path ~add ~kind:Kind.Field ~loc:path.loc path.txt;
        recurse pattern)
    | Poly_variant (tag, arg) ->
      add ~kind:Kind.Poly_tag ~loc:tag.loc [ tag.txt ];
      Option.iter arg ~f:recurse
    | Poly_record { fields; rest } ->
      List.iter fields ~f:(fun ((label : Syntax.Name.t), pattern) ->
        add ~kind:Kind.Poly_field ~loc:label.loc [ label.txt ];
        recurse pattern);
      (match rest with
       | Closed | Open -> ()
       | Bind name ->
         add ~declaration:true ~kind:Kind.Pattern_var ~loc:name.loc [ name.txt ])
    | Exception pattern -> recurse pattern
    | Or (left, right) ->
      recurse left;
      recurse right
  and walk_expr ~(add : add) ~type_expr ~toplevels (expr : Syntax.Expr.t) =
    let recurse = walk_expr ~add ~type_expr ~toplevels in
    let labelled fields =
      List.iter fields ~f:(fun ((label : Syntax.Name.t), expr) ->
        add ~kind:Kind.Poly_field ~loc:label.loc [ label.txt ];
        recurse expr)
    in
    let fields fields =
      List.iter fields ~f:(fun ((path : Syntax.Decl_path.t Located.t), expr) ->
        decl_path ~add ~kind:Kind.Field ~loc:path.loc path.txt;
        recurse expr)
    in
    match expr.txt with
    | Lit _ | Expect _ | Error -> ()
    | Var (path, annot) ->
      decl_path ~add ~kind:Kind.Value ~loc:expr.loc path;
      Option.iter annot.ty ~f:type_expr
    | App (f, arg) | Seq (f, arg) ->
      recurse f;
      recurse arg
    | Let (binder, bindings, annot, body, in_) ->
      let_ ~add ~type_expr binder bindings annot body;
      recurse body;
      recurse in_
    | Let_mut (binder, annot, body, in_) ->
      let_ ~add ~type_expr binder [] annot body;
      recurse body;
      recurse in_
    | Assign (path, value) ->
      decl_path ~add ~kind:Kind.Value ~loc:path.loc path.txt;
      recurse value
    | Store (target, value) ->
      recurse target;
      recurse value
    | Fun (bindings, body) ->
      walk_bindings ~add ~type_expr bindings;
      recurse body
    | Splice { expr; subs } ->
      recurse expr;
      Option.iter subs ~f:(fun subs ->
        List.iter subs ~f:(fun ((name : Syntax.Name.t), expr) ->
          match expr with
          | None -> add ~kind:Kind.Value ~loc:name.loc [ name.txt ]
          | Some expr ->
            add
              ~declaration:true
              ~kind:Kind.Parameter
              ~loc:name.loc
              [ name.txt ];
            recurse expr))
    | Quote expr -> recurse expr
    | Tuple exprs -> List.iter exprs ~f:recurse
    | Deref expr -> recurse expr
    | Construct (path, args) ->
      decl_path ~add ~kind:Kind.Constructor ~loc:path.loc path.txt;
      List.iter args ~f:recurse
    | Record record -> fields record
    | Field (expr, path) ->
      recurse expr;
      decl_path ~add ~kind:Kind.Field ~loc:path.loc path.txt
    | Record_update (expr, record) ->
      recurse expr;
      fields record
    | Type ty -> type_expr ty
    | Match (scrutinee, cases) ->
      recurse scrutinee;
      List.iter cases ~f:(fun (pattern, body) ->
        walk_pattern ~add pattern;
        recurse body)
    | Poly_record record -> labelled record
    | Poly_field (expr, label) | Poly_restrict (expr, label) ->
      recurse expr;
      add ~kind:Kind.Poly_field ~loc:label.loc [ label.txt ]
    | Poly_extend (record, expr) ->
      labelled record;
      recurse expr
    | Poly_update (expr, record) ->
      recurse expr;
      labelled record
    | Poly_variant (tag, arg) ->
      add ~kind:Kind.Poly_tag ~loc:tag.loc [ tag.txt ];
      Option.iter arg ~f:recurse
    | Struct_quote quoted -> toplevels quoted
    | Stack expr
    | Exclave expr
    | Region expr
    | Nontail expr
    | Attributed (expr, _) -> recurse expr
    | Annotated (expr, annot) ->
      recurse expr;
      Option.iter annot.ty ~f:type_expr
    | Forget (_, expr) -> recurse expr
  in
  walk_toplevels toplevels;
  Queue.to_list occurrences
;;
