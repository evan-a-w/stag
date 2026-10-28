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
    ; module_path : string list
    }
  [@@deriving sexp_of]
end

let decl_path : Syntax.Decl_path.t -> string list option = function
  | Path path -> Some (path :> string list)
  | Resolved _ -> None
;;

type add = ?declaration:bool -> kind:Kind.t -> loc:Loc.t -> string list -> unit

let of_toplevels ~module_path toplevels =
  let occurrences = Queue.create () in
  let add ?(declaration = false) ~module_path ~kind ~loc path =
    if not (Loc.is_none loc)
    then
      Queue.enqueue
        occurrences
        { Occurrence.loc; kind; path; declaration; module_path }
  in
  let rec walk_toplevels ~module_path toplevels =
    List.iter toplevels ~f:(walk_toplevel ~module_path)
  and walk_toplevel ~module_path (toplevel : Syntax.Toplevel.t) =
    let add = add ~module_path in
    let name ?declaration kind (name : Syntax.Name.t) =
      add ?declaration ~kind ~loc:name.loc [ name.txt ]
    in
    let type_expr = walk_type_expr ~add in
    let expr = walk_expr ~add ~type_expr in
    match toplevel.txt with
    | Let (binder, bindings, annot, body) ->
      let_ ~add ~type_expr binder bindings annot body;
      expr body
    | Type { name = type_name; params = _; kind } ->
      name ~declaration:true Type type_name;
      (match kind with
       | Variant constructors ->
         List.iter constructors ~f:(fun (constructor, args) ->
           name ~declaration:true Constructor constructor;
           List.iter args ~f:type_expr)
       | Record fields ->
         List.iter fields ~f:(fun (field, ty) ->
           name ~declaration:true Field field;
           type_expr ty)
       | Alias ty -> type_expr ty
       | Abstract -> ())
    | Exception (exn, args) ->
      name ~declaration:true Constructor exn;
      List.iter args ~f:type_expr
    | Module (module_name, toplevels) ->
      name ~declaration:true Module module_name;
      walk_toplevels ~module_path:(module_path @ [ module_name.txt ]) toplevels
    | Module_splice (module_name, body) ->
      name ~declaration:true Module module_name;
      expr body
    | Include body -> expr body
    | Expect_test { name = _; body } -> expr body
    | Bench { name = _; param; body } ->
      Option.iter param ~f:(fun { name = param; args } ->
        name ~declaration:true Kind.Parameter param;
        List.iter args ~f:expr);
      expr body
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
    Option.iter annot ~f:type_expr
  and walk_bindings ~(add : add) ~type_expr bindings =
    List.iter bindings ~f:(fun ((name : Syntax.Name.t), annot) ->
      add ~declaration:true ~kind:Kind.Parameter ~loc:name.loc [ name.txt ];
      Option.iter annot ~f:type_expr)
  and walk_type_expr ~(add : add) (ty : Syntax.Type_expr.t) =
    let recurse = walk_type_expr ~add in
    match ty.txt with
    | Var _ -> ()
    | Con (path, args) ->
      Option.iter (decl_path path.txt) ~f:(add ~kind:Kind.Type ~loc:path.loc);
      List.iter args ~f:(function
        | Ty ty -> recurse ty
        | Row row -> walk_row ~add ~kind:Kind.Poly_field row)
    | Arrow (a, b) ->
      recurse a;
      recurse b
    | Tuple tys -> List.iter tys ~f:recurse
    | Code { inner; context } ->
      recurse inner;
      walk_row ~add ~kind:Kind.Poly_field context
    | Record row -> walk_row ~add ~kind:Kind.Poly_field row
    | Variant row -> walk_row ~add ~kind:Kind.Poly_tag row
  and walk_row ~(add : add) ~kind (row : Syntax.Type_expr.row) =
    match row.txt with
    | Empty | Var _ -> ()
    | Extend (label, ty, rest) ->
      add ~kind ~loc:label.loc [ label.txt ];
      walk_type_expr ~add ty;
      walk_row ~add ~kind rest
  and walk_pattern ~(add : add) (pattern : Syntax.Pattern.t) =
    let recurse = walk_pattern ~add in
    match pattern.txt with
    | Any | Lit _ -> ()
    | Var name ->
      add ~declaration:true ~kind:Kind.Pattern_var ~loc:pattern.loc [ name ]
    | Tuple patterns -> List.iter patterns ~f:recurse
    | Construct (path, args) ->
      Option.iter
        (decl_path path.txt)
        ~f:(add ~kind:Kind.Constructor ~loc:path.loc);
      List.iter args ~f:recurse
    | Record fields ->
      List.iter fields ~f:(fun (path, pattern) ->
        Option.iter (decl_path path.txt) ~f:(add ~kind:Kind.Field ~loc:path.loc);
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
  and walk_expr ~(add : add) ~type_expr (expr : Syntax.Expr.t) =
    let recurse = walk_expr ~add ~type_expr in
    let labelled fields =
      List.iter fields ~f:(fun ((label : Syntax.Name.t), expr) ->
        add ~kind:Kind.Poly_field ~loc:label.loc [ label.txt ];
        recurse expr)
    in
    let fields fields =
      List.iter fields ~f:(fun ((path : Syntax.Decl_path.t Located.t), expr) ->
        Option.iter (decl_path path.txt) ~f:(add ~kind:Kind.Field ~loc:path.loc);
        recurse expr)
    in
    match expr.txt with
    | Lit _ | Expect _ -> ()
    | Var (path, annot) ->
      add ~kind:Kind.Value ~loc:expr.loc (path :> string list);
      Option.iter annot ~f:type_expr
    | App (f, arg) ->
      recurse f;
      recurse arg
    | Let (binder, bindings, annot, body, in_) ->
      let_ ~add ~type_expr binder bindings annot body;
      recurse body;
      recurse in_
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
    | Construct (path, args) ->
      Option.iter
        (decl_path path.txt)
        ~f:(add ~kind:Kind.Constructor ~loc:path.loc);
      List.iter args ~f:recurse
    | Record record -> fields record
    | Field (expr, path) ->
      recurse expr;
      Option.iter (decl_path path.txt) ~f:(add ~kind:Kind.Field ~loc:path.loc)
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
  in
  walk_toplevels ~module_path toplevels;
  Queue.to_list occurrences
;;
