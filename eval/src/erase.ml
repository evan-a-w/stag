open! Import

exception Reached_error

module Uses = struct
  type t =
    { idents : Typed.Ident.Set.t
    ; free : String.Set.t
    }

  let empty = { idents = Typed.Ident.Set.empty; free = String.Set.empty }

  let union a b =
    { idents = Set.union a.idents b.idents; free = Set.union a.free b.free }
  ;;

  let rec of_expr (expr : Typed.Expr.t) =
    let children =
      List.fold (Typed.Expr.sub_exprs expr) ~init:empty ~f:(fun acc e ->
        union acc (of_expr e))
    in
    match expr.desc with
    | Var (Bound (Local ident) | Persistent (Local ident))
    | Assign ((Bound (Local ident) | Persistent (Local ident)), _) ->
      { children with idents = Set.add children.idents ident }
    | Var (Free name) | Assign (Free name, _) ->
      { children with free = Set.add children.free name }
    | Splice { subs = Implicit providers; _ } ->
      { children with
        idents =
          Map.fold providers ~init:children.idents ~f:(fun ~key:_ ~data acc ->
            match (data : Typed.Binder.t) with
            | Local ident -> Set.add acc ident
            | Toplevel _ -> acc)
      }
    | _ -> children
  ;;
end

module Names = struct
  type t =
    { names : string Typed.Ident.Map.t
    ; taken : String.Hash_set.t
    }

  let create (expr : Typed.Expr.t) =
    let taken = String.Hash_set.create () in
    let add_idents =
      List.iter ~f:(fun (i : Typed.Ident.t) -> Hash_set.add taken i.name)
    in
    let rec add (expr : Typed.Expr.t) =
      (match expr.desc with
       | Var (Free name) | Assign (Free name, _) -> Hash_set.add taken name
       | Var (Bound (Local ident) | Persistent (Local ident))
       | Assign ((Bound (Local ident) | Persistent (Local ident)), _) ->
         add_idents [ ident ]
       | Let { ident; args; _ } -> add_idents (ident :: args)
       | Let_mut { ident; _ } -> add_idents [ ident ]
       | Fun { args; _ } -> add_idents args
       | Match { cases; exception_cases; _ } ->
         List.iter (cases @ exception_cases) ~f:(fun (p, _) ->
           add_idents (Typed.Pattern.idents p))
       | _ -> ());
      List.iter (Typed.Expr.sub_exprs expr) ~f:add
    in
    add expr;
    { names = Typed.Ident.Map.empty; taken }
  ;;

  let fresh t base =
    let rec go i =
      let name = sprintf "%s_%d" base i in
      if Hash_set.mem t.taken name then go (i + 1) else name
    in
    let name = go 1 in
    Hash_set.add t.taken name;
    name
  ;;

  (* Binds [idents] over [scope]: each keeps its name unless that would capture
     a variable [scope] uses, of that name but bound elsewhere. *)
  let bind t idents ~(scope : Uses.t) =
    List.fold idents ~init:t ~f:(fun t (ident : Typed.Ident.t) ->
      let captures =
        Set.mem scope.free ident.name
        || Set.exists scope.idents ~f:(fun other ->
          (not (Typed.Ident.equal other ident))
          && Option.equal
               String.equal
               (Map.find t.names other)
               (Some ident.name)
          && not (List.mem idents other ~equal:Typed.Ident.equal))
      in
      let name = if captures then fresh t ident.name else ident.name in
      { t with names = Map.set t.names ~key:ident ~data:name })
  ;;
end

let ty_annot ty : Syntax.Annot.t = { ty; mode = None }
let resolved name : Syntax.Decl_path.t = Resolved name

let expr (expr : Typed.Expr.t) =
  let open Or_error.Let_syntax in
  let located loc txt = Located.create ~loc txt in
  let name (names : Names.t) (ident : Typed.Ident.t) =
    Map.find_exn names.names ident
  in
  let rec pattern names (p : Typed.Pattern.t) : Syntax.Pattern.t =
    let go = pattern names in
    let decl_path name = located p.loc (resolved name) in
    located
      p.loc
      (match p.desc with
       | Any -> Syntax.Pattern.Any
       | Error (_ : Typed.Pattern.t list) -> raise Reached_error
       | Var ident -> Var (name names ident)
       | Lit lit -> Lit lit
       | Tuple ps -> Tuple (List.map ps ~f:go)
       | Deref p -> Deref (go p)
       | Construct (c, ps) -> Construct (decl_path c.name, List.map ps ~f:go)
       | Record fields ->
         Record
           (List.map fields ~f:(fun (f, p) ->
              decl_path (Typed.Field_ref.name f), go p))
       | Poly_variant (tag, arg) ->
         Poly_variant (located p.loc tag, Option.map arg ~f:go)
       | Poly_record { fields; rest } ->
         Poly_record
           { fields =
               List.map fields ~f:(fun (l, field) -> located p.loc l, go field)
           ; rest =
               (match rest with
                | Closed -> Closed
                | Open -> Open
                | Bind ident -> Bind (located p.loc (name names ident)))
           })
  in
  let var_path (names : Names.t) : Typed.Var_ref.t -> _ = function
    | Bound (Local ident) | Persistent (Local ident) ->
      (match Map.find names.names ident with
       | Some name -> Ok (Syntax.Decl_path.Path (Syntax.Path.name name))
       | None ->
         Or_error.error_string
           (sprintf
              "the code embeds %s, a local from outside it, which nothing \
               names where the code is spliced"
              ident.name))
    | Bound (Toplevel ident) | Persistent (Toplevel ident) ->
      Ok (resolved ident.name)
    | Free name -> Ok (Path (Syntax.Path.of_string name))
    | Free_toplevel name -> Ok (resolved name)
    | Prim prim -> Ok (resolved (Prim.canonical_name prim))
    | Intrinsic Ptr_make -> Ok (resolved (Prelude_path.canonical "Ptr.make"))
    | Intrinsic intrinsic ->
      Or_error.error_s
        [%message
          "intrinsics are named only in the prelude, which quotes none"
            (intrinsic : Intrinsic.t)]
  in
  let rec go (names : Names.t) (e : Typed.Expr.t) : Syntax.Expr.t Or_error.t =
    let loc = e.loc in
    let node (desc : Syntax.Expr.desc) = Ok (located loc desc) in
    let label l = located loc l in
    let all es = List.map es ~f:(go names) |> Or_error.all in
    let fields ~key es =
      List.map es ~f:(fun (k, e) ->
        let%map e = go names e in
        key k, e)
      |> Or_error.all
    in
    let field_path f = located loc (resolved (Typed.Field_ref.name f)) in
    let binding names annots ident : Syntax.Binding.t =
      ( label (name names ident)
      , List.Assoc.find annots ident ~equal:Typed.Ident.equal
        |> Option.value ~default:Syntax.Annot.none )
    in
    match e.desc with
    | Lit lit -> node (Lit lit)
    | Var var_ref ->
      let%bind path = var_path names var_ref in
      node (Var (path, Syntax.Annot.none))
    | Assign (var_ref, value) ->
      let%bind path = var_path names var_ref in
      let%bind value = go names value in
      node (Assign (located loc path, value))
    | Store (target, value) ->
      let%bind target = go names target in
      let%bind value = go names value in
      node (Store (target, value))
    | Let_mut { ident; annot; mode; body; in_ } ->
      let%bind body = go names body in
      let names = Names.bind names [ ident ] ~scope:(Uses.of_expr in_) in
      let%bind in_ = go names in_ in
      node
        (Let_mut
           (label (name names ident), { (ty_annot annot) with mode }, body, in_))
    | Annotated (e, annot) ->
      let%bind e = go names e in
      (match e.txt with
       | Var (path, existing) when Syntax.Annot.is_none existing ->
         node (Var (path, annot))
       | _ ->
         Or_error.error_s
           [%message "annotated non-variable" (e : Syntax.Expr.t)])
    | App (f, x) ->
      let%bind f = go names f in
      let%bind x = go names x in
      node (App (f, x))
    | Let
        { ident; args; annots; result_annot; mode; body; in_; generalized = _ }
      ->
      let outer =
        Names.bind
          names
          [ ident ]
          ~scope:(Uses.union (Uses.of_expr body) (Uses.of_expr in_))
      in
      let inner = Names.bind outer args ~scope:(Uses.of_expr body) in
      let%bind body = go inner body in
      let%bind in_ = go outer in_ in
      node
        (Let
           ( label (name outer ident)
           , List.map args ~f:(binding inner annots)
           , { (ty_annot result_annot) with mode }
           , body
           , in_ ))
    | Fun { args; annots; body } ->
      let names = Names.bind names args ~scope:(Uses.of_expr body) in
      let%bind body = go names body in
      node (Fun (List.map args ~f:(binding names annots), body))
    | Splice { code; subs } ->
      let%bind code = go names code in
      let%bind subs =
        match subs with
        | Implicit providers ->
          let renamed =
            Map.existsi providers ~f:(fun ~key ~data ->
              match (data : Typed.Binder.t) with
              | Local ident -> not (String.equal (name names ident) key)
              | Toplevel _ -> false)
          in
          (* Toplevels free by canonical name are provided even so. *)
          let sub ~key (binder : Typed.Binder.t) =
            match binder with
            | Local ident ->
              Some (Syntax.Decl_path.Path (Syntax.Path.name (name names ident)))
            | Toplevel _ when String.mem key '.' -> None
            | Toplevel ident -> Some (resolved ident.name)
          in
          if not renamed
          then Ok None
          else
            Ok
              (Some
                 (Map.to_alist providers
                  |> List.filter_map ~f:(fun (key, binder) ->
                    let%map.Option path = sub ~key binder in
                    ( label key
                    , Some
                        (located
                           loc
                           (Syntax.Expr.Var (path, Syntax.Annot.none))) ))))
        | Explicit subs ->
          let%map subs = fields subs ~key:label in
          Some (List.map subs ~f:(fun (name, e) -> name, Some e))
      in
      node (Splice { expr = code; subs })
    | Toplevel_splice { expansion; code = _ } -> go names expansion
    | Quote body ->
      let%bind body = go names body in
      node (Quote body)
    | Tuple es ->
      let%bind es = all es in
      node (Tuple es)
    | Deref e ->
      let%bind e = go names e in
      node (Deref e)
    | Construct (c, es) ->
      let%bind es = all es in
      node (Construct (located loc (resolved c.name), es))
    | Record fs ->
      let%bind fs = fields fs ~key:field_path in
      node (Record fs)
    | Field (e, f) ->
      let%bind e = go names e in
      node (Field (e, field_path f))
    | Record_update (e, fs) ->
      let%bind e = go names e in
      let%bind fs = fields fs ~key:field_path in
      node (Record_update (e, fs))
    | Match { scrutinee; cases; exception_cases } ->
      let%bind scrutinee = go names scrutinee in
      let case ~exception_ (p, body) =
        let names =
          Names.bind names (Typed.Pattern.idents p) ~scope:(Uses.of_expr body)
        in
        let%map body = go names body in
        let p = pattern names p in
        ( (if exception_ then located p.loc (Syntax.Pattern.Exception p) else p)
        , body )
      in
      let%bind cases =
        List.map cases ~f:(case ~exception_:false) |> Or_error.all
      in
      let%bind exception_cases =
        List.map exception_cases ~f:(case ~exception_:true) |> Or_error.all
      in
      node (Match (scrutinee, cases @ exception_cases))
    | Poly_record fs ->
      let%bind fs = fields fs ~key:label in
      node (Poly_record fs)
    | Poly_field (e, l) ->
      let%bind e = go names e in
      node (Poly_field (e, label l))
    | Poly_extend (fs, e) ->
      let%bind fs = fields fs ~key:label in
      let%bind e = go names e in
      node (Poly_extend (fs, e))
    | Poly_restrict (e, l) ->
      let%bind e = go names e in
      node (Poly_restrict (e, label l))
    | Poly_update (e, fs) ->
      let%bind e = go names e in
      let%bind fs = fields fs ~key:label in
      node (Poly_update (e, fs))
    | Poly_variant (tag, e) ->
      let%bind e =
        match e with
        | None -> Ok None
        | Some e -> go names e >>| Option.some
      in
      node (Poly_variant (label tag, e))
    | Expect expected -> node (Expect expected)
    | Type ty -> node (Type ty)
    | Error (_ : Typed.Expr.t list) -> raise Reached_error
  in
  go (Names.create expr) expr
;;
