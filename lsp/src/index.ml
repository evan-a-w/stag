open! Import

module Span = struct
  module T = struct
    type t =
      { file : Loc.File.t
      ; start : int
      ; stop : int
      }
    [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)

  let of_loc (loc : Loc.t) =
    { file = loc.file; start = loc.start.#offset; stop = loc.stop.#offset }
  ;;
end

module Target = struct
  type t =
    | Loc of Loc.t
    | Local of Typed.Ident.t
    | Value of string
    | Type of string
    | Constructor of string
    | Field of string
end

module Entry = struct
  type t =
    { loc : Loc.t
    ; label : string option
    ; ty : Type.ty option
    ; target : Target.t option
    }
end

module Hover = struct
  type t =
    { loc : Loc.t
    ; text : string
    }
  [@@deriving sexp_of]
end

type t =
  { entries : Entry.t Queue.t String.Table.t
  ; locals : Loc.t Typed.Ident.Table.t
  ; values : Loc.t String.Table.t
  ; types : Loc.t String.Table.t
  ; constructors : Loc.t String.Table.t
  ; fields : Loc.t String.Table.t
  ; functions : Int.Hash_set.t String.Table.t
  ; splices : Span.Hash_set.t
  ; of_prelude : bool
  }

(* The syntax at a typed node's location that names what it doesn't keep. *)
module Syntax_at = struct
  type t =
    { lets : (Syntax.Name.t * Syntax.Binding.t list) Span.Table.t
    ; funs : Syntax.Binding.t list Span.Table.t
    ; constructors : Syntax.Decl_path.t Located.t Span.Table.t
    ; fields : Syntax.Decl_path.t Located.t list Span.Table.t
    ; rests : Syntax.Name.t Span.Table.t
    ; assigned : Syntax.Decl_path.t Located.t Span.Table.t
    }

  let create () =
    { lets = Span.Table.create ()
    ; funs = Span.Table.create ()
    ; constructors = Span.Table.create ()
    ; fields = Span.Table.create ()
    ; rests = Span.Table.create ()
    ; assigned = Span.Table.create ()
    }
  ;;

  let set table loc data = Hashtbl.set table ~key:(Span.of_loc loc) ~data

  (* A [Resolved] one, of a list literal, is not written. *)
  let set_constructor t loc (path : Syntax.Decl_path.t Located.t) =
    match path.txt with
    | Resolved _ -> ()
    | Path _ | Root _ -> set t.constructors loc path
  ;;

  let rec pattern t (p : Syntax.Pattern.t) =
    match p.txt with
    | Any | Var _ | Lit _ -> ()
    | Deref p -> pattern t p
    | Tuple ps -> List.iter ps ~f:(pattern t)
    | Construct (path, ps) ->
      set_constructor t p.loc path;
      List.iter ps ~f:(pattern t)
    | Record fields ->
      set t.fields p.loc (List.map fields ~f:fst);
      List.iter fields ~f:(fun (_, p) -> pattern t p)
    | Poly_variant (_, p) -> Option.iter p ~f:(pattern t)
    | Poly_record { fields; rest } ->
      (match rest with
       | Bind name -> set t.rests p.loc name
       | Closed | Open -> ());
      List.iter fields ~f:(fun (_, p) -> pattern t p)
    | Exception p -> pattern t p
  ;;

  let rec expr t (e : Syntax.Expr.t) =
    let recurse = expr t in
    let snds list = List.iter list ~f:(fun (_, e) -> recurse e) in
    match e.txt with
    | Lit _ | Var _ | Type _ | Expect _ | Error -> ()
    | App (a, b) ->
      recurse a;
      recurse b
    | Let (name, bindings, _, body, in_) ->
      set t.lets e.loc (name, bindings);
      recurse body;
      recurse in_
    | Let_mut (name, _, body, in_) ->
      set t.lets e.loc (name, []);
      recurse body;
      recurse in_
    | Assign (path, value) ->
      set t.assigned e.loc path;
      recurse value
    | Store (target, value) ->
      recurse target;
      recurse value
    | Fun (bindings, body) ->
      set t.funs e.loc bindings;
      recurse body
    | Splice { expr; subs } ->
      recurse expr;
      Option.iter
        subs
        ~f:(List.iter ~f:(fun (_, e) -> Option.iter e ~f:recurse))
    | Deref e | Quote e -> recurse e
    | Struct_quote toplevels ->
      Antiquote.iter toplevels ~f:(fun antiquote -> recurse antiquote.expr)
    | Tuple es -> List.iter es ~f:recurse
    | Construct (path, es) ->
      set_constructor t e.loc path;
      List.iter es ~f:recurse
    | Record fields ->
      set t.fields e.loc (List.map fields ~f:fst);
      snds fields
    | Field (inner, path) ->
      set t.fields e.loc [ path ];
      recurse inner
    | Record_update (inner, fields) ->
      set t.fields e.loc (List.map fields ~f:fst);
      recurse inner;
      snds fields
    | Match (scrutinee, cases) ->
      recurse scrutinee;
      List.iter cases ~f:(fun (p, e) ->
        pattern t p;
        recurse e)
    | Poly_record fields -> snds fields
    | Poly_field (e, _) | Poly_restrict (e, _) -> recurse e
    | Poly_extend (fields, e) | Poly_update (e, fields) ->
      recurse e;
      snds fields
    | Poly_variant (_, e) -> Option.iter e ~f:recurse
  ;;
end

(* Generated declarations are located at their splice, so nothing there is
   indexed but the splice's own expression. *)
let at_splice t loc = Hash_set.mem t.splices (Span.of_loc loc)

let add_function t (loc : Loc.t) =
  if not (at_splice t loc)
  then
    Hash_set.add
      (Hashtbl.find_or_add
         t.functions
         (Loc.file_name loc)
         ~default:Int.Hash_set.create)
      loc.start.#offset
;;

let add t (entry : Entry.t) =
  if not (Loc.is_none entry.loc || at_splice t entry.loc)
  then
    Queue.enqueue
      (Hashtbl.find_or_add
         t.entries
         (Loc.file_name entry.loc)
         ~default:Queue.create)
      entry
;;

let is_arrow ty =
  match Type.canonical_ty ty with
  | TArrow _ -> true
  | _ -> false
;;

let add_binder t (name : Syntax.Name.t) ~label ~ty ~ident =
  Option.iter ident ~f:(fun ident ->
    Hashtbl.set t.locals ~key:ident ~data:name.loc);
  if is_arrow ty then add_function t name.loc;
  add
    t
    { loc = name.loc
    ; label = Some label
    ; ty = Some ty
    ; target = Some (Loc name.loc)
    }
;;

let local_binders t (program : Typed.Program.t) names idents =
  match List.zip names idents with
  | Unequal_lengths -> ()
  | Ok pairs ->
    List.iter pairs ~f:(fun ((name : Syntax.Name.t), (ident : Typed.Ident.t)) ->
      Option.iter (Map.find program.ident_info ident) ~f:(fun info ->
        add_binder t name ~label:ident.name ~ty:info.ty ~ident:(Some ident)))
;;

let last_name (path : Syntax.Decl_path.t) =
  match path with
  | Path path | Root path -> List.last_exn (path :> string list)
  | Resolved name -> name
;;

let unqualified name =
  match String.rsplit2 name ~on:'.' with
  | Some (_, name) -> name
  | None -> name
;;

(* The written field of [fields] that is [field]. *)
let field_path fields (field : Typed.Field_ref.t) =
  List.find fields ~f:(fun (path : Syntax.Decl_path.t Located.t) ->
    String.equal (last_name path.txt) (unqualified (Typed.Field_ref.name field)))
;;

let rec pattern t ~syntax ~program (p : Typed.Pattern.t) =
  let recurse = pattern t ~syntax ~program in
  let at table = Hashtbl.find table (Span.of_loc p.loc) in
  let typed () =
    add t { loc = p.loc; label = None; ty = Some p.ty; target = None }
  in
  match p.desc with
  | Var ident ->
    add_binder
      t
      (Located.create ~loc:p.loc ident.name)
      ~label:ident.name
      ~ty:p.ty
      ~ident:(Some ident)
  | Any | Lit _ -> typed ()
  | Error ps -> List.iter ps ~f:recurse
  | Deref p ->
    typed ();
    recurse p
  | Tuple ps ->
    typed ();
    List.iter ps ~f:recurse
  | Poly_variant (_, p) ->
    typed ();
    Option.iter p ~f:recurse
  | Construct (constructor, ps) ->
    typed ();
    Option.iter (at syntax.Syntax_at.constructors) ~f:(fun path ->
      add
        t
        { loc = path.loc
        ; label = Some constructor.name
        ; ty = Some p.ty
        ; target = Some (Constructor constructor.name)
        });
    List.iter ps ~f:recurse
  | Record fields ->
    typed ();
    let paths = at syntax.fields |> Option.value ~default:[] in
    List.iter fields ~f:(fun (field, p) ->
      Option.iter (field_path paths field) ~f:(fun path ->
        add
          t
          { loc = path.loc
          ; label = Some (Typed.Field_ref.name field)
          ; ty = Some p.ty
          ; target = Some (Field (Typed.Field_ref.name field))
          });
      recurse p)
  | Poly_record { fields; rest } ->
    typed ();
    (match rest, at syntax.rests with
     | Bind ident, Some name -> local_binders t program [ name ] [ ident ]
     | (Bind _ | Closed | Open), _ -> ());
    List.iter fields ~f:(fun (_, p) -> recurse p)
;;

let rec expr t ~syntax ~program (e : Typed.Expr.t) =
  let recurse = expr t ~syntax ~program in
  let at table = Hashtbl.find table (Span.of_loc e.loc) in
  let typed ?label ?target () =
    add t { loc = e.loc; label; ty = Some e.ty; target }
  in
  let fields paths fields =
    List.iter
      fields
      ~f:(fun ((field : Typed.Field_ref.t), (value : Typed.Expr.t)) ->
        Option.iter (field_path paths field) ~f:(fun path ->
          add
            t
            { loc = path.loc
            ; label = Some (Typed.Field_ref.name field)
            ; ty = Some value.ty
            ; target = Some (Field (Typed.Field_ref.name field))
            }))
  in
  let var_target : Typed.Var_ref.t -> _ = function
    | Bound binder | Persistent binder ->
      (match binder with
       | Local ident -> ident.name, Some (Target.Local ident)
       | Toplevel ident -> ident.name, Some (Value ident.name))
    | Free name -> name, None
    | Free_toplevel name -> name, Some (Value name)
    | Prim prim -> Sexp.to_string (Prim.sexp_of_t prim), None
    | Intrinsic intrinsic -> Intrinsic.public_name intrinsic, None
  in
  (match e.desc with
   | Var var ->
     let label, target = var_target var in
     if is_arrow e.ty then add_function t e.loc;
     typed ~label ?target ()
   | Let { ident; args; _ } ->
     typed ();
     Option.iter (at syntax.Syntax_at.lets) ~f:(fun (name, bindings) ->
       local_binders t program (name :: List.map bindings ~f:fst) (ident :: args))
   | Let_mut { ident; _ } ->
     typed ();
     Option.iter (at syntax.Syntax_at.lets) ~f:(fun (name, _) ->
       local_binders t program [ name ] [ ident ])
   | Assign (var, value) ->
     typed ();
     Option.iter (at syntax.assigned) ~f:(fun path ->
       let label, target = var_target var in
       add t { loc = path.loc; label = Some label; ty = Some value.ty; target })
   | Fun { args; _ } ->
     typed ();
     Option.iter (at syntax.funs) ~f:(fun bindings ->
       local_binders t program (List.map bindings ~f:fst) args)
   | Construct (constructor, _) ->
     typed ();
     Option.iter (at syntax.constructors) ~f:(fun path ->
       add
         t
         { loc = path.loc
         ; label = Some constructor.name
         ; ty = Some e.ty
         ; target = Some (Constructor constructor.name)
         })
   | Record record | Record_update (_, record) ->
     typed ();
     fields (Option.value (at syntax.fields) ~default:[]) record
   | Field (_, field) ->
     typed ();
     Option.iter (at syntax.fields) ~f:(fun paths ->
       Option.iter (field_path paths field) ~f:(fun path ->
         add
           t
           { loc = path.loc
           ; label = Some (Typed.Field_ref.name field)
           ; ty = Some e.ty
           ; target = Some (Field (Typed.Field_ref.name field))
           }))
   | Match { cases; exception_cases; _ } ->
     typed ();
     List.iter (cases @ exception_cases) ~f:(fun (p, _) ->
       pattern t ~syntax ~program p)
   | Annotated _ -> ()
   | Lit _
   | Store _
   | App _
   | Splice _
   | Toplevel_splice _
   | Quote _
   | Deref _
   | Tuple _
   | Poly_record _
   | Poly_field _
   | Poly_extend _
   | Poly_restrict _
   | Poly_update _
   | Poly_variant _
   | Expect _
   | Type _
   | Error _ -> typed ());
  match e.desc with
  | Toplevel_splice { code; expansion = _ } -> recurse code
  | _ -> List.iter (Typed.Expr.sub_exprs e) ~f:recurse
;;

(* The declarations written in an inline expansion are those generated, located
   as written, if it is up to date. *)
let up_to_date_expansion ~up_to_date (toplevel : Syntax.Toplevel.t) =
  match toplevel.txt with
  | (Include (_, Some expansion) | Module_splice (_, _, Some expansion))
    when Hash_set.mem up_to_date (Span.of_loc expansion.loc) -> expansion.txt
  | _ -> []
;;

(* Where the declarations generated by a splice are located, unless relocated to
   its expansion: as [Module_scope.Splice.loc]. *)
let splice_span (toplevel : Syntax.Toplevel.t) =
  match toplevel.txt with
  | Include (e, Some _) | Module_splice (_, e, Some _) ->
    Span.of_loc { toplevel.loc with stop = e.loc.stop }
  | _ -> Span.of_loc toplevel.loc
;;

(* The declarations of [toplevels], in the module [prefix] (eg. ["Foo"; "A"]),
   by canonical name. *)
let rec declarations
  t
  ~up_to_date
  ~values
  ~prefix
  (toplevels : Syntax.Toplevel.t list)
  =
  let canonical (name : Syntax.Name.t) =
    String.concat ~sep:"." (prefix @ [ name.txt ])
  in
  let set table (name : Syntax.Name.t) =
    Hashtbl.set table ~key:(canonical name) ~data:name.loc
  in
  List.iter toplevels ~f:(fun toplevel ->
    match toplevel.txt with
    | Let (name, bindings, _, body) ->
      set t.values name;
      Hashtbl.set values ~key:(canonical name) ~data:(name, bindings, body)
    | Let_mut (name, _, body) ->
      set t.values name;
      Hashtbl.set values ~key:(canonical name) ~data:(name, [], body)
    | Type { name; params = _; kind } ->
      set t.types name;
      let rec members : Syntax.Type_decl.Kind.t -> unit = function
        | Variant constructors ->
          List.iter constructors ~f:(fun (name, _) -> set t.constructors name)
        | Record fields ->
          List.iter fields ~f:(fun (name, _) -> set t.fields name)
        | Malformed kind -> members kind
        | Alias _ | Abstract | Splice _ -> ()
      in
      members kind
    | Exception (name, _) | Malformed_exception (name, _) ->
      set t.constructors name
    | Module (name, toplevels) ->
      declarations
        t
        ~up_to_date
        ~values
        ~prefix:(prefix @ [ name.txt ])
        toplevels
    | Module_splice (name, _, _) ->
      Hash_set.add t.splices (splice_span toplevel);
      declarations
        t
        ~up_to_date
        ~values
        ~prefix:(prefix @ [ name.txt ])
        (up_to_date_expansion ~up_to_date toplevel)
    | Include _ ->
      Hash_set.add t.splices (splice_span toplevel);
      declarations
        t
        ~up_to_date
        ~values
        ~prefix
        (up_to_date_expansion ~up_to_date toplevel)
    | Expect_test _ | Bench _ -> ())
;;

let rec files ~prefix (group : Driver.Module_group.t) =
  match group with
  | File toplevel -> [ prefix, toplevel ]
  | Group (name, members) ->
    List.concat_map members ~f:(files ~prefix:(prefix @ [ name.txt ]))
;;

let resolve_type t ~module_path path =
  List.range ~stride:(-1) (List.length module_path) 0 ~stop:`inclusive
  |> List.map ~f:(List.take module_path)
  |> Fn.flip List.append [ [ Prelude_path.root ] ]
  |> List.find_map ~f:(fun enclosing ->
    let name = String.concat ~sep:"." (enclosing @ path) in
    Option.some_if (Hashtbl.mem t.types name) name)
;;

let create
  ?(prelude = [])
  ?(of_prelude = false)
  ~groups
  ~(program : Typed.Program.t)
  ~expansions
  ()
  =
  let up_to_date =
    List.filter_map expansions ~f:(fun (expansion : Expansion.t) ->
      match expansion.status with
      | Up_to_date -> Some (Span.of_loc expansion.expansion.loc)
      | Stale _ -> None)
    |> Span.Hash_set.of_list
  in
  let t =
    { entries = String.Table.create ()
    ; locals = Typed.Ident.Table.create ()
    ; values = String.Table.create ()
    ; types = String.Table.create ()
    ; constructors = String.Table.create ()
    ; fields = String.Table.create ()
    ; functions = String.Table.create ()
    ; splices = Span.Hash_set.create ()
    ; of_prelude
    }
  in
  declarations
    t
    ~up_to_date
    ~values:(String.Table.create ())
    ~prefix:[ Prelude_path.root ]
    prelude;
  let files = List.concat_map groups ~f:(files ~prefix:[]) in
  let values = String.Table.create () in
  List.iter files ~f:(fun (prefix, toplevel) ->
    declarations t ~up_to_date ~values ~prefix [ toplevel ]);
  let syntax = Syntax_at.create () in
  let bench_params = Span.Table.create () in
  let rec syntax_of_toplevel (toplevel : Syntax.Toplevel.t) =
    match toplevel.txt with
    | Module_splice (_, e, _) | Include (e, _) ->
      Syntax_at.expr syntax e;
      List.iter
        (up_to_date_expansion ~up_to_date toplevel)
        ~f:syntax_of_toplevel
    | Let (_, _, _, e) | Let_mut (_, _, e) | Expect_test { body = e; _ } ->
      Syntax_at.expr syntax e
    | Bench { param; body; name = _ } ->
      Option.iter param ~f:(fun { args; name } ->
        Hashtbl.set bench_params ~key:(Span.of_loc toplevel.loc) ~data:name;
        List.iter args ~f:(Syntax_at.expr syntax));
      Syntax_at.expr syntax body
    | Module (_, toplevels) -> List.iter toplevels ~f:syntax_of_toplevel
    | Type _ | Exception _ | Malformed_exception _ -> ()
  in
  List.iter files ~f:(fun (_, toplevel) -> syntax_of_toplevel toplevel);
  List.iter files ~f:(fun (module_path, toplevel) ->
    List.iter
      (Syntax_names.of_toplevels
         ~walk_expansion:(fun expansion ->
           Hash_set.mem up_to_date (Span.of_loc expansion.loc))
         ~module_path
         [ toplevel ])
      ~f:(fun { loc; kind; path; declaration = _; module_path; quoted } ->
        match kind with
        | Type ->
          let target =
            if quoted then None else resolve_type t ~module_path path
          in
          add
            t
            { loc
            ; label =
                Some
                  ("type "
                   ^
                   match target with
                   | Some target
                     when (not
                             (String.is_prefix
                                target
                                ~prefix:(Prelude_path.root ^ ".")))
                          || t.of_prelude -> target
                   | Some _ | None -> String.concat ~sep:"." path)
            ; ty = None
            ; target = Option.map target ~f:(fun name -> Target.Type name)
            }
        | Module
        | Constructor
        | Field
        | Poly_field
        | Poly_tag
        | Value
        | Let _
        | Parameter
        | Pattern_var -> ()));
  List.iter program.sccs ~f:(fun scc ->
    List.iter scc.members ~f:(fun { ident; args; scheme; body } ->
      if at_splice t body.loc
      then Hashtbl.add t.values ~key:ident.name ~data:body.loc |> ignore
      else
        Option.iter
          (Hashtbl.find values ident.name)
          ~f:(fun (name, bindings, (written : Syntax.Expr.t)) ->
            if Span.equal (Span.of_loc written.loc) (Span.of_loc body.loc)
            then (
              add_binder t name ~label:ident.name ~ty:scheme ~ident:None;
              local_binders t program (List.map bindings ~f:fst) args));
      expr t ~syntax ~program body));
  List.iter program.declaration_splices ~f:(expr t ~syntax ~program);
  List.iter program.expect_tests ~f:(fun test ->
    expr t ~syntax ~program test.body);
  List.iter program.benches ~f:(fun bench ->
    Option.iter bench.param ~f:(fun (ident, args) ->
      List.iter args ~f:(expr t ~syntax ~program);
      Option.iter (Map.find program.ident_info ident) ~f:(fun info ->
        Option.iter
          (Hashtbl.find bench_params (Span.of_loc bench.loc))
          ~f:(fun name ->
            add_binder t name ~label:ident.name ~ty:info.ty ~ident:(Some ident))));
    expr t ~syntax ~program bench.body);
  t
;;

let relocate t ~f =
  let entries = String.Table.create () in
  Hashtbl.iter t.entries ~f:(fun queue ->
    Queue.iter queue ~f:(fun (entry : Entry.t) ->
      let loc = f entry.loc in
      let target : Target.t option =
        match entry.target with
        | Some (Loc target) -> Some (Loc (f target))
        | target -> target
      in
      add { t with entries } { entry with loc; target }));
  let relocate_file file = (f (Loc.start_of_file file)).file in
  let file name = Loc.File.(to_string (relocate_file (of_string name))) in
  let functions = String.Table.create () in
  Hashtbl.iteri t.functions ~f:(fun ~key ~data ->
    Hashtbl.set functions ~key:(file key) ~data);
  { entries
  ; locals = Hashtbl.map t.locals ~f
  ; values = Hashtbl.map t.values ~f
  ; types = Hashtbl.map t.types ~f
  ; constructors = Hashtbl.map t.constructors ~f
  ; fields = Hashtbl.map t.fields ~f
  ; functions
  ; splices =
      Hash_set.to_list t.splices
      |> List.map ~f:(fun span ->
        { span with Span.file = relocate_file span.file })
      |> Span.Hash_set.of_list
  ; of_prelude = t.of_prelude
  }
;;

let contains (loc : Loc.t) offset =
  loc.start.#offset <= offset && offset <= loc.stop.#offset
;;

(* The innermost entry at [offset], the last added of those as small. *)
let entry_at t ~file ~offset =
  let%bind.Option entries = Hashtbl.find t.entries file in
  Queue.fold entries ~init:None ~f:(fun best (entry : Entry.t) ->
    if not (contains entry.loc offset)
    then best
    else (
      let size (entry : Entry.t) =
        entry.loc.stop.#offset - entry.loc.start.#offset
      in
      match best with
      | Some best when size best < size entry -> Some best
      | Some _ | None -> Some entry))
;;

let hover t ~file ~offset =
  let%bind.Option entry = entry_at t ~file ~offset in
  (* In the prelude's own sources, its names are relative to it. *)
  let name name =
    if t.of_prelude
    then Option.value (Prelude_path.relative name) ~default:name
    else name
  in
  let label =
    Option.map entry.label ~f:(fun label ->
      match String.chop_prefix label ~prefix:"type " with
      | Some type_ -> "type " ^ name type_
      | None -> name label)
  in
  let ty ty =
    let printer =
      Type.Printer.create ?type_name:(Option.some_if t.of_prelude name) ()
    in
    Type.Printer.ty printer ty ^ Type.Printer.where printer
  in
  let text =
    match label, entry.ty with
    | Some label, Some ty' -> Some (sprintf "%s : %s" label (ty ty'))
    | None, Some ty' -> Some (ty ty')
    | Some label, None -> Some label
    | None, None -> None
  in
  let%map.Option text in
  { Hover.loc = entry.loc; text = sprintf "```stag\n%s\n```" text }
;;

let definition t ~file ~offset =
  let%bind.Option entry = entry_at t ~file ~offset in
  let%bind.Option target = entry.target in
  match target with
  | Loc loc -> Some loc
  | Local ident -> Hashtbl.find t.locals ident
  | Value name -> Hashtbl.find t.values name
  | Type name -> Hashtbl.find t.types name
  | Constructor name -> Hashtbl.find t.constructors name
  | Field name -> Hashtbl.find t.fields name
;;

let is_function t ~file ~offset =
  match Hashtbl.find t.functions file with
  | Some offsets -> Hash_set.mem offsets offset
  | None -> false
;;
