open! Import

module Span = struct
  module T = struct
    type t =
      { file : string
      ; start : int
      ; stop : int
      }
    [@@deriving compare, equal, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)

  let of_loc (loc : Loc.t) =
    { file = loc.file; start = loc.start.offset; stop = loc.stop.offset }
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
  }

(* The syntax at a typed node's location that names what it doesn't keep. *)
module Syntax_at = struct
  type t =
    { lets : (Syntax.Name.t * Syntax.Binding.t list) Span.Table.t
    ; funs : Syntax.Binding.t list Span.Table.t
    ; constructors : Syntax.Decl_path.t Located.t Span.Table.t
    ; fields : Syntax.Decl_path.t Located.t list Span.Table.t
    ; rests : Syntax.Name.t Span.Table.t
    }

  let create () =
    { lets = Span.Table.create ()
    ; funs = Span.Table.create ()
    ; constructors = Span.Table.create ()
    ; fields = Span.Table.create ()
    ; rests = Span.Table.create ()
    }
  ;;

  let set table loc data = Hashtbl.set table ~key:(Span.of_loc loc) ~data

  let rec pattern t (p : Syntax.Pattern.t) =
    match p.txt with
    | Any | Var _ | Lit _ -> ()
    | Tuple ps -> List.iter ps ~f:(pattern t)
    | Construct (path, ps) ->
      set t.constructors p.loc path;
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
    | Lit _ | Var _ | Type _ | Expect _ -> ()
    | App (a, b) ->
      recurse a;
      recurse b
    | Let (name, bindings, _, body, in_) ->
      set t.lets e.loc (name, bindings);
      recurse body;
      recurse in_
    | Fun (bindings, body) ->
      set t.funs e.loc bindings;
      recurse body
    | Splice { expr; subs } ->
      recurse expr;
      Option.iter
        subs
        ~f:(List.iter ~f:(fun (_, e) -> Option.iter e ~f:recurse))
    | Quote e -> recurse e
    | Tuple es -> List.iter es ~f:recurse
    | Construct (path, es) ->
      set t.constructors e.loc path;
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
      (Hashtbl.find_or_add t.functions loc.file ~default:Int.Hash_set.create)
      loc.start.offset
;;

let add t (entry : Entry.t) =
  if not (Loc.is_none entry.loc || at_splice t entry.loc)
  then
    Queue.enqueue
      (Hashtbl.find_or_add t.entries entry.loc.file ~default:Queue.create)
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
  (match e.desc with
   | Var var ->
     let label, target =
       match var with
       | Bound binder | Persistent binder ->
         (match binder with
          | Local ident -> ident.name, Some (Target.Local ident)
          | Toplevel ident -> ident.name, Some (Value ident.name))
       | Free name -> name, None
       | Free_toplevel name -> name, Some (Value name)
       | Prim prim -> Sexp.to_string (Prim.sexp_of_t prim), None
       | Intrinsic intrinsic -> Intrinsic.public_name intrinsic, None
     in
     if is_arrow e.ty then add_function t e.loc;
     typed ~label ?target ()
   | Let { ident; args; _ } ->
     typed ();
     Option.iter (at syntax.Syntax_at.lets) ~f:(fun (name, bindings) ->
       local_binders t program (name :: List.map bindings ~f:fst) (ident :: args))
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
   | App _
   | Splice _
   | Toplevel_splice _
   | Quote _
   | Tuple _
   | Poly_record _
   | Poly_field _
   | Poly_extend _
   | Poly_restrict _
   | Poly_update _
   | Poly_variant _
   | Expect _
   | Type _ -> typed ());
  match e.desc with
  | Toplevel_splice { code; expansion = _ } -> recurse code
  | _ -> List.iter (Typed.Expr.sub_exprs e) ~f:recurse
;;

(* The declarations of [toplevels], in the module [prefix] (eg. ["Foo"; "A"]),
   by canonical name. *)
let rec declarations t ~values ~prefix (toplevels : Syntax.Toplevel.t list) =
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
    | Type { name; params = _; kind } ->
      set t.types name;
      (match kind with
       | Variant constructors ->
         List.iter constructors ~f:(fun (name, _) -> set t.constructors name)
       | Record fields ->
         List.iter fields ~f:(fun (name, _) -> set t.fields name)
       | Alias _ | Abstract -> ())
    | Exception (name, _) -> set t.constructors name
    | Module (name, toplevels) ->
      declarations t ~values ~prefix:(prefix @ [ name.txt ]) toplevels
    | Module_splice _ | Include _ ->
      Hash_set.add t.splices (Span.of_loc toplevel.loc)
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
  |> List.find_map ~f:(fun depth ->
    let name = String.concat ~sep:"." (List.take module_path depth @ path) in
    Option.some_if (Hashtbl.mem t.types name) name)
;;

let create ~groups ~(program : Typed.Program.t) =
  let t =
    { entries = String.Table.create ()
    ; locals = Typed.Ident.Table.create ()
    ; values = String.Table.create ()
    ; types = String.Table.create ()
    ; constructors = String.Table.create ()
    ; fields = String.Table.create ()
    ; functions = String.Table.create ()
    ; splices = Span.Hash_set.create ()
    }
  in
  let files = List.concat_map groups ~f:(files ~prefix:[]) in
  let values = String.Table.create () in
  List.iter files ~f:(fun (prefix, toplevel) ->
    declarations t ~values ~prefix [ toplevel ]);
  let syntax = Syntax_at.create () in
  let bench_params = Span.Table.create () in
  let rec syntax_of_toplevel (toplevel : Syntax.Toplevel.t) =
    match toplevel.txt with
    | Let (_, _, _, e)
    | Module_splice (_, e)
    | Include e
    | Expect_test { body = e; _ } -> Syntax_at.expr syntax e
    | Bench { param; body; name = _ } ->
      Option.iter param ~f:(fun { args; name } ->
        Hashtbl.set bench_params ~key:(Span.of_loc toplevel.loc) ~data:name;
        List.iter args ~f:(Syntax_at.expr syntax));
      Syntax_at.expr syntax body
    | Module (_, toplevels) -> List.iter toplevels ~f:syntax_of_toplevel
    | Type _ | Exception _ -> ()
  in
  List.iter files ~f:(fun (_, toplevel) -> syntax_of_toplevel toplevel);
  List.iter files ~f:(fun (module_path, toplevel) ->
    List.iter
      (Syntax_names.of_toplevels ~module_path [ toplevel ])
      ~f:(fun { loc; kind; path; declaration = _; module_path } ->
        match kind with
        | Type ->
          let target = resolve_type t ~module_path path in
          add
            t
            { loc
            ; label =
                Some
                  ("type "
                   ^ Option.value target ~default:(String.concat ~sep:"." path)
                  )
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
  let file name = (f (Loc.start_of_file name)).file in
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
      |> List.map ~f:(fun span -> { span with Span.file = file span.file })
      |> Span.Hash_set.of_list
  }
;;

let contains (loc : Loc.t) offset =
  loc.start.offset <= offset && offset <= loc.stop.offset
;;

(* The innermost entry at [offset], the last added of those as small. *)
let entry_at t ~file ~offset =
  let%bind.Option entries = Hashtbl.find t.entries file in
  Queue.fold entries ~init:None ~f:(fun best (entry : Entry.t) ->
    if not (contains entry.loc offset)
    then best
    else (
      let size (entry : Entry.t) =
        entry.loc.stop.offset - entry.loc.start.offset
      in
      match best with
      | Some best when size best < size entry -> Some best
      | Some _ | None -> Some entry))
;;

let hover t ~file ~offset =
  let%bind.Option entry = entry_at t ~file ~offset in
  let text =
    match entry.label, entry.ty with
    | Some label, Some ty ->
      Some (sprintf "%s : %s" label (Type.to_string_hum ty))
    | None, Some ty -> Some (Type.to_string_hum ty)
    | Some label, None -> Some label
    | None, None -> None
  in
  let%map.Option text = text in
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
