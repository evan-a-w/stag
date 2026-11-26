open! Import
open Syntax

module Namespace = struct
  type t =
    | Value
    | Type
    | Constructor
    | Field
  [@@deriving sexp_of, compare, equal, hash]
end

(* Members are named per namespace, and modules too. *)
module Member_kind = struct
  type t =
    | Member of Namespace.t
    | Module
  [@@deriving sexp_of, compare, equal, hash]

  let duplicate t name : Typecheck_error.Kind.t =
    match t with
    | Member Value -> Duplicate_toplevel name
    | Member Type -> Duplicate_type name
    | Member Constructor -> Duplicate_constructor name
    | Member Field -> Duplicate_field name
    | Module -> Duplicate_module name
  ;;
end

module Member = struct
  type t = Member_kind.t * string [@@deriving sexp_of, compare, hash]

  include functor Hashable.Make_plain
end

(* The k-th entry of a module is at [At [k]], the j-th an include at [At p]
   generates at [At (p @ [j])]. Lists compare lexicographically, a prefix first,
   so an include is before what it generates. *)
module Position = struct
  type t =
    | At of int list
    | End
  [@@deriving compare]

  let before t t' = compare t t' < 0
end

module Force_error = struct
  type t =
    | Failed of Typecheck_error.t
    | Uses_unfinished
end

type module_ =
  { members : splice option Member.Table.t
      (* By the include generating them, [None] for the others. *)
  ; modules : module_ String.Table.t
  ; entries : entry Queue.t
  ; includes : splice Queue.t
      (* Its own includes and types' definitions [type t = .~e], and those they
         generate. *)
  ; definitions : splice String.Table.t (* By type name. *)
  ; path : string list
  ; loc : Loc.t
  ; enclosing : t option
  ; splice : splice option (* Of a spliced module. *)
  ; program : program
  ; warnings : Warning.Settings.t (* At its start. *)
  }

and t =
  { module_ : module_
  ; position : Position.t
  ; warnings : Warning.Settings.t
  }

and entry =
  | Written of
      { toplevel : Toplevel.t
      ; scope : t
      ; generated_by : splice option
      }
  | Included of splice

and splice =
  { expr : Expr.t
  ; toplevel : Toplevel.t
  ; loc : Loc.t
  ; scope : t (* Where it is written, at its position. *)
  ; into : into
  ; mutable state : state
  }

and into =
  | Into_include of entry Queue.t
  | Into_module of string
  | Into_type of Type_decl.t (* The definition of this written type. *)

and state =
  | Pending
  | Forcing
  | Done of Toplevel.t list
  | Failed (* Its error reported. *)

(* Shared by the modules of a root. *)
and program =
  { declare : t -> Toplevel.t -> generated_by:splice option -> unit
  ; force : splice -> (Toplevel.t list, Force_error.t) Result.t
  ; report : Typecheck_error.t -> unit
  ; outside : t option (* The root's enclosing scope. *)
  }

module Splice = struct
  type nonrec t = splice

  let expr (t : t) = t.expr
  let toplevel (t : t) = t.toplevel
  let loc (t : t) = t.loc
  let module_ (t : t) = t.scope

  let defines_type (t : t) =
    match t.into with
    | Into_type _ -> true
    | Into_include _ | Into_module _ -> false
  ;;

  let generated (t : t) =
    match t.state with
    | Done toplevels -> Some toplevels
    | Pending | Forcing | Failed -> None
  ;;
end

let join path = String.concat path ~sep:"."
let sexp_of_t t = Sexp.Atom (join t.module_.path)
let canonical_name t name = join (t.module_.path @ [ name ])

let at_end module_ = { module_; position = End; warnings = module_.warnings }
let enter t name = at_end (Hashtbl.find_exn t.module_.modules name)
let warnings (t : t) = t.warnings

let create ~path ~loc ~enclosing ~splice ~program ~warnings =
  { members = Member.Table.create ()
  ; modules = String.Table.create ()
  ; entries = Queue.create ()
  ; includes = Queue.create ()
  ; definitions = String.Table.create ()
  ; path
  ; loc
  ; enclosing
  ; splice
  ; program
  ; warnings
  }
;;

let create_root
  ?enclosing
  ?(path = [])
  ?(warnings = Warning.Settings.default)
  ~types
  ~declare
  ~force
  ~report
  ()
  =
  let module_ =
    create
      ~path
      ~loc:Loc.none
      ~enclosing
      ~splice:None
      ~program:{ declare; force; report; outside = enclosing }
      ~warnings
  in
  List.iter types ~f:(fun name ->
    Hashtbl.set module_.members ~key:(Member Type, name) ~data:None);
  at_end module_
;;

let rec declarations_of entries =
  Queue.to_list entries
  |> List.concat_map ~f:(function
    | Written { toplevel; _ } -> [ toplevel ]
    | Included splice ->
      (match splice.into with
       | Into_include entries -> declarations_of entries
       | Into_module _ | Into_type _ -> []))
;;

let declarations t = declarations_of t.module_.entries

(* The members [toplevel] adds to the module it is in. *)
let rec members (toplevel : Toplevel.t) : (Member.t * Name.t) list =
  let member namespace (name : Name.t) =
    (Member_kind.Member namespace, name.txt), name
  in
  match toplevel.txt with
  | Attributed (toplevel, _) -> members toplevel
  | Let (name, _, _, _) | Let_mut (name, _, _) | External { name; _ } ->
    [ member Value name ]
  | Type { name; params = _; kind } ->
    member Type name
    ::
    (let rec kind_members : Type_decl.Kind.t -> _ = function
       | Alias _ | Abstract | Splice _ -> []
       | Malformed kind -> kind_members kind
       | Variant constructors ->
         List.map constructors ~f:(fun (name, _) -> member Constructor name)
       | Record fields ->
         List.map fields ~f:(fun (name, _) -> member Field name)
     in
     kind_members kind)
  | Exception (name, _) | Malformed_exception (name, _) ->
    [ member Constructor name ]
  | Module (name, _) | Module_splice (name, _, _) ->
    [ (Module, name.txt), name ]
  | Expect_test _ | Bench _ | Include _ | Attribute _ -> []
;;

(* [toplevels] but those whose names, or type parameters, are taken by those
   before them or by [taken], the members of the module [path] they are added
   to, and with types' constructors and fields so taken left out, each reported. *)
let rec unique_among ~path taken toplevels ~report =
  List.filter_map toplevels ~f:(fun (written : Toplevel.t) ->
    let error ~loc kind =
      report (Typecheck_error.create ~loc kind);
      None
    in
    let toplevel, _ = Toplevel.unattributed written in
    let params_unique =
      match toplevel.txt with
      | Type { name; params; kind = _ } ->
        let type_name = join (path @ [ name.txt ]) in
        List.fold_until
          params
          ~init:String.Set.empty
          ~f:(fun seen { txt = param; loc } ->
            let param = Type_decl.Param.name param in
            if Set.mem seen param
            then
              Stop (error ~loc (Duplicate_type_parameter { type_name; param }))
            else Continue (Set.add seen param))
          ~finish:(fun _ -> Some ())
      | Let _
      | Let_mut _
      | Exception _
      | External _
      | Malformed_exception _
      | Module _
      | Module_splice _
      | Expect_test _
      | Bench _
      | Include _
      | Attributed _
      | Attribute _ -> Some ()
    in
    let%bind.Option () = params_unique in
    let added = Member.Hash_set.create () in
    let duplicates =
      List.filter (members toplevel) ~f:(fun (member, _) ->
        Hash_set.mem taken member
        || Hash_set.strict_add added member
           |> Hash_set.Ok_or_duplicate.is_duplicate)
    in
    let report_duplicate ((kind, name), (at : Name.t)) =
      error ~loc:at.loc (Member_kind.duplicate kind (join (path @ [ name ])))
    in
    match duplicates, toplevel.txt with
    | ((_, (at : Name.t)) as duplicate) :: _, _
      when phys_equal at (snd (List.hd_exn (members toplevel))) ->
      report_duplicate duplicate
    | _ :: _, Type decl ->
      (* Kept without its duplicate constructors or fields. *)
      List.iter duplicates ~f:(fun duplicate ->
        ignore (report_duplicate duplicate : unit option));
      Hash_set.iter added ~f:(Hash_set.add taken);
      let kept (name, _) =
        not (List.exists duplicates ~f:(fun (_, at) -> phys_equal at name))
      in
      let rec kept_kind : Type_decl.Kind.t -> Type_decl.Kind.t = function
        | Variant constructors -> Variant (List.filter constructors ~f:kept)
        | Record fields -> Record (List.filter fields ~f:kept)
        | Malformed kind -> Malformed (kept_kind kind)
        | (Alias _ | Abstract | Splice _) as kind -> kind
      in
      Some
        (Toplevel.map_unattributed written ~f:(fun toplevel ->
           { toplevel with
             txt = Toplevel.Type { decl with kind = kept_kind decl.kind }
           }))
    | ( _ :: _
      , ( Let _
        | Let_mut _
        | Exception _
        | External _
        | Malformed_exception _
        | Module _
        | Module_splice _
        | Expect_test _
        | Bench _
        | Include _
        | Attributed _
        | Attribute _ ) ) ->
      (* Their only member, if any, is their name. *)
      assert false
    | [], _ ->
      Hash_set.iter added ~f:(Hash_set.add taken);
      Some
        (Toplevel.map_unattributed written ~f:(fun toplevel ->
           match toplevel.txt with
           | Module (name, toplevels) ->
             { toplevel with
               txt =
                 Toplevel.Module
                   ( name
                   , unique_among
                       ~path:(path @ [ name.txt ])
                       (Member.Hash_set.create ())
                       toplevels
                       ~report )
             }
           | Let _
           | Let_mut _
           | Type _
           | Exception _
           | External _
           | Malformed_exception _
           | Module_splice _
           | Expect_test _
           | Bench _
           | Include _
           | Attributed _
           | Attribute _ -> toplevel)))
;;

let unique module_ toplevels ~report =
  unique_among
    ~path:module_.path
    (Member.Hash_set.of_list (Hashtbl.keys module_.members))
    toplevels
    ~report
;;

let submodule (scope : t) (name : Name.t) ~splice =
  let module_ =
    create
      ~path:(scope.module_.path @ [ name.txt ])
      ~loc:name.loc
      ~enclosing:(Some scope)
      ~splice
      ~program:scope.module_.program
      ~warnings:scope.warnings
  in
  Hashtbl.add_exn scope.module_.modules ~key:name.txt ~data:module_;
  module_
;;

(* Adds [toplevels] to [module_], in place of the include [parent] if given. *)
let rec register module_ toplevels ~parent ~generated_by =
  let prefix, into, warnings =
    match parent with
    | None -> [], module_.entries, module_.warnings
    | Some
        { scope = { position = At prefix; warnings; _ }
        ; into = Into_include into
        ; _
        } -> prefix, into, warnings
    | Some _ -> raise_s [%message "Module_scope.register: not an include"]
  in
  ignore
    (List.fold toplevels ~init:warnings ~f:(fun warnings written ->
       let toplevel, attributes = Toplevel.unattributed written in
       let scope =
         { module_
         ; position = At (prefix @ [ Queue.length into ])
         ; warnings = Attribute.apply_warnings attributes warnings
         }
       in
       let splice (expr : Expr.t) ~expansion ~into =
         let loc =
           match expansion with
           | None -> toplevel.loc
           | Some _ -> { toplevel.loc with stop = expr.loc.stop }
         in
         { expr; toplevel; loc; scope; into; state = Pending }
       in
       match toplevel.txt with
       | Attribute attribute -> Attribute.apply_warnings [ attribute ] warnings
       | Include (expr, expansion) ->
         let splice =
           splice expr ~expansion ~into:(Into_include (Queue.create ()))
         in
         Queue.enqueue into (Included splice);
         Queue.enqueue module_.includes splice;
         warnings
       | Let _
       | Let_mut _
       | Type _
       | Exception _
       | External _
       | Malformed_exception _
       | Expect_test _
       | Bench _
       | Module _
       | Module_splice _
       | Attributed _ ->
         module_.program.declare scope written ~generated_by;
         Queue.enqueue into (Written { toplevel = written; scope; generated_by });
         List.iter (members toplevel) ~f:(fun (member, _) ->
           Hashtbl.add_exn module_.members ~key:member ~data:parent);
         (match toplevel.txt with
          | Module (name, toplevels) ->
            let module_ = submodule scope name ~splice:None in
            register module_ toplevels ~parent:None ~generated_by
          | Module_splice (name, expr, expansion) ->
            ignore
              (submodule
                 scope
                 name
                 ~splice:
                   (Some (splice expr ~expansion ~into:(Into_module name.txt)))
               : module_)
          | Type ({ kind = Splice e; _ } as decl) ->
            let splice =
              { expr = e.txt
              ; toplevel
              ; loc = e.loc
              ; scope
              ; into = Into_type decl
              ; state = Pending
              }
            in
            Hashtbl.add_exn module_.definitions ~key:decl.name.txt ~data:splice;
            Queue.enqueue module_.includes splice
          | Let _
          | Let_mut _
          | Type _
          | Exception _
          | External _
          | Malformed_exception _
          | Expect_test _
          | Bench _
          | Include _
          | Attributed _
          | Attribute _ -> ());
         warnings)
     : Warning.Settings.t)
;;

let add t toplevels =
  register
    t.module_
    (unique t.module_ toplevels ~report:t.module_.program.report)
    ~parent:None
    ~generated_by:None
;;

let in_scope (t : t) : Typecheck_error.In_scope.t =
  let outside = t.module_.program.outside in
  let rec modules (t : t) =
    t.module_
    ::
    (match t.module_.enclosing with
     | Some enclosing
       when not
              (Option.exists outside ~f:(fun outside ->
                 phys_equal outside.module_ enclosing.module_)) ->
       modules enclosing
     | Some _ | None -> [])
  in
  let modules = modules t in
  let names ?(modules = modules) kind =
    List.concat_map modules ~f:(fun module_ ->
      Hashtbl.to_alist module_.members
      |> List.filter_map ~f:(fun ((kind', name), generated_by) ->
        Option.some_if
          ([%equal: Member_kind.t] kind kind' && Option.is_none generated_by)
          name))
    |> String.Set.of_list
  in
  { types = names (Member Type)
  ; modules = names Module
  ; root_modules = names ~modules:[ List.last_exn modules ] Module
  }
;;

(* The program's root module, which root paths are found from: past it are the
   prelude and the builtin types. *)
let root t =
  let outside = t.module_.program.outside in
  let rec go (t : t) =
    match t.module_.enclosing with
    | Some enclosing
      when not
             (Option.exists outside ~f:(fun outside ->
                phys_equal outside.module_ enclosing.module_)) -> go enclosing
    | Some _ | None -> { t with position = End }
  in
  go t
;;

(* Where a written path is found from, and its names. *)
let start t (path : Decl_path.t) =
  match path with
  | Path names -> t, names
  | Root names -> root t, names
  | Resolved _ ->
    raise_s [%message "Module_scope: a resolved path is not looked up"]
;;

(* [path] with the names [names], from where it is. *)
let with_names (path : Decl_path.t) names : Decl_path.t =
  match path with
  | Path _ -> Path (Path.of_list_exn names)
  | Root _ -> Root (Path.of_list_exn names)
  | Resolved _ -> path
;;

let within t error = Typecheck_error.within error ~in_scope:(lazy (in_scope t))

let splice_kind (splice : splice) : Typecheck_error.Splice_kind.t =
  match splice.into with
  | Into_type _ -> Type_definition
  | Into_include _ | Into_module _ -> Include
;;

(* [module_] is the canonical name of the module [splice] generates, if it is
   not an include. *)
let cycle ?module_ (splice : splice) ~name ~loc =
  Typecheck_error.create
    ~loc
    (Declaration_splice_cycle
       { name = Decl_path.to_string name
       ; module_
       ; splice = splice.loc
       ; splice_kind = splice_kind splice
       })
;;

(* Runs [splice], which is not running, adding what it generates to its module,
   or, for an include, to the module it is in, in its place. If it uses an
   unfinished toplevel, it is left pending. *)
module Forced = struct
  type t =
    | Forced
    | Failed
    | Uses_unfinished
end

let force (splice : splice) : Forced.t =
  match splice.state with
  | Done _ -> Forced
  | Failed -> Failed
  | Forcing -> raise_s [%message "Module_scope.force: already running"]
  | Pending ->
    splice.state <- Forcing;
    let in_module = splice.scope.module_ in
    let failed () : Forced.t =
      splice.state <- Failed;
      Failed
    in
    let done_ toplevels : Forced.t =
      splice.state <- Done toplevels;
      Forced
    in
    (match in_module.program.force splice with
     | Error Uses_unfinished ->
       splice.state <- Pending;
       Uses_unfinished
     | Error (Failed error) ->
       in_module.program.report error;
       failed ()
     | Ok toplevels ->
       let report error =
         in_module.program.report (within splice.scope error)
       in
       (match splice.into with
        | Into_type decl ->
          let type_member = Member_kind.Member Type, decl.name.txt in
          let taken =
            Member.Hash_set.of_list (Hashtbl.keys in_module.members)
          in
          Hash_set.remove taken type_member;
          (match unique_among ~path:in_module.path taken toplevels ~report with
           | [] -> failed ()
           | toplevels ->
             List.iter toplevels ~f:(fun toplevel ->
               in_module.program.declare
                 splice.scope
                 toplevel
                 ~generated_by:(Some splice);
               List.iter (members toplevel) ~f:(fun (member, _) ->
                 if not ([%compare.equal: Member.t] member type_member)
                 then
                   Hashtbl.add_exn
                     in_module.members
                     ~key:member
                     ~data:(Some splice)));
             done_ toplevels)
        | Into_include _ ->
          let toplevels = unique in_module toplevels ~report in
          register
            in_module
            toplevels
            ~parent:(Some splice)
            ~generated_by:(Some splice);
          done_ toplevels
        | Into_module name ->
          let module_ = Hashtbl.find_exn in_module.modules name in
          let toplevels = unique module_ toplevels ~report in
          register module_ toplevels ~parent:None ~generated_by:(Some splice);
          done_ toplevels))
;;

let is_failed (splice : splice) =
  match splice.state with
  | Failed -> true
  | Pending | Forcing | Done _ -> false
;;

let is_before (splice : splice) position =
  Position.before splice.scope.position position
;;

(* Whether [splice] may generate [member]: a type's definition generates only
   constructors or fields. *)
let may_generate (splice : splice) ((kind, _) : Member.t) =
  match splice.into, kind with
  | Into_type _, Member (Constructor | Field) -> true
  | Into_type _, (Member (Value | Type) | Module) -> false
  | (Into_include _ | Into_module _), _ -> true
;;

let first splices =
  List.min_elt splices ~compare:(fun (splice : splice) (splice' : splice) ->
    Position.compare splice.scope.position splice'.scope.position)
;;

module Lookup = struct
  type t =
    | Visible
    | Hidden of splice (* Generated by a splice after the position. *)
    | Absent of { blocked : splice option }
end

(* [member] of [t]'s module, seen from [t]. While it is not found, the first
   splice before [t] that may generate it, not yet forced, is. Unless it is
   running or uses an unfinished toplevel: then what it generates is unknown,
   and it is [blocked]. *)
let rec lookup (t : t) member =
  match Hashtbl.find t.module_.members member with
  | Some None -> Ok Lookup.Visible
  | Some (Some splice) ->
    Ok (if is_before splice t.position then Lookup.Visible else Hidden splice)
  | None ->
    (match
       Queue.to_list t.module_.includes
       |> List.filter ~f:(fun splice ->
         may_generate splice member
         && is_before splice t.position
         &&
         match splice.state with
         | Done _ | Failed -> false
         | Pending | Forcing -> true)
       |> first
     with
     | None -> Ok (Absent { blocked = None })
     | Some ({ state = Forcing; _ } as splice) ->
       Ok (Absent { blocked = Some splice })
     | Some splice ->
       (match force splice with
        | Forced | Failed -> lookup t member
        | Uses_unfinished -> Ok (Absent { blocked = Some splice })))
;;

(* Whether a splice that failed may have generated [member], not found from [t]. *)
let rec after_failed_splice (t : t) member =
  Queue.exists t.module_.includes ~f:(fun splice ->
    is_failed splice
    && may_generate splice member
    && is_before splice t.position)
  || Option.exists t.module_.enclosing ~f:(fun t ->
    after_failed_splice t member)
;;

module Found = struct
  type nonrec t =
    { scope : t option
    ; blocked : splice option (* The innermost include looked past. *)
    }
end

(* The scope [member] is found in from [t] outwards. *)
let find_scope t member =
  let rec search (t : t) ~blocked =
    match%bind.Result lookup t member with
    | Visible -> Ok { Found.scope = Some t; blocked }
    | Hidden _ -> outward t ~blocked
    | Absent { blocked = here } ->
      outward t ~blocked:(Option.first_some blocked here)
  and outward (t : t) ~blocked =
    match t.module_.enclosing with
    | Some t -> search t ~blocked
    | None -> Ok { Found.scope = None; blocked }
  in
  search t ~blocked:None
;;

(* The splice after [t] that generates [member] ([Hidden]), or may but is
   blocked ([Absent]), in its module or one enclosing it, for messages: all
   their splices are forced, up to one that fails. *)
let rec generated_after (t : t) member =
  match lookup { t with position = End } member, lookup t member with
  | Ok Visible, Ok (Hidden splice) -> Some (Lookup.Hidden splice)
  | ( Ok (Absent { blocked = Some ({ state = Pending; _ } as splice) })
    , Ok (Absent { blocked = None }) ) ->
    Some (Absent { blocked = Some splice })
  | _ -> Option.bind t.module_.enclosing ~f:(fun t -> generated_after t member)
;;

let with_note (error : Typecheck_error.Kind.t) ~blocked ~after
  : Typecheck_error.Kind.t
  =
  match blocked, after with
  | Some (splice : splice), _
  | None, Some (Lookup.Absent { blocked = Some splice }) ->
    Maybe_generated_by_running_include
      { error
      ; include_ = splice.loc
      ; splice_kind = splice_kind splice
      ; after = Option.is_none blocked
      }
  | None, Some (Hidden splice) ->
    Generated_after_include
      { error; include_ = splice.loc; splice_kind = splice_kind splice }
  | None, (Some (Visible | Absent { blocked = None }) | None) -> error
;;

(* The location of the module path [written] at the start of the path at [loc],
   if written without spaces as for [path]. *)
let prefix_loc (loc : Loc.t) ~(path : Decl_path.t) ~written =
  let len = String.length (Decl_path.to_string (with_names path written)) in
  if loc.start.#line = loc.stop.#line
     && loc.stop.#offset - loc.start.#offset
        = String.length (Decl_path.to_string path)
  then
    { loc with
      stop =
        #{ loc.start with
           col = loc.start.#col + len
         ; offset = loc.start.#offset + len
         }
    }
  else loc
;;

(* The member module [name] of [module_], with its declarations generated. *)
let open_module module_ name ~path ~loc =
  let module_ = Hashtbl.find_exn module_.modules name in
  let cycle splice =
    cycle splice ~module_:(join module_.path) ~name:path ~loc
  in
  let%map.Result () =
    match module_.splice with
    | None -> Ok ()
    | Some splice ->
      (match splice.state with
       | Forcing -> Error (cycle splice)
       | Pending | Done _ | Failed ->
         (match force splice with
          | Forced -> Ok ()
          | Failed -> Error Typecheck_error.already_reported
          | Uses_unfinished -> Error (cycle splice)))
  in
  module_
;;

(* The declarations a splice generates, which are, or will be, those of the
   module [path] or in it, which is the spliced module if [spliced]. *)
module Generated = struct
  type t =
    { path : string list
    ; toplevels : Toplevel.t list
    ; spliced : bool
    }

  module Found = struct
    type t =
      | Found of string
      | Absent (* Its first name is not declared. *)
      | Unbound
  end

  let rec find toplevels ~path namespace names : Found.t =
    match names with
    | [] -> assert false
    | name :: rest ->
      let member : Member.t =
        match rest with
        | [] -> Member namespace, name
        | _ :: _ -> Module, name
      in
      (match
         List.find toplevels ~f:(fun toplevel ->
           List.exists (members toplevel) ~f:(fun (member', _) ->
             [%compare.equal: Member.t] member member'))
       with
       | None -> Absent
       | Some toplevel ->
         (match rest, toplevel.txt with
          | [], _ -> Found (join (path @ [ name ]))
          | _ :: _, Module (_, toplevels) ->
            (match find toplevels ~path:(path @ [ name ]) namespace rest with
             | Found name -> Found name
             | Absent | Unbound -> Unbound)
          | _ :: _, _ -> Unbound))
  ;;

  (* [names] from within [module_], if they name [t]'s declarations, so that
     they are found before [t] is added to its module. *)
  let find_from t (module_ : module_) namespace names : Found.t =
    match t with
    | None -> Absent
    | Some t ->
      if List.equal String.equal module_.path t.path
      then find t.toplevels ~path:t.path namespace names
      else (
        match names with
        | head :: (_ :: _ as rest)
          when t.spliced
               && List.equal String.equal (module_.path @ [ head ]) t.path ->
          (match find t.toplevels ~path:t.path namespace rest with
           | Found name -> Found name
           | Absent | Unbound -> Unbound)
        | _ -> Absent)
  ;;
end

(* [name] of [module_] then [rest], from outside it, past [blocked]. Errors of
   names not found are passed through [hint]. *)
let rec find_member
  ?generated
  module_
  namespace
  name
  rest
  ~path
  ~written
  ~loc
  ~unbound
  ~blocked
  ~hint
  =
  match Generated.find_from generated module_ namespace (name :: rest) with
  | Found name -> Ok name
  | Unbound -> Error (Typecheck_error.create ~loc (unbound path))
  | Absent ->
    find_written
      ?generated
      module_
      namespace
      name
      rest
      ~path
      ~written
      ~loc
      ~unbound
      ~blocked
      ~hint

and find_written
  ?generated
  module_
  namespace
  name
  rest
  ~path
  ~written
  ~loc
  ~unbound
  ~blocked
  ~hint
  =
  let written = written @ [ name ] in
  let member : Member.t =
    match rest with
    | [] -> Member namespace, name
    | _ :: _ -> Module, name
  in
  match%bind.Result lookup (at_end module_) member with
  | Visible ->
    (match rest with
     | [] -> Ok (join (module_.path @ [ name ]))
     | next :: rest ->
       let%bind.Result module_ = open_module module_ name ~path ~loc in
       find_member
         ?generated
         module_
         namespace
         next
         rest
         ~path
         ~written
         ~loc
         ~unbound
         ~blocked
         ~hint)
  | (Hidden _ | Absent _) as found ->
    let blocked =
      match found with
      | Absent { blocked = here } -> Option.first_some blocked here
      | Hidden _ | Visible -> blocked
    in
    let kind, loc =
      match rest with
      | [] -> unbound path, loc
      | _ :: _ ->
        ( Typecheck_error.Kind.Unbound_module (with_names path written)
        , prefix_loc loc ~path ~written )
    in
    if after_failed_splice (at_end module_) member
    then Error Typecheck_error.already_reported
    else
      Error
        (hint
           (Typecheck_error.create ~loc (with_note kind ~blocked ~after:None)))
;;

let rec has_path module_ namespace = function
  | [] -> false
  | [ name ] -> Hashtbl.mem module_.members (Member namespace, name)
  | name :: rest ->
    Hashtbl.find module_.modules name
    |> Option.exists ~f:(fun module_ -> has_path module_ namespace rest)
;;

(* [error] for [head :: rest], found in [found], unless it shadows the prelude's
   [head], which has [rest]. *)
let with_prelude_hint found ~head ~rest namespace (error : Typecheck_error.t) =
  match found.module_.program.outside with
  | Some prelude when has_path prelude.module_ namespace (head :: rest) ->
    { error with
      kind =
        Shadowed_prelude_module
          { error = error.kind
          ; module_ = head
          ; shadowed_by = (Hashtbl.find_exn found.module_.modules head).loc
          }
    }
  | Some _ | None -> error
;;

module Resolved = struct
  type t =
    | Found of string
    | Unbound (* Its first name is not found. *)
end

(* What [path] refers to in the program from [t], and the innermost include
   looked past to find its first name. *)
let find_path ?generated t namespace (path : Decl_path.t) ~loc ~unbound =
  let t, names = start t path in
  match (names :> string list) with
  | [] -> assert false
  | [ name ] ->
    let%map.Result { scope; blocked } = find_scope t (Member namespace, name) in
    ( (match scope with
       | Some scope -> Resolved.Found (canonical_name scope name)
       | None -> Unbound)
    , blocked )
  | head :: member :: rest ->
    (match%bind.Result find_scope t (Module, head) with
     | { scope = None; blocked } -> Ok (Resolved.Unbound, blocked)
     | { scope = Some found; blocked } ->
       (match
          Generated.find_from
            generated
            found.module_
            namespace
            (names :> string list)
        with
        | Found name -> Ok (Resolved.Found name, blocked)
        | Unbound -> Error (Typecheck_error.create ~loc (unbound path))
        | Absent ->
          let%bind.Result module_ = open_module found.module_ head ~path ~loc in
          let%map.Result name =
            find_member
              ?generated
              module_
              namespace
              member
              rest
              ~path
              ~written:[ head ]
              ~loc
              ~unbound
              ~blocked
              ~hint:
                (with_prelude_hint found ~head ~rest:(member :: rest) namespace)
          in
          Resolved.Found name, blocked))
;;

(* The error for [path], whose first name is not found from [t] past [blocked]. *)
let unbound_head t namespace (path : Decl_path.t) ~loc ~unbound ~blocked =
  let t, names = start t path in
  let member, kind, loc =
    match (names :> string list), namespace with
    | [], _ -> assert false
    | [ name ], _ -> (Member_kind.Member namespace, name), unbound path, loc
    | head :: _, Namespace.Value when Prim.is_module head ->
      (Module, head), unbound path, loc
    | head :: _, _ ->
      ( (Module, head)
      , Typecheck_error.Kind.Unbound_module (with_names path [ head ])
      , prefix_loc loc ~path ~written:[ head ] )
  in
  if after_failed_splice t member
  then Typecheck_error.already_reported
  else (
    let after =
      match blocked with
      | Some _ -> None
      | None -> generated_after t member
    in
    Typecheck_error.create ~loc (with_note kind ~blocked ~after))
;;

let find t namespace path ~loc ~unbound =
  match%bind.Result find_path t namespace path ~loc ~unbound with
  | Found name, None -> Ok name
  | Found _, Some splice -> Error (cycle splice ~name:path ~loc)
  | Unbound, blocked ->
    Error (unbound_head t namespace path ~loc ~unbound ~blocked)
;;

module Value = struct
  type t =
    | Toplevel of string
    | Prim of Prim.t
    | Intrinsic of Intrinsic.t
    | Unbound
end

let not_in_scope path = Typecheck_error.Kind.Not_in_scope path

(* Prims are outside the root module, so found only where the program has
   nothing of their name: no value, or no module for a path. The prelude also
   names them as [Prim.<prim>] (eg. [Prim.String.length], as [Core.String] hides
   [String.length]), which nothing shadows. *)
let find_value t path ~loc : (Value.t, _) Result.t =
  let in_prelude =
    lazy (Option.is_some (Prelude_path.relative (canonical_name t "_")))
  in
  let names =
    match (path : Decl_path.t) with
    | Path names | Root names -> names
    | Resolved _ -> raise_s [%message "Module_scope.find_value: resolved"]
  in
  match (names :> string list) with
  | "Prim" :: (_ :: _ as prim) when Lazy.force in_prelude ->
    (match Prim.find (Path.of_list_exn prim) with
     | Some prim -> Ok (Prim prim)
     | None -> Error (Typecheck_error.create ~loc (not_in_scope path)))
  | _ ->
    (match%bind.Result find_path t Value path ~loc ~unbound:not_in_scope with
     | Found name, None -> Ok (Value.Toplevel name)
     | Found _, Some splice -> Error (cycle splice ~name:path ~loc)
     | Unbound, blocked ->
       let intrinsic =
         if Lazy.force in_prelude then Intrinsic.find names else None
       in
       (match Prim.find names, blocked, intrinsic, (path : Decl_path.t) with
        | Some _, Some splice, _, _ -> Error (cycle splice ~name:path ~loc)
        | Some prim, None, _, _ -> Ok (Prim prim)
        | None, None, Some intrinsic, _ -> Ok (Intrinsic intrinsic)
        | None, None, None, Path names
          when List.length (names :> string list) = 1 -> Ok Unbound
        | None, _, _, _ ->
          Error (unbound_head t Value path ~loc ~unbound:not_in_scope ~blocked)))
;;

let unbound_value t path ~loc =
  unbound_head t Value path ~loc ~unbound:not_in_scope ~blocked:None
;;

let iter_declarations t ~f =
  let force splice =
    match force splice with
    | Forced | Failed -> ()
    | Uses_unfinished ->
      raise_s
        [%message
          "Module_scope.iter_declarations: a splice uses an unfinished toplevel"]
  in
  let rec iter_module module_ =
    Option.iter module_.splice ~f:force;
    iter_entries module_.entries
  and iter_entries entries =
    Queue.iter entries ~f:(function
      | Included splice ->
        force splice;
        (match splice.into with
         | Into_include entries -> iter_entries entries
         | Into_module _ | Into_type _ -> ())
      | Written { toplevel; scope; generated_by } ->
        f scope toplevel ~generated_by;
        (match (fst (Toplevel.unattributed toplevel)).txt with
         | Module (name, _) | Module_splice (name, _, _) ->
           iter_module (Hashtbl.find_exn scope.module_.modules name.txt)
         | Let _
         | Let_mut _
         | Type _
         | Exception _
         | External _
         | Malformed_exception _
         | Expect_test _
         | Bench _
         | Include _
         | Attributed _
         | Attribute _ -> ()))
  in
  iter_module t.module_
;;

let defining (t : t) name =
  match (Hashtbl.find_exn t.module_.definitions name).state with
  | Forcing -> true
  | Pending | Done _ | Failed -> false
;;

let force_definition (t : t) name ~loc =
  let splice = Hashtbl.find_exn t.module_.definitions name in
  let cycle () =
    Error
      (Typecheck_error.create
         ~loc
         (Type_definition_cycle
            { name = canonical_name t name; splice = splice.loc }))
  in
  match splice.state with
  | Forcing -> cycle ()
  | Pending | Done _ | Failed ->
    (match force splice with
     | Forced -> Ok ()
     | Failed -> Error Typecheck_error.already_reported
     | Uses_unfinished -> cycle ())
;;

let splices t =
  let rec splices module_ =
    Option.to_list module_.splice
    @ Queue.to_list module_.includes
    @ List.concat_map (Hashtbl.data module_.modules) ~f:splices
  in
  splices t.module_
;;

(* What [path] names where it is used in [toplevels], which [splice] generated,
   within their modules [modules] where [locals] are bound. *)
let resolve_in_expansion
  (splice : splice)
  toplevels
  namespace
  ~modules
  ~locals
  (path : Decl_path.t)
  =
  let generated =
    let in_module = splice.scope.module_.path in
    match splice.into with
    | Into_include _ | Into_type _ ->
      { Generated.path = in_module; toplevels; spliced = false }
    | Into_module name ->
      { path = in_module @ [ name ]; toplevels; spliced = true }
  in
  let from_splice (path : Decl_path.t) =
    match
      find_path
        ~generated
        splice.scope
        namespace
        path
        ~loc:splice.loc
        ~unbound:not_in_scope
    with
    | Ok (Found name, None) -> Some name
    | Ok (Unbound, None) ->
      (match namespace, path with
       | Value, (Path names | Root names) ->
         Option.map (Prim.find names) ~f:Prim.canonical_name
       | _ -> None)
    | Ok (_, Some _) | Error _ -> None
  in
  match path with
  | Resolved _ ->
    raise_s [%message "Module_scope: a resolved path is not looked up"]
  | Root _ -> from_splice path
  | Path names ->
    let names = (names :> string list) in
    let body modules =
      List.fold modules ~init:toplevels ~f:(fun toplevels name ->
        List.find_map_exn toplevels ~f:(fun (toplevel : Toplevel.t) ->
          match toplevel.txt with
          | Module (name', toplevels) when String.equal name'.txt name ->
            Some toplevels
          | _ -> None))
    in
    let rec within depth =
      if depth < 0
      then from_splice path
      else (
        let modules = List.take modules depth in
        match
          Generated.find
            (body modules)
            ~path:(generated.path @ modules)
            namespace
            names
        with
        | Found name -> Some name
        | Unbound -> None
        | Absent -> within (depth - 1))
    in
    (match names with
     | [ name ] when Set.mem locals name -> None
     | _ -> within (List.length modules))
;;

let path_from_splice splice toplevels namespace ~modules ~locals path =
  let resolve =
    resolve_in_expansion splice toplevels namespace ~modules ~locals
  in
  let%bind.Option name =
    match (path : Decl_path.t) with
    | Resolved name -> Some name
    | Path _ | Root _ -> resolve path
  in
  let resolves path =
    Option.some_if (Option.equal String.equal (resolve path) (Some name)) path
  in
  let names = String.split name ~on:'.' in
  List.init (List.length names) ~f:(fun i ->
    Decl_path.Path
      (Path.of_list_exn (List.drop names (List.length names - i - 1))))
  |> List.find_map ~f:resolves
  |> function
  | Some path -> Some path
  | None -> resolves (Root (Path.of_string (Prelude_path.root_path name)))
;;
