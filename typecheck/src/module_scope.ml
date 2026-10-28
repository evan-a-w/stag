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
  module T = struct
    type t = Member_kind.t * string [@@deriving sexp_of, compare, hash]
  end

  include T
  include Hashable.Make_plain (T)
end

(* The k-th entry of a module is at [At [k]], the j-th an include at [At p]
   generates at [At (p @ [j])]. Lists compare lexicographically, a prefix
   first, so an include is before what it generates. *)
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
  ; includes : splice Queue.t (* Its own, and those they generate. *)
  ; path : string list
  ; loc : Loc.t
  ; enclosing : t option
  ; splice : splice option (* Of a spliced module. *)
  ; program : program
  }

and t =
  { module_ : module_
  ; position : Position.t
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

and state =
  | Pending
  | Forcing
  | Done
  | Failed of Typecheck_error.t

(* Shared by the modules of a root. *)
and program =
  { declare : t -> Toplevel.t -> generated_by:splice option -> unit
  ; force : splice -> (Toplevel.t list, Force_error.t) Result.t
  ; outside : t option (* The root's enclosing scope. *)
  }

module Splice = struct
  type nonrec t = splice

  let expr (t : t) = t.expr
  let toplevel (t : t) = t.toplevel
  let loc (t : t) = t.loc
  let module_ (t : t) = t.scope
end

let join path = String.concat path ~sep:"."
let sexp_of_t t = Sexp.Atom (join t.module_.path)
let canonical_name t name = join (t.module_.path @ [ name ])

let enter t name =
  { module_ = Hashtbl.find_exn t.module_.modules name; position = End }
;;

let create ~path ~loc ~enclosing ~splice ~program =
  { members = Member.Table.create ()
  ; modules = String.Table.create ()
  ; entries = Queue.create ()
  ; includes = Queue.create ()
  ; path
  ; loc
  ; enclosing
  ; splice
  ; program
  }
;;

let create_root ?enclosing ?(path = []) ~types ~declare ~force () =
  let module_ =
    create
      ~path
      ~loc:Loc.none
      ~enclosing
      ~splice:None
      ~program:{ declare; force; outside = enclosing }
  in
  List.iter types ~f:(fun name ->
    Hashtbl.set module_.members ~key:(Member Type, name) ~data:None);
  { module_; position = End }
;;

let rec declarations_of entries =
  Queue.to_list entries
  |> List.concat_map ~f:(function
    | Written { toplevel; _ } -> [ toplevel ]
    | Included splice ->
      (match splice.into with
       | Into_include entries -> declarations_of entries
       | Into_module _ -> []))
;;

let declarations t = declarations_of t.module_.entries
let iter_result xs ~f = List.fold_result xs ~init:() ~f:(fun () x -> f x)

(* The members [toplevel] adds to the module it is in. *)
let members (toplevel : Toplevel.t) : (Member.t * Name.t) list =
  let member namespace (name : Name.t) =
    (Member_kind.Member namespace, name.txt), name
  in
  match toplevel.txt with
  | Let (name, _, _, _) -> [ member Value name ]
  | Type { name; params = _; kind } ->
    member Type name
    ::
    (match kind with
     | Alias _ | Abstract -> []
     | Variant constructors ->
       List.map constructors ~f:(fun (name, _) -> member Constructor name)
     | Record fields -> List.map fields ~f:(fun (name, _) -> member Field name))
  | Exception (name, _) -> [ member Constructor name ]
  | Module (name, _) | Module_splice (name, _, _) ->
    [ (Module, name.txt), name ]
  | Expect_test _ | Bench _ | Include _ -> []
;;

(* Checks that adding [toplevels] to the module [path], whose members are
   [taken], keeps names unique: a member per module, a parameter per type. *)
let rec check_unique ~path taken toplevels =
  iter_result toplevels ~f:(fun (toplevel : Toplevel.t) ->
    let%bind.Result () =
      match toplevel.txt with
      | Type { name; params; kind = _ } ->
        let type_name = join (path @ [ name.txt ]) in
        List.fold_result
          params
          ~init:String.Set.empty
          ~f:(fun seen { txt = param; loc } ->
            let param = Type_decl.Param.name param in
            if Set.mem seen param
            then
              Error
                (Typecheck_error.create
                   ~loc
                   (Duplicate_type_parameter { type_name; param }))
            else Ok (Set.add seen param))
        |> Result.ignore_m
      | Let _
      | Exception _
      | Module _
      | Module_splice _
      | Expect_test _
      | Bench _
      | Include _ -> Ok ()
    in
    let%bind.Result () =
      iter_result (members toplevel) ~f:(fun (((kind, name) as member), at) ->
        if Hash_set.mem taken member
        then
          Error
            (Typecheck_error.create
               ~loc:at.loc
               (Member_kind.duplicate kind (join (path @ [ name ]))))
        else Ok (Hash_set.add taken member))
    in
    match toplevel.txt with
    | Module (name, toplevels) ->
      check_unique
        ~path:(path @ [ name.txt ])
        (Member.Hash_set.create ())
        toplevels
    | Let _
    | Type _
    | Exception _
    | Module_splice _
    | Expect_test _
    | Bench _
    | Include _ -> Ok ())
;;

let check_unique module_ toplevels =
  check_unique
    ~path:module_.path
    (Member.Hash_set.of_list (Hashtbl.keys module_.members))
    toplevels
;;

let submodule (scope : t) (name : Name.t) ~splice =
  let module_ =
    create
      ~path:(scope.module_.path @ [ name.txt ])
      ~loc:name.loc
      ~enclosing:(Some scope)
      ~splice
      ~program:scope.module_.program
  in
  Hashtbl.add_exn scope.module_.modules ~key:name.txt ~data:module_;
  module_
;;

(* Adds [toplevels] to [module_], in place of the include [parent] if given. *)
let rec register module_ toplevels ~parent ~generated_by =
  let prefix, into =
    match parent with
    | None -> [], module_.entries
    | Some { scope = { position = At prefix; _ }; into = Into_include into; _ }
      -> prefix, into
    | Some _ -> raise_s [%message "Module_scope.register: not an include"]
  in
  List.iter toplevels ~f:(fun (toplevel : Toplevel.t) ->
    let scope = { module_; position = At (prefix @ [ Queue.length into ]) } in
    let splice (expr : Expr.t) ~expansion ~into =
      let loc =
        match expansion with
        | None -> toplevel.loc
        | Some _ -> { toplevel.loc with stop = expr.loc.stop }
      in
      { expr; toplevel; loc; scope; into; state = Pending }
    in
    match toplevel.txt with
    | Include (expr, expansion) ->
      let splice =
        splice expr ~expansion ~into:(Into_include (Queue.create ()))
      in
      Queue.enqueue into (Included splice);
      Queue.enqueue module_.includes splice
    | Let _
    | Type _
    | Exception _
    | Expect_test _
    | Bench _
    | Module _
    | Module_splice _ ->
      module_.program.declare scope toplevel ~generated_by;
      Queue.enqueue into (Written { toplevel; scope; generated_by });
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
       | Let _ | Type _ | Exception _ | Expect_test _ | Bench _ | Include _ ->
         ()))
;;

let add t toplevels =
  let%map.Result () = check_unique t.module_ toplevels in
  register t.module_ toplevels ~parent:None ~generated_by:None
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
  let names kind =
    List.concat_map (modules t) ~f:(fun module_ ->
      Hashtbl.to_alist module_.members
      |> List.filter_map ~f:(fun ((kind', name), generated_by) ->
        Option.some_if
          ([%equal: Member_kind.t] kind kind' && Option.is_none generated_by)
          name))
    |> String.Set.of_list
  in
  { types = names (Member Type); modules = names Module }
;;

let within t error = Typecheck_error.within error ~in_scope:(lazy (in_scope t))

(* [module_] is the canonical name of the module [splice] generates, if it is
   not an include. *)
let cycle ?module_ (splice : splice) ~name ~loc =
  Typecheck_error.create
    ~loc
    (Declaration_splice_cycle
       { name = Path.to_string name; module_; splice = splice.loc })
;;

(* Runs [splice], which is not running, adding what it generates to its
   module, or, for an include, to the module it is in, in its place. If it
   uses an unfinished toplevel, it is left pending. *)
let force (splice : splice) : (unit, Force_error.t) Result.t =
  match splice.state with
  | Done -> Ok ()
  | Failed error -> Error (Failed error)
  | Forcing -> raise_s [%message "Module_scope.force: already running"]
  | Pending ->
    splice.state <- Forcing;
    let in_module = splice.scope.module_ in
    (match in_module.program.force splice with
     | Error Uses_unfinished ->
       splice.state <- Pending;
       Error Uses_unfinished
     | Error (Failed error) ->
       splice.state <- Failed error;
       Error (Failed error)
     | Ok toplevels ->
       let module_, parent =
         match splice.into with
         | Into_include _ -> in_module, Some splice
         | Into_module name -> Hashtbl.find_exn in_module.modules name, None
       in
       (match check_unique module_ toplevels with
        | Ok () ->
          register module_ toplevels ~parent ~generated_by:(Some splice);
          splice.state <- Done;
          Ok ()
        | Error error ->
          let error = within splice.scope error in
          splice.state <- Failed error;
          Error (Failed error)))
;;

let is_before (splice : splice) position =
  Position.before splice.scope.position position
;;

let first splices =
  List.min_elt splices ~compare:(fun (splice : splice) (splice' : splice) ->
    Position.compare splice.scope.position splice'.scope.position)
;;

module Lookup = struct
  type t =
    | Visible
    | Hidden of splice (* Generated by an include after the position. *)
    | Absent of { blocked : splice option }
end

(* [member] of [t]'s module, seen from [t]. While it is not found, the first
   include before [t] not yet forced is. Unless it is running or uses an
   unfinished toplevel: then what it generates is unknown, and it is
   [blocked]. *)
let rec lookup (t : t) member =
  match Hashtbl.find t.module_.members member with
  | Some None -> Ok Lookup.Visible
  | Some (Some include_) ->
    Ok
      (if is_before include_ t.position then Lookup.Visible else Hidden include_)
  | None ->
    (match
       Queue.to_list t.module_.includes
       |> List.filter ~f:(fun splice ->
         is_before splice t.position
         &&
         match splice.state with
         | Done -> false
         | Pending | Forcing | Failed _ -> true)
       |> first
     with
     | None -> Ok (Absent { blocked = None })
     | Some ({ state = Forcing; _ } as splice) ->
       Ok (Absent { blocked = Some splice })
     | Some splice ->
       (match force splice with
        | Ok () -> lookup t member
        | Error (Failed error) -> Error error
        | Error Uses_unfinished -> Ok (Absent { blocked = Some splice })))
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

(* The include generating [member] after [t], in its module or one enclosing
   it, for messages: all their includes are forced, up to one that fails. *)
let rec generated_after (t : t) member =
  match lookup { t with position = End } member, lookup t member with
  | Ok Visible, Ok (Hidden include_) -> Some include_
  | _ -> Option.bind t.module_.enclosing ~f:(fun t -> generated_after t member)
;;

let with_note (error : Typecheck_error.Kind.t) ~blocked ~hidden
  : Typecheck_error.Kind.t
  =
  match blocked, hidden with
  | Some (include_ : splice), _ ->
    Maybe_generated_by_running_include { error; include_ = include_.loc }
  | None, Some (include_ : splice) ->
    Generated_after_include { error; include_ = include_.loc }
  | None, None -> error
;;

(* The location of the module path [written] at the start of the path at
   [loc], if written without spaces as for [path]. *)
let prefix_loc (loc : Loc.t) ~(path : Path.t) ~written =
  let len = String.length (Path.to_string (Path.of_list_exn written)) in
  if
    loc.start.line = loc.stop.line
    && loc.stop.offset - loc.start.offset = String.length (Path.to_string path)
  then
    { loc with
      stop =
        { loc.start with
          col = loc.start.col + len
        ; offset = loc.start.offset + len
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
       | Pending | Done | Failed _ ->
         (match force splice with
          | Ok () -> Ok ()
          | Error (Failed error) -> Error error
          | Error Uses_unfinished -> Error (cycle splice)))
  in
  module_
;;

(* [name] of [module_] then [rest], from outside it, past [blocked]. Errors of
   names not found are passed through [hint]. *)
let rec find_member
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
  match%bind.Result lookup { module_; position = End } member with
  | Visible ->
    (match rest with
     | [] -> Ok (join (module_.path @ [ name ]))
     | next :: rest ->
       let%bind.Result module_ = open_module module_ name ~path ~loc in
       find_member
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
        ( Typecheck_error.Kind.Unbound_module (Path.of_list_exn written)
        , prefix_loc loc ~path ~written )
    in
    Error
      (hint
         (Typecheck_error.create ~loc (with_note kind ~blocked ~hidden:None)))
;;

let rec has_path module_ namespace = function
  | [] -> false
  | [ name ] -> Hashtbl.mem module_.members (Member namespace, name)
  | name :: rest ->
    Hashtbl.find module_.modules name
    |> Option.exists ~f:(fun module_ -> has_path module_ namespace rest)
;;

(* [error] for [head :: rest], found in [found], unless it shadows the
   prelude's [head], which has [rest]. *)
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
let find_path t namespace (path : Path.t) ~loc ~unbound =
  match (path :> string list) with
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
       let%bind.Result module_ = open_module found.module_ head ~path ~loc in
       let%map.Result name =
         find_member
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
       Resolved.Found name, blocked)
;;

(* The error for [path], whose first name is not found from [t] past
   [blocked]. *)
let unbound_head t namespace (path : Path.t) ~loc ~unbound ~blocked =
  let member, kind, loc =
    match (path :> string list), namespace with
    | [], _ -> assert false
    | [ name ], _ -> (Member_kind.Member namespace, name), unbound path, loc
    | head :: _, Namespace.Value when Prim.is_module head ->
      (Module, head), unbound path, loc
    | head :: _, _ ->
      ( (Module, head)
      , Typecheck_error.Kind.Unbound_module (Path.of_list_exn [ head ])
      , prefix_loc loc ~path ~written:[ head ] )
  in
  let hidden =
    match blocked with
    | Some _ -> None
    | None -> generated_after t member
  in
  Typecheck_error.create ~loc (with_note kind ~blocked ~hidden)
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
   names them as [Prim.<prim>] (eg. [Prim.String.length], as [Core.String]
   hides [String.length]), which nothing shadows. *)
let find_value t path ~loc : (Value.t, _) Result.t =
  let in_prelude =
    lazy (Option.is_some (Prelude_path.relative (canonical_name t "_")))
  in
  match (path : Path.t :> string list) with
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
         if Lazy.force in_prelude then Intrinsic.find path else None
       in
       (match Prim.find path, blocked, intrinsic, (path :> string list) with
        | Some _, Some splice, _, _ -> Error (cycle splice ~name:path ~loc)
        | Some prim, None, _, _ -> Ok (Prim prim)
        | None, None, Some intrinsic, _ -> Ok (Intrinsic intrinsic)
        | None, None, None, [ _ ] -> Ok Unbound
        | None, _, _, _ ->
          Error (unbound_head t Value path ~loc ~unbound:not_in_scope ~blocked)))
;;

let unbound_value t path ~loc =
  unbound_head t Value path ~loc ~unbound:not_in_scope ~blocked:None
;;

let iter_declarations t ~f =
  let force splice =
    match force splice with
    | Ok () -> Ok ()
    | Error (Failed error) -> Error error
    | Error Uses_unfinished ->
      raise_s
        [%message
          "Module_scope.iter_declarations: a splice uses an unfinished toplevel"]
  in
  let rec iter_module module_ =
    let%bind.Result () =
      match module_.splice with
      | Some splice -> force splice
      | None -> Ok ()
    in
    iter_entries module_.entries
  and iter_entries entries =
    iter_result (Queue.to_list entries) ~f:(function
      | Included splice ->
        let%bind.Result () = force splice in
        (match splice.into with
         | Into_include entries -> iter_entries entries
         | Into_module _ -> Ok ())
      | Written { toplevel; scope; generated_by } ->
        let%bind.Result () = f scope toplevel ~generated_by in
        (match toplevel.txt with
         | Module (name, _) | Module_splice (name, _, _) ->
           iter_module (Hashtbl.find_exn scope.module_.modules name.txt)
         | Let _ | Type _ | Exception _ | Expect_test _ | Bench _ | Include _ ->
           Ok ()))
  in
  iter_module t.module_
;;
