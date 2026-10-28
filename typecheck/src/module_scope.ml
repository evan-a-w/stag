open! Import
open Syntax

module Namespace = struct
  type t =
    | Value
    | Type
    | Constructor
    | Field
  [@@deriving sexp_of, compare, hash]
end

(* Members are named per namespace, and modules too. *)
module Member_kind = struct
  type t =
    | Member of Namespace.t
    | Module
  [@@deriving sexp_of, compare, hash]
end

module Member = struct
  module T = struct
    type t = Member_kind.t * string [@@deriving sexp_of, compare, hash]
  end

  include T
  include Hashable.Make_plain (T)
end

type module_ =
  { members : splice option Member.Table.t
    (* By the include generating them, [None] for the others. *)
  ; modules : module_ String.Table.t
  ; entries : entry Queue.t
  ; path : string list
  ; loc : Loc.t
  ; enclosing : t option
  ; splice : splice option (* Of a spliced module. *)
  ; callbacks : callbacks
  }

and t =
  { module_ : module_
  ; position : position
  }

(* [At]: after the include [prev] (or first), among the declarations [parent]
   generates (or those of the module). *)
and position =
  | End
  | At of
      { prev : splice option
      ; parent : splice option
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
  ; loc : Loc.t
  ; scope : t (* Where it is written: an include's is just before it. *)
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

and callbacks =
  { declare : t -> Toplevel.t -> generated_by:splice option -> unit
  ; force : splice -> (Toplevel.t list, Typecheck_error.t) Result.t
  }

module Splice = struct
  type nonrec t = splice

  let expr (t : t) = t.expr
  let loc (t : t) = t.loc
  let module_ (t : t) = t.scope
end

let join path = String.concat path ~sep:"."
let sexp_of_t t = Sexp.Atom (join t.module_.path)
let canonical_name t name = join (t.module_.path @ [ name ])

let enter t name =
  { module_ = Hashtbl.find_exn t.module_.modules name; position = End }
;;

let create ~path ~loc ~enclosing ~splice ~callbacks =
  { members = Member.Table.create ()
  ; modules = String.Table.create ()
  ; entries = Queue.create ()
  ; path
  ; loc
  ; enclosing
  ; splice
  ; callbacks
  }
;;

let create_root ?enclosing ?(path = []) ~types ~declare ~force () =
  let module_ =
    create
      ~path
      ~loc:Loc.none
      ~enclosing
      ~splice:None
      ~callbacks:{ declare; force }
  in
  List.iter types ~f:(fun name ->
    Hashtbl.set module_.members ~key:(Member Type, name) ~data:None);
  { module_; position = End }
;;

let submodule (scope : t) (name : Name.t) ~splice =
  let module_ =
    create
      ~path:(scope.module_.path @ [ name.txt ])
      ~loc:name.loc
      ~enclosing:(Some scope)
      ~splice
      ~callbacks:scope.module_.callbacks
  in
  Hashtbl.add_exn scope.module_.modules ~key:name.txt ~data:module_;
  module_
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

let add_unique names ({ txt = name; loc } : Name.t) ~duplicate =
  if Hash_set.mem names name
  then Error (Typecheck_error.create ~loc (duplicate name))
  else Ok (Hash_set.add names name)
;;

(* Checks that adding [toplevels] to [module_] keeps names unique, without
   changing it. *)
let rec check_unique module_ toplevels =
  let taken = Member.Hash_set.of_list (Hashtbl.keys module_.members) in
  let canonical_name name = join (module_.path @ [ name ]) in
  let add kind ({ txt = name; loc } : Name.t) ~duplicate =
    if Hash_set.mem taken (kind, name)
    then Error (Typecheck_error.create ~loc (duplicate (canonical_name name)))
    else Ok (Hash_set.add taken (kind, name))
  in
  let add_all kind members ~duplicate =
    iter_result members ~f:(fun (name, _) -> add kind name ~duplicate)
  in
  iter_result toplevels ~f:(fun (toplevel : Toplevel.t) ->
    match toplevel.txt with
    | Let (name, _, _, _) ->
      add (Member Value) name ~duplicate:(fun name -> Duplicate_toplevel name)
    | Type { name; params; kind } ->
      let%bind.Result () =
        add (Member Type) name ~duplicate:(fun name -> Duplicate_type name)
      in
      let type_name = canonical_name name.txt in
      let seen = String.Hash_set.create () in
      let%bind.Result () =
        iter_result params ~f:(fun { txt = param; loc } ->
          add_unique
            seen
            { txt = Type_decl.Param.name param; loc }
            ~duplicate:(fun param ->
              Duplicate_type_parameter { type_name; param }))
      in
      (match kind with
       | Alias _ | Abstract -> Ok ()
       | Variant members ->
         add_all (Member Constructor) members ~duplicate:(fun name ->
           Duplicate_constructor name)
       | Record members ->
         add_all (Member Field) members ~duplicate:(fun name ->
           Duplicate_field name))
    | Expect_test _ | Include _ -> Ok ()
    | Module_splice (name, _) ->
      add Module name ~duplicate:(fun name -> Duplicate_module name)
    | Module (name, toplevels) ->
      let%bind.Result () =
        add Module name ~duplicate:(fun name -> Duplicate_module name)
      in
      check_unique
        (create
           ~path:(module_.path @ [ name.txt ])
           ~loc:name.loc
           ~enclosing:None
           ~splice:None
           ~callbacks:module_.callbacks)
        toplevels)
;;

(* The members [toplevel] adds to the module it is in. *)
let members (toplevel : Toplevel.t) : Member.t list =
  let member namespace (name : Name.t) =
    Member_kind.Member namespace, name.txt
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
  | Module (name, _) | Module_splice (name, _) -> [ Module, name.txt ]
  | Expect_test _ | Include _ -> []
;;

(* Adds [toplevels], which [parent] generates if given, to [module_], after
   [prev]. *)
let rec register module_ toplevels ~parent ~into ~generated_by =
  ignore
    (List.fold toplevels ~init:None ~f:(fun prev (toplevel : Toplevel.t) ->
       let scope = { module_; position = At { prev; parent } } in
       let splice expr ~into =
         { expr; loc = toplevel.loc; scope; into; state = Pending }
       in
       match toplevel.txt with
       | Include expr ->
         let splice = splice expr ~into:(Into_include (Queue.create ())) in
         Queue.enqueue into (Included splice);
         Some splice
       | Let _ | Type _ | Expect_test _ | Module _ | Module_splice _ ->
         module_.callbacks.declare scope toplevel ~generated_by;
         Queue.enqueue into (Written { toplevel; scope; generated_by });
         List.iter (members toplevel) ~f:(fun member ->
           Hashtbl.add_exn module_.members ~key:member ~data:parent);
         (match toplevel.txt with
          | Module (name, toplevels) ->
            let module_ = submodule scope name ~splice:None in
            register
              module_
              toplevels
              ~parent:None
              ~into:module_.entries
              ~generated_by
          | Module_splice (name, expr) ->
            ignore
              (submodule
                 scope
                 name
                 ~splice:(Some (splice expr ~into:(Into_module name.txt)))
               : module_)
          | Let _ | Type _ | Expect_test _ | Include _ -> ());
         prev)
     : splice option)
;;

let add t toplevels =
  let%map.Result () = check_unique t.module_ toplevels in
  register
    t.module_
    toplevels
    ~parent:None
    ~into:t.module_.entries
    ~generated_by:None
;;

let cycle (splice : splice) ~name ~loc =
  let module_ =
    match splice.into with
    | Into_include _ -> None
    | Into_module name -> Some (canonical_name splice.scope name)
  in
  Error
    (Typecheck_error.create
       ~loc
       (Declaration_splice_cycle
          { name = Path.to_string name; module_; splice = splice.loc }))
;;

(* Runs [splice], which is not running, adding what it generates to its
   module, or, for an include, to the module it is in, in its place. *)
let force (splice : splice) =
  match splice.state with
  | Done -> Ok ()
  | Failed error -> Error error
  | Forcing -> raise_s [%message "Module_scope.force: already running"]
  | Pending ->
    splice.state <- Forcing;
    let in_module = splice.scope.module_ in
    let result =
      let%bind.Result toplevels = in_module.callbacks.force splice in
      match splice.into with
      | Into_include into ->
        let%map.Result () = check_unique in_module toplevels in
        register
          in_module
          toplevels
          ~parent:(Some splice)
          ~into
          ~generated_by:(Some splice)
      | Into_module name ->
        let module_ = Hashtbl.find_exn in_module.modules name in
        let%map.Result () = check_unique module_ toplevels in
        register
          module_
          toplevels
          ~parent:None
          ~into:module_.entries
          ~generated_by:(Some splice)
    in
    splice.state
    <- (match result with
        | Ok () -> Done
        | Error error -> Failed error);
    result
;;

let includes entries =
  Queue.to_list entries
  |> List.filter_map ~f:(function
    | Included splice -> Some splice
    | Written _ -> None)
;;

let children (splice : splice) =
  match splice.into with
  | Into_include entries -> includes entries
  | Into_module _ -> []
;;

let prev_of (splice : splice) =
  match splice.scope.position with
  | At { prev; parent = _ } -> prev
  | End -> None
;;

let parent_of (splice : splice) =
  match splice.scope.position with
  | At { prev = _; parent } -> parent
  | End -> None
;;

let rec ancestors = function
  | None -> []
  | Some splice -> splice :: ancestors (parent_of splice)
;;

(* The includes before [prev] and [parent], whose subtrees are all before:
   those of the outermost first, in order. *)
let rec before ~prev ~parent =
  let rec siblings acc = function
    | None -> acc
    | Some splice -> siblings (splice :: acc) (prev_of splice)
  in
  (match parent with
   | None -> []
   | Some splice -> before ~prev:(prev_of splice) ~parent:(parent_of splice))
  @ siblings [] prev
;;

let rec within root splice =
  phys_equal root splice || Option.exists (parent_of splice) ~f:(within root)
;;

let visible t generated_by =
  match generated_by, t.position with
  | None, _ | Some _, End -> true
  | Some splice, At { prev; parent } ->
    List.exists (ancestors parent) ~f:(phys_equal splice)
    || List.exists (before ~prev ~parent) ~f:(fun root -> within root splice)
;;

(* Whether [member] of [t]'s module is visible from [t], forcing the includes
   visible from [t] in order while it is unknown. The first running include
   met is put in [blocked]: what it generates is unknown. *)
let mem t member ~blocked =
  let found () =
    Option.map (Hashtbl.find t.module_.members member) ~f:(visible t)
  in
  let rec search = function
    | [] -> Ok false
    | (splice : splice) :: rest ->
      (match splice.state with
       | Forcing ->
         if Option.is_none !blocked then blocked := Some splice;
         search rest
       | Pending | Done | Failed _ ->
         let%bind.Result () = force splice in
         (match found () with
          | Some visible -> Ok visible
          | None -> search (children splice @ rest)))
  in
  match found () with
  | Some visible -> Ok visible
  | None ->
    search
      (match t.position with
       | End -> includes t.module_.entries
       | At { prev; parent } -> before ~prev ~parent)
;;

let prim_has ((kind, name) : Member.t) =
  match kind with
  | Member Value -> Option.is_some (Prim.find (Path.name name))
  | Module -> Prim.is_module name
  | Member (Type | Constructor | Field) -> false
;;

(* The scope [member] is found in from [t] outwards. Found past an include
   that is running, or as a prim, it may be what the include generates. *)
let find_scope t member ~name ~loc =
  let rec search t ~blocked =
    let here = ref None in
    match%bind.Result mem t member ~blocked:here with
    | true ->
      (match blocked with
       | Some splice -> cycle splice ~name ~loc
       | None -> Ok (Some t))
    | false ->
      let blocked = Option.first_some blocked !here in
      (match t.module_.enclosing, blocked with
       | Some t, _ -> search t ~blocked
       | None, Some splice when prim_has member -> cycle splice ~name ~loc
       | None, (Some _ | None) -> Ok None)
  in
  search t ~blocked:None
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

let unbound_module ~loc ~path ~written =
  Error
    (Typecheck_error.create
       ~loc:(prefix_loc loc ~path ~written)
       (Unbound_module (Path.of_list_exn written)))
;;

(* The member module [name] of [module_], with its declarations generated. *)
let open_module module_ name ~path ~loc =
  let module_ = Hashtbl.find_exn module_.modules name in
  let%map.Result () =
    match module_.splice with
    | None -> Ok ()
    | Some splice ->
      (match splice.state with
       | Forcing -> cycle splice ~name:path ~loc
       | Pending | Done | Failed _ -> force splice)
  in
  module_
;;

(* [name] of [module_] then [rest], from outside it. Errors of names not found
   are passed through [hint]. *)
let rec find_member
          module_
          namespace
          name
          rest
          ~path
          ~written
          ~loc
          ~unbound
          ~hint
  =
  let written = written @ [ name ] in
  let blocked = ref None in
  let member : Member.t =
    match rest with
    | [] -> Member namespace, name
    | _ :: _ -> Module, name
  in
  match%bind.Result mem { module_; position = End } member ~blocked with
  | true ->
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
         ~hint)
  | false ->
    (match !blocked, rest with
     | Some splice, _ -> cycle splice ~name:path ~loc
     | None, [] -> Error (hint (Typecheck_error.create ~loc (unbound path)))
     | None, _ :: _ ->
       unbound_module ~loc ~path ~written |> Result.map_error ~f:hint)
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
  let rec prelude module_ =
    if [%equal: string list] module_.path [ Prelude_path.root ]
    then Some module_
    else Option.bind module_.enclosing ~f:(fun scope -> prelude scope.module_)
  in
  match
    Option.bind found.module_.enclosing ~f:(fun scope -> prelude scope.module_)
  with
  | Some prelude when has_path prelude namespace (head :: rest) ->
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

let find_path t namespace (path : Path.t) ~loc ~unbound =
  match (path :> string list) with
  | [] -> assert false
  | [ name ] ->
    let%map.Result found =
      find_scope t (Member namespace, name) ~name:path ~loc
    in
    Option.map found ~f:(fun found -> canonical_name found name)
  | head :: member :: rest ->
    (match%bind.Result find_scope t (Module, head) ~name:path ~loc with
     | None -> Ok None
     | Some found ->
       (let%bind.Result module_ = open_module found.module_ head ~path ~loc in
        find_member
          module_
          namespace
          member
          rest
          ~path
          ~written:[ head ]
          ~loc
          ~unbound
          ~hint:(with_prelude_hint found ~head ~rest:(member :: rest) namespace))
       |> Result.map ~f:Option.some)
;;

let unbound_head (path : Path.t) ~loc ~unbound =
  match (path :> string list) with
  | [] | [ _ ] -> Error (Typecheck_error.create ~loc (unbound path))
  | head :: _ :: _ -> unbound_module ~loc ~path ~written:[ head ]
;;

let find t namespace path ~loc ~unbound =
  match%bind.Result find_path t namespace path ~loc ~unbound with
  | Some name -> Ok name
  | None -> unbound_head path ~loc ~unbound
;;

module Value = struct
  type t =
    | Toplevel of string
    | Prim of Prim.t
    | Unbound
end

(* Prims are outside the root module, so found only where the program has
   nothing of their name: no value, or no module for a path. *)
let find_value t path ~loc : (Value.t, _) Result.t =
  let unbound path = Typecheck_error.Kind.Not_in_scope path in
  match%bind.Result find_path t Value path ~loc ~unbound with
  | Some name -> Ok (Value.Toplevel name)
  | None ->
    (match Prim.find path, (path :> string list) with
     | Some prim, _ -> Ok (Prim prim)
     | None, [ _ ] -> Ok Unbound
     | None, head :: _ when Prim.is_module head ->
       Error (Typecheck_error.create ~loc (unbound path))
     | None, _ -> unbound_head path ~loc ~unbound)
;;

let iter_declarations t ~f =
  let rec iter_module module_ =
    let%bind.Result () =
      match module_.splice with
      | Some splice -> force splice
      | None -> Ok ()
    in
    iter_entries module_.entries
  and iter_entries entries =
    let rec from index =
      if index >= Queue.length entries
      then Ok ()
      else (
        let%bind.Result () =
          match Queue.get entries index with
          | Included splice ->
            let%bind.Result () = force splice in
            iter_entries
              (match splice.into with
               | Into_include entries -> entries
               | Into_module _ -> Queue.create ())
          | Written { toplevel; scope; generated_by } ->
            let%bind.Result () = f scope toplevel ~generated_by in
            (match toplevel.txt with
             | Module (name, _) | Module_splice (name, _) ->
               iter_module (Hashtbl.find_exn scope.module_.modules name.txt)
             | Let _ | Type _ | Expect_test _ | Include _ -> Ok ())
        in
        from (index + 1))
    in
    from 0
  in
  iter_module t.module_
;;

let shadows_prelude t relative =
  let rec has module_ (member : Member.t) =
    Hashtbl.mem module_.members member
    || Hashtbl.exists module_.modules ~f:(fun module_ ->
      (not ([%equal: string list] module_.path [ Prelude_path.root ]))
      && has module_ member)
  in
  Prelude_path.shadowed relative ~has:(fun kind name ->
    has
      t.module_
      ( (match kind with
         | `Type -> Member Type
         | `Module -> Module)
      , name ))
;;
