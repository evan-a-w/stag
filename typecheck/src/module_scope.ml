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

type t =
  { values : String.Hash_set.t
  ; types : String.Hash_set.t
  ; constructors : String.Hash_set.t
  ; fields : String.Hash_set.t
  ; modules : t String.Table.t
  ; entries : entry Queue.t
  ; includes : splice Queue.t
    (* Every include of the module, generated or not, in the order added. *)
  ; missed : Loc.t Member.Table.t
    (* Names looked up and not found while an include of the module was being
       forced, so no include may define them, by where they were used. *)
  ; path : string list
  ; enclosing : t option
  ; splice : splice option (* Of a spliced module. *)
  ; callbacks : callbacks
  }

and entry =
  | Written of Toplevel.t * splice option (* The splice generating it. *)
  | Included of splice

and splice =
  { expr : Expr.t
  ; loc : Loc.t
  ; in_module : t
  ; included : entry Queue.t (* An include's declarations, once forced. *)
  ; mutable state : state
  }

and state =
  | Pending
  | Forcing
  | Done

and callbacks =
  { declare : t -> Toplevel.t -> generated_by:splice option -> unit
  ; force : splice -> (Toplevel.t list, Typecheck_error.t) Result.t
  }

module Splice = struct
  type nonrec t = splice

  let expr t = t.expr
  let loc t = t.loc
  let module_ t = t.in_module
end

let names t : Namespace.t -> _ = function
  | Value -> t.values
  | Type -> t.types
  | Constructor -> t.constructors
  | Field -> t.fields
;;

let has t ((kind, name) : Member.t) =
  match kind with
  | Member namespace -> Hash_set.mem (names t namespace) name
  | Module -> Hashtbl.mem t.modules name
;;

let join path = String.concat path ~sep:"."
let sexp_of_t t = Sexp.Atom (join t.path)
let canonical_name t name = join (t.path @ [ name ])
let enter t name = Hashtbl.find_exn t.modules name

let create ~path ~enclosing ~splice ~callbacks =
  { values = String.Hash_set.create ()
  ; types = String.Hash_set.create ()
  ; constructors = String.Hash_set.create ()
  ; fields = String.Hash_set.create ()
  ; modules = String.Table.create ()
  ; entries = Queue.create ()
  ; includes = Queue.create ()
  ; missed = Member.Table.create ()
  ; path
  ; enclosing
  ; splice
  ; callbacks
  }
;;

let create_root ?enclosing ?(path = []) ~types ~declare ~force () =
  let t = create ~path ~enclosing ~splice:None ~callbacks:{ declare; force } in
  List.iter types ~f:(Hash_set.add t.types);
  t
;;

let submodule t name ~splice =
  create
    ~path:(t.path @ [ name ])
    ~enclosing:(Some t)
    ~splice
    ~callbacks:t.callbacks
;;

let rec declarations_of entries =
  Queue.to_list entries
  |> List.concat_map ~f:(function
    | Written (toplevel, _) -> [ toplevel ]
    | Included splice -> declarations_of splice.included)
;;

let declarations t = declarations_of t.entries
let iter_result xs ~f = List.fold_result xs ~init:() ~f:(fun () x -> f x)

let add_unique names ({ txt = name; loc } : Name.t) ~duplicate =
  if Hash_set.mem names name
  then Error (Typecheck_error.create ~loc (duplicate name))
  else Ok (Hash_set.add names name)
;;

let add_all_unique t names new_names ~duplicate =
  iter_result new_names ~f:(fun (name, _) ->
    add_unique names name ~duplicate:(fun name ->
      duplicate (canonical_name t name)))
;;

(* Checks that adding [toplevels] to [t] keeps names unique, without changing
   [t]. *)
let rec check_unique t toplevels =
  let taken namespace = Hash_set.copy (names t namespace) in
  let values = taken Value in
  let types = taken Type in
  let constructors = taken Constructor in
  let fields = taken Field in
  let modules = String.Hash_set.of_list (Hashtbl.keys t.modules) in
  let add_member names name ~duplicate =
    add_unique names name ~duplicate:(fun name ->
      duplicate (canonical_name t name))
  in
  let add_module name =
    add_member modules name ~duplicate:(fun name -> Duplicate_module name)
  in
  iter_result toplevels ~f:(fun (toplevel : Toplevel.t) ->
    match toplevel.txt with
    | Let (name, _, _, _) ->
      add_member values name ~duplicate:(fun name -> Duplicate_toplevel name)
    | Type { name; params; kind } ->
      let%bind.Result () =
        add_member types name ~duplicate:(fun name -> Duplicate_type name)
      in
      let type_name = canonical_name t name.txt in
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
         add_all_unique t constructors members ~duplicate:(fun name ->
           Duplicate_constructor name)
       | Record members ->
         add_all_unique t fields members ~duplicate:(fun name ->
           Duplicate_field name))
    | Expect_test _ | Include _ -> Ok ()
    | Module_splice (name, _) -> add_module name
    | Module (name, toplevels) ->
      let%bind.Result () = add_module name in
      check_unique (submodule t name.txt ~splice:None) toplevels)
;;

(* The members [toplevels] add to the module they are in. *)
let members toplevels : Member.t list =
  List.concat_map toplevels ~f:(fun (toplevel : Toplevel.t) ->
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
       | Record fields ->
         List.map fields ~f:(fun (name, _) -> member Field name))
    | Module (name, _) | Module_splice (name, _) -> [ Module, name.txt ]
    | Expect_test _ | Include _ -> [])
;;

let rec register t toplevels ~into ~generated_by =
  List.iter toplevels ~f:(fun (toplevel : Toplevel.t) ->
    let add_module name ~splice =
      let module_ = submodule t name ~splice in
      Hashtbl.add_exn t.modules ~key:name ~data:module_;
      module_
    in
    match toplevel.txt with
    | Include expr ->
      let splice =
        { expr
        ; loc = toplevel.loc
        ; in_module = t
        ; included = Queue.create ()
        ; state = Pending
        }
      in
      Queue.enqueue t.includes splice;
      Queue.enqueue into (Included splice)
    | Let _ | Type _ | Expect_test _ | Module _ | Module_splice _ ->
      t.callbacks.declare t toplevel ~generated_by;
      Queue.enqueue into (Written (toplevel, generated_by));
      List.iter (members [ toplevel ]) ~f:(fun (kind, name) ->
        match kind with
        | Member namespace -> Hash_set.add (names t namespace) name
        | Module -> ());
      (match toplevel.txt with
       | Module ({ txt = name; loc = _ }, toplevels) ->
         let module_ = add_module name ~splice:None in
         register module_ toplevels ~into:module_.entries ~generated_by
       | Module_splice ({ txt = name; loc = _ }, expr) ->
         ignore
           (add_module
              name
              ~splice:
                (Some
                   { expr
                   ; loc = toplevel.loc
                   ; in_module = t
                   ; included = Queue.create ()
                   ; state = Pending
                   })
            : t)
       | Let _ | Type _ | Expect_test _ | Include _ -> ()))
;;

let add t toplevels =
  let%map.Result () = check_unique t toplevels in
  register t toplevels ~into:t.entries ~generated_by:None
;;

let cycle ~loc (splice : splice) =
  Error
    (Typecheck_error.create
       ~loc
       (Declaration_splice_cycle { splice = splice.loc }))
;;

(* Runs [splice], adding what it generates to its module, or, for an include,
   to the module it is in. [loc] is where what it generates is needed. *)
let force_splice (splice : splice) ~module_ ~loc =
  match splice.state with
  | Done -> Ok ()
  | Forcing -> cycle ~loc splice
  | Pending ->
    splice.state <- Forcing;
    let%bind.Result toplevels = module_.callbacks.force splice in
    let%bind.Result () = check_unique module_ toplevels in
    let%map.Result () =
      match
        List.find_map (members toplevels) ~f:(Hashtbl.find module_.missed)
      with
      | Some loc -> cycle ~loc splice
      | None -> Ok ()
    in
    let into =
      match module_.splice with
      | Some module_splice when phys_equal module_splice splice ->
        module_.entries
      | Some _ | None -> splice.included
    in
    register module_ toplevels ~into ~generated_by:(Some splice);
    splice.state <- Done
;;

(* Forces the includes of [t] not yet forced nor being forced, including those
   they generate. *)
let force_includes t ~loc =
  let rec from index =
    if index >= Queue.length t.includes
    then Ok ()
    else (
      let splice = Queue.get t.includes index in
      let%bind.Result () =
        match splice.state with
        | Pending -> force_splice splice ~module_:t ~loc
        | Forcing | Done -> Ok ()
      in
      from (index + 1))
  in
  from 0
;;

(* Whether [t] has [member], forcing its includes if it is not yet known:
   they may define it. *)
let mem t member ~loc =
  if has t member
  then Ok true
  else (
    let%map.Result () = force_includes t ~loc in
    let found = has t member in
    if
      (not found)
      && Queue.exists t.includes ~f:(fun splice ->
        match splice.state with
        | Forcing -> true
        | Pending | Done -> false)
    then
      Hashtbl.add t.missed ~key:member ~data:loc
      |> (ignore : [ `Ok | `Duplicate ] -> unit);
    found)
;;

(* The member module [name] of [t], with its declarations generated. *)
let open_module t name ~loc =
  let module_ = enter t name in
  let%map.Result () =
    match module_.splice with
    | Some splice -> force_splice splice ~module_ ~loc
    | None -> Ok ()
  in
  module_
;;

let iter_declarations t ~f =
  let rec iter_module t =
    match t.splice with
    | Some splice ->
      let%bind.Result () = force_splice splice ~module_:t ~loc:splice.loc in
      iter_entries t t.entries
    | None -> iter_entries t t.entries
  and iter_entries t entries =
    let rec from index =
      if index >= Queue.length entries
      then Ok ()
      else (
        let%bind.Result () =
          match Queue.get entries index with
          | Included splice ->
            let%bind.Result () =
              force_splice splice ~module_:t ~loc:splice.loc
            in
            iter_entries t splice.included
          | Written (toplevel, generated_by) ->
            let%bind.Result () = f t toplevel ~generated_by in
            (match toplevel.txt with
             | Module (name, _) | Module_splice (name, _) ->
               iter_module (enter t name.txt)
             | Let _ | Type _ | Expect_test _ | Include _ -> Ok ())
        in
        from (index + 1))
    in
    from 0
  in
  iter_module t
;;

let rec find_module t name ~loc =
  match%bind.Result mem t (Module, name) ~loc with
  | true -> Ok (Some t)
  | false ->
    (match t.enclosing with
     | None -> Ok None
     | Some t -> find_module t name ~loc)
;;

let rec find_lexically t namespace name ~loc =
  match%bind.Result mem t (Member namespace, name) ~loc with
  | true -> Ok (Some (canonical_name t name))
  | false ->
    (match t.enclosing with
     | None -> Ok None
     | Some t -> find_lexically t namespace name ~loc)
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

let rec find_member t namespace name rest ~path ~written ~loc =
  match rest with
  | [] ->
    let%map.Result found = mem t (Member namespace, name) ~loc in
    Option.some_if found (canonical_name t name)
  | member :: rest ->
    let written = written @ [ name ] in
    (match%bind.Result mem t (Module, name) ~loc with
     | true ->
       let%bind.Result module_ = open_module t name ~loc in
       find_member module_ namespace member rest ~path ~written ~loc
     | false ->
       Error
         (Typecheck_error.create
            ~loc:(prefix_loc loc ~path ~written)
            (Unbound_module (Path.of_list_exn written))))
;;

let find t namespace (path : Path.t) ~loc =
  match (path :> string list) with
  | [] -> assert false
  | [ name ] -> find_lexically t namespace name ~loc
  | name :: member :: rest ->
    (match%bind.Result find_module t name ~loc with
     | None ->
       Error
         (Typecheck_error.create
            ~loc:(prefix_loc loc ~path ~written:[ name ])
            (Unbound_module (Path.name name)))
     | Some enclosing ->
       let%bind.Result module_ = open_module enclosing name ~loc in
       find_member module_ namespace member rest ~path ~written:[ name ] ~loc)
;;
