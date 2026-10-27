open! Import
open Syntax

module Namespace = struct
  type t =
    | Value
    | Type
    | Constructor
    | Field
end

type t =
  { values : String.Hash_set.t
  ; types : String.Hash_set.t
  ; constructors : String.Hash_set.t
  ; fields : String.Hash_set.t
  ; modules : t String.Table.t
  ; declarations : Toplevel.t Queue.t
  ; path : string list
  ; enclosing : t option
  }

let names t : Namespace.t -> _ = function
  | Value -> t.values
  | Type -> t.types
  | Constructor -> t.constructors
  | Field -> t.fields
;;

let join path = String.concat path ~sep:"."
let sexp_of_t t = Sexp.Atom (join t.path)
let canonical_name t name = join (t.path @ [ name ])
let enter t name = Hashtbl.find_exn t.modules name
let declarations t = Queue.to_list t.declarations

let create ~path ~enclosing =
  { values = String.Hash_set.create ()
  ; types = String.Hash_set.create ()
  ; constructors = String.Hash_set.create ()
  ; fields = String.Hash_set.create ()
  ; modules = String.Table.create ()
  ; declarations = Queue.create ()
  ; path
  ; enclosing
  }
;;

let create_root ~types =
  let t = create ~path:[] ~enclosing:None in
  List.iter types ~f:(Hash_set.add t.types);
  t
;;

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
    | Expect_test _ -> Ok ()
    | Module (name, toplevels) ->
      let%bind.Result () =
        add_member modules name ~duplicate:(fun name -> Duplicate_module name)
      in
      check_unique
        (create ~path:(t.path @ [ name.txt ]) ~enclosing:(Some t))
        toplevels)
;;

let rec register t toplevels ~declare =
  List.iter toplevels ~f:(fun (toplevel : Toplevel.t) ->
    declare t toplevel;
    Queue.enqueue t.declarations toplevel;
    let add names (name : Name.t) = Hash_set.add names name.txt in
    match toplevel.txt with
    | Let (name, _, _, _) -> add t.values name
    | Type { name; params = _; kind } ->
      add t.types name;
      (match kind with
       | Alias _ | Abstract -> ()
       | Variant constructors ->
         List.iter constructors ~f:(fun (name, _) -> add t.constructors name)
       | Record fields ->
         List.iter fields ~f:(fun (name, _) -> add t.fields name))
    | Expect_test _ -> ()
    | Module ({ txt = name; loc = _ }, toplevels) ->
      let module_ = create ~path:(t.path @ [ name ]) ~enclosing:(Some t) in
      register module_ toplevels ~declare;
      Hashtbl.add_exn t.modules ~key:name ~data:module_)
;;

let add t toplevels ~declare =
  let%map.Result () = check_unique t toplevels in
  register t toplevels ~declare
;;

let rec iter_declarations t ~f =
  let rec from index =
    if index >= Queue.length t.declarations
    then Ok ()
    else (
      let toplevel : Toplevel.t = Queue.get t.declarations index in
      let%bind.Result () = f t toplevel in
      let%bind.Result () =
        match toplevel.txt with
        | Module (name, _) -> iter_declarations (enter t name.txt) ~f
        | Let _ | Type _ | Expect_test _ -> Ok ()
      in
      from (index + 1))
  in
  from 0
;;

let rec find_module t name =
  match Hashtbl.find t.modules name with
  | Some module_ -> Some module_
  | None -> Option.bind t.enclosing ~f:(fun t -> find_module t name)
;;

let rec find_lexically t namespace name =
  if Hash_set.mem (names t namespace) name
  then Some (canonical_name t name)
  else Option.bind t.enclosing ~f:(fun t -> find_lexically t namespace name)
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
    Ok
      (Option.some_if
         (Hash_set.mem (names t namespace) name)
         (canonical_name t name))
  | member :: rest ->
    let written = written @ [ name ] in
    (match Hashtbl.find t.modules name with
     | Some module_ ->
       find_member module_ namespace member rest ~path ~written ~loc
     | None ->
       Error
         (Typecheck_error.create
            ~loc:(prefix_loc loc ~path ~written)
            (Unbound_module (Path.of_list_exn written))))
;;

let find t namespace (path : Path.t) ~loc =
  match (path :> string list) with
  | [] -> assert false
  | [ name ] -> Ok (find_lexically t namespace name)
  | name :: member :: rest ->
    (match find_module t name with
     | None ->
       Error
         (Typecheck_error.create
            ~loc:(prefix_loc loc ~path ~written:[ name ])
            (Unbound_module (Path.name name)))
     | Some module_ ->
       find_member module_ namespace member rest ~path ~written:[ name ] ~loc)
;;
