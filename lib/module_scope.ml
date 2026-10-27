open! Core
open Syntax

module Namespace = struct
  type t =
    | Value
    | Type
    | Constructor
    | Field
end

module Members = struct
  type t =
    { values : String.Set.t
    ; types : String.Set.t
    ; constructors : String.Set.t
    ; fields : String.Set.t
    ; modules : t String.Map.t
    ; toplevels : Toplevel.t list
    }

  let names t : Namespace.t -> _ = function
    | Value -> t.values
    | Type -> t.types
    | Constructor -> t.constructors
    | Field -> t.fields
  ;;
end

type t =
  { members : Members.t
  ; path : string list
  ; enclosing : t option
  }

let join path = String.concat path ~sep:"."
let sexp_of_t t = Sexp.Atom (join t.path)
let canonical_name t name = join (t.path @ [ name ])

let enter t name =
  { members = Map.find_exn t.members.modules name
  ; path = t.path @ [ name ]
  ; enclosing = Some t
  }
;;

let add_name names ({ txt = name; loc } : Name.t) ~path ~duplicate =
  if Set.mem names name
  then Error (Typecheck_error.create ~loc (duplicate (join (path @ [ name ]))))
  else Ok (Set.add names name)
;;

let add_names names new_names ~path ~duplicate =
  List.fold_result new_names ~init:names ~f:(fun names name ->
    add_name names name ~path ~duplicate)
;;

let rec members_of_toplevels toplevels ~path
  : (Members.t, Typecheck_error.t) Result.t
  =
  let init =
    { Members.values = String.Set.empty
    ; types = String.Set.empty
    ; constructors = String.Set.empty
    ; fields = String.Set.empty
    ; modules = String.Map.empty
    ; toplevels
    }
  in
  List.fold_result
    toplevels
    ~init
    ~f:(fun (members : Members.t) (toplevel : Toplevel.t) ->
      match toplevel.txt with
      | Let (name, _, _, _) ->
        let%map.Result values =
          add_name members.values name ~path ~duplicate:(fun name ->
            Duplicate_toplevel name)
        in
        { members with values }
      | Type { name; kind; _ } ->
        let%bind.Result types =
          add_name members.types name ~path ~duplicate:(fun name ->
            Duplicate_type name)
        in
        let members = { members with types } in
        (match kind with
         | Alias _ | Abstract -> Ok members
         | Variant constructors ->
           let%map.Result constructors =
             add_names
               members.constructors
               (List.map constructors ~f:fst)
               ~path
               ~duplicate:(fun name -> Duplicate_constructor name)
           in
           { members with constructors }
         | Record fields ->
           let%map.Result fields =
             add_names
               members.fields
               (List.map fields ~f:fst)
               ~path
               ~duplicate:(fun name -> Duplicate_field name)
           in
           { members with fields })
      | Expect_test _ -> Ok members
      | Module ({ txt = name; loc }, toplevels) ->
        let%bind.Result module_ =
          members_of_toplevels toplevels ~path:(path @ [ name ])
        in
        (match Map.add members.modules ~key:name ~data:module_ with
         | `Ok modules -> Ok { members with modules }
         | `Duplicate ->
           Error
             (Typecheck_error.create
                ~loc
                (Duplicate_module (join (path @ [ name ]))))))
;;

let create program =
  let%map.Result members = members_of_toplevels program ~path:[] in
  { members; path = []; enclosing = None }
;;

let rec toplevels t =
  List.concat_map t.members.toplevels ~f:(fun (toplevel : Toplevel.t) ->
    (t, toplevel)
    ::
    (match toplevel.txt with
     | Module (name, _) -> toplevels (enter t name.txt)
     | Let _ | Type _ | Expect_test _ -> []))
;;

let rec find_module t name =
  if Map.mem t.members.modules name
  then Some (enter t name)
  else Option.bind t.enclosing ~f:(fun t -> find_module t name)
;;

let rec find_lexically t namespace name =
  if Set.mem (Members.names t.members namespace) name
  then Some (canonical_name t name)
  else Option.bind t.enclosing ~f:(fun t -> find_lexically t namespace name)
;;

let rec find_member t namespace name rest ~written ~loc =
  match rest with
  | [] ->
    Ok
      (Option.some_if
         (Set.mem (Members.names t.members namespace) name)
         (canonical_name t name))
  | member :: rest ->
    let written = written @ [ name ] in
    if Map.mem t.members.modules name
    then find_member (enter t name) namespace member rest ~written ~loc
    else
      Error
        (Typecheck_error.create
           ~loc
           (Unbound_module (Path.of_list_exn written)))
;;

let find t namespace (path : Path.t) ~loc =
  match (path :> string list) with
  | [] -> assert false
  | [ name ] -> Ok (find_lexically t namespace name)
  | name :: member :: rest ->
    (match find_module t name with
     | None ->
       Error (Typecheck_error.create ~loc (Unbound_module (Path.name name)))
     | Some module_ ->
       find_member module_ namespace member rest ~written:[ name ] ~loc)
;;
