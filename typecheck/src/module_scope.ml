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

let create_root () = create ~path:[] ~enclosing:None

let add_name t names ({ txt = name; loc } : Name.t) ~duplicate =
  if Hash_set.mem names name
  then Error (Typecheck_error.create ~loc (duplicate (canonical_name t name)))
  else Ok (Hash_set.add names name)
;;

let add_names t names new_names ~duplicate =
  List.fold_result new_names ~init:() ~f:(fun () name ->
    add_name t names name ~duplicate)
;;

let rec add t toplevels =
  let%map.Result added =
    List.fold_result toplevels ~init:[] ~f:(fun added (toplevel : Toplevel.t) ->
      Queue.enqueue t.declarations toplevel;
      let%map.Result inner =
        match toplevel.txt with
        | Let (name, _, _, _) ->
          let%map.Result () =
            add_name t t.values name ~duplicate:(fun name ->
              Duplicate_toplevel name)
          in
          []
        | Type { name; kind; _ } ->
          let%bind.Result () =
            add_name t t.types name ~duplicate:(fun name -> Duplicate_type name)
          in
          let%map.Result () =
            match kind with
            | Alias _ | Abstract -> Ok ()
            | Variant constructors ->
              add_names
                t
                t.constructors
                (List.map constructors ~f:fst)
                ~duplicate:(fun name -> Duplicate_constructor name)
            | Record fields ->
              add_names
                t
                t.fields
                (List.map fields ~f:fst)
                ~duplicate:(fun name -> Duplicate_field name)
          in
          []
        | Expect_test _ -> Ok []
        | Module ({ txt = name; loc }, toplevels) ->
          let module_ = create ~path:(t.path @ [ name ]) ~enclosing:(Some t) in
          let%bind.Result inner = add module_ toplevels in
          (match Hashtbl.add t.modules ~key:name ~data:module_ with
           | `Ok -> Ok inner
           | `Duplicate ->
             Error
               (Typecheck_error.create
                  ~loc
                  (Duplicate_module (canonical_name t name))))
      in
      List.rev_append inner ((t, toplevel) :: added))
  in
  List.rev added
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
