open! Core

module Error = struct
  type t =
    | Source of Source_file.Error.t
    | Typecheck of Typecheck_error.t
  [@@deriving sexp_of]
end

type t =
  { checked : Typecheck.Checked.t
  ; interpreter : Treewalk.t
  ; eval_order : Typed.Ident.t list
  }
[@@deriving fields ~getters]

let create_interpreter ~limits ~output =
  let interpreter = Treewalk.create ~stamps:(Stamp_counter.create ()) () in
  Option.iter output ~f:(Treewalk.set_output interpreter);
  Treewalk.set_limits interpreter limits;
  interpreter
;;

let check ?(limits = Limits.unlimited) ?output program =
  let interpreter = create_interpreter ~limits ~output in
  let%map.Result checked =
    Typecheck.typecheck (Typecheck.create ~interpreter ()) ~program
  in
  { checked; interpreter; eval_order = [] }
;;

module Module_group = struct
  type t =
    | File of Syntax.Toplevel.t
    | Group of Syntax.Name.t * t list

  let name = function
    | File { txt = Module (name, _); _ } | Group (name, _) -> name.txt
    | File { txt = Let _ | Type _ | Expect_test _; _ } ->
      raise_s [%message "Driver.check_modules: a file is not a module"]
  ;;

  let rec to_toplevel = function
    | File toplevel -> toplevel
    | Group (name, members) ->
      Located.create
        ~loc:name.loc
        (Syntax.Toplevel.Module (name, List.map members ~f:to_toplevel))
  ;;
end

(* Pairs of toplevels, by canonical name, where the first refers to the
   second. *)
let references (program : Typed.Program.t) =
  List.concat_map program.sccs ~f:(fun scc ->
    List.concat_map scc.members ~f:(fun member ->
      let to_ = Queue.create () in
      ignore
        (Typed.Expr.refers_to member.body ~f:(function
           | Toplevel ident ->
             Queue.enqueue to_ ident.name;
             false
           | Local _ -> false)
         : bool);
      Queue.to_list to_ |> List.map ~f:(fun to_ -> member.ident.name, to_)))
;;

(* The members of a group depend on those their toplevels refer to.
   Components of members come after those they depend on, and otherwise in
   [members] order, as do members within a component. *)
let group_order ~prefix ~references (members : Module_group.t list) =
  let index = String.Table.create () in
  List.iteri members ~f:(fun i member ->
    Hashtbl.set index ~key:(Module_group.name member) ~data:i);
  let member_of name =
    let%bind.Option rest = String.chop_prefix name ~prefix in
    let%bind.Option member, _ = String.lsplit2 rest ~on:'.' in
    Hashtbl.find index member
  in
  let deps = Array.create ~len:(List.length members) Int.Set.empty in
  List.iter references ~f:(fun (from, to_) ->
    match member_of from, member_of to_ with
    | Some from, Some to_ when from <> to_ ->
      deps.(from) <- Set.add deps.(from) to_
    | _ -> ());
  let members = Array.of_list members in
  Scc.topological_components ~successors:(Array.map deps ~f:Set.to_list)
  |> List.concat
  |> List.map ~f:(Array.get members)
;;

(* The toplevels of each item, in program order, named canonically. *)
let rec toplevel_names ~prefix (items : Signature.t) =
  List.concat_map items ~f:(fun (item : Signature.Item.t) ->
    match item with
    | Value { name; _ } -> [ prefix ^ name ]
    | Type _ -> []
    | Module { name; items } ->
      toplevel_names ~prefix:(prefix ^ name ^ ".") items)
;;

let rec eval_order ~prefix ~references ~(items : Signature.t) members =
  group_order ~prefix ~references members
  |> List.concat_map ~f:(fun (member : Module_group.t) ->
    let name = Module_group.name member in
    let items =
      List.find_map items ~f:(function
        | Module { name = name'; items } when String.equal name name' ->
          Some items
        | Module _ | Value _ | Type _ -> None)
      |> Option.value ~default:[]
    in
    let prefix = prefix ^ name ^ "." in
    match member with
    | File _ -> toplevel_names ~prefix items
    | Group (_, members) -> eval_order ~prefix ~references ~items members)
;;

let check_modules ?(limits = Limits.unlimited) ?output groups =
  let interpreter = create_interpreter ~limits ~output in
  let%map.Result checked =
    Typecheck.typecheck
      (Typecheck.create ~interpreter ())
      ~program:(List.map groups ~f:Module_group.to_toplevel)
  in
  let idents =
    List.concat_map checked.program.sccs ~f:(fun scc ->
      List.map scc.members ~f:(fun member -> member.ident.name, member.ident))
    |> String.Map.of_alist_exn
  in
  let eval_order =
    eval_order
      ~prefix:""
      ~references:(references checked.program)
      ~items:checked.signature
      groups
    |> List.map ~f:(Map.find_exn idents)
  in
  { checked; interpreter; eval_order }
;;

let eval ?(limits = Limits.unlimited) t =
  Treewalk.set_limits t.interpreter limits;
  let%bind.Or_error () =
    List.fold_result t.eval_order ~init:() ~f:(fun () ident ->
      Treewalk.eval_toplevel t.interpreter ~ident)
  in
  Treewalk.eval t.interpreter
;;

let check_files ?limits ?output files =
  let%bind.Result modules =
    List.map files ~f:Source_file.parse
    |> Result.all
    |> Result.map_error ~f:(fun error -> Error.Source error)
  in
  check_modules
    ?limits
    ?output
    (List.map modules ~f:(fun file -> Module_group.File file))
  |> Result.map_error ~f:(fun error -> Error.Typecheck error)
;;
