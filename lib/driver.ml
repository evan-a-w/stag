open! Core

module Error = struct
  type t =
    | Source of Source_file.Error.t
    | Typecheck of Typecheck_error.t
  [@@deriving sexp_of]

  let loc = function
    | Source error -> Source_file.Error.loc error
    | Typecheck error -> error.loc
  ;;
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

(* The toplevels of each item, in program order, named canonically. *)
let rec toplevel_names ~prefix (items : Signature.t) =
  List.concat_map items ~f:(fun (item : Signature.Item.t) ->
    match item with
    | Value { name; _ } -> [ prefix ^ name ]
    | Type _ -> []
    | Module { name; items } ->
      toplevel_names ~prefix:(prefix ^ name ^ ".") items)
;;

let file_of_ident (ident : Typed.Ident.t) =
  String.lsplit2 ident.name ~on:'.' |> Option.map ~f:fst
;;

(* Files depend on the files whose toplevels theirs refer to. Components of
   files come after those they depend on, and otherwise in [files] order, as do
   files within a component. *)
let file_order ~(files : string list) (program : Typed.Program.t) =
  let index = String.Table.create () in
  List.iteri files ~f:(fun i file -> Hashtbl.set index ~key:file ~data:i);
  let n = List.length files in
  let deps = Array.create ~len:n Int.Set.empty in
  List.iter program.sccs ~f:(fun scc ->
    List.iter scc.members ~f:(fun member ->
      Option.bind (file_of_ident member.ident) ~f:(Hashtbl.find index)
      |> Option.iter ~f:(fun from ->
        ignore
          (Typed.Expr.refers_to member.body ~f:(function
             | Toplevel ident ->
               Option.bind (file_of_ident ident) ~f:(Hashtbl.find index)
               |> Option.iter ~f:(fun to_ ->
                 if to_ <> from then deps.(from) <- Set.add deps.(from) to_);
               false
             | Local _ -> false)))));
  (* Tarjan's algorithm. *)
  let component = Array.create ~len:n (-1) in
  let lowlink = Array.create ~len:n 0 in
  let order = Array.create ~len:n (-1) in
  let stack = Stack.create () in
  let next = ref 0 in
  let components = ref 0 in
  let rec visit v =
    order.(v) <- !next;
    lowlink.(v) <- !next;
    incr next;
    Stack.push stack v;
    Set.iter deps.(v) ~f:(fun w ->
      if order.(w) < 0
      then (
        visit w;
        lowlink.(v) <- Int.min lowlink.(v) lowlink.(w))
      else if component.(w) < 0
      then lowlink.(v) <- Int.min lowlink.(v) order.(w));
    if lowlink.(v) = order.(v)
    then (
      let rec pop () =
        let w = Stack.pop_exn stack in
        component.(w) <- !components;
        if w <> v then pop ()
      in
      pop ();
      incr components)
  in
  for v = 0 to n - 1 do
    if order.(v) < 0 then visit v
  done;
  let members = Array.create ~len:!components [] in
  for v = n - 1 downto 0 do
    members.(component.(v)) <- v :: members.(component.(v))
  done;
  let component_deps c =
    List.concat_map members.(c) ~f:(fun v ->
      Set.to_list deps.(v) |> List.map ~f:(fun w -> component.(w)))
    |> List.filter ~f:(fun d -> d <> c)
  in
  let done_ = Array.create ~len:!components false in
  (* Each step takes the ready component with the first file. *)
  let rec loop acc =
    let ready =
      List.init n ~f:Fn.id
      |> List.find ~f:(fun v ->
        let c = component.(v) in
        (not done_.(c))
        && List.for_all (component_deps c) ~f:(fun d -> done_.(d)))
    in
    match ready with
    | None -> List.rev acc
    | Some v ->
      let c = component.(v) in
      done_.(c) <- true;
      loop (List.rev_append members.(c) acc)
  in
  loop [] |> List.map ~f:(List.nth_exn files)
;;

let check_files ?(limits = Limits.unlimited) ?output files =
  let%bind.Result modules =
    List.map files ~f:Source_file.parse
    |> Result.all
    |> Result.map_error ~f:(fun error -> Error.Source error)
  in
  let interpreter = create_interpreter ~limits ~output in
  let%map.Result checked =
    Typecheck.typecheck (Typecheck.create ~interpreter ()) ~program:modules
    |> Result.map_error ~f:(fun error -> Error.Typecheck error)
  in
  let idents =
    List.concat_map checked.program.sccs ~f:(fun scc ->
      List.map scc.members ~f:(fun member -> member.ident.name, member.ident))
    |> String.Map.of_alist_exn
  in
  let files_in_order =
    file_order
      ~files:
        (List.filter_map modules ~f:(fun (toplevel : Syntax.Toplevel.t) ->
           match toplevel.txt with
           | Module (name, _) -> Some name.txt
           | Let _ | Type _ | Expect_test _ -> None))
      checked.program
  in
  let eval_order =
    List.concat_map files_in_order ~f:(fun file ->
      List.find_map checked.signature ~f:(function
        | Module { name; items } when String.equal name file ->
          Some (toplevel_names ~prefix:(name ^ ".") items)
        | Module _ | Value _ | Type _ -> None)
      |> Option.value ~default:[])
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
