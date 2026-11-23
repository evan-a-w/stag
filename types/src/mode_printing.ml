open! Import
module Stage = Mode.Stage

module Operand = struct
  type t =
    | Var of int
    | Const of Stage.t

  let to_string ~name = function
    | Var rep -> name rep
    | Const stage -> Sexp.to_string (Stage.sexp_of_t stage) |> String.lowercase
  ;;
end

type t =
  { vars : Mode.Var.t Int.Table.t
  ; order : int Int.Table.t
  ; counts : int Int.Table.t
  ; rep : int Int.Table.t
  ; operand : Operand.t Int.Table.t
  ; elided : Int.Hash_set.t
  ; names : string Int.Table.t
  ; mutable constraints : (Operand.t * Operand.t) list
  }

let create () =
  { vars = Int.Table.create ()
  ; order = Int.Table.create ()
  ; counts = Int.Table.create ()
  ; rep = Int.Table.create ()
  ; operand = Int.Table.create ()
  ; elided = Int.Hash_set.create ()
  ; names = Int.Table.create ()
  ; constraints = []
  }
;;

let count t var =
  let id = Mode.Var.id var in
  Hashtbl.set t.vars ~key:id ~data:var;
  if not (Hashtbl.mem t.order id)
  then Hashtbl.set t.order ~key:id ~data:(Hashtbl.length t.order);
  Hashtbl.incr t.counts id
;;

let settle t =
  let edges = Int.Table.create () in
  let nodes = Int.Hash_set.create () in
  let add_edge lo hi =
    Hash_set.add nodes lo;
    Hash_set.add nodes hi;
    Hashtbl.add_multi edges ~key:lo ~data:hi
  in
  Hashtbl.iteri t.vars ~f:(fun ~key:id ~data:var ->
    Hash_set.add nodes id;
    List.iter (Mode.Var.generic_relations var Stage) ~f:(function
      | `Le, other -> add_edge id other
      | `Ge, other -> add_edge other id));
  let reach = Int.Table.create () in
  Hash_set.iter nodes ~f:(fun start ->
    let seen = Int.Hash_set.create () in
    let rec go id =
      if not (Hash_set.mem seen id)
      then (
        Hash_set.add seen id;
        List.iter (Hashtbl.find_multi edges id) ~f:go)
    in
    go start;
    Hashtbl.set reach ~key:start ~data:seen);
  let reaches a b = Hash_set.mem (Hashtbl.find_exn reach a) b in
  (* Counted variables first, in the order they are named, then the others. *)
  let position id =
    match Hashtbl.find t.order id with
    | Some position -> position, id
    | None -> Int.max_value, id
  in
  let by_position =
    Hash_set.to_list nodes
    |> List.sort ~compare:(fun a b ->
      [%compare: int * int] (position a) (position b))
  in
  Hashtbl.clear t.rep;
  List.iter by_position ~f:(fun id ->
    match
      List.find by_position ~f:(fun other ->
        reaches id other && reaches other id)
    with
    | Some rep -> Hashtbl.set t.rep ~key:id ~data:rep
    | None -> Hashtbl.set t.rep ~key:id ~data:id);
  let reps =
    List.filter by_position ~f:(fun id -> Hashtbl.find_exn t.rep id = id)
  in
  let members rep =
    List.filter by_position ~f:(fun id -> Hashtbl.find_exn t.rep id = rep)
  in
  let bounds rep =
    List.find_map (members rep) ~f:(fun id ->
      Hashtbl.find t.vars id
      |> Option.map ~f:(fun var ->
        (Mode.Var.lower var).stage, (Mode.Var.upper var).stage))
    |> Option.value ~default:(Stage.min, Stage.max)
  in
  let operand rep : Operand.t =
    let lower, upper = bounds rep in
    if Stage.equal lower upper then Const lower else Var rep
  in
  Hashtbl.clear t.operand;
  List.iter reps ~f:(fun rep ->
    Hashtbl.set t.operand ~key:rep ~data:(operand rep));
  (* Strictly above, between classes. *)
  let above a b = (not (a = b)) && reaches a b in
  let edges =
    List.concat_map reps ~f:(fun a ->
      List.filter_map reps ~f:(fun b ->
        Option.some_if
          (above a b
           && not
                (List.exists reps ~f:(fun c ->
                   c <> a && c <> b && above a c && above c b)))
          (a, b)))
  in
  let constraints =
    List.concat_map reps ~f:(fun rep ->
      let lower, upper = bounds rep in
      let op = Hashtbl.find_exn t.operand rep in
      match op with
      | Const _ ->
        List.filter_map edges ~f:(fun (a, b) ->
          Option.some_if (a = rep) (op, Hashtbl.find_exn t.operand b))
      | Var _ ->
        let implied_lower =
          Stage.equal lower Stage.min
          || List.exists reps ~f:(fun other ->
            above other rep && Stage.le lower (fst (bounds other)))
        in
        let implied_upper =
          Stage.equal upper Stage.max
          || List.exists reps ~f:(fun other ->
            above rep other && Stage.le (snd (bounds other)) upper)
        in
        (if implied_lower then [] else [ Operand.Const lower, op ])
        @ List.filter_map edges ~f:(fun (a, b) ->
          Option.some_if (a = rep) (op, Hashtbl.find_exn t.operand b))
        @ if implied_upper then [] else [ op, Operand.Const upper ])
    |> List.filter ~f:(fun ((lo : Operand.t), (hi : Operand.t)) ->
      match lo, hi with
      | Const _, Const _ -> false
      | _ -> true)
  in
  t.constraints <- constraints;
  Hash_set.clear t.elided;
  let mentioned = Int.Hash_set.create () in
  List.iter constraints ~f:(fun (lo, hi) ->
    List.iter [ lo; hi ] ~f:(function
      | Var rep -> Hash_set.add mentioned rep
      | Const _ -> ()));
  List.iter reps ~f:(fun rep ->
    match members rep, Hashtbl.find_exn t.operand rep with
    | [ id ], Var _ ->
      if [%equal: int option] (Hashtbl.find t.counts id) (Some 1)
         && not (Hash_set.mem mentioned rep)
      then Hash_set.add t.elided rep
    | _ -> ())
;;

let name t rep =
  Hashtbl.find_or_add t.names rep ~default:(fun () ->
    sprintf "'m%d" (Hashtbl.length t.names))
;;

let side t var =
  let id = Mode.Var.id var in
  let rep = Hashtbl.find t.rep id |> Option.value ~default:id in
  if Hash_set.mem t.elided rep
  then None
  else (
    let operand =
      Hashtbl.find t.operand rep |> Option.value ~default:(Operand.Var rep)
    in
    Some (" @ " ^ Operand.to_string ~name:(name t) operand))
;;

let constraints t =
  List.map t.constraints ~f:(fun (lo, hi) ->
    Operand.to_string ~name:(name t) lo
    ^ " <= "
    ^ Operand.to_string ~name:(name t) hi)
;;
