open! Import

type t =
  { vars : Mode.Var.t Int.Table.t
  ; floors : (Mode.Var.Node.t * int) list
  ; base : int Mode_node.Table.t
  ; raises : (int * int Mode_node.Table.t) list Mode_node.Table.t
  }

let solver_node t (node : Mode_node.t) =
  let (T axis) = node.axis in
  Mode.Var.node (Hashtbl.find_exn t.vars node.var) axis
;;

(* The levels of the nodes of [t]'s variables, with [seeds] raised. *)
let levels ?floors t seeds =
  let table = Mode_node.Table.create () in
  match Mode.Var.Node.raised ?floors seeds with
  | None ->
    raise_s
      [%message
        "Mode_closure: a raise within bounds fails"
          (List.map seeds ~f:(fun (n, p) ->
             Mode.Var.Node.var_id n, Mode.Var.Node.axis n, p)
           : (int * Mode.Axis.Any.t * int) list)]
  | Some raised ->
    List.iter raised ~f:(fun (n, point) ->
      let var = Mode.Var.Node.var_id n in
      if Hashtbl.mem t.vars var
      then
        Hashtbl.update
          table
          { var; axis = Mode.Var.Node.axis n }
          ~f:(function
            | None -> point
            | Some point' -> Int.max point point'));
    table
;;

let lower t node =
  match Hashtbl.find t.base node with
  | Some point -> point
  | None -> Mode.Var.Node.lower (solver_node t node)
;;

let upper t node = Mode.Var.Node.upper (solver_node t node)

let create vars ~floors =
  let table = Int.Table.create () in
  List.iter vars ~f:(fun var ->
    Hashtbl.set table ~key:(Mode.Var.id var) ~data:var);
  let t =
    { vars = table
    ; floors = []
    ; base = Mode_node.Table.create ()
    ; raises = Mode_node.Table.create ()
    }
  in
  (* A generic node's bound its scheme adds once a layout has pointers raises
     what it is below. *)
  let shared = Mode.Var.Node.Floors.create () in
  let scheme_lowers =
    List.concat_map vars ~f:(fun var ->
      List.map Mode.Axis.Any.all ~f:(fun (T axis) ->
        let node = Mode.Var.node var axis in
        node, Mode.Var.Node.Floors.lower shared node))
  in
  let floors =
    List.map floors ~f:(fun (node, point) -> solver_node t node, point)
    @ scheme_lowers
  in
  { t with floors; base = levels ~floors:shared t floors }
;;

let domain t node =
  List.range ~start:`exclusive (lower t node) ~stop:`inclusive (upper t node)
;;

let raised t x p =
  let cached = Hashtbl.find_multi t.raises x in
  match List.Assoc.find cached p ~equal:Int.equal with
  | Some raised -> raised
  | None ->
    let raised = levels t ((solver_node t x, p) :: t.floors) in
    Hashtbl.add_multi t.raises ~key:x ~data:(p, raised);
    raised
;;

(* [x]'s raise to [p], as the nodes raised and their points. *)
let raised_alist t x p = Hashtbl.to_alist (raised t x p)

let raise t x p y =
  if p <= lower t x
  then lower t y
  else (
    match Hashtbl.find (raised t x p) y with
    | Some point -> Int.max point (lower t y)
    | None -> lower t y)
;;

let reached t x =
  List.concat_map (domain t x) ~f:(fun p ->
    Hashtbl.to_alist (raised t x p)
    |> List.filter_map ~f:(fun (y, point) ->
      Option.some_if (point > lower t y) y))
  |> List.dedup_and_sort ~compare:Mode_node.compare
;;
