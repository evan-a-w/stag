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
let levels t seeds =
  let table = Mode_node.Table.create () in
  match Mode.Var.Node.raised seeds with
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
  let floors =
    List.map floors ~f:(fun (node, point) -> solver_node t node, point)
  in
  { t with floors; base = levels t floors }
;;

let domain t node =
  List.range ~start:`exclusive (lower t node) ~stop:`inclusive (upper t node)
;;

let raise t x p y =
  if p <= lower t x
  then lower t y
  else (
    let raised =
      let cached = Hashtbl.find_multi t.raises x in
      match List.Assoc.find cached p ~equal:Int.equal with
      | Some raised -> raised
      | None ->
        let raised = levels t ((solver_node t x, p) :: t.floors) in
        Hashtbl.add_multi t.raises ~key:x ~data:(p, raised);
        raised
    in
    match Hashtbl.find raised y with
    | Some point -> point
    | None -> lower t y)
;;
