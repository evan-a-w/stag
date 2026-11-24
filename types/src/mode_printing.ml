open! Import
module Written = Mode.Written_morphism

(* [lo <= w(hi)], as [w]'s left adjoint, [w(lo) <= hi] reads: raising [lo]
   raises [hi]. *)
module Relation = struct
  type t =
    { lo : Mode_node.t
    ; written : Written.t
    ; hi : Mode_node.t
    }

  let edge t =
    { Mode_model.Edge.lo = t.lo
    ; morphism = Written.morphism t.written
    ; hi = t.hi
    }
  ;;
end

module Printed = struct
  type t =
    | Lower of Mode_node.t * int
    | Upper of Mode_node.t * int
    | Plain of int * int (** On every axis. *)
    | Relation of Relation.t

  let item : t -> Mode_model.Item.t = function
    | Lower (n, p) -> Lower (n, p)
    | Upper (n, p) -> Upper (n, p)
    | Relation r -> Edges [ Relation.edge r ]
    | Plain (a, b) ->
      Edges
        (List.map Mode.Axis.Any.all ~f:(fun axis ->
           Relation.edge
             { lo = { var = a; axis }
             ; written = Plain axis
             ; hi = { var = b; axis }
             }))
  ;;
end

type t =
  { vars : Mode.Var.t Int.Table.t
  ; order : int Int.Table.t
  ; counts : int Int.Table.t
  ; results : Int.Hash_set.t
  ; on_arrows : Int.Hash_set.t
  ; mode_args : Int.Hash_set.t
  ; rep : int Int.Table.t
  ; atoms : (Mode.Axis.Any.t * int) list Int.Table.t
  ; names : string Int.Table.t
  ; mutable printed : Printed.t list
  }

let create () =
  { vars = Int.Table.create ()
  ; order = Int.Table.create ()
  ; counts = Int.Table.create ()
  ; results = Int.Hash_set.create ()
  ; on_arrows = Int.Hash_set.create ()
  ; mode_args = Int.Hash_set.create ()
  ; rep = Int.Table.create ()
  ; atoms = Int.Table.create ()
  ; names = Int.Table.create ()
  ; printed = []
  }
;;

let count t var ~(side : [ `Arg | `Res | `Mode_arg ]) =
  let id = Mode.Var.id var in
  Hashtbl.set t.vars ~key:id ~data:var;
  (match side with
   | `Arg -> Hash_set.add t.on_arrows id
   | `Res ->
     Hash_set.add t.on_arrows id;
     Hash_set.add t.results id
   | `Mode_arg -> Hash_set.add t.mode_args id);
  if not (Hashtbl.mem t.order id)
  then Hashtbl.set t.order ~key:id ~data:(Hashtbl.length t.order);
  Hashtbl.incr t.counts id
;;

let portable = 1

(* Whether [b] is at least [a] on the axis. *)
let at_least closure a b axis =
  let a = { Mode_node.var = a; axis } in
  let b = { Mode_node.var = b; axis } in
  List.for_all (Mode_closure.domain closure a) ~f:(fun p ->
    Mode_closure.raise closure a p b >= p)
;;

(* Each written form, with its morphism and table. *)
let written_forms =
  List.map Written.all ~f:(fun written ->
    let m = Written.morphism written in
    ( written
    , m
    , Array.init
        (Mode.Axis.Any.size (Mode.Morphism.dst m))
        ~f:(Mode.Morphism.left m) ))
;;

(* The relation of a written form from [lo] to [hi] the closure has, the
   greatest (they are a chain), or [None] if it implies nothing more than the
   bounds do (or, from a node to itself, than that it is itself). *)
let strongest closure ~(lo : Mode_node.t) ~(hi : Mode_node.t) =
  let points =
    Array.of_list
      (Mode_closure.lower closure lo :: Mode_closure.domain closure lo)
  in
  let raised =
    Array.map points ~f:(fun p -> Mode_closure.raise closure lo p hi)
  in
  let valid =
    List.filter written_forms ~f:(fun (_, m, table) ->
      Mode.Axis.Any.equal (Mode.Morphism.dst m) lo.axis
      && Mode.Axis.Any.equal (Mode.Morphism.src m) hi.axis
      && Array.for_alli points ~f:(fun i p -> table.(p) <= raised.(i)))
  in
  let le (_, _, a) (_, _, b) =
    Array.for_all points ~f:(fun p -> a.(p) <= b.(p))
  in
  List.find valid ~f:(fun w -> List.for_all valid ~f:(fun w' -> le w' w))
  |> Option.filter ~f:(fun (_, m, _) ->
    List.exists (Mode_closure.domain closure lo) ~f:(fun p ->
      Mode.Morphism.left m p
      > if Mode_node.equal lo hi then p else Mode_closure.lower closure hi))
  |> Option.map ~f:(fun (written, _, _) -> { Relation.lo; written; hi })
;;

let is_plain (r : Relation.t) =
  match r.written with
  | Plain _ -> true
  | Meet _ | Release | Static_if_const | Const_if_static -> false
;;

(* The variables are grouped into classes equal on every axis, each printed as
   one. Every relation of the written forms the constraints imply between
   classes (as the greatest form per pair of nodes), or of every axis between
   two classes, and every bound is a candidate; [Mode_model.reduce] drops those
   the rest imply. A class related to nothing, named once (or fixed on every
   axis), prints as the atoms of its fixed axes.

   A mode argument has no locality, so neither has a variable only of mode
   arguments (a [with] clause can't relate it, and relates it on its other axes
   only): its locality is left out. *)
let settle t =
  let ids =
    Hashtbl.keys t.vars
    |> List.sort ~compare:(fun a b ->
      Int.compare (Hashtbl.find_exn t.order a) (Hashtbl.find_exn t.order b))
  in
  let closure =
    Mode_closure.create
      (List.map ids ~f:(Hashtbl.find_exn t.vars))
      ~floors:
        (List.filter_map ids ~f:(fun id ->
           Option.some_if
             (Hash_set.mem t.results id)
             ({ Mode_node.var = id; axis = T Stage }, portable)))
  in
  let live var (Mode.Axis.Any.T axis) =
    match axis with
    | Locality -> Hash_set.mem t.on_arrows var
    | Stage | Provenance -> true
  in
  let axes var = List.filter Mode.Axis.Any.all ~f:(live var) in
  (* Candidates to be equal share their bounds, so are found by them. *)
  let signature var =
    List.fold Mode.Axis.Any.all ~init:0 ~f:(fun acc axis ->
      (acc * 16)
      +
      if live var axis
      then (
        let node = { Mode_node.var; axis } in
        1
        + (Mode_closure.lower closure node * 4)
        + Mode_closure.upper closure node)
      else 0)
  in
  let equal a b =
    List.for_all (axes a) ~f:(fun axis ->
      at_least closure a b axis && at_least closure b a axis)
  in
  Hashtbl.clear t.rep;
  let classes = Int.Table.create () in
  let reps = Queue.create () in
  let members = Int.Table.create () in
  List.iter ids ~f:(fun id ->
    let key = signature id in
    let candidates = Hashtbl.find_multi classes key in
    let rep =
      match List.find candidates ~f:(equal id) with
      | Some rep -> rep
      | None ->
        Hashtbl.add_multi classes ~key ~data:id;
        Queue.enqueue reps id;
        id
    in
    Hashtbl.set t.rep ~key:id ~data:rep;
    Hashtbl.add_multi members ~key:rep ~data:id);
  let reps = Queue.to_list reps in
  let members rep = Hashtbl.find_multi members rep in
  let nodes =
    List.concat_map reps ~f:(fun var ->
      List.map (axes var) ~f:(fun axis -> { Mode_node.var; axis }))
  in
  let is_node = Mode_node.Hash_set.of_list nodes in
  let base_lower ({ var; axis } : Mode_node.t) =
    match axis with
    | T Stage when List.exists (members var) ~f:(Hash_set.mem t.results) ->
      portable
    | T (Stage | Provenance | Locality) -> 0
  in
  (* Every variable's [const] is [static]. *)
  let implicit =
    List.map reps ~f:(fun var ->
      { Mode_model.Edge.lo = { var; axis = T Provenance }
      ; morphism = Mode.Morphism.const_is_static
      ; hi = { var; axis = T Stage }
      })
  in
  let relations =
    List.concat_map nodes ~f:(fun lo ->
      List.filter_map (Mode_closure.reached closure lo) ~f:(fun hi ->
        if Hash_set.mem is_node hi then strongest closure ~lo ~hi else None))
  in
  (* A pair related on every axis both have prints as one relation. *)
  let plain_valid a b =
    List.for_all (axes a) ~f:(fun axis ->
      (not (live b axis))
      ||
      let lo = { Mode_node.var = a; axis } in
      let hi = { Mode_node.var = b; axis } in
      List.for_all
        (Mode_closure.lower closure lo :: Mode_closure.domain closure lo)
        ~f:(fun p -> Mode_closure.raise closure lo p hi >= p))
  in
  let order id = Hashtbl.find_exn t.order id in
  let plain_pairs =
    List.filter_map relations ~f:(fun r ->
      Option.some_if (is_plain r && r.lo.var <> r.hi.var) (r.lo.var, r.hi.var))
    |> List.dedup_and_sort ~compare:(fun (a, b) (a', b') ->
      [%compare: int * int] (order a, order b) (order a', order b'))
    |> List.filter ~f:(fun (a, b) -> plain_valid a b)
  in
  let plain_set = Int.Table.create () in
  List.iter plain_pairs ~f:(fun (a, b) ->
    Hash_set.add
      (Hashtbl.find_or_add plain_set a ~default:Int.Hash_set.create)
      b);
  let in_plain (r : Relation.t) =
    is_plain r
    && Option.exists (Hashtbl.find plain_set r.lo.var) ~f:(fun set ->
      Hash_set.mem set r.hi.var)
  in
  let bounds =
    List.concat_map nodes ~f:(fun n ->
      let lower = Mode_closure.lower closure n
      and upper = Mode_closure.upper closure n in
      (if lower > base_lower n then [ Printed.Lower (n, lower) ] else [])
      @
      if upper < Mode_node.max n.axis then [ Printed.Upper (n, upper) ] else [])
  in
  let item : Printed.t -> Mode_model.Item.t = function
    | Plain (a, b) ->
      Edges
        (List.filter_map Mode.Axis.Any.all ~f:(fun axis ->
           Option.some_if
             (live a axis && live b axis)
             (Relation.edge
                { lo = { var = a; axis }
                ; written = Plain axis
                ; hi = { var = b; axis }
                })))
    | printed -> Printed.item printed
  in
  (* Latest first, then pairs, then bounds. *)
  let printed =
    Mode_model.reduce
      closure
      ~nodes
      ~base_lower
      ~implicit
      (List.rev_map
         (List.filter relations ~f:(Fn.non in_plain))
         ~f:(fun r -> Printed.Relation r)
       @ List.rev_map plain_pairs ~f:(fun (a, b) -> Printed.Plain (a, b))
       @ bounds)
      ~item
  in
  let related = Int.Hash_set.create () in
  let bounded = Mode_node.Hash_set.create () in
  List.iter printed ~f:(function
    | Relation r ->
      Hash_set.add related r.lo.var;
      Hash_set.add related r.hi.var
    | Plain (a, b) ->
      Hash_set.add related a;
      Hash_set.add related b
    | Lower (n, _) | Upper (n, _) -> Hash_set.add bounded n);
  (* A class's axis reads back from an atom if it is fixed, or, on an arrow, if
     no bound of it is printed. A mode argument's unwritten axes read back as
     the default, so a class of one is written as atoms only if fixed. *)
  Hashtbl.clear t.atoms;
  List.iter reps ~f:(fun rep ->
    let members = members rep in
    let node axis = { Mode_node.var = rep; axis } in
    let fixed axis =
      Mode_closure.lower closure (node axis)
      = Mode_closure.upper closure (node axis)
    in
    let all_fixed = List.for_all (axes rep) ~f:fixed in
    if (not (Hash_set.mem related rep))
       && List.for_all (axes rep) ~f:(fun axis ->
         fixed axis || not (Hash_set.mem bounded (node axis)))
       && (all_fixed
           || ((not (List.exists members ~f:(Hash_set.mem t.mode_args)))
               &&
               match members with
               | [ id ] ->
                 [%equal: int option] (Hashtbl.find t.counts id) (Some 1)
                 && Mode.Var.is_generic (Hashtbl.find_exn t.vars id)
               | _ -> false))
    then
      Hashtbl.set
        t.atoms
        ~key:rep
        ~data:
          (List.filter_map (axes rep) ~f:(fun axis ->
             Option.some_if
               ((fixed axis && List.exists members ~f:(Hash_set.mem t.mode_args))
                || Hash_set.mem bounded (node axis))
               (axis, Mode_closure.lower closure (node axis)))));
  let rank = Int.Table.create () in
  List.iteri reps ~f:(fun i rep -> Hashtbl.set rank ~key:rep ~data:i);
  let rank = Hashtbl.find_exn rank in
  let key : Printed.t -> int * int * int * int = function
    | Lower (n, _) -> rank n.var, 0, 0, Mode.Axis.Any.index n.axis
    | Plain (a, b) -> rank a, 1, rank b, -1
    | Relation r ->
      rank r.lo.var, 1, rank r.hi.var, Mode.Axis.Any.index r.lo.axis
    | Upper (n, _) -> rank n.var, 2, 0, Mode.Axis.Any.index n.axis
  in
  t.printed
  <- List.filter printed ~f:(function
       | Lower (n, _) | Upper (n, _) -> not (Hashtbl.mem t.atoms n.var)
       | Relation _ | Plain _ -> true)
     |> List.stable_sort ~compare:(fun a b ->
       [%compare: int * int * int * int] (key a) (key b))
;;

let name t rep =
  Hashtbl.find_or_add t.names rep ~default:(fun () ->
    let weak =
      Hashtbl.find t.vars rep
      |> Option.exists ~f:(fun var -> not (Mode.Var.is_generic var))
    in
    sprintf "'%sm%d" (if weak then "_" else "") (Hashtbl.length t.names))
;;

let rep t var =
  let id = Mode.Var.id var in
  Hashtbl.find t.rep id |> Option.value ~default:id
;;

let side t var =
  let rep = rep t var in
  match Hashtbl.find t.atoms rep with
  | Some [] -> None
  | Some atoms ->
    Some
      (" @ "
       ^ String.concat
           ~sep:" "
           (List.map atoms ~f:(fun (axis, point) -> Mode_node.atom axis point))
      )
  | None -> Some (" @ " ^ name t rep)
;;

(* Never elided: [ptr[t]] reads back as [nonportable gc], not as a fresh
   variable. Atoms only when fixed (on its axes), as [settle] makes them. *)
let held t var =
  let rep = rep t var in
  match Hashtbl.find t.atoms rep with
  | Some atoms -> `Atoms atoms
  | None -> `Name (name t rep)
;;

let constraints t =
  let on ({ var; axis } : Mode_node.t) =
    sprintf "%s(%s)" (Mode_node.axis_name axis) (name t var)
  in
  (* A class's bounds on one side print together. *)
  let rec group (printed : Printed.t list) =
    match printed with
    | [] -> []
    | Lower (n, p) :: rest ->
      let same, rest =
        List.split_while rest ~f:(function
          | Lower (n', _) -> n'.var = n.var
          | Upper _ | Plain _ | Relation _ -> false)
      in
      let atoms =
        List.map
          ((n, p)
           :: List.filter_map same ~f:(function
             | Lower (n, p) -> Some (n, p)
             | Upper _ | Plain _ | Relation _ -> None))
          ~f:(fun ((n : Mode_node.t), p) -> Mode_node.atom n.axis p)
      in
      (String.concat ~sep:" " atoms ^ " <= " ^ name t n.var) :: group rest
    | Upper (n, p) :: rest ->
      let same, rest =
        List.split_while rest ~f:(function
          | Upper (n', _) -> n'.var = n.var
          | Lower _ | Plain _ | Relation _ -> false)
      in
      let atoms =
        List.map
          ((n, p)
           :: List.filter_map same ~f:(function
             | Upper (n, p) -> Some (n, p)
             | Lower _ | Plain _ | Relation _ -> None))
          ~f:(fun ((n : Mode_node.t), p) -> Mode_node.atom n.axis p)
      in
      (name t n.var ^ " <= " ^ String.concat ~sep:" " atoms) :: group rest
    | Plain (lo, hi) :: rest -> (name t lo ^ " <= " ^ name t hi) :: group rest
    | Relation { lo; written; hi } :: rest ->
      (match written with
       | Plain _ -> on lo ^ " <= " ^ on hi
       | Meet axis ->
         sprintf
           "meet(%s, %s) <= %s"
           (on lo)
           (Mode_node.atom axis (Written.middle axis))
           (on hi)
       | Release -> sprintf "release(%s) <= %s" (on lo) (on hi)
       | Static_if_const -> sprintf "%s <= static_if_const(%s)" (on lo) (on hi)
       | Const_if_static -> sprintf "%s <= const_if_static(%s)" (on lo) (on hi))
      :: group rest
  in
  group t.printed
;;
