open! Import

(* An axis, with its points as indices from [0], its least. *)
module Axis = struct
  type t = Mode.Axis.Any.t

  let all = Mode.Axis.Any.all
  let equal = Mode.Axis.Any.equal
  let max axis = Mode.Axis.Any.size axis - 1

  let name : t -> string = function
    | T Stage -> "stage"
    | T Provenance -> "provenance"
    | T Locality -> "locality"
  ;;

  let atom (T axis : t) index =
    let module C = (val Mode.Axis.chain axis) in
    Sexp.to_string (C.sexp_of_t (C.of_index_exn index)) |> String.lowercase
  ;;

  let node (T axis : t) var = Mode.Var.node var axis

  let is_stage : t -> bool = function
    | T Stage -> true
    | T (Provenance | Locality) -> false
  ;;

  let portable = 1
end

module Written = Mode.Written_morphism

(* A variable's axis, by the variable's id. *)
module Node = struct
  type t = int * Axis.t

  let equal (a, axis) (b, axis') = a = b && Axis.equal axis axis'
end

(* [lo <= w(hi)], as [w]'s left adjoint, [w(lo) <= hi] reads: raising [lo]
   raises [hi]. *)
module Relation = struct
  type t =
    { lo : Node.t
    ; written : Written.t
    ; hi : Node.t
    }

  let morphism t = Written.morphism t.written
end

module Printed = struct
  type t =
    | Lower of (Axis.t * int) list * int (** Atoms, at most one per axis. *)
    | Upper of int * (Axis.t * int) list
    | Plain of int * int (** On every axis. *)
    | Relation of Relation.t
end

type t =
  { vars : Mode.Var.t Int.Table.t
  ; order : int Int.Table.t
  ; counts : int Int.Table.t
  ; results : Int.Hash_set.t
  ; args : Int.Hash_set.t
  ; mode_args : Int.Hash_set.t
  ; rep : int Int.Table.t
  ; atoms : (Axis.t * int) list Int.Table.t
  ; names : string Int.Table.t
  ; mutable printed : Printed.t list
  }

let create () =
  { vars = Int.Table.create ()
  ; order = Int.Table.create ()
  ; counts = Int.Table.create ()
  ; results = Int.Hash_set.create ()
  ; args = Int.Hash_set.create ()
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
   | `Arg -> Hash_set.add t.args id
   | `Res -> Hash_set.add t.results id
   | `Mode_arg -> Hash_set.add t.mode_args id);
  if not (Hashtbl.mem t.order id)
  then Hashtbl.set t.order ~key:id ~data:(Hashtbl.length t.order);
  Hashtbl.incr t.counts id
;;

(* What the constraints imply of the counted variables, as read back: their
   bounds, and how far raising one raises each other. A result is read back at
   least [portable] ([Type_env.result_floor]), so it is taken as such. *)
module Real = struct
  type nonrec t =
    { lower : Node.t -> int
    ; upper : Node.t -> int
    ; raise : Node.t -> int -> Node.t -> int
    (** [raise x p y]: [y]'s least point when [x] is at least [p]. *)
    }

  let create t ~ids =
    let node (id, axis) = Axis.node axis (Hashtbl.find_exn t.vars id) in
    let floors =
      List.filter_map ids ~f:(fun id ->
        Option.some_if
          (Hash_set.mem t.results id)
          (node (id, T Stage), Axis.portable))
    in
    let floors =
      match Mode.Var.Node.raised floors with
      | Some _ -> floors
      | None -> []
    in
    let levels seeds =
      let table = Hashtbl.Poly.create () in
      Option.iter (Mode.Var.Node.raised seeds) ~f:(fun raised ->
        List.iter raised ~f:(fun (n, point) ->
          let key = Mode.Var.Node.var_id n, Mode.Var.Node.axis n in
          if Hashtbl.mem t.vars (fst key)
          then
            Hashtbl.update table key ~f:(function
              | None -> point
              | Some point' -> Int.max point point')));
      fun (id, axis) ->
        Option.value
          (Hashtbl.find table (id, axis))
          ~default:(Mode.Var.Node.lower (node (id, axis)))
    in
    let base = levels floors in
    let raises = Hashtbl.Poly.create () in
    let raise x p =
      Hashtbl.find_or_add raises (x, p) ~default:(fun () ->
        levels ((node x, p) :: floors))
    in
    { lower = base
    ; upper = (fun n -> Mode.Var.Node.upper (node n))
    ; raise = (fun x p y -> if p <= base x then base y else raise x p y)
    }
  ;;
end

(* A model of what a [with] clause reads back as: bounds, relations and what
   every variable has, propagated. [exact] checks that it implies exactly what
   [Real] does. *)
module Model = struct
  type t =
    { nodes : Node.t list
    ; base_lower : Node.t -> int
    ; edges : (Node.t * Mode.Morphism.t * Node.t) list
    }

  let index nodes =
    let table = Hashtbl.Poly.create () in
    List.iteri nodes ~f:(fun i n -> Hashtbl.set table ~key:n ~data:i);
    Hashtbl.find_exn table
  ;;

  (* Raising by the edges from [levels], in place. *)
  let propagate edges levels =
    let changed = ref true in
    while !changed do
      changed := false;
      List.iter edges ~f:(fun (lo, m, hi) ->
        let point = Mode.Morphism.left m levels.(lo) in
        if point > levels.(hi)
        then (
          levels.(hi) <- point;
          changed := true))
    done
  ;;

  let exact model (real : Real.t) ~lower ~upper =
    let nodes = Array.of_list model.nodes in
    let index = index model.nodes in
    let edges =
      List.map model.edges ~f:(fun (lo, m, hi) -> index lo, m, index hi)
    in
    let lows =
      Array.map nodes ~f:(fun n ->
        Int.max (model.base_lower n) (Option.value (lower n) ~default:0))
    in
    propagate edges lows;
    let ups =
      Array.map nodes ~f:(fun ((_, axis) as n) ->
        Option.value (upper n) ~default:(Axis.max axis))
    in
    let changed = ref true in
    while !changed do
      changed := false;
      List.iter edges ~f:(fun (lo, m, hi) ->
        let point = Mode.Morphism.right m ups.(hi) in
        if point < ups.(lo)
        then (
          ups.(lo) <- point;
          changed := true))
    done;
    Array.for_alli nodes ~f:(fun i n ->
      lows.(i) = real.lower n && ups.(i) = real.upper n)
    && Array.for_alli nodes ~f:(fun i x ->
      List.for_all
        (List.range ~start:`exclusive lows.(i) ~stop:`inclusive ups.(i))
        ~f:(fun p ->
          let levels = Array.copy lows in
          levels.(i) <- p;
          propagate edges levels;
          Array.for_alli nodes ~f:(fun j y -> levels.(j) = real.raise x p y)))
  ;;
end

(* The variables are grouped into classes equal on every axis, each printed as
   one. Every relation of the written forms that the constraints imply between
   classes is a candidate, and bounds are each class's: then each one that the
   rest imply as read back is dropped. A class related to no other, named once
   (or fixed on every axis), prints as the atoms of its fixed axes. *)
let settle t =
  let ids =
    Hashtbl.keys t.vars
    |> List.sort ~compare:(fun a b ->
      Int.compare (Hashtbl.find_exn t.order a) (Hashtbl.find_exn t.order b))
  in
  (* A mode argument has no locality: a variable only of mode arguments is left
     out of the locality relations and bounds. *)
  let relevant id =
    if Hash_set.mem t.mode_args id
       && not (Hash_set.mem t.args id || Hash_set.mem t.results id)
    then List.filter Axis.all ~f:(Fn.non (Axis.equal (T Locality)))
    else Axis.all
  in
  let real = Real.create t ~ids in
  let is_relevant id axis = List.mem (relevant id) axis ~equal:Axis.equal in
  let domain ((_, axis) as n) =
    List.range ~start:`exclusive (real.lower n) ~stop:`inclusive (real.upper n)
    |> List.filter ~f:(fun p -> p <= Axis.max axis)
  in
  (* [b] is at least [a] on the axis. *)
  let at_least a b axis =
    List.for_all
      (domain (a, axis))
      ~f:(fun p -> real.raise (a, axis) p (b, axis) >= p)
  in
  let equal_on a b axis =
    match is_relevant a axis, is_relevant b axis with
    | false, false -> true
    | true, false | false, true -> false
    | true, true ->
      real.lower (a, axis) = real.lower (b, axis)
      && real.upper (a, axis) = real.upper (b, axis)
      && at_least a b axis
      && at_least b a axis
  in
  Hashtbl.clear t.rep;
  List.iter ids ~f:(fun id ->
    let rep =
      List.find_exn ids ~f:(fun other ->
        List.for_all Axis.all ~f:(fun axis -> equal_on id other axis))
    in
    Hashtbl.set t.rep ~key:id ~data:rep);
  let reps = List.filter ids ~f:(fun id -> Hashtbl.find_exn t.rep id = id) in
  let members rep =
    List.filter ids ~f:(fun id -> Hashtbl.find_exn t.rep id = rep)
  in
  let nodes =
    List.concat_map reps ~f:(fun rep ->
      List.map (relevant rep) ~f:(fun a -> rep, a))
  in
  let base_lower (rep, axis) =
    if Axis.is_stage axis
       && List.exists (members rep) ~f:(Hash_set.mem t.results)
    then Axis.portable
    else 0
  in
  (* Every variable's [const] is [static]. *)
  let own =
    List.map reps ~f:(fun rep ->
      ( ((rep, T Provenance) : Node.t)
      , Mode.Morphism.const_is_static
      , ((rep, T Stage) : Node.t) ))
  in
  let valid (r : Relation.t) =
    let m = Relation.morphism r in
    List.for_all (real.lower r.lo :: domain r.lo) ~f:(fun p ->
      Mode.Morphism.left m p <= real.raise r.lo p r.hi)
  in
  let useful (r : Relation.t) =
    let m = Relation.morphism r in
    List.exists (domain r.lo) ~f:(fun p ->
      Mode.Morphism.left m p > real.lower r.hi)
  in
  let below (r : Relation.t) (r' : Relation.t) =
    List.for_all (domain r.lo) ~f:(fun p ->
      Mode.Morphism.left (Relation.morphism r) p
      <= Mode.Morphism.left (Relation.morphism r') p)
  in
  let candidates =
    List.concat_map nodes ~f:(fun lo ->
      List.concat_map nodes ~f:(fun hi ->
        if Node.equal lo hi
        then []
        else (
          let all =
            List.filter_map Written.all ~f:(fun written ->
              let m = Written.morphism written in
              let r = { Relation.lo; written; hi } in
              Option.some_if
                (Axis.equal (Mode.Morphism.dst m) (snd lo)
                 && Axis.equal (Mode.Morphism.src m) (snd hi)
                 && valid r
                 && useful r)
                r)
          in
          List.filter all ~f:(fun r ->
            not
              (List.exists all ~f:(fun r' ->
                 (not (phys_equal r r')) && below r r' && not (below r' r))))
          |> List.remove_consecutive_duplicates ~equal:(fun r r' ->
            below r r' && below r' r))))
  in
  let model relations =
    { Model.nodes
    ; base_lower
    ; edges =
        own
        @ List.map relations ~f:(fun (r : Relation.t) ->
          r.lo, Relation.morphism r, r.hi)
    }
  in
  let all_lower n =
    Option.some_if (real.lower n > base_lower n) (real.lower n)
  in
  let all_upper ((_, axis) as n) =
    Option.some_if (real.upper n < Axis.max axis) (real.upper n)
  in
  (* Dropping each in turn, latest first, while the rest still imply it. *)
  let reduce items ~exact =
    List.fold (List.rev items) ~init:items ~f:(fun kept item ->
      let without = List.filter kept ~f:(fun i -> not (phys_equal i item)) in
      if exact without then without else kept)
  in
  let relations =
    reduce candidates ~exact:(fun relations ->
      Model.exact (model relations) real ~lower:all_lower ~upper:all_upper)
  in
  (* A pair related plainly on an axis, and on each other one by what holds,
     prints as [a <= b], every axis at once: then the rest are reduced again,
     with its relations on every axis kept. *)
  let is_plain (r : Relation.t) =
    match r.written with
    | Plain _ -> true
    | Meet _ | Release | Static_if_const | Const_if_static -> false
  in
  let plain_pairs =
    List.filter_map relations ~f:(fun r ->
      let a = fst r.lo
      and b = fst r.hi in
      Option.some_if
        (is_plain r
         && a <> b
         && List.for_all Axis.all ~f:(fun axis ->
           (not (is_relevant a axis && is_relevant b axis))
           || valid { lo = a, axis; written = Plain axis; hi = b, axis }))
        (a, b))
    |> List.dedup_and_sort ~compare:[%compare: int * int]
  in
  let plain_relations =
    List.concat_map plain_pairs ~f:(fun (a, b) ->
      List.filter_map Axis.all ~f:(fun axis ->
        Option.some_if
          (is_relevant a axis && is_relevant b axis)
          { Relation.lo = a, axis; written = Plain axis; hi = b, axis }))
  in
  let in_plain (r : Relation.t) =
    List.mem plain_pairs (fst r.lo, fst r.hi) ~equal:[%equal: int * int]
  in
  let relations =
    reduce
      (List.filter relations ~f:(fun r -> not (is_plain r && in_plain r)))
      ~exact:(fun relations ->
        Model.exact
          (model (plain_relations @ relations))
          real
          ~lower:all_lower
          ~upper:all_upper)
  in
  let model = model (plain_relations @ relations) in
  let bounds =
    List.concat_map nodes ~f:(fun n ->
      Option.to_list (Option.map (all_lower n) ~f:(fun p -> `Lower, n, p))
      @ Option.to_list (Option.map (all_upper n) ~f:(fun p -> `Upper, n, p)))
  in
  let find bounds side n =
    List.find_map bounds ~f:(fun (side', n', p) ->
      Option.some_if (Poly.equal side side' && Node.equal n n') p)
  in
  let bounds =
    reduce bounds ~exact:(fun bounds ->
      Model.exact
        model
        real
        ~lower:(find bounds `Lower)
        ~upper:(find bounds `Upper))
  in
  let related rep =
    List.exists relations ~f:(fun r -> fst r.lo = rep || fst r.hi = rep)
    || List.exists plain_pairs ~f:(fun (a, b) -> a = rep || b = rep)
  in
  (* A class's axis reads back from an atom if it is fixed, or if no bound of it
     is printed. *)
  Hashtbl.clear t.atoms;
  List.iter reps ~f:(fun rep ->
    let members = members rep in
    let fixed axis = real.lower (rep, axis) = real.upper (rep, axis) in
    let printed axis =
      Option.is_some (find bounds `Lower (rep, axis))
      || Option.is_some (find bounds `Upper (rep, axis))
    in
    if (not (related rep))
       && (not (List.exists members ~f:(Hash_set.mem t.mode_args)))
       && List.for_all (relevant rep) ~f:(fun axis ->
         fixed axis || not (printed axis))
       && (List.for_all (relevant rep) ~f:fixed
           ||
           match members with
           | [ id ] ->
             [%equal: int option] (Hashtbl.find t.counts id) (Some 1)
             && Mode.Var.is_generic (Hashtbl.find_exn t.vars id)
           | _ -> false)
    then
      Hashtbl.set
        t.atoms
        ~key:rep
        ~data:
          (List.filter_map (relevant rep) ~f:(fun axis ->
             Option.some_if (printed axis) (axis, real.lower (rep, axis)))));
  let bounds_of rep side =
    if Hashtbl.mem t.atoms rep
    then []
    else
      List.filter_map bounds ~f:(fun (side', (rep', axis), point) ->
        Option.some_if (rep = rep' && Poly.equal side side') (axis, point))
  in
  t.printed
  <- List.concat_map reps ~f:(fun rep ->
       (match bounds_of rep `Lower with
        | [] -> []
        | atoms -> [ Printed.Lower (atoms, rep) ])
       @ List.concat_map reps ~f:(fun other ->
         (if List.mem plain_pairs (rep, other) ~equal:[%equal: int * int]
          then [ Printed.Plain (rep, other) ]
          else [])
         @ List.filter_map relations ~f:(fun r ->
           Option.some_if
             (fst r.lo = rep && fst r.hi = other)
             (Printed.Relation r)))
       @
       match bounds_of rep `Upper with
       | [] -> []
       | atoms -> [ Printed.Upper (rep, atoms) ])
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
           (List.map atoms ~f:(fun (axis, point) -> Axis.atom axis point)))
  | None -> Some (" @ " ^ name t rep)
;;

let held t var = name t (rep t var)

let constraints t =
  let atoms points =
    String.concat
      ~sep:" "
      (List.map points ~f:(fun (axis, point) -> Axis.atom axis point))
  in
  let on (rep, axis) = sprintf "%s(%s)" (Axis.name axis) (name t rep) in
  List.map t.printed ~f:(function
    | Lower (points, rep) -> atoms points ^ " <= " ^ name t rep
    | Upper (rep, points) -> name t rep ^ " <= " ^ atoms points
    | Plain (lo, hi) -> name t lo ^ " <= " ^ name t hi
    | Relation { lo; written; hi } ->
      (match written with
       | Plain _ -> on lo ^ " <= " ^ on hi
       | Meet axis ->
         sprintf
           "meet(%s, %s) <= %s"
           (on lo)
           (Axis.atom axis (Written.middle axis))
           (on hi)
       | Release -> sprintf "release(%s) <= %s" (on lo) (on hi)
       | Static_if_const -> sprintf "%s <= static_if_const(%s)" (on lo) (on hi)
       | Const_if_static -> sprintf "%s <= const_if_static(%s)" (on lo) (on hi)))
;;
