open! Import

let work = Work.create "typecheck.types"

module Step = struct
  type t =
    | Index of int
    | Field of string
    | Arg of string * int
    | Label of string
    | Tag of string
  [@@deriving sexp_of, equal, compare]

  (* Of one of a variant's alternatives, so only there if the value is it. *)
  let is_alternative = function
    | Arg _ | Tag _ -> true
    | Index _ | Field _ | Label _ -> false
  ;;
end

(* The components of a value derived from others' (a use of a binding, or a
   value that is one of several) are made when first asked for, as most never
   are: a chain of values each holding the one before would otherwise copy all
   of those before it at each step. *)
type t =
  { whole : Mode.Var.t
  ; parts : parts
  }

and parts =
  | Opaque
  | Known of (Step.t * t) list
  | Derived of derived

and derived =
  { steps : Step.t list
  ; make : Step.t -> t option
  ; mutable made : (Step.t * t option) list
  ; mutable lowered : int option
  (** The least rank its modes were lowered to, which those made later are
      lowered to as well. *)
  }

let rec sexp_of_t { whole; parts } =
  let parts =
    match parts with
    | Opaque -> None
    | Known parts -> Some parts
    | Derived { made; _ } ->
      Some
        (List.filter_map made ~f:(fun (step, part) ->
           Option.map part ~f:(fun part -> step, part)))
  in
  [%sexp { whole : Mode.Var.t; parts : (Step.t * t) list option }]
;;

let known ~whole parts = { whole; parts = Known parts }
let opaque whole = { whole; parts = Opaque }

let steps t =
  match t.parts with
  | Opaque -> None
  | Known parts -> Some (List.map parts ~f:fst)
  | Derived { steps; _ } -> Some steps
;;

let rec lower_rank t ~rank =
  Work.incr work;
  Mode.Var.lower_rank t.whole ~rank;
  match t.parts with
  | Opaque -> ()
  | Known parts -> List.iter parts ~f:(fun (_, part) -> lower_rank part ~rank)
  | Derived derived ->
    let lowered = derived.lowered in
    if Option.for_all lowered ~f:(fun lowered -> rank < lowered)
    then
      Undo.change
        ~undo:(fun () -> derived.lowered <- lowered)
        (fun () -> derived.lowered <- Some rank);
    List.iter derived.made ~f:(fun (_, part) ->
      Option.iter part ~f:(lower_rank ~rank))
;;

let part t step =
  Work.incr work;
  match t.parts with
  | Opaque -> None
  | Known parts -> List.Assoc.find parts step ~equal:Step.equal
  | Derived derived ->
    if not (List.mem derived.steps step ~equal:Step.equal)
    then None
    else (
      match List.Assoc.find derived.made step ~equal:Step.equal with
      | Some part -> part
      | None ->
        let part = derived.make step in
        Option.iter derived.lowered ~f:(fun rank ->
          Option.iter part ~f:(lower_rank ~rank));
        let made = derived.made in
        Undo.change
          ~undo:(fun () -> derived.made <- made)
          (fun () -> derived.made <- (step, part) :: made);
        part)
;;

let derived ~whole ~steps make =
  { whole; parts = Derived { steps; make; made = []; lowered = None } }
;;

let rec map t ~whole ~component =
  match steps t with
  | None -> opaque whole
  | Some steps ->
    derived ~whole ~steps (fun step ->
      Option.map (part t step) ~f:(fun part ->
        map part ~whole:(component part.whole) ~component))
;;

let rec join ts ~whole ~component =
  match Option.all (List.map ts ~f:steps) with
  | None -> opaque whole
  | Some of_each ->
    let steps =
      List.concat of_each
      |> List.dedup_and_sort ~compare:Step.compare
      |> List.filter ~f:(fun step ->
        (* A variant's alternative is in the values that are it; another
           component is in all of them, else not known. *)
        Step.is_alternative step
        || List.for_all of_each ~f:(fun steps ->
          List.mem steps step ~equal:Step.equal))
    in
    derived ~whole ~steps (fun step ->
      let of_step = List.filter_map ts ~f:(fun t -> part t step) in
      let whole = component (List.map of_step ~f:(fun part -> part.whole)) in
      Some (join of_step ~whole ~component))
;;

module Path = struct
  module Step = struct
    type nonrec t =
      | Part of Step.t * Modality.t
      | Through of Modality.t

    let modality = function
      | Part (_, modality) | Through modality -> modality
    ;;
  end

  type t = Step.t list

  let modality t =
    List.fold_right t ~init:Modality.id ~f:(fun step inner ->
      Modality.compose (Step.modality step) inner)
  ;;
end

let rec follow t (path : Path.t) =
  let unknown () = t.whole, Path.modality path, None in
  match path with
  | [] -> t.whole, Modality.id, Some t
  | Part (step, (_ : Modality.t)) :: rest ->
    (match part t step with
     (* The component's mode was checked to fit the modality when the value was
        made: reading it through that changes nothing. *)
     | Some component -> follow component rest
     | None -> unknown ())
  | Through _ :: _ -> unknown ()
;;
