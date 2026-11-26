open! Import

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

type t =
  { whole : Mode.Var.t
  ; parts : (Step.t * t) list option
  }
[@@deriving sexp_of]

let known ~whole parts = { whole; parts = Some parts }
let opaque whole = { whole; parts = None }

let part t step =
  Option.bind t.parts ~f:(fun parts ->
    List.Assoc.find parts step ~equal:Step.equal)
;;

let rec map t ~whole ~component =
  { whole
  ; parts =
      Option.map
        t.parts
        ~f:
          (List.map ~f:(fun (step, part) ->
             step, map part ~whole:(component part.whole) ~component))
  }
;;

let rec join ts ~whole ~component =
  let parts =
    Option.all (List.map ts ~f:(fun t -> t.parts))
    |> Option.map ~f:(fun parts ->
      List.concat_map parts ~f:(List.map ~f:fst)
      |> List.dedup_and_sort ~compare:Step.compare
      |> List.filter_map ~f:(fun step ->
        let of_step = List.filter_map ts ~f:(fun t -> part t step) in
        (* A variant's alternative is in the values that are it; another
           component is in all of them, else not known. *)
        if Step.is_alternative step || List.length of_step = List.length ts
        then (
          let whole =
            component (List.map of_step ~f:(fun part -> part.whole))
          in
          Some (step, join of_step ~whole ~component))
        else None))
  in
  { whole; parts }
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

let rec lower_rank t ~rank =
  Mode.Var.lower_rank t.whole ~rank;
  Option.iter t.parts ~f:(List.iter ~f:(fun (_, part) -> lower_rank part ~rank))
;;
