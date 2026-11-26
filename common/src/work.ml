open! Import

type t =
  { name : string
  ; mutable count : int
  }

let all = Queue.create ()

let create name =
  let t = { name; count = 0 } in
  Queue.enqueue all t;
  t
;;

let incr t = t.count <- t.count + 1

module Private = struct
  module Counts = struct
    (* Large blocks are allocated straight on the major heap. *)
    let allocated_words () =
      let minor, promoted, major = Stdlib.Gc.counters () in
      Float.to_int (minor +. major -. promoted)
    ;;

    type t = int String.Map.t [@@deriving sexp_of]

    let read () =
      Queue.fold
        all
        ~init:
          (String.Map.singleton "allocated_words" (allocated_words ()))
        ~f:(fun counts t ->
          Map.update counts t.name ~f:(fun count ->
            Option.value count ~default:0 + t.count))
    ;;
  end

  let measure f =
    let before = Counts.read () in
    let result = f () in
    let after = Counts.read () in
    ( result
    , Map.merge after before ~f:(fun ~key:_ -> function
        | `Both (after, before) ->
          Option.some_if (after > before) (after - before)
        | `Left after -> Option.some_if (after > 0) after
        | `Right (_ : int) -> None) )
  ;;

  module Growth = struct
    type t =
      | Linear
      | Quadratic
      | Worse
    [@@deriving sexp_of]

    let of_ratio ratio ~scale =
      let exponent = Float.log ratio /. Float.log scale in
      if Float.( <= ) exponent 1.3
      then Linear
      else if Float.( <= ) exponent 2.3
      then Quadratic
      else Worse
    ;;
  end

  let growth ?net_of ~small ~large counts =
    (* Once first, for what is done once, eg. loading a prelude. *)
    ignore (counts (Option.value net_of ~default:small) : Counts.t);
    let base = Option.map net_of ~f:counts in
    let net size =
      Map.mapi (counts size) ~f:(fun ~key ~data ->
        match Option.bind base ~f:(fun base -> Map.find base key) with
        | Some base -> data - base
        | None -> data)
    in
    let small_counts = net small in
    let large_counts = net large in
    Map.filter_mapi large_counts ~f:(fun ~key ~data:large_count ->
      match Map.find small_counts key with
      | Some small_count when small_count > 0 && large_count > 0 ->
        Some
          (Growth.of_ratio
             (Float.of_int large_count /. Float.of_int small_count)
             ~scale:(Float.of_int large /. Float.of_int small))
      | Some _ | None -> Option.some_if (large_count > 0) Growth.Worse)
  ;;
end
