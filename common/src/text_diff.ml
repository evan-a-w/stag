open! Import

module Line = struct
  type t =
    | Same of string
    | Removed of string
    | Added of string

  let is_change = function
    | Same _ -> false
    | Removed _ | Added _ -> true
  ;;

  let to_string = function
    | Same line -> " " ^ line
    | Removed line -> "-" ^ line
    | Added line -> "+" ^ line
  ;;
end

let lines string =
  match String.split string ~on:'\n' with
  | [ "" ] -> []
  | lines when String.is_suffix string ~suffix:"\n" -> List.drop_last_exn lines
  | lines -> lines
;;

(* By longest common subsequence, only tabulating the lines between the common
   prefix and suffix. Past them, the shorter side's remaining lines are a
   subsequence of the other's, which are matched greedily as the full table
   would. *)
let diff (old : string array) (new_ : string array) =
  let n = Array.length old in
  let m = Array.length new_ in
  let prefix = ref 0 in
  while
    !prefix < n && !prefix < m && String.equal old.(!prefix) new_.(!prefix)
  do
    incr prefix
  done;
  let p = !prefix in
  let suffix = ref 0 in
  while
    !suffix < n - p
    && !suffix < m - p
    && String.equal old.(n - 1 - !suffix) new_.(m - 1 - !suffix)
  do
    incr suffix
  done;
  let n' = n - !suffix in
  let m' = m - !suffix in
  let lcs = Array.make_matrix ~dimx:(n' - p + 1) ~dimy:(m' - p + 1) 0 in
  for i = n' - 1 downto p do
    for j = m' - 1 downto p do
      lcs.(i - p).(j - p)
      <- (if String.equal old.(i) new_.(j)
          then lcs.(i - p + 1).(j - p + 1) + 1
          else Int.max lcs.(i - p + 1).(j - p) lcs.(i - p).(j - p + 1))
    done
  done;
  let rec go i j acc =
    if i < n && j < m && String.equal old.(i) new_.(j)
    then go (i + 1) (j + 1) (Line.Same old.(i) :: acc)
    else if if i < n' && j < m'
            then lcs.(i - p + 1).(j - p) >= lcs.(i - p).(j - p + 1)
            else n - i > m - j
    then go (i + 1) j (Line.Removed old.(i) :: acc)
    else if j < m
    then go i (j + 1) (Line.Added new_.(j) :: acc)
    else List.rev acc
  in
  go 0 0 []
;;

let context = 3

let unified ~old_name ~new_name ~old ~new_ =
  if String.equal old new_
  then None
  else (
    let lines =
      diff (Array.of_list (lines old)) (Array.of_list (lines new_))
      |> Array.of_list
    in
    let count = Array.length lines in
    (* Each line's position in old and new, counting from 1. *)
    let positions =
      Array.folding_map lines ~init:(1, 1) ~f:(fun (o, n) line ->
        let next : int * int =
          match line with
          | Same _ -> o + 1, n + 1
          | Removed _ -> o + 1, n
          | Added _ -> o, n + 1
        in
        next, (o, n))
    in
    let changes =
      List.filter (List.range 0 count) ~f:(fun i -> Line.is_change lines.(i))
    in
    (* Hunks as inclusive ranges of lines, merging those whose context overlaps. *)
    let hunks =
      List.fold changes ~init:[] ~f:(fun hunks i ->
        let start = Int.max 0 (i - context) in
        let stop = Int.min (count - 1) (i + context) in
        match hunks with
        | (hunk_start, hunk_stop) :: rest when start <= hunk_stop + 1 ->
          (hunk_start, stop) :: rest
        | _ -> (start, stop) :: hunks)
      |> List.rev
    in
    let hunk (start, stop) =
      let range = List.range start stop ~stop:`inclusive in
      let old_count =
        List.count range ~f:(fun i ->
          match lines.(i) with
          | Same _ | Removed _ -> true
          | Added _ -> false)
      in
      let new_count =
        List.count range ~f:(fun i ->
          match lines.(i) with
          | Same _ | Added _ -> true
          | Removed _ -> false)
      in
      let old_start, new_start = positions.(start) in
      let header_start start count = if count = 0 then start - 1 else start in
      sprintf
        "@@ -%d,%d +%d,%d @@\n"
        (header_start old_start old_count)
        old_count
        (header_start new_start new_count)
        new_count
      ^ String.concat
          (List.map range ~f:(fun i -> Line.to_string lines.(i) ^ "\n"))
    in
    Some
      (sprintf "--- %s\n+++ %s\n" old_name new_name
       ^ String.concat (List.map hunks ~f:hunk)))
;;
