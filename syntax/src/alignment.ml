open! Import

(* Each element or diagonal an alignment looks at. *)
let work = Work.create "syntax.format"

(* [v.(k + d)], of the [d]th of [vs], is the furthest x reached on diagonal [k]
   (= x - y) with [d] edits. *)
let myers a b ~equal =
  let n = Array.length a in
  let m = Array.length b in
  let snake x y =
    let x = ref x in
    let y = ref y in
    while !x < n && !y < m && equal a.(!x) b.(!y) do
      Work.incr work;
      incr x;
      incr y
    done;
    !x
  in
  (* The diagonal of the previous step a step to [k] comes from, and the x it
     starts at. *)
  let step prev ~d k =
    let at k = prev.(k + d - 1) in
    if k = -d || (k <> d && at (k - 1) < at (k + 1))
    then k + 1, at (k + 1)
    else k - 1, at (k - 1) + 1
  in
  let rec forward vs ~d =
    let v = Array.create ~len:((2 * d) + 1) 0 in
    let rec diagonals k =
      Work.incr work;
      if k > d
      then None
      else (
        let start =
          match vs with
          | [] -> 0
          | prev :: _ -> snd (step prev ~d k)
        in
        let x = snake start (start - k) in
        v.(k + d) <- x;
        if x >= n && x - k >= m then Some k else diagonals (k + 2))
    in
    match diagonals (-d) with
    | Some k -> k, d, v :: vs
    | None -> forward (v :: vs) ~d:(d + 1)
  in
  let k, d, vs = forward [] ~d:0 in
  let matched = Array.create ~len:n None in
  let rec back vs ~d ~k =
    match vs with
    | [] -> ()
    | v :: earlier ->
      let x = v.(k + d) in
      let prev_k, start =
        match earlier with
        | [] -> k, 0
        | prev :: _ -> step prev ~d k
      in
      for i = start to x - 1 do
        matched.(i) <- Some (i - k)
      done;
      back earlier ~d:(d - 1) ~k:prev_k
  in
  back vs ~d ~k;
  matched
;;

(* The pairs of indices of the elements that occur once in each of [a] and [b],
   in order of [a]. *)
let unique_pairs a b ~compare =
  let uniques arr =
    let sorted =
      Array.mapi arr ~f:(fun i x -> x, i)
      |> Array.sorted_copy ~compare:(fun (x, _) (y, _) -> compare x y)
    in
    let n = Array.length sorted in
    Array.filteri sorted ~f:(fun i (x, _) ->
      (i = 0 || compare (fst sorted.(i - 1)) x <> 0)
      && (i = n - 1 || compare (fst sorted.(i + 1)) x <> 0))
  in
  let ua = uniques a in
  let ub = uniques b in
  let rec merge i j acc =
    if i >= Array.length ua || j >= Array.length ub
    then acc
    else (
      let x, ia = ua.(i) in
      let y, ib = ub.(j) in
      let c = compare x y in
      if c < 0
      then merge (i + 1) j acc
      else if c > 0
      then merge i (j + 1) acc
      else merge (i + 1) (j + 1) ((ia, ib) :: acc))
  in
  merge 0 0 []
  |> Array.of_list
  |> Array.sorted_copy ~compare:(fun (ia, _) (ib, _) -> Int.compare ia ib)
;;

(* The longest subsequence of [pairs] increasing in their second index. *)
let longest_increasing pairs =
  let n = Array.length pairs in
  (* [tails.(l)]: the index in [pairs] of the smallest end of an increasing
     subsequence of length [l + 1]. *)
  let tails = Array.create ~len:n 0 in
  let prev = Array.create ~len:n (-1) in
  let length = ref 0 in
  for i = 0 to n - 1 do
    let b = snd pairs.(i) in
    let rec search lo hi =
      if lo >= hi
      then lo
      else (
        let mid = (lo + hi) / 2 in
        if snd pairs.(tails.(mid)) < b
        then search (mid + 1) hi
        else search lo mid)
    in
    let l = search 0 !length in
    if l > 0 then prev.(i) <- tails.(l - 1);
    tails.(l) <- i;
    if l = !length then incr length
  done;
  let rec collect i acc =
    if i < 0 then acc else collect prev.(i) (pairs.(i) :: acc)
  in
  if !length = 0 then [] else collect tails.(!length - 1) []
;;

(* As patience diff: the elements occurring once in each, matched in order,
   split the arrays into parts aligned apart, by Myers' diff when they have
   none. *)
let align a b ~compare =
  let matched = Array.create ~len:(Array.length a) None in
  let equal x y = compare x y = 0 in
  let rec go ~a_pos ~a_len ~b_pos ~b_len =
    let prefix = ref 0 in
    while
      !prefix < a_len
      && !prefix < b_len
      && equal a.(a_pos + !prefix) b.(b_pos + !prefix)
    do
      Work.incr work;
      matched.(a_pos + !prefix) <- Some (b_pos + !prefix);
      incr prefix
    done;
    let a_pos = a_pos + !prefix
    and b_pos = b_pos + !prefix
    and a_len = a_len - !prefix
    and b_len = b_len - !prefix in
    if a_len > 0 && b_len > 0
    then (
      let sub_a = Array.sub a ~pos:a_pos ~len:a_len in
      let sub_b = Array.sub b ~pos:b_pos ~len:b_len in
      match longest_increasing (unique_pairs sub_a sub_b ~compare) with
      | [] ->
        Array.iteri (myers sub_a sub_b ~equal) ~f:(fun i m ->
          matched.(a_pos + i) <- Option.map m ~f:(( + ) b_pos))
      | anchors ->
        let ia, ib =
          List.fold anchors ~init:(0, 0) ~f:(fun (ia, ib) (ja, jb) ->
            go
              ~a_pos:(a_pos + ia)
              ~a_len:(ja - ia)
              ~b_pos:(b_pos + ib)
              ~b_len:(jb - ib);
            matched.(a_pos + ja) <- Some (b_pos + jb);
            ja + 1, jb + 1)
        in
        go
          ~a_pos:(a_pos + ia)
          ~a_len:(a_len - ia)
          ~b_pos:(b_pos + ib)
          ~b_len:(b_len - ib))
  in
  go ~a_pos:0 ~a_len:(Array.length a) ~b_pos:0 ~b_len:(Array.length b);
  matched
;;
