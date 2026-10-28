open! Import

(* [v.(k + d)], of the [d]th of [vs], is the furthest x reached on diagonal
   [k] (= x - y) with [d] edits. *)
let align a b ~equal =
  let n = Array.length a in
  let m = Array.length b in
  let snake x y =
    let x = ref x in
    let y = ref y in
    while !x < n && !y < m && equal a.(!x) b.(!y) do
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
