open! Import
open Mode_helpers

let big_apply n =
  let args = List.init n ~f:(sprintf "a%d") |> String.concat ~sep:" " in
  sprintf "let big f %s = f %s" args args
;;

let curried n =
  let args = List.init n ~f:(sprintf "a%d") |> String.concat ~sep:" " in
  sprintf "let cur %s = a0" args
;;

let record n =
  let params = List.init n ~f:(sprintf "'p%d : mode") |> String.concat ~sep:", " in
  let fields = List.init n ~f:(fun i -> sprintf "f%d : ptr[string @@ 'p%d]" i i) |> String.concat ~sep:"; " in
  let args = List.init n ~f:(sprintf "p%d") |> String.concat ~sep:" " in
  let inits = List.init n ~f:(fun i -> sprintf "f%d = p%d" i i) |> String.concat ~sep:"; " in
  sprintf "type r[%s] = {%s}\nlet mk %s = {%s}" params fields args inits
;;

let time name source =
  let t0 = Time_ns.now () in
  let items = Result.ok_or_failwith (Result.map_error (checked source) ~f:(String.concat ~sep:"\n")) in
  let t1 = Time_ns.now () in
  let printed = Signature.to_string_hum ~modes:true items in
  let t2 = Time_ns.now () in
  printf "%s: check %s print %s size %d\n" name
    (Time_ns.Span.to_string_hum (Time_ns.diff t1 t0))
    (Time_ns.Span.to_string_hum (Time_ns.diff t2 t1))
    (String.length printed)
;;

let%expect_test "timing" =
  if Option.is_none (Sys.getenv "ONLY") then (
  time "hof40" (big_apply 40);
  time "hof80" (big_apply 80);
  time "cur120" (curried 120);
  time "rec80" (record 80));
  [%expect {||}]
;;

let%expect_test "only" =
  match Sys.getenv "ONLY" with
  | None -> ()
  | Some which ->
    for _ = 1 to 5 do
      match which with
      | "cur" -> time "cur120" (curried 120)
      | "hof" -> time "hof80" (big_apply 80)
      | _ -> time "rec80" (record 80)
    done
;;
