open! Import

(* [Core.List] on a million elements runs in constant stack and linear time:
   within a depth limit far below the length, and fuel proportional to it. *)

let n = 1_000_000

let program =
  {|
let sexp_of_ints l = Core.Sexp.sexp_of_list Core.Sexp.sexp_of_int l
let double n = Core.Int.mul n 2
let ok x = Ok x
let odd n = Core.Bool.not (is_even n)
let scramble i = Core.Int.rem (Core.Int.mul i 7919) 100003
let is_even n = Core.Int.equal (Core.Int.rem n 2) 0

let check name l =
  let length = Core.List.length l in
  print_endline
    (Core.String.concat " "
       (Cons (name, Cons (Core.Int.to_string length,
        Cons (Core.Sexp.to_string (sexp_of_ints (Core.List.take 3 l)),
        Cons (Core.Sexp.to_string (Core.Sexp.sexp_of_option Core.Sexp.sexp_of_int (Core.List.last l)),
        Nil))))))

let l = Core.List.init LENGTH Core.Int.succ
let a = check "init" l
let b = check "map" (Core.List.map double l)
let e = check "filter" (Core.List.filter is_even l)
let f = check "append" (Core.List.append l l)
let g = check "concat_map" (Core.List.concat_map Core.List.singleton l)
let h = check "sort reversed" (Core.List.sort Core.Int.compare (Core.List.rev l))
let evens_then_odds =
  Core.List.append (Core.List.filter is_even l) (Core.List.filter odd l)
let h2 = check "sort two runs" (Core.List.sort Core.Int.compare evens_then_odds)
let scrambled =
  Core.List.init 100000 scramble
let h3 = check "sort scrambled" (Core.List.sort Core.Int.compare scrambled)
let i = print_endline (Core.Int.to_string (Core.List.fold_right Core.Int.add l 0))
let j =
  match Core.List.zip l l with
  | Some pairs -> (
    match Core.List.unzip pairs with
    | (a, _) -> check "zip, unzip" a)
  | None -> print_endline "zip failed"
let k =
  match Core.Result.all (Core.List.map ok l) with
  | Ok l -> check "Result.all" l
  | Error () -> print_endline "Result.all failed"
|}
;;

let%expect_test "a million elements" =
  let source =
    String.substr_replace_all program ~pattern:"LENGTH" ~with_:(Int.to_string n)
  in
  let limits =
    { Limits.fuel = Some (1500 * n)
    ; max_depth = Some 10_000
    ; time_limit = None
    }
  in
  match
    Driver.check_files
      ~limits
      ~output:print_string
      [ { Source_file.path = "t.stag"; contents = source } ]
  with
  | Error error ->
    List.iter error ~f:(fun error -> print_s [%sexp (error : Driver.Error.t)])
  | Ok driver ->
    (match Driver.eval ~limits driver with
     | Ok () -> ()
     | Error error -> print_s [%sexp (error : Error.t)]);
    [%expect
      {|
      init 1000000 (1 2 3) (1000000)
      map 1000000 (2 4 6) (2000000)
      filter 500000 (2 4 6) (1000000)
      append 2000000 (1 2 3) (1000000)
      concat_map 1000000 (1 2 3) (1000000)
      sort reversed 1000000 (1 2 3) (1000000)
      sort two runs 1000000 (1 2 3) (1000000)
      sort scrambled 100000 (0 1 2) (100002)
      500000500000
      zip, unzip 1000000 (1 2 3) (1000000)
      Result.all 1000000 (1 2 3) (1000000)
      |}]
;;
