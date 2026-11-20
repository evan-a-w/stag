open! Import

(* Tail calls run in constant memory: the peak resident memory of a loop of
   millions of iterations is that of a short one, unlike non-tail recursion. *)

let definitions =
  {|
let count n acc =
  match Int.equal n 0 with
  | true -> acc
  | false ->
    let pair = (n, acc) in
    match pair with n, acc -> count (Int.sub n 1) (Int.add acc 1)

let catching n acc =
  match Int.equal n 0 with
  | true -> acc
  | false ->
    match raise (Failure "next") with
    | () -> acc
    | exception Failure _ -> catching (Int.sub n 1) (Int.add acc 1)

let deep n =
  match Int.equal n 0 with
  | true -> 0
  | false -> Int.add 1 (deep (Int.sub n 1))
|}
;;

let peak_kb () =
  In_channel.read_lines "/proc/self/status"
  |> List.find_map_exn ~f:(fun line ->
    String.chop_prefix line ~prefix:"VmHWM:"
    |> Option.map ~f:(fun kb ->
      String.strip kb |> String.chop_suffix_exn ~suffix:" kB" |> Int.of_string))
;;

(* Peak resident memory grown by evaluating [main], in MB. *)
let peak_growth main =
  match
    Driver.check_files
      ~limits:Limits.unlimited
      ~output:print_string
      [ { Source_file.path = "t.stag"; contents = definitions ^ main } ]
  with
  | Error errors -> raise_s [%sexp (errors : Driver.Error.t list)]
  | Ok driver ->
    Gc.compact ();
    (* Resets the peak to the current resident memory. *)
    Out_channel.write_all "/proc/self/clear_refs" ~data:"5";
    let before = peak_kb () in
    Driver.eval ~limits:Limits.unlimited driver |> ok_exn;
    (peak_kb () - before) / 1024
;;

(* A leak of a word per iteration of a loop of [long_loop] iterations exceeds
   it. *)
let bound_mb = 32
let long_loop = 10_000_000

(* Whether the growth is within [bound_mb], and the growth too if that is not
   [expected], to debug a failure. *)
let print_growth ~name ~short ~long ~long_within_bound:expected =
  let check growth ~expected =
    let within_bound = growth < bound_mb in
    if Bool.equal within_bound expected
    then [%sexp (within_bound : bool)]
    else [%sexp (within_bound : bool), ~~(growth : int), "MB"]
  in
  let short = check (peak_growth (short ())) ~expected:true in
  let long = check (peak_growth (long ())) ~expected in
  print_s
    [%message
      name
        ~short_within_bound:(short : Sexp.t)
        ~long_within_bound:(long : Sexp.t)]
;;

let%expect_test "tail calls run in constant memory" =
  let loops n =
    [%string
      {|
let a = print_endline (Int.to_string (count %{n#Int} 0))
let b = print_endline (Int.to_string (catching %{n#Int} 0))
|}]
  in
  print_growth
    ~name:"tail loops"
    ~short:(fun () -> loops 100_000)
    ~long:(fun () -> loops long_loop)
    ~long_within_bound:true;
  [%expect
    {|
    100000
    100000
    10000000
    10000000
    ("tail loops" (short_within_bound true) (long_within_bound true))
    |}];
  (* The bound does not just hold of anything: non-tail recursion exceeds it. *)
  let deep n =
    [%string {|
let a = print_endline (Int.to_string (deep %{n#Int}))
|}]
  in
  print_growth
    ~name:"non-tail recursion"
    ~short:(fun () -> deep 1_000)
    ~long:(fun () -> deep 300_000)
    ~long_within_bound:false;
  [%expect
    {|
    1000
    300000
    ("non-tail recursion" (short_within_bound true) (long_within_bound false))
    |}]
;;
