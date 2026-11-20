open! Import

let print (t : (int, string) Local_result.t @ local) =
  print_s
    [%sexp
      (Result.globalize Int.globalize String.globalize t
       : (int, string) Result.t)]
;;

let half n : (int, string) Local_result.t @ local = exclave_
  if n % 2 = 0 then Local_result.return (n / 2) else Error (sprintf "odd %d" n)
;;

let%expect_test "binding and mapping" =
  print
    (let%bindl.Local_result a = half 24 in
     let%bindl.Local_result b = half a in
     let%mapl_fun.Local_result c = half b in
     a + b + c);
  print
    (let%bindl.Local_result a = half 12 in
     let%bindl.Local_result b = half a in
     half b);
  [%expect {|
    (Ok 21)
    (Error "odd 3")
    |}]
;;

let%expect_test "local payloads and binding allocate nothing on the heap" =
  let pair n : (int * int, string) Local_result.t @ local = exclave_
    let%mapl_fun.Local_result h = half n in
    exclave_ n, h
  in
  let #(sum, allocation) =
    Gc.For_testing.measure_allocation_local (fun () -> exclave_
      let%bindl.Local_result a, b = pair 40 in
      let%bindl.Local_result c, d = pair b in
      let%mapl_fun.Local_result e, f = pair d in
      a + b + c + d + e + f)
  in
  print sum;
  let { Gc.For_testing.Allocation_report.major_words_allocated = major
      ; minor_words_allocated = minor
      }
    =
    allocation
  in
  print_s [%message (major : int) (minor : int)];
  [%expect {|
    (Ok 105)
    ((major 0) (minor 0))
    |}]
;;
