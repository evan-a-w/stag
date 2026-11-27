open! Import

let reason = Reason.create
let locality var = Var.node var Locality

let ok result =
  match result with
  | Ok () -> ()
  | Error error -> print_s [%message "unexpected" (error : Error.t)]
;;

let above_each sources ~keep =
  let { Var.Node.Above_each.kept; above } =
    Var.Node.Above_each.create (List.map sources ~f:locality) ~keep
  in
  List.map2_exn sources (Array.to_list above) ~f:(fun source above ->
    ( Var.id source
    , Bitset.to_list above
      |> List.map ~f:(fun at -> Var.Node.var_id kept.(at))
      |> List.sort ~compare:Int.compare ))
;;

let%expect_test "the kept nodes above each source, through cycles" =
  let ids = Id_source.create () in
  let v = Array.init 6 ~f:(fun (_ : int) -> Var.create ids ~rank:1) in
  let edge a b = ok (Var.submode v.(a) v.(b) (reason "edge")) in
  (* [0 <-> 1 -> 2 <-> 3 -> 4], and [5] alone. *)
  edge 0 1;
  edge 1 0;
  edge 1 2;
  edge 2 3;
  edge 3 2;
  edge 3 4;
  let all = above_each [ v.(0); v.(2); v.(4); v.(5) ] ~keep:(fun _ -> true) in
  let odd =
    above_each
      [ v.(0); v.(2); v.(4); v.(5) ]
      ~keep:(fun node -> Var.Node.var_id node % 2 = 1)
  in
  print_s [%message (all : (int * int list) list) (odd : (int * int list) list)];
  [%expect
    {|
    ((all ((0 (0 1 2 3 4)) (2 (2 3 4)) (4 (4)) (5 (5))))
     (odd ((0 (1 3)) (2 (3)) (4 ()) (5 (5)))))
    |}]
;;

let%expect_test "a chain of equalities takes linear time" =
  let chain k =
    let ids = Id_source.create () in
    let vars = Array.init k ~f:(fun (_ : int) -> Var.create ids ~rank:1) in
    for i = 0 to k - 2 do
      ok (Var.submode vars.(i) vars.(i + 1) (reason "up"));
      ok (Var.submode vars.(i + 1) vars.(i) (reason "down"))
    done;
    let { Var.Node.Above_each.above; _ }, counts =
      Work.Private.measure (fun () ->
        Var.Node.Above_each.create [ locality vars.(0) ] ~keep:(fun _ -> true))
    in
    List.length (Bitset.to_list above.(0)), counts
  in
  let small_above, _ = chain 250 in
  let large_above, _ = chain 1000 in
  let growth =
    Work.Private.growth ~small:250 ~large:1000 (fun k -> snd (chain k))
  in
  print_s
    [%message
      ""
        (small_above : int)
        (large_above : int)
        (growth : Work.Private.Growth.t String.Map.t)];
  [%expect
    {|
    ((small_above 250) (large_above 1000)
     (growth ((allocated_words Linear) (mode.graph Linear))))
    |}]
;;
