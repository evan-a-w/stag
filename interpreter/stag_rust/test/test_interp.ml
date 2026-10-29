open! Import

let compare ?(gc_stress = Debug.Gc_stress.all) source ~name =
  match Source_helpers.typecheck ~prelude:false source with
  | None -> ()
  | Some t ->
    let program = (Source_helpers.checked t).program in
    let rust =
      List.map gc_stress ~f:(fun gc_stress ->
        Interp.eval_toplevel ~gc_stress program ~name
        |> [%sexp_of: Interp_value.t Or_error.t])
      |> List.dedup_and_sort ~compare:Sexp.compare
    in
    let treewalk =
      let driver = Source_helpers.driver t in
      match Driver.eval ~limits:Source_helpers.limits driver with
      | Error error -> [%sexp (error : Error.t)]
      | Ok () ->
        (match
           List.find (Source_helpers.members t) ~f:(fun member ->
             String.equal member.ident.name name)
         with
         | None -> [%sexp "no such toplevel"]
         | Some member ->
           [%sexp
             (Treewalk.find (Driver.interpreter driver) ~ident:member.ident
              : Treewalk.Value.t Or_error.t)])
    in
    print_s [%sexp { rust : Sexp.t list; treewalk : Sexp.t }]
;;

let%expect_test "eval_toplevel" =
  compare
    ~name:"result"
    {|
let fib n =
  match Int.lt n 2 with
  | true -> n
  | false -> Int.add (fib (Int.sub n 1)) (fib (Int.sub n 2))

let result = fib 10
|};
  [%expect
    {|
    ((rust ((Error "not implemented: eval_toplevel \"result\"")))
     (treewalk (Ok 55)))
    |}]
;;
