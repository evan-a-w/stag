open! Import

let show (error : Mode.Error.t) =
  print_endline
    (Typecheck_error.message
       (Typecheck_error.within
          (Typecheck_error.create ~loc:Loc.none (Mode_error error))
          ~in_scope:
            (lazy
              { types = String.Set.empty
              ; modules = String.Set.empty
              ; root_modules = String.Set.empty
              })))
;;

let mismatch ~lower_because ~upper_because : Mode.Error.t =
  Mismatch
    { var = 0
    ; axis = Locality
    ; lower = Local
    ; upper = Global
    ; lower_because
    ; upper_because
    }
;;

let%expect_test "a reason shown only alone is hidden by others, shown when the \
                 others were already given"
  =
  let held = Mode.Reason.layout "it is held" in
  let used = Mode.Reason.create ~shown:If_alone "a function is used" in
  show
    (mismatch
       ~lower_because:[ Mode.Reason.create "it is annotated local"; held ]
       ~upper_because:[ held; used ]);
  show (mismatch ~lower_because:[ held ] ~upper_because:[ held ]);
  [%expect
    {|
    This is local, because it is annotated local, then it is held, but must be at most global, because a function is used
    This is local, because it is held, but must be at most global
    |}]
;;
