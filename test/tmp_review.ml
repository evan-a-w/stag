open! Core
open Stag
open Syntax
open Helpers

let run = Test_eval.run

let%expect_test "merge persistent through nested splice then toplevel" =
  run
    [ top "a" []
        (splice
           (let_ "h" [ "y" ] (tuple [ var "y"; var "y" ])
              (let_ "c" [] (quote (app (var "h") [ int 1 ]))
                 (quote (tuple [ splice (var "c"); splice (var "c") ])))))
    ];
  [%expect {| |}]
;;

let%expect_test "explicit subs at toplevel, closure captures" =
  run
    [ top "f" [ "y" ]
        (splice ~subs:[ "x", Some (var "y") ]
           (quote (let_ "g" [ "u" ] (tuple [ var "x"; var "u" ]) (var "g"))))
    ; top "r" [] (app (var "f") [ int 1; int 2 ])
    ];
  [%expect {| |}]
;;

let%expect_test "toplevel provider" =
  run
    [ top "n" [] (int 3)
    ; top "f" [ "u" ] (splice (quote (tuple [ var "n"; var "u" ])))
    ; top "r" [] (app (var "f") [ int 9 ])
    ];
  [%expect {| |}]
;;

let%expect_test "level 0 persistent in toplevel splice" =
  run
    [ top "f" [ "x" ]
        (let_ "h" [ "y" ] (tuple [ var "x"; var "y" ])
           (let_ "g" [ "u" ] (splice (quote (app (var "h") [ var "u" ]))) (var "g")))
    ; top "r" [] (app (var "f") [ int 1; int 2 ])
    ];
  [%expect {| |}]
;;

let%expect_test "nested toplevel splice in splice code" =
  run
    [ top "a" []
        (splice
           (let_ "h" [ "y" ] (var "y")
              (quote (tuple [ app (var "h") [int 1]; splice (splice (quote (quote (int 2)))) ]))))
    ];
  [%expect {| |}]
;;

let%expect_test "level -2 persistent" =
  run
    [ top "a" []
        (splice
           (splice
              (let_ "h" [ "y" ] (var "y")
                 (quote (quote (app (var "h") [ int 1 ]))))))
    ];
  [%expect {| |}]
;;

let%expect_test "closure in quote twice spliced with persistent" =
  run
    [ top "a" []
        (splice
           (let_ "mk" [ "n" ]
              (let_ "h" [ "u" ] (var "n")
                 (quote (let_ "g" [ "v" ] (app (var "h") [ var "v" ]) (var "g"))))
              (quote
                 (tuple
                    [ app (splice (app (var "mk") [ int 1 ])) [ unit ]
                    ; app (splice (app (var "mk") [ int 2 ])) [ unit ]
                    ]))))
    ];
  [%expect {| |}]
;;

let%expect_test "error order" =
  run
    [ top "only" [ "t" ] (match_ (var "t") [ Pat.tag "A", int 0 ])
    ; top "e" [] (tuple [ app (var "only") [ tag "B" ]; app (var "only") [ tag "C" ] ])
    ];
  [%expect {| |}]
;;

let%expect_test "poly record rest open" =
  let r = poly_record [ "a", int 1; "b", bool true; "c", unit ] in
  run
    [ top "o" [] (match_ r [ Pat.poly_record ~rest:Open [ "b", Pat.var "b" ], var "b" ])
    ; top "rb" [] (match_ r [ Pat.poly_record ~rest:(Bind "rest") [ "b", Pat.tag "X" ], var "rest" ; Pat.any, int 0])
    ];
  [%expect {| |}]
;;

let%expect_test "inner quote quote splice" =
  run
    [ top "a" []
        (splice
           (splice
              (let_ "c" [] (quote (quote (int 5)))
                 (quote (quote (tuple [ splice (splice (var "c")); int 1 ]))))))
    ];
  [%expect {| |}]
;;
