open! Import
open Syntax

let relocate_printed program =
  let printed = Printer.program program in
  match Parser.program ~file:"t.stag" printed with
  | Error error -> Error (Parser.Error.to_string error)
  | Ok written ->
    (match Relocate.toplevels ~written program with
     | None -> Error ("not relocated:\n" ^ printed)
     | Some relocated ->
       if [%equal: Sexp.t]
            (Round_trip.without_resolved [%sexp (relocated : Program.t)])
            (Round_trip.without_resolved [%sexp (written : Program.t)])
       then Ok ()
       else Error ("relocated differs:\n" ^ printed))
;;

let%expect_test "random programs relocate to themselves printed and parsed" =
  let random = Random.State.make [| 2 |] in
  let failures =
    List.init 3000 ~f:(fun size ->
      let gen = { Gen.random; size = size % 60; deep = false } in
      relocate_printed (List.init 3 ~f:(fun _ -> Gen.toplevel gen)))
    |> List.filter_map ~f:Result.error
  in
  print_s [%sexp (List.take failures 3 : string list)];
  [%expect {| () |}]
;;

let parse source =
  match Parser.program ~file:"t.stag" source with
  | Ok program -> program
  | Error error -> failwith (Parser.Error.to_string error)
;;

let show ~written generated =
  print_s
    [%sexp
      (Relocate.toplevels ~written:(parse written) (parse generated)
       : Program.t option)]
;;

let%expect_test "relocating" =
  show ~written:"let x =\n  f   (y)" "let x = f y";
  [%expect
    {|
    ((((Let (x @ t.stag:1:4-5) () ()
        ((App ((Var f ()) @ t.stag:2:2-3) ((Var y ()) @ t.stag:2:6-9)) @
         t.stag:2:2-9))
       @ t.stag:1:0-2:9)))
    |}];
  show ~written:"let x = f y" "let x = f z";
  [%expect {| () |}];
  show ~written:"let x = 1 let y = 2" "let x = 1";
  [%expect {| () |}];
  let resolved =
    match parse "type u = t" with
    | [ ({ txt =
             Type
               ({ kind = Alias ({ txt = Con (path, []); _ } as ty); _ } as decl)
         ; _
         } as toplevel)
      ] ->
      [ { toplevel with
          txt =
            Toplevel.Type
              { decl with
                kind =
                  Alias
                    { ty with
                      txt = Con ({ path with txt = Resolved "M.t" }, [])
                    }
              }
        }
      ]
    | _ -> assert false
  in
  print_s
    [%sexp
      (Relocate.toplevels ~written:(parse "\ntype u = M.t") resolved
       : Program.t option)];
  [%expect
    {|
    ((((Type
        ((name (u @ t.stag:2:5-6)) (params ())
         (kind (Alias ((Con (<M.t> @ t.stag:2:9-12) ()) @ t.stag:2:9-12)))))
       @ t.stag:2:0-12)))
    |}]
;;
