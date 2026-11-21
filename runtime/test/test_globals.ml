open! Import
module Addr = Memory.Addr

let word n = Addr.of_word (Nativeint_u.of_int n)
let int_of addr = Nativeint_u.to_int (Addr.to_word addr)

let values globals =
  let values = ref [] in
  Root_source.iter (Globals.root_source globals) ~f:(fun slot ->
    values := int_of (Memory.load_addr slot) :: !values);
  List.sort ~compare !values
;;

let%expect_test "globals" =
  let globals = Globals.create () in
  let source = Globals.root_source globals in
  let chunk = Memory.Chunk.create ~bytes:80 ~align:8 in
  let global i = Addr.add (Memory.Chunk.base chunk) (i * 8) in
  for i = 0 to 9 do
    Memory.store_addr (global i) (word (1000 + i));
    Globals.add globals (global i)
  done;
  print_s
    [%message
      ""
        ~name:(Root_source.name source : string)
        ~holds_locals:(Root_source.holds_locals source : bool)
        ~count:(Globals.count globals : int)
        ~values:(values globals : int list)];
  Globals.remove globals (global 3);
  Globals.remove globals (global 0);
  print_s
    [%message
      ""
        ~count:(Globals.count globals : int)
        ~values:(values globals : int list)
        ~mem:
          (List.map [ 0; 1; 3; 9 ] ~f:(fun i ->
             i, Globals.mem globals (global i))
           : (int * bool) list)];
  let show f = print_s [%sexp (Or_error.try_with f : unit Or_error.t)] in
  show (fun () -> Globals.add globals (global 1));
  show (fun () -> Globals.remove globals (global 3));
  Globals.add globals (global 3);
  print_s [%message "" ~values:(values globals : int list)];
  Memory.Chunk.destroy chunk;
  [%expect
    {|
    ((name globals) (holds_locals false) (count 10)
     (values (1000 1001 1002 1003 1004 1005 1006 1007 1008 1009)))
    ((count 8) (values (1001 1002 1004 1005 1006 1007 1008 1009))
     (mem ((0 false) (1 true) (3 false) (9 true))))
    (Error "Globals.add: already added")
    (Error "Globals.remove: not added")
    (values (1001 1002 1003 1004 1005 1006 1007 1008 1009))
    |}]
;;
