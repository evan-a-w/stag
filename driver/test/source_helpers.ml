open! Import

type t =
  { driver : Driver.t
  ; prelude : Syntax.Program.t
  }

let file = "t.stag"

let limits =
  { Limits.fuel = Some 1_000_000; max_depth = Some 10_000; time_limit = None }
;;

let typechecker () =
  let interpreter = Treewalk.create ~stamps:(Stamp_counter.create ()) () in
  Treewalk.set_limits interpreter limits;
  Typecheck.create ~interpreter ()
;;

let prelude =
  {|let add (x : int) (y : int) : int = x
let eq (x : int) (y : int) : bool = true
let cond (c : bool) x y = cond c y x; x
let id x = x
|}
;;

(* Every program parsed from source is also checked to round trip through
   [Printer], which is silent unless it does not. Only at one width, and not
   for long sources (eg. the deep nesting of [test_limits]), to keep tests
   fast. *)
let round_trip ~source program =
  if String.length source <= 10_000
  then Round_trip.program ~widths:[ 80 ] program
;;

let parse ?(file = file) source =
  let result = Parser.program ~file source in
  Result.iter result ~f:(round_trip ~source);
  result
;;

let parse_exn ?file source =
  match parse ?file source with
  | Ok program -> program
  | Error error -> raise_s [%sexp (Parser.Error.to_string error : string)]
;;

let prelude_program = lazy (parse_exn ~file:"prelude.stag" prelude)
let prelude_if prelude = if prelude then force prelude_program else []
let program ?(prelude = true) source = prelude_if prelude @ parse_exn source
let driver t = t.driver
let checked t = Driver.checked t.driver

let typecheck_program ?(limits = limits) ?(prelude = true) program =
  let prelude = prelude_if prelude in
  match Driver.check ~limits (prelude @ program) with
  | Error error ->
    print_s [%sexp (error : Typecheck_error.t)];
    None
  | Ok driver -> Some { driver; prelude }
;;

let typecheck ?limits ?prelude source =
  match parse source with
  | Error error ->
    print_endline (Parser.Error.to_string error);
    None
  | Ok program -> typecheck_program ?limits ?prelude program
;;

let check_files ?(limits = limits) files =
  let parse (path, contents) =
    let%map.Result (toplevel : Syntax.Toplevel.t) =
      Source_file.parse { path; contents }
    in
    (match toplevel.txt with
     | Module (_, toplevels) -> round_trip ~source:contents toplevels
     | _ -> ());
    Driver.Module_group.File toplevel
  in
  let result =
    let%bind.Result modules =
      List.map files ~f:parse
      |> Result.all
      |> Result.map_error ~f:(fun error -> Driver.Error.Source error)
    in
    Driver.check_modules ~limits modules
    |> Result.map_error ~f:(fun error -> Driver.Error.Typecheck error)
  in
  match result with
  | Error error ->
    print_s [%sexp (error : Driver.Error.t)];
    None
  | Ok driver -> Some { driver; prelude = [] }
;;

(* Toplevels get stamps in program order, from 0, so the test prelude's come
   first. The real prelude's are named within it. *)
let is_prelude t (ident : Typed.Ident.t) =
  ident.stamp < List.length t.prelude
  || Option.is_some (Prelude_path.relative ident.name)
;;

let members t =
  List.concat_map (checked t).program.sccs ~f:(fun scc -> scc.members)
  |> List.filter ~f:(fun (member : Typed.Toplevel.t) ->
    not (is_prelude t member.ident))
;;

let print_signature t =
  print_string
    (Signature.to_string_hum
       (List.drop (checked t).signature (List.length t.prelude)))
;;

let print_bodies ?only t =
  List.iter (members t) ~f:(fun { ident; body; _ } ->
    if
      Option.for_all only ~f:(fun only ->
        List.mem only ident.name ~equal:String.equal)
    then print_s [%sexp (ident : Typed.Ident.t), (body : Typed.Expr.t)])
;;

let print_values t =
  List.iter (members t) ~f:(fun { ident; _ } ->
    match Treewalk.find (Driver.interpreter t.driver) ~ident with
    | Ok value ->
      print_s [%sexp (ident.name : string), (value : Treewalk.Value.t)]
    | Error _ -> ())
;;

let eval_and_print_values ?(limits = limits) t =
  let result = Driver.eval ~limits t.driver in
  print_values t;
  match result with
  | Ok () -> ()
  | Error error -> print_s [%sexp (error : Error.t)]
;;

let toplevel_idents (program : Typed.Program.t) =
  List.concat_map program.sccs ~f:(fun scc ->
    List.map scc.members ~f:(fun member -> member.ident))
  |> Typed.Ident.Set.of_list
;;

(* The prelude's locals get the stamps after the toplevels', as the prelude is
   checked first. *)
let prelude_locals =
  lazy
    (match Driver.check (force prelude_program) with
     | Error error -> raise_s [%sexp (error : Typecheck_error.t)]
     | Ok driver ->
       let program = (Driver.checked driver).program in
       Map.length program.ident_info - Set.length (toplevel_idents program))
;;

let print_typed ?(idents = false) t =
  let program = (checked t).program in
  List.iter program.sccs ~f:(fun (scc : Typed.Scc.t) ->
    if
      not
        (List.for_all scc.members ~f:(fun member -> is_prelude t member.ident))
    then print_s [%sexp (scc : Typed.Scc.t)]);
  if idents
  then (
    let toplevels = toplevel_idents program in
    let first_local =
      Set.length toplevels
      + if List.is_empty t.prelude then 0 else force prelude_locals
    in
    print_s
      [%sexp
        (Map.filter_keys program.ident_info ~f:(fun ident ->
           if Set.mem toplevels ident
           then not (is_prelude t ident)
           else ident.stamp >= first_local)
         |> Map.map ~f:(fun { Typed.Ident_info.ty; level } ->
           Type.to_string_hum ty, level)
         : (string * int) Typed.Ident.Map.t)])
;;

let print_run ?limits ?(bodies = []) ?(eval = true) t =
  print_signature t;
  print_bodies ~only:bodies t;
  print_endline "-- evaluated at compile time";
  print_values t;
  if eval
  then (
    print_endline "-- evaluated";
    eval_and_print_values ?limits t)
;;

let check ?limits ?prelude source =
  Option.iter (typecheck ?limits ?prelude source) ~f:print_signature
;;

let check_program ?limits ?prelude program =
  Option.iter (typecheck_program ?limits ?prelude program) ~f:print_signature
;;

let run ?limits ?prelude ?bodies ?eval source =
  Option.iter
    (typecheck ?limits ?prelude source)
    ~f:(print_run ?limits ?bodies ?eval)
;;

let run_program ?limits ?prelude ?bodies ?eval program =
  Option.iter
    (typecheck_program ?limits ?prelude program)
    ~f:(print_run ?limits ?bodies ?eval)
;;
