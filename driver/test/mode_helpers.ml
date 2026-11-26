open! Import
open Source_helpers
module Arrow_modes = Stag_types.Arrow_modes
module Mode_arg = Stag_types.Mode_arg

let typecheck_source source =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let typecheck = Typecheck.create ~stamps ~interpreter () in
  Typecheck.typecheck typecheck ~program:(program source)
;;

let typecheck source =
  let prelude = program ~prelude:true "" in
  match typecheck_source source with
  | Ok checked -> Ok (List.drop checked.signature (List.length prelude))
  | Error { errors; partial = _ } -> Error errors
;;

let checked source =
  Result.map_error (typecheck source) ~f:(List.map ~f:Typecheck_error.message)
;;

let located_errors source =
  Result.map_error
    (Result.ignore_m (typecheck source))
    ~f:
      (List.map ~f:(fun (error : Typecheck_error.t) ->
         error.loc, Typecheck_error.message error))
;;

let signature source =
  Result.map (checked source) ~f:(Signature.to_string_hum ~modes:true)
;;

let placement_lines (checked : Typecheck.Checked.t) =
  let placement = function
    | Typed.Allocation.Placement.Heap -> "heap"
    | Region -> "region"
    | Caller_region -> "caller's region"
  in
  (* Owners of the same name, as those of code spliced in twice, numbered in the
     order they are first seen. *)
  let owner_name =
    let seen = String.Table.create () in
    List.iter checked.program.allocations ~f:(fun allocation ->
      let name = Typed.Allocation.Owner.to_string allocation.owner in
      let owners = Hashtbl.find seen name |> Option.value ~default:[] in
      if not
           (List.mem
              owners
              allocation.owner
              ~equal:Typed.Allocation.Owner.equal)
      then Hashtbl.set seen ~key:name ~data:(owners @ [ allocation.owner ]));
    fun owner ->
      let name = Typed.Allocation.Owner.to_string owner in
      match Hashtbl.find seen name with
      | Some (_ :: _ :: _ as owners) ->
        let index, _ =
          List.findi_exn owners ~f:(fun _ -> Typed.Allocation.Owner.equal owner)
        in
        sprintf "%s#%d" name (index + 1)
      | Some _ | None -> name
  in
  let line (allocation : Typed.Allocation.t) =
    let where =
      match allocation.follows with
      | None -> placement allocation.placement
      | Some { owner; side; if_local } ->
        sprintf
          "%s, %s if %s%s is local"
          (placement allocation.placement)
          (placement if_local)
          (if Typed.Allocation.Owner.equal owner allocation.owner
           then ""
           else owner_name owner ^ "'s ")
          (Typed.Allocation.Side.to_string side)
    in
    let owner = owner_name allocation.owner in
    ( allocation.loc
    , sprintf
        !"%{Sexp} in %s: %s"
        (Typed.Allocation.Kind.sexp_of_t allocation.kind)
        owner
        where )
  in
  List.filter_map checked.program.allocations ~f:(fun allocation ->
    Option.some_if
      (String.equal (Loc.file_name allocation.loc) file)
      (line allocation))
;;

let placements source =
  let prelude = program ~prelude:true "" in
  match typecheck_source source with
  | Error { errors; partial = _ } ->
    Error (List.map errors ~f:Typecheck_error.message)
  | Ok checked ->
    Ok
      ( List.drop checked.signature (List.length prelude)
      , placement_lines checked )
;;

let allocations_despite_errors source =
  let print_lines checked =
    List.iter (placement_lines checked) ~f:(fun (loc, line) ->
      printf "%s %s\n" (Loc.to_string loc) line)
  in
  match typecheck_source source with
  | Ok checked -> print_lines checked
  | Error { errors; partial } ->
    List.iter errors ~f:(fun error ->
      print_endline (Typecheck_error.message error));
    print_lines partial
;;

let allocations source =
  match placements source with
  | Error errors -> List.iter errors ~f:print_endline
  | Ok (signature, lines) ->
    print_string (Signature.to_string_hum ~modes:true signature);
    List.iter lines ~f:(fun (loc, line) ->
      printf "%s %s\n" (Loc.to_string loc) line)
;;

(* The modes of a type's arrows' sides (not their [curry]s, which aren't
   printed) and mode arguments, in order. *)
module Positions = struct
  type position =
    { side : Arrow_modes.Side.t
    ; result : bool
    ; mode_arg : bool
    }

  let of_ty ty =
    let positions = Queue.create () in
    let add side ~result ~mode_arg =
      Queue.enqueue positions { side; result; mode_arg }
    in
    let rec ty_ (ty : Type.ty) ~seen =
      match Type.canonical_ty ty with
      | TCon (_, args) ->
        List.iter args ~f:(function
          | ATy ty -> ty_ ty ~seen
          | ARow row -> row_ row ~seen
          | AMode (Fixed { stage; provenance }) ->
            (* Its locality is unread. *)
            add
              (Fixed { stage; provenance; locality = Global })
              ~result:false
              ~mode_arg:true
          | AMode (Var var) -> add (Var var) ~result:false ~mode_arg:true)
      | TArrow (ty1, ty2, modes) ->
        ty_ ty1 ~seen;
        add modes.arg ~result:false ~mode_arg:false;
        ty_ ty2 ~seen;
        add modes.res ~result:true ~mode_arg:false
      | TTuple tys -> List.iter tys ~f:(ty_ ~seen)
      | TCode { inner; context } ->
        ty_ inner ~seen;
        row_ context ~seen
      | TRecord { id; row } | TVariant { id; row } ->
        if Set.mem seen id
        then raise_s [%message "cyclic"]
        else row_ row ~seen:(Set.add seen id)
      | TVar _ -> ()
    and row_ (row : Type.row) ~seen =
      match Type.canonical_row row with
      | REmpty | RVar _ -> ()
      | RExtend (_, _, _, ty, rest) ->
        ty_ ty ~seen;
        row_ rest ~seen
    in
    match ty_ ty ~seen:Int.Set.empty with
    | () -> Ok (Queue.to_list positions)
    | exception exn -> Error (Exn.sexp_of_t exn)
  ;;
end

(* What the modes' constraints imply of the positions, as a [with] clause reads
   back: each axis's bounds, and how far raising one raises the others, with
   results at least [portable]. A mode argument's locality is never compared: a
   variable's is where it is an arrow's. A weak variable is compared as it is at
   the end of the program. *)
let projection positions =
  let open Positions in
  let dead (p : position) (Mode.Axis.Any.T axis) =
    match axis with
    | Locality -> p.mode_arg
    | Stage | Provenance -> false
  in
  let point (const : Mode.Const.t) (Mode.Axis.Any.T axis) =
    let module C = (val Mode.Axis.chain axis) in
    match axis with
    | Stage -> C.index const.stage
    | Provenance -> C.index const.provenance
    | Locality -> C.index const.locality
  in
  let node var (Mode.Axis.Any.T axis) = Mode.Var.node var axis in
  let floors =
    List.filter_map positions ~f:(fun p ->
      match p.side, p.result with
      | Var var, true -> Some (node var (T Stage), Mode.Stage.index Portable)
      | _ -> None)
  in
  let levels seeds =
    match Mode.Var.Node.raised (seeds @ floors) with
    | None -> raise_s [%message "a raise within bounds fails"]
    | Some raised ->
      List.concat_map positions ~f:(fun p ->
        List.filter_map Mode.Axis.Any.all ~f:(fun axis ->
          Option.some_if
            (not (dead p axis))
            (match p.side with
             | Fixed const -> point const axis
             | Var var ->
               let n = node var axis in
               List.fold
                 raised
                 ~init:(Mode.Var.Node.lower n)
                 ~f:(fun acc (n', p) ->
                   if Mode.Var.Node.var_id n' = Mode.Var.id var
                      && Mode.Axis.Any.equal (Mode.Var.Node.axis n') axis
                   then Int.max acc p
                   else acc))))
  in
  let base = levels [] in
  Ok
    ( base
    , List.concat_map positions ~f:(fun p ->
        List.concat_map Mode.Axis.Any.all ~f:(fun axis ->
          match p.side with
          | _ when dead p axis -> []
          | Fixed const -> [ point const axis ]
          | Var var ->
            let n = node var axis in
            [ Mode.Var.Node.upper n ]))
    , List.concat_map positions ~f:(fun p ->
        List.concat_map Mode.Axis.Any.all ~f:(fun axis ->
          match p.side with
          | Fixed _ -> []
          | Var _ when dead p axis -> []
          | Var var ->
            let n = node var axis in
            let floor =
              List.fold
                (Option.value (Mode.Var.Node.raised floors) ~default:[])
                ~init:(Mode.Var.Node.lower n)
                ~f:(fun acc (n', p) ->
                  if Mode.Var.Node.var_id n' = Mode.Var.id var
                     && Mode.Axis.Any.equal (Mode.Var.Node.axis n') axis
                  then Int.max acc p
                  else acc)
            in
            List.range
              ~start:`exclusive
              floor
              ~stop:`inclusive
              (Mode.Var.Node.upper n)
            |> List.map ~f:(fun point -> levels [ n, point ]))) )
;;

let round_trip_diverge =
  "let round_trip_diverge (u : unit) = round_trip_diverge u"
;;

let find_scheme items name =
  List.find_map items ~f:(function
    | Signature.Item.Value { name = name'; scheme; _ }
      when String.equal name name' -> Some scheme
    | _ -> None)
;;

let project scheme = Result.bind (Positions.of_ty scheme) ~f:projection

let compare_schemes original copy ~name ~printed ~show ~context =
  match project original with
  | Error error ->
    (match show with
     | `Quiet -> ()
     | `Signature | `Copies ->
       print_s [%message "not compared" name (error : Sexp.t)])
  | Ok expected ->
    (match project copy with
     | Ok actual
       when [%equal: int list * int list * int list list] expected actual -> ()
     | Ok actual ->
       print_s
         [%message
           "differs"
             (name : string)
             (context : Sexp.t option)
             (expected : int list * int list * int list list)
             (actual : int list * int list * int list list)
             (printed : string)]
     | Error error ->
       print_s [%message "differs" (name : string) (error : Sexp.t)])
;;

(* A function's annotation alone: [fun] adds nothing to its arrow's sides but
   what every function has. *)
let alone name ty =
  let is_arrow =
    let ty =
      match String.substr_index ty ~pattern:" with " with
      | Some i -> String.prefix ty i
      | None -> ty
    in
    match Stag_syntax.Parser.type_expr ~file:"t.stag" ty with
    | Ok { txt = Arrow _; _ } -> true
    | Ok _ | Error _ -> false
  in
  sprintf
    "let %s_alone :%s = %s"
    name
    ty
    (if is_arrow
     then "fun v -> round_trip_diverge ()"
     else "round_trip_diverge ()")
;;

(* Each variable name of a printed type replaced by [f]'s, given what precedes
   it. *)
let map_vars ty ~f =
  let buffer = Buffer.create (String.length ty) in
  let length = String.length ty in
  let is_ident c = Char.is_alphanum c || Char.equal c '_' in
  let rec go i =
    if i < length
    then
      if Char.equal ty.[i] '\''
      then (
        let stop =
          Option.value
            (String.lfindi ty ~pos:(i + 1) ~f:(fun _ c -> not (is_ident c)))
            ~default:length
        in
        Buffer.add_string
          buffer
          (f
             ~before:(String.prefix ty i)
             (String.sub ty ~pos:i ~len:(stop - i)));
        go stop)
      else (
        Buffer.add_char buffer ty.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents buffer
;;

(* As [Mode_printing] names them: ['m0], or ['_m0] if weak. *)
let is_mode_var name =
  Option.first_some
    (String.chop_prefix name ~prefix:"'_m")
    (String.chop_prefix name ~prefix:"'m")
  |> Option.exists ~f:(fun digits ->
    (not (String.is_empty digits)) && String.for_all digits ~f:Char.is_digit)
;;

let is_weak name = String.is_prefix name ~prefix:"'_"

module Weak_item = struct
  (* A printed type with weak variables, as its body and [with] items. *)
  type t =
    { body : string
    ; items : string list
    ; type_vars : string list
    }

  let parse ty =
    let body, items =
      match String.substr_index ty ~pattern:" with " with
      | None -> ty, []
      | Some i ->
        ( String.prefix ty i
        , String.drop_prefix ty (i + String.length " with ")
          |> String.substr_replace_all ~pattern:" and " ~with_:"\000"
          |> String.split ~on:'\000' )
    in
    let type_vars = ref [] in
    let row_or_presence = ref false in
    let (_ : string) =
      map_vars body ~f:(fun ~before name ->
        if is_weak name && not (is_mode_var name)
        then (
          if String.is_suffix before ~suffix:".."
             || String.is_suffix before ~suffix:"?"
          then row_or_presence := true;
          type_vars := name :: !type_vars);
        name)
    in
    let type_vars = List.dedup_and_sort !type_vars ~compare:String.compare in
    let guards =
      List.concat_map items ~f:(fun item ->
        match String.substr_index item ~pattern:" unless pointer_free(" with
        | None -> []
        | Some i ->
          String.drop_prefix item (i + String.length " unless pointer_free(")
          |> String.chop_suffix_exn ~suffix:")"
          |> String.split ~on:','
          |> List.map ~f:String.strip)
    in
    if !row_or_presence
    then Error "a weak row or presence variable"
    else if not
              (List.for_all guards ~f:(List.mem type_vars ~equal:String.equal))
    then Error "a guard not in the type"
    else if List.length type_vars > 3
    then Error "too many weak type variables"
    else Ok { body; items; type_vars }
  ;;

  let guarded t =
    List.exists
      t.items
      ~f:(String.is_substring ~substring:" unless pointer_free(")
  ;;

  let pointer_free t =
    List.filter_map t.items ~f:(fun item ->
      String.chop_suffix item ~suffix:" : pointer_free"
      |> Option.filter ~f:(List.mem t.type_vars ~equal:String.equal))
  ;;

  (* The weak type variables as [int] or [string], those annotated pointer-free
     only as [int]. *)
  let assignments t =
    List.fold_right t.type_vars ~init:[ [] ] ~f:(fun var rest ->
      let types =
        if List.mem (pointer_free t) var ~equal:String.equal
        then [ "int" ]
        else [ "int"; "string" ]
      in
      List.concat_map types ~f:(fun ty ->
        List.map rest ~f:(fun rest -> (var, ty) :: rest)))
  ;;

  let substitute ty assignment ~mode =
    map_vars ty ~f:(fun ~before:_ name ->
      if is_mode_var name
      then mode name
      else
        List.Assoc.find assignment name ~equal:String.equal
        |> Option.value ~default:name)
  ;;

  (* The type with every mode variable occurrence fresh, and no [with]: what any
     instance of it has. *)
  let wild t assignment =
    let fresh = ref 0 in
    substitute t.body assignment ~mode:(fun _ ->
      incr fresh;
      sprintf "'z%d" !fresh)
  ;;

  (* The type as it reads at [assignment]: a guarded constraint holds if one of
     its guards has pointers, and is never added if none has. *)
  let instance t assignment =
    let mode name =
      match String.chop_prefix name ~prefix:"'_m" with
      | Some n -> "'mw" ^ n
      | None -> name
    in
    let items =
      List.filter_map t.items ~f:(fun item ->
        if Option.exists
             (String.chop_suffix item ~suffix:" : pointer_free")
             ~f:(List.mem (pointer_free t) ~equal:String.equal)
        then None
        else (
          match String.substr_index item ~pattern:" unless pointer_free(" with
          | None -> Some item
          | Some i ->
            let guards =
              String.drop_prefix item (i + String.length " unless pointer_free(")
              |> String.chop_suffix_exn ~suffix:")"
              |> String.split ~on:','
              |> List.map ~f:String.strip
            in
            Option.some_if
              (List.exists guards ~f:(fun guard ->
                 String.equal
                   (List.Assoc.find_exn assignment guard ~equal:String.equal)
                   "string"))
              (String.prefix item i)))
      |> List.map ~f:(fun item -> substitute item assignment ~mode)
    in
    substitute t.body assignment ~mode
    ^
    match items with
    | [] -> ""
    | items -> " with " ^ String.concat ~sep:" and " items
  ;;
end

(* The source up to the last toplevel defining [name]: later items may constrain
   its weak variables, and their constraints are theirs to print. *)
let defined_by source ~name =
  let stop =
    List.filter_map (parse_exn source) ~f:(fun { txt; loc } ->
      match txt with
      | (Let (name', _, _, _) | Let_mut (name', _, _))
        when String.equal name'.txt name -> Some loc.stop.#offset
      | _ -> None)
    |> List.last_exn
  in
  String.prefix source stop
;;

(* A weak item, at each assignment of its weak type variables: instantiated so,
   in the program up to it, against its annotation as it reads at that
   assignment, alone. They imply the same, or both fail. *)
let round_trip_weak source ~name ~ty ~show =
  let source = defined_by source ~name in
  match Weak_item.parse ty with
  | Error reason ->
    (match show with
     | `Quiet -> ()
     | `Signature | `Copies ->
       print_s [%message "not compared" (name : string) (reason : string)])
  | Ok item ->
    List.iter (Weak_item.assignments item) ~f:(fun assignment ->
      let instance = Weak_item.instance item assignment in
      let with_ lines =
        checked
          (String.concat ~sep:"\n" (source :: round_trip_diverge :: lines))
      in
      let context = [%sexp (assignment : (string * string) list)] in
      match
        ( with_
            [ sprintf "let _ = (%s :%s)" name (Weak_item.wild item assignment) ]
        , with_ [ alone name instance ] )
      with
      | Error _, Error _ -> ()
      | Ok instantiated, Ok alone ->
        let find items name = Option.value_exn (find_scheme items name) in
        compare_schemes
          (find instantiated name)
          (find alone (name ^ "_alone"))
          ~name
          ~printed:instance
          ~show
          ~context:(Some context)
      | Ok _, Error errors | Error errors, Ok _ ->
        print_s
          [%message
            "differs"
              name
              (context : Sexp.t)
              (instance : string)
              (errors : string list)])
;;

let round_trip source ~show =
  match checked source with
  | Error errors -> List.iter errors ~f:print_endline
  | Ok items ->
    let printed = Signature.to_string_hum ~modes:true items in
    let annotated =
      String.split_lines printed
      |> List.filter_map ~f:(fun line ->
        String.chop_prefix line ~prefix:"val "
        |> Option.map ~f:(fun rest ->
          let name, ty = String.lsplit2_exn rest ~on:':' in
          String.strip name, ty))
    in
    let comparable (_, ty) =
      not
        (String.is_substring ty ~substring:" where "
         || String.is_substring ty ~substring:" as '"
         || String.is_suffix ty ~suffix:" @ external")
    in
    List.iter annotated ~f:(fun (name, ty) ->
      if not
           (comparable (name, ty)
            ||
            match show with
            | `Quiet -> true
            | `Signature | `Copies -> false)
      then print_s [%message "not compared" (name : string) ty]);
    let annotated, weak =
      List.filter annotated ~f:comparable
      |> List.partition_tf ~f:(fun (_, ty) ->
        not (String.is_substring ty ~substring:"'_"))
    in
    let copies =
      List.map annotated ~f:(fun (name, ty) ->
        sprintf "let %s_copy :%s = %s" name ty name)
    in
    (match show with
     | `Signature -> print_string printed
     | `Copies -> print_endline (String.concat ~sep:"\n" copies)
     | `Quiet -> ());
    (match
       checked
         (String.concat
            ~sep:"\n"
            ((source :: round_trip_diverge :: copies)
             @ List.map annotated ~f:(fun (name, ty) -> alone name ty)))
     with
     | Error errors -> List.iter errors ~f:print_endline
     | Ok items' ->
       let find name = Option.value_exn (find_scheme items' name) in
       List.iter annotated ~f:(fun (original, ty) ->
         List.iter [ "_copy"; "_alone" ] ~f:(fun suffix ->
           compare_schemes
             (find original)
             (find (original ^ suffix))
             ~name:(original ^ suffix)
             ~printed:ty
             ~show
             ~context:None)));
    List.iter weak ~f:(fun (name, ty) -> round_trip_weak source ~name ~ty ~show)
;;
