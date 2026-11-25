open! Import
open Source_helpers
module Arrow_modes = Stag_types.Arrow_modes
module Mode_arg = Stag_types.Mode_arg

let checked source =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let typecheck = Typecheck.create ~stamps ~interpreter () in
  let prelude = program ~prelude:true "" in
  match Typecheck.typecheck typecheck ~program:(program source) with
  | Ok checked -> Ok (List.drop checked.signature (List.length prelude))
  | Error { errors; partial = _ } ->
    Error (List.map errors ~f:Typecheck_error.message)
;;

let signature source =
  Result.map (checked source) ~f:(Signature.to_string_hum ~modes:true)
;;

let allocations source =
  let stamps = Stamp_counter.create () in
  let interpreter = Interpreter.create Treewalk.interpreter ~stamps in
  Interpreter.set_limits interpreter limits;
  let typecheck = Typecheck.create ~stamps ~interpreter () in
  let prelude = program ~prelude:true "" in
  match Typecheck.typecheck typecheck ~program:(program source) with
  | Error { errors; partial = _ } ->
    List.iter errors ~f:(fun error ->
      print_endline (Typecheck_error.message error))
  | Ok checked ->
    print_string
      (Signature.to_string_hum
         ~modes:true
         (List.drop checked.signature (List.length prelude)));
    List.iter
      checked.program.allocations
      ~f:(fun (allocation : Typed.Allocation.t) ->
        if String.equal (Loc.file_name allocation.loc) file
        then (
          let placement = function
            | Typed.Allocation.Placement.Heap -> "heap"
            | Region -> "region"
            | Caller_region -> "caller's region"
          in
          let where =
            if Typed.Allocation.Placement.equal
                 allocation.if_global
                 allocation.if_local
            then placement allocation.if_global
            else
              sprintf
                "%s if global, %s if local"
                (placement allocation.if_global)
                (placement allocation.if_local)
          in
          printf
            !"%s %{Sexp} in %s: %s\n"
            (Loc.to_string allocation.loc)
            (Typed.Allocation.Kind.sexp_of_t allocation.kind)
            allocation.toplevel.name
            where))
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
          | AMode (Fixed const) ->
            add (Fixed const) ~result:false ~mode_arg:true
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
   results at least [portable]. A mode argument's locality is not compared
   unless the variable is also an arrow's. *)
let projection positions =
  let open Positions in
  let vars =
    List.filter_map positions ~f:(fun p ->
      match p.side with
      | Var var -> Some var
      | Fixed _ -> None)
  in
  let on_arrow var =
    List.exists positions ~f:(fun p ->
      (not p.mode_arg)
      &&
      match p.side with
      | Var var' -> Mode.Var.id var' = Mode.Var.id var
      | Fixed _ -> false)
  in
  let dead (p : position) (Mode.Axis.Any.T axis) =
    match axis, p.side with
    | Locality, Var var -> not (on_arrow var)
    | Locality, Fixed _ -> p.mode_arg
    | (Stage | Provenance), _ -> false
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
      | Var var, true -> Some (node var (T Stage), 1)
      | _ -> None)
  in
  if List.exists vars ~f:(Fn.non Mode.Var.is_generic)
  then Error [%message "weak"]
  else (
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
              |> List.map ~f:(fun point -> levels [ n, point ]))) ))
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
    let annotated = List.filter annotated ~f:comparable in
    let copies =
      List.map annotated ~f:(fun (name, ty) ->
        sprintf "let %s_copy :%s = %s" name ty name)
    in
    (* A function's annotation alone: [fun] adds nothing to its arrow's sides
       but what every function has. *)
    let is_arrow ty =
      let ty =
        match String.substr_index ty ~pattern:" with " with
        | Some i -> String.prefix ty i
        | None -> ty
      in
      match Stag_syntax.Parser.type_expr ~file:"t.stag" ty with
      | Ok { txt = Arrow _; _ } -> true
      | Ok _ | Error _ -> false
    in
    let alone =
      List.map annotated ~f:(fun (name, ty) ->
        sprintf
          "let %s_alone :%s = %s"
          name
          ty
          (if is_arrow ty
           then "fun v -> round_trip_diverge ()"
           else "round_trip_diverge ()"))
    in
    (match show with
     | `Signature -> print_string printed
     | `Copies -> print_endline (String.concat ~sep:"\n" copies)
     | `Quiet -> ());
    let round_trip_diverge =
      "let round_trip_diverge (u : unit) = round_trip_diverge u"
    in
    (match
       checked
         (String.concat
            ~sep:"\n"
            ((source :: round_trip_diverge :: copies) @ alone))
     with
     | Error errors -> List.iter errors ~f:print_endline
     | Ok items' ->
       let find items name =
         List.find_map items ~f:(function
           | Signature.Item.Value { name = name'; scheme; _ }
             when String.equal name name' -> Some scheme
           | _ -> None)
       in
       let project scheme =
         Result.bind (Positions.of_ty scheme) ~f:projection
       in
       List.iter annotated ~f:(fun (original, _) ->
         match project (Option.value_exn (find items original)) with
         | Error error ->
           (match show with
            | `Quiet -> ()
            | `Signature | `Copies ->
              print_s [%message "not compared" original (error : Sexp.t)])
         | Ok expected ->
           List.iter [ "_copy"; "_alone" ] ~f:(fun suffix ->
             let name = original ^ suffix in
             match project (Option.value_exn (find items' name)) with
             | Ok actual
               when [%equal: int list * int list * int list list]
                      expected
                      actual -> ()
             | Ok actual ->
               print_s
                 [%message
                   "differs"
                     (name : string)
                     (expected : int list * int list * int list list)
                     (actual : int list * int list * int list list)
                     ~printed:
                       (List.Assoc.find annotated original ~equal:String.equal
                        : string option)]
             | Error error ->
               print_s [%message "differs" (name : string) (error : Sexp.t)])))
;;
