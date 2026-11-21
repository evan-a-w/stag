open! Import
open Shape
open Type
module Error = Shape_error
module Resolver = Resolver

let rec map_result list ~f =
  match list with
  | [] -> Ok []
  | x :: xs ->
    let%bind.Result x = f x in
    let%map.Result xs = map_result xs ~f in
    x :: xs
;;

let default_ptr_name = "ptr"
let ptr_name = default_ptr_name

(* How a declaration's parameter is instantiated. *)
module Binding = struct
  type t =
    | Ty of Type.ty
    | Row of Type.row
  [@@deriving sexp_of]
end

(* The expansions whose bodies are being mapped, innermost first. *)
module Path = struct
  module Step = struct
    type t =
      | Declaration of
          { name : string
          ; args : Type.arg list
          }
      | Record of Type.id
      | Variant of Type.id
  end

  type t = Step.t list

  let name (step : Step.t) =
    match step with
    | Declaration { name; _ } -> name
    | Record id -> sprintf "record %d" id
    | Variant id -> sprintf "variant %d" id
  ;;

  let same_arguments arguments1 arguments2 =
    List.length arguments1 = List.length arguments2
    && List.for_all2_exn arguments1 arguments2 ~f:(fun argument1 argument2 ->
      match argument1, argument2 with
      | ATy ty1, ATy ty2 -> Type.same_ty ty1 ty2
      | ARow row1, ARow row2 -> Type.same_row row1 row2
      | ATy _, ARow _ | ARow _, ATy _ -> false)
  ;;

  (* Re-entering the same declaration at the same arguments, or a structural
     type, would expand for ever. Another application of a declaration is
     finite: the expansion of [pair[pair[int]]] maps [pair[int]], which is not
     its own expansion. *)
  let same (step : Step.t) (step' : Step.t) =
    match step, step' with
    | Declaration { name; args }, Declaration { name = name'; args = args' } ->
      String.equal name name' && same_arguments args args'
    | Record id, Record id' | Variant id, Variant id' -> Int.equal id id'
    | Declaration _, (Record _ | Variant _)
    | Record _, (Declaration _ | Variant _)
    | Variant _, (Declaration _ | Record _) -> false
  ;;

  let rec repeats step = function
    | [] -> false
    | step' :: rest -> same step step' || repeats step rest
  ;;

  let names path = List.rev (List.map path ~f:name)
end

module Context = struct
  type t =
    { bindings : Binding.t Int.Map.t
    ; path : Path.t
    }

  let empty = { bindings = Int.Map.empty; path = [] }
end

(* Deeper than any value's type, and bounds the expansions of polymorphic
   recursion, whose types have no finite layout. *)
let max_expansions = 1000

type t =
  { resolver : Resolver.t
  ; ptr_name : string
  }

let create ?(ptr_name = default_ptr_name) resolver = { resolver; ptr_name }

(* Scalars the language has from the start. [bool] is a byte with only 0 and 1,
   so has [option]'s niche. *)
let builtin = function
  | "int" -> Some (Shape.Scalar { scalar = Scalar.I64; valid = None })
  | "char" -> Some (Shape.Scalar { scalar = Scalar.I8; valid = None })
  | "bool" -> Some (Shape.Scalar { scalar = Scalar.I8; valid = Some (0, 1) })
  | "unit" -> Some (Shape.Struct { repr = Repr.Default; fields = [] })
  | "string" | "exn" -> Some Ptr
  | _ -> None
;;

let rec struct_ t context tys =
  let%map.Result fields = map_result tys ~f:(shape t context) in
  Struct { repr = Repr.Default; fields }

(* The present entries of [row], first of each label (which shadows later ones),
   in row order. *)
and entries t context row =
  let rec go seen_labels seen_vars acc row =
    match Type.canonical_row row with
    | REmpty -> Ok (List.rev acc)
    | RVar { id; _ } ->
      (match Map.find context.Context.bindings id with
       | Some (Binding.Row row) ->
         if Set.mem seen_vars id
         then Error (Error.Recursive_row id)
         else go seen_labels (Set.add seen_vars id) acc row
       | Some (Binding.Ty _) -> Error (Error.Row_expected id)
       | None -> Error (Error.Unresolved_row id))
    | RExtend (label, presence, _assigned, entry, rest) ->
      if Set.mem seen_labels label
      then go seen_labels seen_vars acc rest
      else (
        let seen_labels = Set.add seen_labels label in
        match presence_of context presence with
        | Error error -> Error error
        | Ok `Absent -> go seen_labels seen_vars acc rest
        | Ok `Present -> go seen_labels seen_vars ((label, entry) :: acc) rest)
  in
  go String.Set.empty Int.Set.empty [] row

and presence_of context p : ([ `Present | `Absent ], Error.t) Result.t =
  match Type.canonical_presence p with
  | Present -> Ok `Present
  | Absent -> Ok `Absent
  | PVar { id; _ } -> Error (Error.Unresolved_presence id)

(* Labels are sorted, so the layout does not depend on the order of a row, and
   matches the maps that hold structural records and variants. *)
and sorted_entries t context row =
  let%map.Result entries = entries t context row in
  List.sort entries ~compare:(fun (label1, _) (label2, _) ->
    String.compare label1 label2)

and record t context row =
  let%bind.Result entries = sorted_entries t context row in
  let%map.Result fields =
    map_result entries ~f:(fun (_, ty) -> shape t context ty)
  in
  Struct { repr = Repr.Default; fields }

and variant t context row =
  let%bind.Result entries = sorted_entries t context row in
  let%map.Result cases =
    map_result entries ~f:(fun (_, ty) -> struct_ t context [ ty ])
  in
  Union { repr = Repr.Default; cases }

(* Mapping the body of the expansion [step], unless that is a cycle. *)
and expand t context step f =
  if Path.repeats step context.Context.path
  then Error (Error.Recursive (Path.names (step :: context.path)))
  else if List.length context.path >= max_expansions
  then Error (Error.Too_deep max_expansions)
  else f { context with path = step :: context.path }

and declared t context ~name ~args =
  match Resolver.find t.resolver name with
  | None -> Error (Error.Unknown_type name)
  | Some declaration ->
    expand
      t
      context
      (Path.Step.Declaration { name; args })
      (fun context ->
        let%bind.Result bindings = bind_parameters declaration ~args in
        let context =
          { context with
            bindings =
              Map.fold
                bindings
                ~init:context.bindings
                ~f:(fun ~key ~data bindings -> Map.set bindings ~key ~data)
          }
        in
        match declaration.kind with
        | Abstract -> Error (Error.Abstract_type name)
        | Alias body -> shape t context body
        | Variant constructors ->
          let%map.Result cases =
            map_result
              constructors
              ~f:(fun (constructor : Signature.Constructor.t) ->
                struct_ t context constructor.args)
          in
          Union { repr = Repr.Default; cases }
        | Record fields ->
          let%map.Result fields =
            map_result fields ~f:(fun (field : Signature.Field.t) ->
              shape t context field.ty)
          in
          Struct { repr = Repr.Default; fields })

and bind_parameters (declaration : Signature.Type_decl.t) ~args =
  if List.length declaration.params <> List.length args
  then
    Error
      (Error.Wrong_arity
         { name = declaration.name
         ; expected = List.length declaration.params
         ; got = List.length args
         })
  else
    List.fold2_exn
      declaration.params
      args
      ~init:(Ok Int.Map.empty)
      ~f:(fun bindings parameter argument ->
        let%bind.Result bindings in
        match parameter, argument with
        | ATy ty, argument ->
          (match Type.canonical_ty ty with
           | TVar { id; _ } ->
             let binding =
               match argument with
               | ATy ty -> Binding.Ty ty
               | ARow row -> Binding.Row row
             in
             Ok (Map.set bindings ~key:id ~data:binding)
           | _ -> Ok bindings (* Not a variable: the body already has it. *))
        | ARow row, (ARow row' as _argument) ->
          (match Type.canonical_row row with
           | RVar { id; _ } ->
             Ok (Map.set bindings ~key:id ~data:(Binding.Row row'))
           | _ -> Ok bindings)
        | ARow _, ATy _ -> Ok bindings)

and shape t context (ty : Type.ty) : (Shape.t, Error.t) Result.t =
  match Type.canonical_ty ty with
  | TVar { id; _ } ->
    (match Map.find context.Context.bindings id with
     | Some (Binding.Ty ty) -> shape t context ty
     | Some (Binding.Row row) -> record t context row
     | None -> Error (Error.Unresolved_ty id))
  | TCon (name, args) ->
    (match builtin name with
     | Some shape -> Ok shape
     | None ->
       if String.equal name t.ptr_name
       then Ok Ptr
       else if String.equal name Type.error_name
       then Error Error.Error_type
       else declared t context ~name ~args)
  | TArrow _ | TCode _ -> Ok Ptr
  | TTuple tys -> struct_ t context tys
  | TRecord { id; row } ->
    expand t context (Path.Step.Record id) (fun context -> record t context row)
  | TVariant { id; row } ->
    expand t context (Path.Step.Variant id) (fun context ->
      variant t context row)
;;

let shape t ty = shape t Context.empty ty

let layout t ty =
  let%bind.Result shape = shape t ty in
  Layout.of_shape shape
  |> Result.map_error ~f:(fun error ->
    Error.Layout (Core.Error.sexp_of_t error))
;;
