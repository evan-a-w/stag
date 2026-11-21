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

(* Raised while substituting a variable bound, through other variables, to
   itself: the type is not concrete. *)
exception Cycle of Type.id

(* A row variable bound to a type: the parameter and argument disagree, which
   [bind_parameters] rejects. *)
exception Not_a_row of Type.id

(* The expansions whose bodies are being mapped, innermost first. Arguments and
   rows are substituted through the bindings in effect before a step is
   recorded, so that the same instantiation is the same step however it was
   reached: [r['a] = u['a]] gives ['a] the value it has where the step is, not
   the name it has there. *)
module Path = struct
  module Step = struct
    type t =
      | Declaration of
          { name : string
          ; args : Type.arg list
          }
      | Record of Type.id * Type.row
      | Variant of Type.id * Type.row
  end

  type t = Step.t list

  let name (step : Step.t) =
    match step with
    | Declaration { name; _ } -> name
    | Record (id, _) -> sprintf "record %d" id
    | Variant (id, _) -> sprintf "variant %d" id
  ;;

  let same_arguments arguments1 arguments2 =
    List.length arguments1 = List.length arguments2
    && List.for_all2_exn arguments1 arguments2 ~f:(fun argument1 argument2 ->
      match argument1, argument2 with
      | ATy ty1, ATy ty2 -> Type.same_ty ty1 ty2
      | ARow row1, ARow row2 -> Type.same_row row1 row2
      | ATy _, ARow _ | ARow _, ATy _ -> false)
  ;;

  (* Re-entering the same declaration at the same instantiation, or a structural
     type, would expand for ever. Another application of a declaration is
     finite: the expansion of [pair[pair[int]]] maps [pair[int]], which is not
     its own expansion. *)
  let same (step : Step.t) (step' : Step.t) =
    match step, step' with
    | Declaration { name; args }, Declaration { name = name'; args = args' } ->
      String.equal name name' && same_arguments args args'
    | Record (id, row), Record (id', row')
    | Variant (id, row), Variant (id', row') ->
      Int.equal id id' && Type.same_row row row'
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

(* Shapes of declarations already mapped, by canonical name and instantiation.
   Only a shape that was computed is stored, never one that was refused, and
   [declared] looks for a cycle first: a shape computed outside a cycle would
   not be the one inside it. *)
module Cache = struct
  type t = (Type.arg list * Shape.t) list String.Table.t

  let create () = String.Table.create ()

  let find t ~name ~args =
    Option.bind (Hashtbl.find t name) ~f:(fun shapes ->
      List.find_map shapes ~f:(fun (args', shape) ->
        if Path.same_arguments args args' then Some shape else None))
  ;;

  let add t ~name ~args shape =
    Hashtbl.update t name ~f:(fun shapes ->
      (args, shape) :: Option.value shapes ~default:[])
  ;;
end

module Context = struct
  type t =
    { bindings : Binding.t Int.Map.t
    ; resolving : Int.Set.t
        (* The variables whose bindings are being substituted. *)
    ; path : Path.t
    ; cache : Cache.t
    ; ids : int ref
        (* For the ids of the copies substitution makes: negative, so distinct
           from [Fresh]'s, which a key compares records by. *)
    ; work : int ref
    }
end

(* Deeper than any value's type, and bounds the expansions of polymorphic
   recursion, whose types have no finite layout. *)
let max_expansions = 1000

(* Bounds the time of a type that expands without repeating, but whose shape
   grows exponentially with the depth. *)
let max_work = 100_000

type t =
  { resolver : Resolver.t
  ; ptr_name : string
  }

let create ?(ptr_name = default_ptr_name) resolver = { resolver; ptr_name }

(* Substitution through the bindings. [Type.map_ty] keeps what does not change
   and closes the copies of cycles, and only bound variables reach the
   callbacks, so this terminates on any type it is given. Bindings cannot cycle
   in a checked program: an argument is substituted before it is bound, so a
   parameter is never bound to itself. The guard is for types built by hand. *)
let rec subst_ty ~bindings ~ids ~resolving ty =
  if Map.is_empty bindings
  then ty
  else Type.map_ty (mapper ~bindings ~ids ~resolving) ty

and subst_row ~bindings ~ids ~resolving row =
  if Map.is_empty bindings
  then row
  else Type.map_row (mapper ~bindings ~ids ~resolving) row

and mapper ~bindings ~ids ~resolving : Type.Mapper.t =
  let bound id f value =
    if Set.mem resolving id
    then raise (Cycle id)
    else f ~bindings ~ids ~resolving:(Set.add resolving id) value
  in
  { keeps = (fun var -> not (Map.mem bindings var.id))
  ; ty =
      (fun var ->
        match Map.find bindings var.id with
        | Some (Binding.Ty ty) -> bound var.id subst_ty ty
        | Some (Binding.Row row) ->
          (* A parameter instantiated with a row is the record of that row. *)
          Type.TRecord { id = var.id; row = bound var.id subst_row row }
        | None -> TVar var)
  ; row =
      (fun var ->
        match Map.find bindings var.id with
        | Some (Binding.Row row) -> bound var.id subst_row row
        | Some (Binding.Ty _) -> raise (Not_a_row var.id)
        | None -> RVar var)
  ; presence = (fun var -> PVar var)
  ; id =
      (fun () ->
        decr ids;
        !ids)
  }
;;

(* Substitute the bindings into a type, or a row: each call starts its own
   guard, as [shape]'s own descent into bindings is unrelated. *)
let substitute_ty context ty =
  subst_ty
    ~bindings:context.Context.bindings
    ~ids:context.ids
    ~resolving:Int.Set.empty
    ty
;;

let substitute_row context row =
  subst_row
    ~bindings:context.Context.bindings
    ~ids:context.ids
    ~resolving:Int.Set.empty
    row
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
        match presence_of presence with
        | Error error -> Error error
        | Ok `Absent -> go seen_labels seen_vars acc rest
        | Ok `Present -> go seen_labels seen_vars ((label, entry) :: acc) rest)
  in
  go String.Set.empty Int.Set.empty [] row

and presence_of p : ([ `Present | `Absent ], Error.t) Result.t =
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

(* Whether [step] closes a cycle, or is deeper than [max_expansions]. *)
and cycle_check context step =
  if Path.repeats step context.Context.path
  then Error (Error.Recursive (Path.names (step :: context.path)))
  else if List.length context.path >= max_expansions
  then Error (Error.Too_deep max_expansions)
  else Ok ()

and arity ~name ~args expected =
  if List.length args = expected
  then Ok ()
  else Error (Error.Wrong_arity { name; expected; got = List.length args })

(* Scalars the language has from the start. [bool] is a byte with only 0 and 1,
   so has [option]'s niche. *)
and builtin ~name ~args : (Shape.t, Error.t) Result.t option =
  let scalar scalar = Shape.Scalar { scalar; valid = None } in
  let nullary shape =
    Some (Result.map (arity ~name ~args 0) ~f:(fun () -> shape))
  in
  match name with
  | "int" -> nullary (scalar Scalar.I64)
  | "char" -> nullary (scalar Scalar.I8)
  | "bool" -> nullary (Shape.Scalar { scalar = Scalar.I8; valid = Some (0, 1) })
  | "unit" -> nullary (Struct { repr = Repr.Default; fields = [] })
  | "string" | "exn" -> nullary Ptr
  | _ -> None

and subst_args context args =
  map_result args ~f:(function
    | ATy ty -> Ok (ATy (substitute_ty context ty))
    | ARow row -> Ok (ARow (substitute_row context row)))

and declared t context ~name ~args =
  match Resolver.find t.resolver name with
  | None -> Error (Error.Unknown_type name)
  | Some declaration ->
    let%bind.Result args = subst_args context args in
    (match cycle_check context (Path.Step.Declaration { name; args }) with
     | Error error -> Error error
     | Ok () ->
       (match Cache.find context.Context.cache ~name ~args with
        | Some shape -> Ok shape
        | None ->
          let path = Path.Step.Declaration { name; args } :: context.path in
          let%bind.Result bindings = bind_parameters declaration ~name ~args in
          let context =
            { context with
              path
            ; bindings =
                Map.fold
                  bindings
                  ~init:context.bindings
                  ~f:(fun ~key ~data acc -> Map.set acc ~key ~data)
            ; resolving = Int.Set.empty
            }
          in
          let%bind.Result shape =
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
              Struct { repr = Repr.Default; fields }
          in
          Cache.add context.cache ~name ~args shape;
          Ok shape))

(* A parameter is bound to its argument as it stands, which [declared] has
   substituted: a type parameter takes a type, or a row (the record of that
   row), and a row parameter takes a row. *)
and bind_parameters (declaration : Signature.Type_decl.t) ~name ~args =
  if List.length declaration.params <> List.length args
  then
    Error
      (Error.Wrong_arity
         { name
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
        | ARow row, argument ->
          (match Type.canonical_row row with
           | RVar { id; _ } ->
             (match argument with
              | ARow row' ->
                Ok (Map.set bindings ~key:id ~data:(Binding.Row row'))
              | ATy _ -> Error (Error.Row_expected id))
           | _ -> Ok bindings))

and shape t context ty =
  incr context.Context.work;
  if !(context.work) > max_work
  then Error (Error.Too_large max_work)
  else expand t context ty

and expand t context (ty : Type.ty) : (Shape.t, Error.t) Result.t =
  match Type.canonical_ty ty with
  | TVar { id; _ } ->
    (match Map.find context.Context.bindings id with
     | Some (Binding.Ty ty) ->
       if Set.mem context.resolving id
       then Error (Error.Recursive_ty id)
       else shape t { context with resolving = Set.add context.resolving id } ty
     | Some (Binding.Row row) ->
       if Set.mem context.resolving id
       then Error (Error.Recursive_ty id)
       else
         record t { context with resolving = Set.add context.resolving id } row
     | None -> Error (Error.Unresolved_ty id))
  | TCon (name, args) ->
    if String.equal name t.ptr_name
    then Result.map (arity ~name ~args 1) ~f:(fun () -> Ptr)
    else (
      match builtin ~name ~args with
      | Some shape -> shape
      | None ->
        if String.equal name Type.error_name
        then Error Error.Error_type
        else declared t context ~name ~args)
  | TArrow _ | TCode _ -> Ok Ptr
  | TTuple tys -> struct_ t context tys
  | TRecord { id; row } ->
    let row = substitute_row context row in
    (match cycle_check context (Path.Step.Record (id, row)) with
     | Error error -> Error error
     | Ok () ->
       record
         t
         { context with path = Path.Step.Record (id, row) :: context.path }
         row)
  | TVariant { id; row } ->
    let row = substitute_row context row in
    (match cycle_check context (Path.Step.Variant (id, row)) with
     | Error error -> Error error
     | Ok () ->
       variant
         t
         { context with path = Path.Step.Variant (id, row) :: context.path }
         row)
;;

let context () =
  { Context.bindings = Int.Map.empty
  ; resolving = Int.Set.empty
  ; path = []
  ; cache = Cache.create ()
  ; ids = ref 0
  ; work = ref 0
  }
;;

let shape t ty =
  match shape t (context ()) ty with
  | result -> result
  | exception Cycle id -> Error (Error.Recursive_ty id)
  | exception Not_a_row id -> Error (Error.Row_expected id)
;;

let layout t ty =
  let%bind.Result shape = shape t ty in
  Layout.of_shape shape
  |> Result.map_error ~f:(fun error ->
    Error.Layout (Core.Error.sexp_of_t error))
;;
