open! Import

module Site = struct
  module Position = struct
    type t =
      | Here
      | Returned
      | Exclaved
      | Caller
    [@@deriving sexp_of]
  end

  type t =
    { loc : Loc.t
    ; kind : Typed.Allocation.Kind.t
    ; mode : Mode.Var.t
    ; position : Position.t
    }
  [@@deriving sexp_of]
end

module Ctx = struct
  type t =
    { modes : Mode.Id_source.t
    ; rank : int
    ; holds : loc:Loc.t -> (unit, Mode.Error.t) Result.t -> bool
    ; crossing : Type.ty -> Mode.Crossing.t
    ; report : Typecheck_error.t -> unit
    ; allocated : Site.t -> Typed.Allocation.Id.t option
    ; exclaved : bool
    }

  let create ~modes ~rank ~holds ~crossing ~report ~allocated ~locks =
    let exclaved =
      List.find_map locks ~f:(fun (lock : Env.Lock.t) ->
        match lock.kind with
        | Exclave -> Some true
        | Closure _ | Region -> Some false
        | Quote _ | Splice -> None)
      |> Option.value ~default:false
    in
    { modes; rank; holds; crossing; report; allocated; exclaved }
  ;;

  let fresh t = Mode.Var.create t.modes ~rank:t.rank
  let add t ~loc result = ignore (t.holds ~loc result : bool)

  (* Adds [f]'s constraints with the crossing of [ty] (of none without it).
     While [ty] isn't known, what crossing leaves out is added once it is found
     to have pointers. *)
  let add_crossing ?ty t ~loc f =
    add
      t
      ~loc
      (f
         (match ty with
          | None -> Mode.Crossing.none
          | Some ty -> t.crossing ty))
  ;;
end

let reason ~loc why = Mode.Reason.create ~loc why

module Tail = struct
  type t =
    | Nontail
    | Returns of Mode.Var.t (** The function's result mode. *)
    | Ends of Mode.Var.t
    (** The [region_]'s result mode: the region ends after a call there, so it
        is not a tail call, but [exclave_] may end it early. *)
    | Quoted (** Where the code is spliced decides. *)
end

module Expected : sig
  type t = private
    { mode : Mode.Var.t
    ; region : Tail.t
    ; stack : bool ref option
    ; applied : bool
    (** A function a tail call gives further arguments: the result of an
        application. *)
    ; returned : bool
    (** The value, or a part of it, is what its function (or [region_]) gives. *)
    ; shape : Mode_shape.t option ref option
    (** Where the modes of the value's components are wanted, given by what
        makes the value from them: else they are not known. Only what is the
        same value keeps it. *)
    }

  val of_mode : Mode.Var.t -> t
  val fresh : Ctx.t -> t
  val quoted : Ctx.t -> t
  val is_tail : t -> bool
  val returned : t -> bool

  (** The same value. *)

  val whole : t -> t
  val nontail : t -> t

  (** At a mode of its own, related to [t]'s by [stack_]. *)
  val stack : Ctx.t -> t -> allocates:bool ref -> t

  (** Other values. *)

  val boxed : t -> whole:t -> t

  (** A part of [t]'s value, at [mode], allocated by [stack]. *)
  val part : t -> mode:Mode.Var.t -> stack:bool ref option -> t

  (** At a mode of its own, given to the caller in [region]. *)
  val annotated : Ctx.t -> t -> region:Tail.t -> t

  val body : Ctx.t -> region:Tail.t -> t
  val callee : Ctx.t -> t -> t
  val captured : t -> t

  (** Wanting the modes of its components. *)
  val wanting : t -> t * Mode_shape.t option ref

  val shape : t -> Mode_shape.t option ref -> Mode_shape.t
  val gives : t -> Mode_shape.t -> unit
end = struct
  type t =
    { mode : Mode.Var.t
    ; region : Tail.t
    ; stack : bool ref option
    ; applied : bool
    ; returned : bool
    ; shape : Mode_shape.t option ref option
    }

  let of_mode mode =
    { mode
    ; region = Nontail
    ; stack = None
    ; applied = false
    ; returned = false
    ; shape = None
    }
  ;;

  let fresh ctx = of_mode (Ctx.fresh ctx)
  let quoted ctx = { (fresh ctx) with region = Quoted }

  let is_tail t =
    match t.region with
    | Returns _ -> true
    | Ends _ | Nontail | Quoted -> false
  ;;

  let returned t = t.returned
  let whole t = { t with stack = None; applied = false }
  let nontail t = { t with region = Nontail; applied = false; returned = false }

  let stack ctx t ~allocates =
    { t with mode = Ctx.fresh ctx; stack = Some allocates; applied = false }
  ;;

  (* [shape] is of [t]'s value, so another value doesn't keep it. *)
  let other t = { t with shape = None }
  let boxed t ~whole = other { t with returned = whole.returned }

  let part t ~mode ~stack =
    other { t with mode; stack; region = Nontail; applied = false }
  ;;

  let annotated ctx t ~region =
    { (fresh ctx) with region; stack = t.stack; returned = t.returned }
  ;;

  let body ctx ~region = { (fresh ctx) with region; returned = true }
  let callee ctx t = { (fresh ctx) with region = t.region; applied = true }

  let captured t =
    other { t with region = Nontail; stack = None; returned = false }
  ;;

  let wanting t =
    let shape = ref None in
    { t with shape = Some shape }, shape
  ;;

  let shape t shape = Option.value !shape ~default:(Mode_shape.opaque t.mode)

  let gives t shape =
    Option.iter t.shape ~f:(fun wanted -> wanted := Some shape)
  ;;
end

(* With [ty]'s crossing, adjusted by [cross], or else the [crossing] given. *)
let joins
  ?ty
  ?(cross = Fn.id)
  ?crossing
  ?locality
  ctx
  (expected : Expected.t)
  mode
  ~loc
  ~why
  =
  let submode crossing =
    Mode.Var.submode ~crossing ?locality mode expected.mode (reason ~loc why)
  in
  match crossing with
  | Some crossing -> Ctx.add ctx ~loc (submode crossing)
  | None ->
    Ctx.add_crossing ?ty ctx ~loc (fun crossing -> submode (cross crossing))
;;

(* What the value reaches, but not what it is allocated with. *)
let held ctx expected mode ~loc ~why =
  joins ctx expected mode ~locality:Mode.Locality_map.ignore ~loc ~why
;;

let evaluated ctx expected mode ~loc ~why =
  joins
    ctx
    expected
    mode
    ~crossing:{ Mode.Crossing.none with provenance = Always }
    ~locality:Mode.Locality_map.ignore
    ~loc
    ~why
;;

let evaluate_shaped ?inner ctx expected ~loc ~why f =
  let sub, shape =
    Expected.wanting (Expected.fresh (Option.value inner ~default:ctx))
  in
  let result = f sub in
  evaluated ctx expected sub.mode ~loc ~why;
  result, Expected.shape sub shape
;;

let evaluate ?inner ctx expected ~loc ~why f =
  let sub = Expected.fresh (Option.value inner ~default:ctx) in
  let result = f sub in
  evaluated ctx expected sub.mode ~loc ~why;
  result, sub.mode
;;

module Arms = struct
  type t =
    { ctx : Ctx.t
    ; expected : Expected.t
    ; arms : (Expected.t * Mode_shape.t option ref) Queue.t
    }

  let create ctx expected = { ctx; expected; arms = Queue.create () }

  let arm t =
    let arm = Expected.whole t.expected in
    match arm.shape with
    | None -> arm
    | Some _ ->
      let arm, shape = Expected.wanting arm in
      Queue.enqueue t.arms (arm, shape);
      arm
  ;;

  let finish t ~loc =
    if Option.is_some t.expected.shape
    then
      Mode_shape.join
        (Queue.to_list t.arms
         |> List.map ~f:(fun (arm, shape) -> Expected.shape arm shape))
        ~whole:t.expected.mode
        ~component:(fun modes ->
          let joined = Ctx.fresh t.ctx in
          List.iter modes ~f:(fun mode ->
            Ctx.add
              t.ctx
              ~loc
              (Mode.Var.submode mode joined (reason ~loc "it is of a case")));
          joined)
      |> Expected.gives t.expected
  ;;
end

let runs ctx (expected : Expected.t) stage ~loc ~why =
  Ctx.add
    ctx
    ~loc
    (Mode.Var.at_least expected.mode Stage stage (reason ~loc why))
;;

let stage_at_least ctx stage ~loc ~why =
  let mode = Ctx.fresh ctx in
  Mode.Error.ok_exn (Mode.Var.at_least mode Stage stage (reason ~loc why));
  mode
;;

(* A fresh mode of a constant of the code: [const static global]. *)
let constant ctx ~loc ~why =
  let mode = Ctx.fresh ctx in
  Mode.Error.ok_exn
    (Mode.Var.at_most_const mode Mode.Const.min (reason ~loc why));
  mode
;;

let locality_at_most ~report_loc ctx mode ~ty locality ~loc ~why =
  Ctx.add_crossing ~ty ctx ~loc:report_loc (fun crossing ->
    Mode.Var.at_most ~crossing mode Locality locality (reason ~loc why))
;;

let arrow_mode = Ctx.fresh
let fresh_held ctx = Mode_arg.Var (Ctx.fresh ctx)

let annotate ?(exact = false) ctx mode (annot : Syntax.Mode_expr.t option) ~var =
  Option.iter annot ~f:(fun { txt; loc } ->
    match txt with
    | Var name ->
      Ctx.add
        ctx
        ~loc
        ((if exact
          then Mode.Var.equate
          else Mode.Var.submode ?crossing:None ?locality:None)
           mode
           (var name ~loc)
           (reason ~loc (sprintf "it is annotated '%s" name)))
    | Atoms atoms ->
      (match Mode_atoms.invalid atoms with
       | Some why -> ctx.report (Typecheck_error.create ~loc (Invalid_mode why))
       | None ->
         List.iter atoms ~f:(fun atom ->
           let reason =
             reason
               ~loc
               (sprintf
                  "it is annotated %s"
                  (Syntax.Mode_expr.Atom.to_string atom))
           in
           let fix axis point =
             Ctx.add
               ctx
               ~loc
               (Mode.Var.at_most mode axis point reason
                |> Result.bind ~f:(fun () ->
                  if exact
                  then Mode.Var.at_least mode axis point reason
                  else Ok ()))
           in
           let (T (axis, point)) = Mode_atoms.point atom in
           fix axis point)))
;;

let annotated ctx annot ~var =
  let mode = Ctx.fresh ctx in
  annotate ~exact:true ctx mode annot ~var;
  mode
;;

let annotated_use ctx (expected : Expected.t) ~annot ~var ~loc f =
  match annot with
  | None -> f expected
  | Some _ ->
    (* A value given in the caller's region (by a tail call or [exclave_]) is
       related to the result by the region, not by [expected.mode]: the
       annotation bounds it there too. *)
    let given_to = Ctx.fresh ctx in
    let region : Tail.t =
      match expected.region with
      | Returns _ -> Returns given_to
      | Ends _ -> Ends given_to
      | (Nontail | Quoted) as region -> region
    in
    let used = Expected.annotated ctx expected ~region in
    let result = f used in
    Result.iter result ~f:(fun (used_expr : Typed.Expr.t) ->
      joins ctx expected used.mode ~ty:used_expr.ty ~loc ~why:"it is used";
      annotate ctx used.mode annot ~var;
      match expected.region with
      | Returns result | Ends result ->
        annotate ctx given_to annot ~var;
        Ctx.add_crossing ~ty:used_expr.ty ctx ~loc (fun crossing ->
          Mode.Var.submode
            ~crossing
            given_to
            result
            (reason ~loc "it is given to the caller"))
      | Nontail | Quoted -> ());
    result
;;

(* [value] fits [modality]: it may be stored in a part with it. *)
let fits ?ty ctx value (modality : Modality.t) ~loc ~why =
  let reason = reason ~loc why in
  List.iter modality ~f:(fun (step : Modality.Step.t) ->
    Ctx.add_crossing ?ty ctx ~loc (fun crossing ->
      match step with
      | Meet const -> Mode.Var.at_most_const ~crossing value const reason
      | Param mode | Cell mode ->
        Mode_arg.on_axes
          { f =
              (fun axis ~get ->
                match mode with
                | Fixed const ->
                  Mode.Var.at_most ~crossing value axis (get const) reason
                | Var var -> Mode.Var.submode_on ~crossing value var axis reason)
          }))
;;

(* At least what a part of a block of mode [block] is, through [step]. *)
let read_step ctx block (step : Modality.Step.t) ~loc ~why =
  let read = Ctx.fresh ctx in
  let reason =
    reason
      ~loc
      (match step with
       | Cell _ -> "it is read from a pointer's cell, which may hold it"
       | Meet _ | Param _ -> why)
  in
  let from_param (mode : Mode_arg.t) ~meet =
    Mode_arg.on_axes
      { f =
          (fun axis ~get ->
            match mode with
            | Fixed const ->
              if meet
              then Mode.Var.submode_meet_on block axis (get const) read reason
              else Mode.Var.at_least read axis (get const) reason
            | Var var -> Mode.Var.submode_on var read axis reason)
      }
  in
  (* What is read is [const] only if it is the same on every evaluation: a
     part's only if its block's is (a parameter's isn't), a cell's never, as its
     contents may change. *)
  let not_const =
    match step with
    | Meet _ | Param _ -> Mode.Var.submode_meet_on block Stage Portable read
    | Cell _ -> Mode.Var.at_least read Stage Portable
  in
  Ctx.add
    ctx
    ~loc
    (Result.all_unit
       [ (match step with
          | Meet const -> Mode.Var.submode_meet block const read reason
          | Param mode ->
            Result.all_unit
              [ from_param mode ~meet:true
              ; Mode.Var.submode_on block read Locality reason
              ]
          | Cell mode -> from_param mode ~meet:false)
       ; not_const reason
       ]);
  read
;;

let read ctx block (modality : Modality.t) ~loc ~why =
  List.fold modality ~init:block ~f:(fun block step ->
    read_step ctx block step ~loc ~why)
;;

(* Of its own mode, so that it is known apart from the rest of the value: what
   [stack_] allocates of it is local, whatever the rest is. Its components'
   modes are found if [want]ed. *)
let own ?ty ~want ctx (expected : Expected.t) (modality : Modality.t) f =
  let global = Modality.is_global modality in
  let allocates = Option.map expected.stack ~f:(fun _ -> ref false) in
  (* What a [global] part holds is not allocated with the block. *)
  let sub =
    if global
    then Expected.fresh ctx
    else Expected.part expected ~mode:(Ctx.fresh ctx) ~stack:allocates
  in
  let sub, shape =
    if want
    then (
      let sub, shape = Expected.wanting sub in
      sub, Some shape)
    else sub, None
  in
  let (part : Typed.Expr.t) = f sub in
  let ty = Option.value ty ~default:part.ty in
  let loc = part.loc in
  (match allocates, expected.stack with
   | Some allocates, Some whole when !allocates ->
     whole := true;
     Ctx.add
       ctx
       ~loc
       (Mode.Var.at_least
          sub.mode
          Locality
          Local
          (reason ~loc "it is allocated by [stack_]"))
   | _ -> ());
  fits
    ~ty
    ctx
    sub.mode
    modality
    ~loc
    ~why:"it is held by a field or argument with a modality";
  if global
  then held ctx expected sub.mode ~loc ~why:"it is held"
  else joins ctx expected sub.mode ~ty ~loc ~why:"it is held";
  part, Option.map shape ~f:(Expected.shape sub)
;;

let part ?ty ctx expected modality f =
  fst (own ?ty ~want:false ctx expected modality f)
;;

module Parts = struct
  type t =
    { ctx : Ctx.t
    ; expected : Expected.t
    ; known : (Mode_shape.Step.t * Mode_shape.t) Queue.t option
    }

  let create ctx (expected : Expected.t) =
    { ctx
    ; expected
    ; known = Option.map expected.shape ~f:(fun _ -> Queue.create ())
    }
  ;;

  let part ?ty t step modality f =
    let part, shape =
      own ?ty ~want:(Option.is_some t.known) t.ctx t.expected modality f
    in
    (match t.known, shape with
     | Some known, Some shape -> Queue.enqueue known (step, shape)
     | _ -> ());
    part
  ;;

  let finish t =
    Option.iter t.known ~f:(fun known ->
      Expected.gives
        t.expected
        (Mode_shape.known ~whole:t.expected.mode (Queue.to_list known)))
  ;;
end

let project ctx expected ~loc ~why f =
  let block, shape = Expected.wanting (Expected.fresh ctx) in
  let result, (modality : Modality.t), step = f block in
  let read =
    match
      Option.bind step ~f:(Mode_shape.part (Expected.shape block shape))
    with
    | Some (component : Mode_shape.t) -> component.whole
    | None -> read ctx block.mode modality ~loc ~why
  in
  joins ctx expected read ~loc ~why;
  result
;;

let initializes ?ty ctx ~value ~held ~name ~loc =
  Ctx.add_crossing ?ty ctx ~loc (fun crossing ->
    Mode.Var.submode
      ~crossing
      value
      held
      (reason ~loc (sprintf "it initializes %s" name)))
;;

module Binding = struct
  type t = Binding.t [@@deriving sexp_of]

  let of_mode = Binding.of_mode

  let value ctx mode (annot : Syntax.Mode_expr.t option) ~var =
    match annot with
    | None -> Binding.of_mode mode
    | Some { txt; loc } ->
      let bound = Ctx.fresh ctx in
      let reason = reason ~loc "it is bound by an annotated let" in
      (* Annotated first, so a value exceeding it is the one error, the binding
         keeping its annotated mode for its uses. *)
      annotate ~exact:true ctx bound annot ~var;
      Ctx.add ctx ~loc (Mode.Var.submode mode bound reason);
      let named =
        match txt with
        | Atoms atoms ->
          List.filter_map atoms ~f:(fun (atom : Syntax.Mode_expr.Atom.t) ->
            match atom with
            | Const | Portable | Nonportable -> Some (Mode.Axis.Any.T Stage)
            | Global | Regional | Local -> Some (T Locality)
            | Static | Gc | External -> Some (T Provenance))
        | Var _ -> [ T Stage; T Locality; T Provenance ]
      in
      List.iter
        [ Mode.Axis.Any.T Stage; T Provenance; T Locality ]
        ~f:(fun (T axis as any) ->
          if not (List.mem named any ~equal:Mode.Axis.Any.equal)
          then Ctx.add ctx ~loc (Mode.Var.submode_on bound mode axis reason));
      Binding.of_mode bound
  ;;

  let closure = Binding.of_mode

  let defining ctx (expected : Expected.t) ~name ~loc =
    let own =
      stage_at_least
        ctx
        Nonportable
        ~loc
        ~why:(sprintf "%s is being defined" name)
    in
    Ctx.add
      ctx
      ~loc
      (Mode.Var.submode
         expected.mode
         own
         (reason ~loc (sprintf "it defines %s" name)));
    Binding.of_mode own
  ;;

  let mutable_ ?ty ctx ~value annot ~var ~name ~loc =
    let held = Ctx.fresh ctx in
    initializes ?ty ctx ~value ~held ~name ~loc;
    annotate ~exact:true ctx held annot ~var;
    let cell =
      stage_at_least ctx Nonportable ~loc ~why:(sprintf "%s is mutable" name)
    in
    Binding.of_mode cell, Binding.of_mode held
  ;;

  let component ctx block path ~loc =
    let mode, modality, shape = Mode_shape.follow block path in
    ( Binding.of_mode (read ctx mode modality ~loc ~why:"it is bound to a part")
    , shape )
  ;;

  let embedded ctx ~name ~loc =
    of_mode
      (constant
         ctx
         ~loc
         ~why:
           (sprintf
              "%s was computed at compile time, and lifted into the code"
              name))
  ;;

  let being_checked ctx ~name ~loc =
    Binding.of_mode
      (stage_at_least
         ctx
         Nonportable
         ~loc
         ~why:(sprintf "%s is being checked" name))
  ;;
end

let raised ctx ~loc =
  let mode =
    stage_at_least ctx Nonportable ~loc ~why:"it is a raised exception"
  in
  Mode.Error.ok_exn
    (Mode.Var.at_least
       mode
       Provenance
       Gc
       (reason ~loc "it is a raised exception"));
  mode
;;

module Alloc = struct
  let site (expected : Expected.t) =
    Option.iter expected.stack ~f:(fun allocates -> allocates := true)
  ;;

  let allocates mode ~loc ~why =
    Mode.Var.allocated
      mode
      (reason ~loc (sprintf "%s at run time, unless it is [const]" why))
  ;;

  let allocated ctx ~loc ~why =
    let mode = Ctx.fresh ctx in
    Mode.Error.ok_exn (allocates mode ~loc ~why);
    mode
  ;;

  (* Under [exclave_], its mode is the caller's, returned or not. *)
  let here (ctx : Ctx.t) : Site.Position.t =
    if ctx.exclaved then Exclaved else Here
  ;;

  let record (ctx : Ctx.t) (expected : Expected.t) ~loc ~kind mode =
    site expected;
    ctx.allocated
      { Site.loc
      ; kind
      ; mode
      ; position =
          (if Expected.returned expected && not ctx.exclaved
           then Returned
           else here ctx)
      }
  ;;

  let closure ctx expected ~loc f =
    let why = "the closure is made" in
    let closure = allocated ctx ~loc ~why in
    let id = record ctx expected ~loc ~kind:Closure closure in
    let result = f closure in
    joins ctx expected closure ~loc ~why;
    result, id
  ;;

  let code ctx expected ~loc =
    let why = "the code is built" in
    let code = allocated ctx ~loc ~why in
    let id = record ctx expected ~loc ~kind:Code code in
    joins ctx expected code ~loc ~why;
    code, id
  ;;

  let pointer ctx expected ~loc ~kind mode = record ctx expected ~loc ~kind mode

  let on_heap (ctx : Ctx.t) ~loc ~kind =
    let mode = Ctx.fresh ctx in
    Mode.Error.ok_exn
      (Mode.Var.at_most
         mode
         Locality
         Global
         (reason ~loc "it is made by a function used as a value"));
    ctx.allocated { Site.loc; kind; mode; position = here ctx }
  ;;

  let cell (ctx : Ctx.t) ~loc mode =
    ctx.allocated { Site.loc; kind = Cell; mode; position = here ctx }
  ;;

  let stack ctx expected ~loc f =
    let allocates = ref false in
    let inner = Expected.stack ctx expected ~allocates in
    let (result : Typed.Expr.t) = f inner in
    if !allocates || Typed.Expr.has_error result
    then (
      Ctx.add
        ctx
        ~loc
        (Mode.Var.at_least
           inner.mode
           Locality
           Local
           (reason ~loc "it is allocated by [stack_]"));
      joins
        ctx
        expected
        inner.mode
        ~ty:result.ty
        ~loc
        ~why:"it is part of the value";
      Ok result)
    else Error result
  ;;
end

module Function = struct
  let partial_applications ctx ~closure ~arg_modes ~partial_modes ~loc =
    let reason = reason ~loc "a partial application holds it" in
    List.filter_mapi partial_modes ~f:(fun index partial ->
      let id =
        ctx.Ctx.allocated
          { Site.loc
          ; kind = Partial_application
          ; mode = partial
          ; position = Caller
          }
      in
      Ctx.add
        ctx
        ~loc
        (Alloc.allocates partial ~loc ~why:"the partial application is made"
         |> Result.bind ~f:(fun () ->
           Mode.Var.at_least
             partial
             Stage
             Portable
             (Mode.Reason.create
                ~loc
                "a partial application is made at run time")));
      Ctx.add ctx ~loc (Mode.Var.submode closure partial reason);
      List.iteri arg_modes ~f:(fun index' arg ->
        if index' <= index
        then Ctx.add ctx ~loc (Mode.Var.submode arg partial reason));
      id)
  ;;

  let param ctx ~arg_mode ~name ~loc =
    (* A parameter may differ between calls, so is never [const] in the body,
       whatever its callers pass. *)
    let mode =
      stage_at_least
        ctx
        Portable
        ~loc
        ~why:(sprintf "%s is a parameter, which may differ between calls" name)
    in
    Ctx.add
      ctx
      ~loc
      (Mode.Var.submode
         ~locality:Mode.Locality_map.borrow
         arg_mode
         mode
         (reason ~loc (sprintf "%s is passed to the function" name)));
    Binding.of_mode mode
  ;;

  let result_curry ctx ~loc =
    let curry = Ctx.fresh ctx in
    Mode.Error.ok_exn
      (Mode.Var.at_most
         curry
         Locality
         Regional
         (reason
            ~loc
            "the function returns a function value, not the closure of a \
             partial application"));
    curry
  ;;

  (* Of type [ty], else its value's. *)
  let body' ?ty ctx ~res_mode ~what ~region f =
    let expected = Expected.body ctx ~region in
    let (body : Typed.Expr.t) = f expected in
    let ty = Option.value ty ~default:body.ty in
    let loc = body.loc in
    Ctx.add
      ctx
      ~loc
      (Mode.Var.at_least
         res_mode
         Stage
         Portable
         (reason
            ~loc
            "it is the result of a function, computed when it is called"));
    Ctx.add_crossing ~ty ctx ~loc (fun crossing ->
      Mode.Var.at_most
        ~crossing
        expected.mode
        Locality
        Regional
        (reason
           ~loc
           (sprintf
              "%s returns it, ending its region (return it with [exclave_] \
               instead)"
              what)));
    Ctx.add_crossing ~ty ctx ~loc (fun crossing ->
      Mode.Var.submode
        ~crossing
        ~locality:Mode.Locality_map.release
        expected.mode
        res_mode
        (reason ~loc (sprintf "%s returns it" what)));
    body
  ;;

  let body ctx ~res_mode ~ty f =
    body' ctx ~res_mode ~ty ~what:"the function" ~region:(Returns res_mode) f
  ;;
end

let region ctx expected ~loc f =
  let res_mode = Ctx.fresh ctx in
  let (body : Typed.Expr.t) =
    Function.body' ctx ~res_mode ~what:"the [region_]" ~region:(Ends res_mode) f
  in
  joins ctx expected res_mode ~loc ~why:"the [region_] gives it";
  body
;;

let exclave ctx (expected : Expected.t) ~loc f =
  match expected.region with
  | Nontail -> None
  | (Returns _ | Ends _ | Quoted) as region ->
    let inner = Expected.fresh ctx in
    let (result : Typed.Expr.t) = f inner in
    (match region with
     | Returns res_mode | Ends res_mode ->
       Ctx.add_crossing ~ty:result.ty ctx ~loc (fun crossing ->
         Mode.Var.submode
           ~crossing
           inner.mode
           res_mode
           (reason
              ~loc
              (match region with
               | Ends _ -> "the [region_] gives it"
               | Returns _ | Nontail | Quoted -> "the function returns it")))
     | Nontail | Quoted -> ());
    held
      ctx
      expected
      inner.mode
      ~loc
      ~why:"it is returned in the caller's region";
    Some result
;;

let apply
  ctx
  (expected : Expected.t)
  ~loc
  ~curried
  ~arg_mode
  ~res_mode
  ~curry
  ~arg_ty
  ~res_ty
  ~callee
  ~arg
  =
  let tail_reason =
    "it is passed in a tail call, after the current region has ended (mark the \
     call [@nontail])"
  in
  let f_expected =
    if curried
    then Expected.callee ctx expected
    else Expected.fresh ctx
  in
  let (f : Typed.Expr.t), allocation = callee f_expected in
  let allocator = Option.is_some allocation in
  (* An allocator is no call: its region doesn't end before it runs. *)
  let tail_call = Expected.is_tail expected && not allocator in
  if tail_call && not curried
  then
    locality_at_most
      ~report_loc:loc
      ctx
      f_expected.mode
      ~ty:f.ty
      Regional
      ~loc:f.loc
      ~why:tail_reason;
  let x_expected = Expected.fresh ctx in
  let (x : Typed.Expr.t) = arg allocation x_expected in
  Ctx.add_crossing ~ty:arg_ty ctx ~loc (fun crossing ->
    Mode.Var.submode
      ~crossing
      x_expected.mode
      arg_mode
      (reason ~loc "it is passed to a function"));
  if tail_call
  then
    locality_at_most
      ~report_loc:loc
      ctx
      x_expected.mode
      ~ty:arg_ty
      Regional
      ~loc:x.loc
      ~why:tail_reason;
  if expected.applied && Expected.is_tail expected
  then
    Ctx.add
      ctx
      ~loc
      (Mode.Var.submode_on
         res_mode
         curry
         Locality
         (reason
            ~loc
            "a tail call gives it further arguments, after the current region \
             has ended (mark the call [@nontail])"));
  (match expected.region with
   | Returns function_res when (not expected.applied) && not allocator ->
     (* A tail call ends the region before the callee runs, so what the callee
        returns is made in the caller's region, as by [exclave_]. So is the
        closure of a partial application there (a closure's partial applications
        are made in their caller's region). An allocator like [Box.make], or a
        prim given too few arguments, allocates here. *)
     Ctx.add_crossing ~ty:res_ty ctx ~loc (fun crossing ->
       Mode.Var.submode
         ~crossing
         res_mode
         function_res
         (reason ~loc "the function returns it, by a tail call"));
     held ctx expected res_mode ~loc ~why:"the function returns it"
   | Returns _ | Ends _ | Nontail | Quoted ->
     joins ctx expected res_mode ~ty:res_ty ~loc ~why:"the function returns it");
  f, x, allocation
;;

(* How a value bound outside [locks] relates on locality to its use inside them:
   a function body is a region of its own, and [exclave_] leaves it. Assigning
   goes the other way, from the use to the binding. *)
let locality_map ?(assign = false) (locks : Env.Lock.t list) =
  List.fold
    locks
    ~init:Mode.Locality_map.id
    ~f:(fun inner (lock : Env.Lock.t) ->
      let map : Mode.Locality_map.t =
        match lock.kind, assign with
        | (Closure _ | Region), false | Exclave, true ->
          Mode.Locality_map.borrow
        | Exclave, false | (Closure _ | Region), true ->
          Mode.Locality_map.release
        | (Quote _ | Splice), _ -> Mode.Locality_map.id
      in
      if assign
      then Mode.Locality_map.compose inner map
      else Mode.Locality_map.compose map inner)
;;

(* Whether a value used inside [crossed] is used at a later stage than it is
   bound, so is copied into the code. *)
let lifted (crossed : Env.Lock.t list) =
  List.sum (module Int) crossed ~f:(fun (lock : Env.Lock.t) ->
    match lock.kind with
    | Quote _ -> 1
    | Splice -> -1
    | Closure _ | Exclave | Region -> 0)
  > 0
;;

let is_closure (lock : Env.Lock.t) =
  match lock.kind with
  | Closure _ -> true
  | Quote _ | Splice | Exclave | Region -> false
;;

let assign ctx expected ~held ~crossed ~ty ~name ~loc f =
  let value = Expected.fresh ctx in
  let result = f value in
  evaluated ctx expected value.mode ~loc ~why:"it is assigned";
  Option.iter held ~f:(fun held ->
    let held = (held : Binding.t :> Mode.Var.t) in
    (* A closure may be called from a region shorter than the variable's, so may
       only store what outlives every region. *)
    if List.exists crossed ~f:is_closure
    then
      locality_at_most
        ~report_loc:loc
        ctx
        value.mode
        ~ty
        Global
        ~loc
        ~why:
          (sprintf
             "it is assigned to %s from a closure, which may be called from a \
              shorter region"
             name);
    (* What is local to a [region_] is gone once it ends. *)
    List.iteri crossed ~f:(fun index (lock : Env.Lock.t) ->
      match lock.kind with
      | Region ->
        let ended = Ctx.fresh ctx in
        Ctx.add_crossing ~ty ctx ~loc (fun crossing ->
          Mode.Var.submode
            ~crossing
            ~locality:(locality_map ~assign:true (List.take crossed index))
            value.mode
            ended
            (reason ~loc (sprintf "it is assigned to %s" name))
          |> Result.bind ~f:(fun () ->
            Mode.Var.at_most
              ended
              Locality
              Regional
              (reason
                 ~loc
                 (sprintf
                    "%s outlives the [region_], which it is assigned in"
                    name))))
      | Closure _ | Quote _ | Splice | Exclave -> ());
    Ctx.add_crossing ~ty ctx ~loc (fun crossing ->
      Mode.Var.submode
        ~crossing
        ~locality:(locality_map ~assign:true crossed)
        value.mode
        held
        (reason ~loc (sprintf "it is assigned to %s" name))));
  result
;;

let store ctx expected ~loc ~target ~value =
  let ((target : Typed.Expr.t), (modality : Modality.t)), (_ : Mode.Var.t) =
    evaluate ctx expected ~loc ~why:"it is stored to" target
  in
  let sub = Expected.fresh ctx in
  let (value : Typed.Expr.t) = value sub ~ty:target.ty in
  evaluated ctx expected sub.mode ~loc ~why:"it is stored";
  locality_at_most
    ~report_loc:loc
    ctx
    sub.mode
    ~ty:target.ty
    Global
    ~loc:value.loc
    ~why:"it is stored through a pointer, which may outlive any region";
  fits
    ~ty:target.ty
    ctx
    sub.mode
    (Modality.from_last_cell modality)
    ~loc:value.loc
    ~why:"it is stored into a cell, which holds only what fits its modality";
  target, value
;;

let use
  ?(embedding = fun add -> add ())
  ctx
  expected
  (entry : Env.Entry.t)
  ~locks
  ~by_symbol
  ~toplevel
  ~name
  ~loc
  =
  let reason what = reason ~loc (sprintf what name) in
  let held = (entry.held :> Mode.Var.t option) in
  let mode = (entry.mode :> Mode.Var.t) in
  let mutable_ = Option.is_some held in
  let by_symbol = by_symbol || (toplevel && mutable_) in
  let ty = entry.scheme in
  (* What a mutable's own mode is checked with: its cell crosses nothing. *)
  let own_ty = if mutable_ then None else Some ty in
  let crossed = Env.Entry.crossed entry ~locks in
  List.iteri crossed ~f:(fun index lock ->
    match lock.kind with
    | Splice -> ()
    | (Closure _ | Exclave | Region) when toplevel -> ()
    | Exclave ->
      (* As seen where [exclave_] ends the region: what is local to it then is
         gone. *)
      let inside = List.take crossed index in
      let outside = List.drop crossed (index + 1) in
      let ended ?ty value ~why =
        Ctx.add_crossing ?ty ctx ~loc (fun crossing ->
          let ended = Ctx.fresh ctx in
          Mode.Var.submode
            ~crossing
            ~locality:(locality_map outside)
            value
            ended
            (reason why)
          |> Result.bind ~f:(fun () ->
            Mode.Var.at_most
              ended
              Locality
              Regional
              (Mode.Reason.create ~loc "the [exclave_] has ended its region")))
      in
      ended
        ~ty
        (Option.value held ~default:mode)
        ~why:"%s is used in an [exclave_]";
      (* A closure in it capturing a mutable holds its cell, which must then
         outlive the region. *)
      if mutable_ && List.exists inside ~f:is_closure
      then
        ended
          mode
          ~why:
            "%s is captured by a closure in an [exclave_], which holds its cell"
    | Closure closure ->
      Ctx.add_crossing ?ty:own_ty ctx ~loc (fun crossing ->
        Mode.Var.submode
          ~crossing
          mode
          closure
          (reason "%s is captured by a closure"));
      Option.iter held ~f:(fun held ->
        Ctx.add_crossing ~ty ctx ~loc (fun crossing ->
          Mode.Var.submode
            ~crossing
            held
            closure
            (reason "%s holds it, and is captured by a closure")));
      (* A closure capturing itself is as portable as anything else it captures. *)
      if entry.defining && Mode.Var.id closure <> Mode.Var.id mode
      then
        Ctx.add
          ctx
          ~loc
          (Mode.Var.at_least
             closure
             Stage
             Nonportable
             (reason "%s is captured by a closure in its own definition"))
    | Region -> ()
    | Quote _ when by_symbol -> ()
    | Quote code ->
      embedding (fun () ->
        Ctx.add_crossing ?ty:own_ty ctx ~loc (fun crossing ->
          Mode.Var.at_most
            ~crossing
            mode
            Stage
            Portable
            (reason "%s is used in a quote")));
      if not toplevel
      then
        Ctx.add_crossing ?ty:own_ty ctx ~loc (fun crossing ->
          Mode.Var.submode
            ~crossing
            mode
            code
            (reason "%s is embedded in a quote")));
  let locality =
    if toplevel then Mode.Locality_map.id else locality_map crossed
  in
  let why = sprintf "%s is used" name in
  match held with
  | Some held ->
    runs ctx expected Nonportable ~loc ~why:(sprintf "%s is mutable" name);
    joins ctx expected held ~ty ~locality ~loc ~why
  | None when entry.defining ->
    runs
      ctx
      expected
      Nonportable
      ~loc
      ~why:(sprintf "%s is used in its own definition" name);
    joins
      ctx
      expected
      mode
      ~ty
      ~cross:(fun crossing ->
        { crossing with stage = Always; provenance = Always })
      ~locality
      ~loc
      ~why
  | None when by_symbol ->
    runs
      ctx
      expected
      Nonportable
      ~loc
      ~why:(sprintf "%s is read where the code runs" name);
    joins ctx expected mode ~ty ~locality ~loc ~why
  | None when lifted crossed ->
    (* A constant of the code: what it holds is portable, so deeply immutable,
       and copied into the code's static data, which can't refer outside the GC
       heap. *)
    Ctx.add_crossing ?ty:own_ty ctx ~loc (fun crossing ->
      Mode.Var.at_most
        ~crossing
        mode
        Provenance
        Gc
        (reason "%s is lifted into the code's static data"));
    joins
      ctx
      expected
      (constant ctx ~loc ~why:(sprintf "%s is lifted into the code" name))
      ~loc
      ~why
  | None ->
    joins ctx expected mode ~ty ~locality ~loc ~why;
    Option.iter entry.shape ~f:(fun shape ->
      Mode_shape.map shape ~whole:expected.mode ~component:(fun component ->
        let used = Ctx.fresh ctx in
        Ctx.add
          ctx
          ~loc
          (Mode.Var.submode ~locality component used (reason "%s is used"));
        used)
      |> Expected.gives expected)
;;

let use_free ctx expected ~locks ~loc ~why =
  Env.Lock.cancel_splices locks
  |> List.take_while ~f:(fun (lock : Env.Lock.t) ->
    match lock.kind with
    | Closure _ | Exclave | Region -> true
    | Quote _ | Splice -> false)
  |> List.iter ~f:(fun (lock : Env.Lock.t) ->
    match lock.kind with
    | Closure closure ->
      Ctx.add
        ctx
        ~loc
        (Mode.Var.at_least closure Stage Nonportable (reason ~loc why))
    | Quote _ | Splice | Exclave | Region -> ());
  runs ctx expected Nonportable ~loc ~why
;;

module Splice = struct
  let code ~code f = f (Expected.of_mode code)

  let captured = Expected.captured

  let substitution ctx expected ~loc f =
    let (sub : Typed.Expr.t), mode =
      evaluate ctx expected ~loc ~why:"it is substituted into spliced code" f
    in
    sub, Binding.of_mode mode
  ;;
end

let const ctx mode ~loc ~why =
  ctx.Ctx.holds ~loc (Mode.Var.at_most mode Stage Const (reason ~loc why))
;;

module Toplevel = struct
  type t =
    { expected : Expected.t
    ; value : Mode.Var.t
    ; binding : Binding.t
    ; held : Binding.t option
    }

  let create ctx ~is_function ~mut annot ~var ~name ~loc =
    let expected = Expected.fresh ctx in
    let value = if is_function then Ctx.fresh ctx else expected.mode in
    let binding, held =
      if mut
      then (
        let cell, held = Binding.mutable_ ctx ~value annot ~var ~name ~loc in
        (* A toplevel cell outlives every generalization: its readers relate to
           it rather than copying its bounds. *)
        List.iter [ cell; held ] ~f:(fun binding ->
          Mode.Var.lower_rank (binding :> Mode.Var.t) ~rank:0);
        cell, Some held)
      else if is_function
      then Binding.closure value, None
      else Binding.value ctx value annot ~var, None
    in
    { expected; value; binding; held }
  ;;

  let outlives ctx t ~ty ~name ~loc =
    List.iter
      (t.value
       :: (t.binding :> Mode.Var.t)
       :: (Option.to_list t.held :> Mode.Var.t list))
      ~f:(fun mode ->
        locality_at_most
          ~report_loc:loc
          ctx
          mode
          ~ty
          Global
          ~loc
          ~why:(sprintf "%s is a toplevel" name))
  ;;
end

let ptr_make ctx =
  let arg = Ctx.fresh ctx in
  let res = Ctx.fresh ctx in
  let held = Ctx.fresh ctx in
  let reason = Mode.Reason.create "Ptr.make holds it" in
  let made = Mode.Reason.create "Ptr.make allocates it at run time" in
  Result.all_unit
    [ Mode.Var.at_most arg Locality Global reason
    ; Mode.Var.submode_on arg held Provenance reason
    ; Mode.Var.submode_on arg held Stage reason
    ; Mode.Var.at_least res Stage Nonportable made
    ; Mode.Var.at_least res Provenance Gc made
    ]
  |> Result.iter_error ~f:(fun error ->
    raise_s [%message "Ptr.make" (error : Mode.Error.t)]);
  arg, res, Mode_arg.Var held
;;

let box_make ctx =
  let arg = Ctx.fresh ctx in
  let res = Ctx.fresh ctx in
  let reason = Mode.Reason.create "Box.make holds it" in
  let made = Mode.Reason.create "Box.make allocates it at run time" in
  Result.all_unit
    [ Mode.Var.submode arg res reason
    ; Mode.Var.at_least res Stage Portable made
    ; Mode.Var.at_least res Provenance Gc made
    ]
  |> Result.iter_error ~f:(fun error ->
    raise_s [%message "Box.make" (error : Mode.Error.t)]);
  arg, res
;;

let prim ctx ~args =
  let made =
    Mode.Reason.create "a partial application is allocated at run time"
  in
  let held = Mode.Reason.create "a partial application holds it" in
  let last = List.length args - 1 in
  let _, arrows =
    List.fold_mapi args ~init:[] ~f:(fun i taken ty ->
      let arg = Ctx.fresh ctx in
      let taken = (arg, ty) :: taken in
      let res : Arrow_modes.Side.t =
        if i = last
        then Fixed Arrow_modes.default_const
        else (
          let res = Ctx.fresh ctx in
          Result.all_unit
            (Mode.Var.at_least res Stage Portable made
             :: Mode.Var.allocated res made
             :: List.map taken ~f:(fun (arg, ty) ->
               Mode.Var.submode ~crossing:(ctx.crossing ty) arg res held))
          |> Result.iter_error ~f:(fun error ->
            raise_s [%message "prim" (error : Mode.Error.t)]);
          Var res)
      in
      ( taken
      , ({ arg = Var arg; res; curry = Arrow_modes.default_curry }
         : Arrow_modes.t) ))
  in
  arrows
;;

let assume_global ctx =
  let arg = Ctx.fresh ctx in
  let res = Ctx.fresh ctx in
  let reason = Mode.Reason.create "Unsafe.assume_global keeps it" in
  Result.all_unit
    [ Mode.Var.submode_on arg res Stage reason
    ; Mode.Var.submode_on arg res Provenance reason
    ; Mode.Var.at_least
        arg
        Locality
        Local
        (Mode.Reason.create "Unsafe.assume_global takes it")
    ; Mode.Var.at_most
        res
        Locality
        Global
        (Mode.Reason.create "Unsafe.assume_global assumes it")
    ]
  |> Result.iter_error ~f:(fun error ->
    raise_s [%message "Unsafe.assume_global" (error : Mode.Error.t)]);
  arg, res
;;

let default_mode_arg var (position : Mode_arg_position.t option) ~name =
  if not (Mode.Var.is_generic var)
  then (
    let fix (type a) (axis : a Mode.Axis.t) ~(default : a) reason =
      let module C = (val Mode.Axis.chain axis) in
      let lower = Mode.Const.get (Mode.Var.lower var) axis
      and upper = Mode.Const.get (Mode.Var.upper var) axis in
      let held =
        match position with
        | Some Mode_arg_position.Covariant -> lower
        | Some Contravariant -> upper
        | Some Held | None ->
          if C.le default upper then C.join lower default else lower
      in
      let%bind.Result () = Mode.Var.at_least var axis held reason in
      Mode.Var.at_most var axis held reason
    in
    let default = Mode_arg.Const.default in
    let reason =
      Mode.Reason.create
        (match position with
         | Some Held | None ->
           sprintf
             "%s is not generalized, so what its cells hold is fixed by its \
              definition (nonportable gc by default)"
             name
         | Some (Covariant | Contravariant) ->
           sprintf
             "%s is not generalized, so its type's mode arguments are fixed by \
              its definition"
             name)
    in
    (let%bind.Result () = fix Stage ~default:default.stage reason in
     fix Provenance ~default:default.provenance reason)
    |> Mode.Error.ok_exn)
;;

let placement (site : Site.t) : Typed.Allocation.Placement.t =
  match site.position, (Mode.Var.upper site.mode).locality with
  | (Here | Returned), Local -> Region
  | Returned, Regional | (Exclaved | Caller), Local -> Caller_region
  | (Here | Exclaved | Caller), Regional | _, Global -> Heap
;;
