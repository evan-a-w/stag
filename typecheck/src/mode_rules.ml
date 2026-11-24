open! Import

module Ctx = struct
  type t =
    { modes : Mode.Id_source.t
    ; rank : int
    ; holds : loc:Loc.t -> (unit, Mode.Error.t) Result.t -> bool
    ; crossing : Type.ty -> Mode.Crossing.t
    }

  let create ~modes ~rank ~holds ~crossing = { modes; rank; holds; crossing }
  let fresh t = Mode.Var.create t.modes ~rank:t.rank
  let add t ~loc result = ignore (t.holds ~loc result : bool)
end

let reason ~loc why = Mode.Reason.create ~loc why

module Tail = struct
  type t =
    | Nontail
    | Returns of Mode.Var.t (** The function's result mode. *)
    | Quoted (** Where the code is spliced decides. *)
end

module Expected = struct
  type t =
    { mode : Mode.Var.t
    ; region : Tail.t
    ; stack : bool ref option
    ; applied : bool
    (** A function a tail call gives further arguments: the result of an
        application. *)
    }

  let of_mode mode = { mode; region = Nontail; stack = None; applied = false }
  let fresh ctx = of_mode (Ctx.fresh ctx)
  let quoted ctx = { (fresh ctx) with region = Quoted }

  let is_tail t =
    match t.region with
    | Returns _ -> true
    | Nontail | Quoted -> false
  ;;

  let whole t = { t with stack = None; applied = false }
  let nontail t = { t with region = Nontail; applied = false }
  let part t = { t with region = Nontail; applied = false }

  (* The same value, at a mode of its own, which is then related to [t]'s. *)
  let same_value ctx t = { (fresh ctx) with region = t.region; stack = t.stack }
end

let joins ?crossing ?locality ctx (expected : Expected.t) mode ~loc ~why =
  Ctx.add
    ctx
    ~loc
    (Mode.Var.submode ?crossing ?locality mode expected.mode (reason ~loc why))
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
    ~crossing:{ Mode.Crossing.none with provenance = true }
    ~locality:Mode.Locality_map.ignore
    ~loc
    ~why
;;

let evaluate ?inner ctx expected ~loc ~why f =
  let sub = Expected.fresh (Option.value inner ~default:ctx) in
  let result = f sub in
  evaluated ctx expected sub.mode ~loc ~why;
  result, sub.mode
;;

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

let locality_at_most ctx mode ~ty locality ~loc ~why =
  Mode.Var.at_most
    ~crossing:(ctx.Ctx.crossing ty)
    mode
    Locality
    locality
    (reason ~loc why)
;;

let arrow_mode = Ctx.fresh

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
           (var name)
           (reason ~loc (sprintf "it is annotated '%s" name)))
    | Atoms atoms ->
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
               if exact then Mode.Var.at_least mode axis point reason else Ok ())
            )
        in
        match atom with
        | Const -> fix Stage Const
        | Portable -> fix Stage Portable
        | Nonportable -> fix Stage Nonportable
        | Global -> fix Locality Global
        | Regional -> fix Locality Regional
        | Local -> fix Locality Local
        | Static -> fix Provenance Static
        | Gc -> fix Provenance Gc
        | External -> fix Provenance External))
;;

let annotated ctx annot ~var =
  let mode = Ctx.fresh ctx in
  annotate ~exact:true ctx mode annot ~var;
  mode
;;

let annotated_use ctx expected ~annot ~var ~loc f =
  match annot with
  | None -> f expected
  | Some _ ->
    let used = Expected.same_value ctx expected in
    let result = f used in
    Result.iter result ~f:(fun (used_expr : Typed.Expr.t) ->
      joins
        ctx
        expected
        used.mode
        ~crossing:(ctx.crossing used_expr.ty)
        ~loc
        ~why:"it is used";
      annotate ctx used.mode annot ~var);
    result
;;

let part ?ty ctx expected (modality : Modality.t) f =
  if not modality.global
  then f (Expected.part expected)
  else (
    (* What a [global_] field holds is not allocated with the block. *)
    let sub = Expected.fresh ctx in
    let (part : Typed.Expr.t) = f sub in
    Ctx.add
      ctx
      ~loc:part.loc
      (locality_at_most
         ctx
         sub.mode
         ~ty:(Option.value ty ~default:part.ty)
         Global
         ~loc:part.loc
         ~why:"it is held by a [global_] field");
    held ctx expected sub.mode ~loc:part.loc ~why:"it is held";
    part)
;;

(* What may be stored into the cell of a [ptr[t @@ external]] is [external],
   whatever the provenance of the pointer read through: as with deep provenance,
   a pointer is then taken to be at least what its cell holds. *)
let holds_external ctx block ~loc =
  Ctx.add
    ctx
    ~loc
    (Mode.Var.at_least
       block
       Provenance
       External
       (reason ~loc "its cell may be given external values"))
;;

let project ctx expected ~loc ~why f =
  let block = Expected.fresh ctx in
  let result, (modality : Modality.t) = f block in
  if modality.external_ then holds_external ctx block.mode ~loc;
  if modality.global
  then held ctx expected block.mode ~loc ~why
  else joins ctx expected block.mode ~loc ~why;
  result
;;

let initializes ?ty ctx ~value ~held ~name ~loc =
  Ctx.add
    ctx
    ~loc
    (Mode.Var.submode
       ?crossing:(Option.map ty ~f:ctx.Ctx.crossing)
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
      Ctx.add ctx ~loc (Mode.Var.submode mode bound reason);
      annotate ~exact:true ctx bound annot ~var;
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

  let project ctx ~block (modality : Modality.t) ~loc =
    let mode =
      if not modality.global
      then block
      else (
        let global = Ctx.fresh ctx in
        let reason = reason ~loc "it is held by a [global_] field" in
        Ctx.add
          ctx
          ~loc
          (Mode.Var.submode
             ~locality:Mode.Locality_map.ignore
             block
             global
             reason
           |> Result.bind ~f:(fun () ->
             Mode.Var.at_most global Locality Global reason));
        global)
    in
    if not modality.external_
    then Binding.of_mode mode
    else (
      let held = Ctx.fresh ctx in
      Ctx.add
        ctx
        ~loc
        (Mode.Var.submode
           mode
           held
           (reason ~loc "it is held by a [ptr[_ @@ external]]"));
      holds_external ctx held ~loc;
      Binding.of_mode held)
  ;;

  let embedded ctx ~name ~loc =
    let mode =
      stage_at_least
        ctx
        Portable
        ~loc
        ~why:(sprintf "%s was computed at compile time" name)
    in
    let reason = reason ~loc (sprintf "%s was computed at compile time" name) in
    Ctx.add
      ctx
      ~loc
      (Mode.Var.at_most mode Locality Global reason
       |> Result.bind ~f:(fun () ->
         Mode.Var.at_most mode Provenance Static reason));
    of_mode mode
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

  let closure ctx expected ~loc f =
    site expected;
    let why = "the closure is made" in
    let closure = allocated ctx ~loc ~why in
    let result = f closure in
    joins ctx expected closure ~loc ~why;
    result
  ;;

  let code ctx expected ~loc =
    site expected;
    let why = "the code is built" in
    let code = allocated ctx ~loc ~why in
    joins ctx expected code ~loc ~why;
    code
  ;;

  let pointer = site

  let stack ctx expected ~loc f =
    let allocates = ref false in
    let inner =
      { (Expected.same_value ctx expected) with stack = Some allocates }
    in
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
        ~crossing:(ctx.crossing result.ty)
        ~loc
        ~why:"it is part of the value";
      Ok result)
    else Error result
  ;;
end

module Function = struct
  let partial_applications ctx ~closure ~arg_modes ~partial_modes ~loc =
    let reason = reason ~loc "a partial application holds it" in
    List.iteri partial_modes ~f:(fun index partial ->
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
        then Ctx.add ctx ~loc (Mode.Var.submode arg partial reason)))
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

  let body ctx ~res_mode ~ty f =
    let expected = { (Expected.fresh ctx) with region = Returns res_mode } in
    let (body : Typed.Expr.t) = f expected in
    let crossing = ctx.Ctx.crossing ty in
    let loc = body.loc in
    Ctx.add
      ctx
      ~loc
      (Mode.Var.at_most
         ~crossing
         expected.mode
         Locality
         Regional
         (reason
            ~loc
            "the function returns it, ending its region (return it with \
             [exclave_] instead)"));
    Ctx.add
      ctx
      ~loc
      (Mode.Var.submode
         ~crossing
         ~locality:Mode.Locality_map.release
         expected.mode
         res_mode
         (reason ~loc "the function returns it"));
    body
  ;;
end

let exclave ctx (expected : Expected.t) ~loc f =
  match expected.region with
  | Nontail -> None
  | (Returns _ | Quoted) as region ->
    let inner = Expected.fresh ctx in
    let (result : Typed.Expr.t) = f inner in
    (match region with
     | Returns res_mode ->
       Ctx.add
         ctx
         ~loc
         (Mode.Var.submode
            ~crossing:(ctx.crossing result.ty)
            inner.mode
            res_mode
            (reason ~loc "the function returns it"))
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
    then { (Expected.fresh ctx) with region = expected.region; applied = true }
    else Expected.fresh ctx
  in
  let (f : Typed.Expr.t) = callee f_expected in
  if Expected.is_tail expected && not curried
  then
    Ctx.add
      ctx
      ~loc
      (locality_at_most
         ctx
         f_expected.mode
         ~ty:f.ty
         Regional
         ~loc:f.loc
         ~why:tail_reason);
  let x_expected = Expected.fresh ctx in
  let (x : Typed.Expr.t) = arg x_expected in
  Ctx.add
    ctx
    ~loc
    (Mode.Var.submode
       ~crossing:(ctx.crossing arg_ty)
       x_expected.mode
       arg_mode
       (reason ~loc "it is passed to a function"));
  if Expected.is_tail expected
  then
    Ctx.add
      ctx
      ~loc
      (locality_at_most
         ctx
         x_expected.mode
         ~ty:arg_ty
         Regional
         ~loc:x.loc
         ~why:tail_reason);
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
  joins
    ctx
    expected
    res_mode
    ~crossing:(ctx.crossing res_ty)
    ~loc
    ~why:"the function returns it";
  f, x
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
        | Closure _, false | Exclave, true -> Mode.Locality_map.borrow
        | Exclave, false | Closure _, true -> Mode.Locality_map.release
        | (Quote _ | Splice), _ -> Mode.Locality_map.id
      in
      if assign
      then Mode.Locality_map.compose inner map
      else Mode.Locality_map.compose map inner)
;;

let is_closure (lock : Env.Lock.t) =
  match lock.kind with
  | Closure _ -> true
  | Quote _ | Splice | Exclave -> false
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
      Ctx.add
        ctx
        ~loc
        (locality_at_most
           ctx
           value.mode
           ~ty
           Global
           ~loc
           ~why:
             (sprintf
                "it is assigned to %s from a closure, which may be called from \
                 a shorter region"
                name));
    Ctx.add
      ctx
      ~loc
      (Mode.Var.submode
         ~crossing:(ctx.crossing ty)
         ~locality:(locality_map ~assign:true crossed)
         value.mode
         held
         (reason ~loc (sprintf "it is assigned to %s" name))));
  result
;;

let store ctx expected ~loc ~target ~value =
  let ((target : Typed.Expr.t), (pointee : Type.Pointee.t)), (_ : Mode.Var.t) =
    evaluate ctx expected ~loc ~why:"it is stored to" target
  in
  let sub = Expected.fresh ctx in
  let (value : Typed.Expr.t) = value sub ~ty:target.ty in
  evaluated ctx expected sub.mode ~loc ~why:"it is stored";
  Ctx.add
    ctx
    ~loc
    (locality_at_most
       ctx
       sub.mode
       ~ty:target.ty
       Global
       ~loc:value.loc
       ~why:"it is stored through a pointer, which may outlive any region");
  (match pointee with
   | External -> ()
   | Gc ->
     Ctx.add
       ctx
       ~loc
       (Mode.Var.at_most
          ~crossing:(ctx.crossing target.ty)
          sub.mode
          Provenance
          Gc
          (reason
             ~loc:value.loc
             "it is stored through a [ptr[t]], which holds only gc values (a \
              [ptr[t @@ external]] may hold external ones)")));
  target, value
;;

let use
  ctx
  expected
  (entry : Env.Entry.t)
  ~locks
  ~toplevel
  ~name
  ~loc
  ~mutable_captured_in_exclave
  =
  let reason what = reason ~loc (sprintf what name) in
  let held = (entry.held :> Mode.Var.t option) in
  let mode = (entry.mode :> Mode.Var.t) in
  let mutable_ = Option.is_some held in
  let crossing = ctx.Ctx.crossing entry.scheme in
  let crossed = Env.Entry.crossed entry ~locks in
  List.iteri crossed ~f:(fun index lock ->
    match lock.kind with
    | Splice -> ()
    | (Closure _ | Exclave) when toplevel -> ()
    | Exclave ->
      (* As seen where [exclave_] ends the region: what is local to it then is
         gone. *)
      let inside = List.take crossed index in
      let outside = List.drop crossed (index + 1) in
      let ended = Ctx.fresh ctx in
      Ctx.add
        ctx
        ~loc
        (Mode.Var.submode
           ~crossing
           ~locality:(locality_map outside)
           (Option.value held ~default:mode)
           ended
           (reason "%s is used in an [exclave_]")
         |> Result.bind ~f:(fun () ->
           Mode.Var.at_most
             ended
             Locality
             Regional
             (Mode.Reason.create ~loc "the [exclave_] has ended its region")));
      (* The cell of a mutable captured by a closure only lives as long as its
         region, when the closure is local. *)
      if mutable_
         && List.exists inside ~f:is_closure
         && not (List.exists outside ~f:is_closure)
      then mutable_captured_in_exclave ()
    | Closure closure ->
      Ctx.add
        ctx
        ~loc
        (Mode.Var.submode
           ?crossing:(if mutable_ then None else Some crossing)
           mode
           closure
           (reason "%s is captured by a closure"));
      Option.iter held ~f:(fun held ->
        Ctx.add
          ctx
          ~loc
          (Mode.Var.submode
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
    | Quote _ when toplevel && mutable_ -> ()
    | Quote code ->
      Ctx.add
        ctx
        ~loc
        (Mode.Var.at_most
           ?crossing:(if mutable_ then None else Some crossing)
           mode
           Stage
           Portable
           (reason "%s is used in a quote"));
      if not toplevel
      then
        Ctx.add
          ctx
          ~loc
          (Mode.Var.submode
             ?crossing:(if mutable_ then None else Some crossing)
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
    joins ctx expected held ~crossing ~locality ~loc ~why
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
      ~crossing:{ crossing with stage = true; provenance = true }
      ~locality
      ~loc
      ~why
  | None -> joins ctx expected mode ~crossing ~locality ~loc ~why
;;

let use_free ctx expected ~locks ~loc ~why =
  Env.Lock.cancel_splices locks
  |> List.take_while ~f:(fun (lock : Env.Lock.t) ->
    match lock.kind with
    | Closure _ | Exclave -> true
    | Quote _ | Splice -> false)
  |> List.iter ~f:(fun (lock : Env.Lock.t) ->
    match lock.kind with
    | Closure closure ->
      Ctx.add
        ctx
        ~loc
        (Mode.Var.at_least closure Stage Nonportable (reason ~loc why))
    | Quote _ | Splice | Exclave -> ());
  runs ctx expected Nonportable ~loc ~why
;;

module Splice = struct
  let code ~code f = f (Expected.of_mode code)

  let captured (expected : Expected.t) =
    { expected with region = Nontail; stack = None }
  ;;

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
        Ctx.add
          ctx
          ~loc
          (locality_at_most
             ctx
             mode
             ~ty
             Global
             ~loc
             ~why:(sprintf "%s is a toplevel" name)))
  ;;
end

let ptr_make ctx =
  let arg = Ctx.fresh ctx in
  let res = Ctx.fresh ctx in
  let reason = Mode.Reason.create "Ptr.make holds it" in
  let made = Mode.Reason.create "Ptr.make allocates it at run time" in
  Result.all_unit
    [ Mode.Var.at_most arg Locality Global reason
    ; Mode.Var.submode_on arg res Provenance reason
    ; Mode.Var.at_least res Stage Nonportable made
    ; Mode.Var.at_least res Provenance Gc made
    ]
  |> Result.iter_error ~f:(fun error ->
    raise_s [%message "Ptr.make" (error : Mode.Error.t)]);
  arg, res
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
