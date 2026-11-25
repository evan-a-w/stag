open! Import

module Layout_of = struct
  type t =
    | Pointer_free
    | Has_pointers
    | Depends of Mode.Layout.t list
end

module Unbound = struct
  type t =
    { id : Type.id
    ; rank : unit -> Type.rank option
    ; layout : Mode.Layout.t
    }
end

module Frame = struct
  type t =
    { first_id : Type.id
    ; mutable unbound : Unbound.t list
    ; mutable waiting : (final:bool -> bool) list
    }

  let create ~first_id = { first_id; unbound = []; waiting = [] }
end

type t =
  { layout_of : Type.ty -> Layout_of.t
  ; ids : Mode.Id_source.t
  ; frames : Frame.t Stack.t
  (** Innermost first, the outermost of all variables. *)
  }

let create ~layout_of =
  let frames = Stack.create () in
  Stack.push frames (Frame.create ~first_id:Int.min_value);
  { layout_of; ids = Mode.Id_source.create (); frames }
;;

let layout_of t ty = t.layout_of ty

let has_pointers ty =
  Mode.Reason.create
    (sprintf
       "its type turned out to be %s, which has pointers"
       (Type.Printer.ty (Type.Printer.create ()) ty))
;;

let decide t layout ty =
  match t.layout_of ty with
  | Pointer_free ->
    Mode.Layout.pointer_free layout;
    Ok ()
  | Has_pointers -> Mode.Layout.raise layout ~because:[ has_pointers ty ]
  | Depends lowers ->
    List.fold_result lowers ~init:() ~f:(fun () lower ->
      Mode.Layout.add_below ~lower layout)
;;

let present = Mode.Reason.create "a field of the row turned out to be present"

let generalize layout =
  match Mode.Layout.bound layout with
  | Some _ -> Ok ()
  | None -> Mode.Layout.raise layout ~because:[]
;;

let find_or_add t (var : (_, _) Type.Var.t) ~bind =
  match Type.Var.layout var with
  | Some layout -> layout
  | None ->
    let layout = Mode.Layout.create t.ids in
    let watcher =
      Watcher.create
        Type.Var.layout_key
        layout
        ~bind:(fun layout target ->
          bind layout target
          |> Result.map_error ~f:(fun error -> Type.Bound_var error))
        ~instantiate:(fun layout ~at ->
          (* An instance's own uses decide the rest. *)
          Option.map (Mode.Layout.bound layout) ~f:(fun reason ->
            let copy = Mode.Layout.create t.ids in
            Mode.Error.ok_exn (Mode.Layout.bound_by ?instance:at copy reason);
            copy))
        ~generalize:(fun layout ->
          generalize layout
          |> Result.map_error ~f:(fun error -> Type.Bound_var error))
    in
    var.watchers <- watcher :: var.watchers;
    (match var.state with
     | Unbound _ ->
       let rank () =
         match var.state with
         | Unbound rank -> Some rank
         | Generic | Link _ -> None
       in
       (* The frame it was made in, whichever is innermost now. *)
       let frame =
         Stack.find t.frames ~f:(fun frame -> frame.first_id <= var.id)
         |> Option.value_exn
       in
       frame.unbound <- { Unbound.id = var.id; rank; layout } :: frame.unbound
     | Generic | Link _ -> ());
    layout
;;

let of_ty t var = find_or_add t var ~bind:(fun layout ty -> decide t layout ty)

let of_row t var =
  find_or_add t var ~bind:(fun layout row ->
    decide t layout (Type.TRecord { id = -1; row }))
;;

let rec of_presence t var =
  find_or_add t var ~bind:(fun layout (presence : Type.presence) ->
    match Type.canonical_presence presence with
    | Present -> Mode.Layout.raise layout ~because:[ present ]
    | Absent ->
      Mode.Layout.pointer_free layout;
      Ok ()
    | PVar var -> Mode.Layout.add_below ~lower:(of_presence t var) layout)
;;

let above t lowers =
  let layout = Mode.Layout.create t.ids in
  List.iter lowers ~f:(fun lower ->
    Mode.Error.ok_exn (Mode.Layout.add_below ~lower layout));
  layout
;;

let both t layout lowers =
  Mode.Layout.both
    t.ids
    layout
    (match lowers with
     | [ lower ] -> lower
     | lowers -> above t lowers)
;;

let within t ~first_id f =
  let frame = Frame.create ~first_id in
  Stack.push t.frames frame;
  let pop () =
    let popped = Stack.pop_exn t.frames in
    assert (phys_equal popped frame)
  in
  match f frame with
  | result ->
    pop ();
    let parent = Stack.top_exn t.frames in
    parent.unbound <- frame.unbound @ parent.unbound;
    parent.waiting <- frame.waiting @ parent.waiting;
    result
  | exception exn ->
    (* Abandoned: what was checked in it is checked again. *)
    pop ();
    raise exn
;;

let check_waiting t =
  Stack.iter t.frames ~f:(fun (frame : Frame.t) ->
    frame.waiting
    <- List.filter (List.rev frame.waiting) ~f:(fun check ->
         not (check ~final:false))
       |> List.rev)
;;

let wait t check =
  let frame = Stack.top_exn t.frames in
  frame.waiting <- check :: frame.waiting
;;

let decide ?(except = fun _ -> false) ?(final = false) (frame : Frame.t) ~rank =
  let decided, kept =
    List.partition_map frame.unbound ~f:(fun (entry : Unbound.t) ->
      match entry.rank () with
      | None -> First None
      | Some rank' when rank' > rank && not (except entry.id) ->
        First (Some entry.layout)
      | Some _ -> Second entry)
  in
  frame.unbound <- kept;
  let errors =
    List.filter_map decided ~f:(fun layout ->
      Option.bind layout ~f:(fun layout -> Result.error (generalize layout)))
  in
  frame.waiting
  <- List.filter (List.rev frame.waiting) ~f:(fun check -> not (check ~final))
     |> List.rev;
  errors
;;
