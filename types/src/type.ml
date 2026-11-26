open! Import

module Id = struct
  type t = int [@@deriving sexp, compare, equal]
end

module Rank = struct
  type t = int [@@deriving sexp, compare, equal]
end

type 'a var_state =
  | Unbound of Rank.t
  | Generic
  | Link of 'a

type ('a, 'bound) var =
  { id : Id.t
  ; mutable state : 'a var_state
  ; mutable bounds : 'bound list
  ; mutable in_bounds_of : bounded list
  ; mutable kept : kept list
  ; mutable in_record : bool
  ; mutable watchers : ('a, unification_error) Watcher.t list
  }

and ty =
  | TCon of string * arg list
  | TArrow of ty * ty * Arrow_modes.t
  | TTuple of ty list
  | TCode of
      { inner : ty
      ; context : row
      }
  | TRecord of
      { id : Id.t
      ; row : row
      }
  | TVariant of
      { id : Id.t
      ; row : row
      }
  | TVar of (ty, Nothing.t) var

and row =
  | REmpty
  | RExtend of string * presence * presence * ty * row
  | RVar of (row, bound) var

and presence =
  | Present
  | Absent
  | PVar of (presence, presence_bound) var

and presence_bound =
  | At_most of presence
  | At_least of presence
  | If_present of ty * ty

and arg =
  | ATy of ty
  | ARow of row
  | AMode of Mode_arg.t

and bound =
  { row : row
  ; shadowed : String.Set.t
  }

and bounded =
  | Row_var of (row, bound) var
  | Presence_var of (presence, presence_bound) var

and kept =
  | Row_bound of
      { id : Id.t
      ; lower : (row, bound) var
      ; bound : bound
      }
  | If_present_kept of
      { id : Id.t
      ; presence : (presence, presence_bound) var
      ; ty1 : ty
      ; ty2 : ty
      }

and unification_error =
  | Type_mismatch of ty * ty * type_mismatch_reason
  | Row_mismatch of row * row * row_mismatch_reason
  | Occurs_check_failed of arg
  | Assigned_in_record of string * row
  | Bound_var of Mode.Error.t

and type_mismatch_reason =
  | Base_types_not_equal of string * string
  | Args_length_mismatch of int * int
  | Mismatch_arg_kind of arg * arg
  | Structural_mismatch
  | Modes of Mode.Error.t
  | Unknown_variances of string * Mode.Error.t

and row_mismatch_reason =
  | Missing_label of string
  | Presence_mismatch of string
  | Assigned of string
  | Cyclic_row

let kept_id = function
  | Row_bound { id; _ } | If_present_kept { id; _ } -> id
;;

module Var = struct
  type 'a state = 'a var_state =
    | Unbound of Rank.t
    | Generic
    | Link of 'a

  type ('a, 'bound) t = ('a, 'bound) var =
    { id : Id.t
    ; mutable state : 'a state
    ; mutable bounds : 'bound list
    ; mutable in_bounds_of : bounded list
    ; mutable kept : kept list
    ; mutable in_record : bool
    ; mutable watchers : ('a, unification_error) Watcher.t list
    }

  let create id state =
    { id
    ; state
    ; bounds = []
    ; in_bounds_of = []
    ; kept = []
    ; in_record = false
    ; watchers = []
    }
  ;;

  let change ~undo f = Mode.Private.Undo.change ~undo f

  let set_state t state =
    let old = t.state in
    change ~undo:(fun () -> t.state <- old) (fun () -> t.state <- state)
  ;;

  let lower_rank t ~rank =
    match t.state with
    | Unbound rank' when rank' > rank -> set_state t (Unbound rank)
    | Unbound _ | Generic | Link _ -> ()
  ;;

  let set_bounds t bounds =
    let old = t.bounds in
    change ~undo:(fun () -> t.bounds <- old) (fun () -> t.bounds <- bounds)
  ;;

  let add_bound t bound = set_bounds t (bound :: t.bounds)

  let set_in_bounds_of t in_bounds_of =
    let old = t.in_bounds_of in
    change
      ~undo:(fun () -> t.in_bounds_of <- old)
      (fun () -> t.in_bounds_of <- in_bounds_of)
  ;;

  let set_kept t kept =
    let old = t.kept in
    change ~undo:(fun () -> t.kept <- old) (fun () -> t.kept <- kept)
  ;;

  let mark_in_record t =
    if not t.in_record
    then
      change
        ~undo:(fun () -> t.in_record <- false)
        (fun () -> t.in_record <- true)
  ;;

  let set_watchers t watchers =
    let old = t.watchers in
    change
      ~undo:(fun () -> t.watchers <- old)
      (fun () -> t.watchers <- watchers)
  ;;

  let layout_key = Type_equal.Id.create ~name:"layout" sexp_of_opaque
  let layout t = Watcher.find t.watchers layout_key

  let rank_exn t =
    match t.state with
    | Unbound rank -> rank
    | Generic | Link _ ->
      raise_s [%message "Type.Var.rank_exn: variable not unbound" (t.id : Id.t)]
  ;;

  let sexp_of_t sexp_of_a _ t =
    match t.state with
    | Unbound rank -> [%sexp "Unbound", (t.id : Id.t), (rank : Rank.t)]
    | Generic -> [%sexp "Generic", (t.id : Id.t)]
    | Link a -> [%sexp "Link", (a : a)]
  ;;
end

let int = TCon ("int", [])
let bool = TCon ("bool", [])
let string = TCon ("string", [])
let char = TCon ("char", [])
let unit = TCon ("unit", [])
let exn_name = "exn"
let exn = TCon (exn_name, [])
let error_name = "<error>"
let error = TCon (error_name, [])
let arrow arg res = TArrow (arg, res, Arrow_modes.default)

(* Path compression, for a [Link] not already to the canonical value. Its new
   [Link] is the only allocation of the [canonical_*] functions, and rare once
   paths are compressed, so it is not checked. *)
let[@cold] [@zero_alloc assume] relink (var : (_, _) Var.t) target =
  Var.set_state var (Link target)
;;

let[@zero_alloc] rec canonical_ty ty =
  match ty with
  | TVar ({ state = Link target; _ } as var) ->
    let canonical = canonical_ty target in
    if not (phys_equal canonical target) then relink var canonical;
    canonical
  | TVar _ | TCon _ | TArrow _ | TTuple _ | TCode _ | TRecord _ | TVariant _ ->
    ty
;;

let[@zero_alloc] rec canonical_row row =
  match row with
  | RVar ({ state = Link target; _ } as var) ->
    let canonical = canonical_row target in
    if not (phys_equal canonical target) then relink var canonical;
    canonical
  | RVar _ | REmpty | RExtend _ -> row
;;

let[@zero_alloc] rec canonical_presence presence =
  match presence with
  | PVar ({ state = Link target; _ } as var) ->
    let canonical = canonical_presence target in
    if not (phys_equal canonical target) then relink var canonical;
    canonical
  | PVar _ | Present | Absent -> presence
;;

let is_error ty =
  match canonical_ty ty with
  | TCon (name, []) -> String.equal name error_name
  | _ -> false
;;

let ptr_name = "ptr"

let ptr ?(held = Mode_arg.gc) pointee =
  TCon (ptr_name, [ ATy pointee; AMode held ])
;;

let of_ptr ty =
  match canonical_ty ty with
  | TCon (name, [ ATy pointee; AMode held ]) when String.equal name ptr_name ->
    Some (pointee, held)
  | _ -> None
;;

let box_name = Prelude_path.canonical "box"

let is_pointer_name name =
  String.equal name ptr_name || String.equal name box_name
;;

let box contents = TCon (box_name, [ ATy contents ])

let of_box ty =
  match canonical_ty ty with
  | TCon (name, [ ATy contents ]) when String.equal name box_name ->
    Some contents
  | _ -> None
;;

let rec tail_var row =
  match row with
  | REmpty -> None
  | RExtend (_, _, _, _, row') | RVar { state = Link row'; _ } -> tail_var row'
  | RVar var -> Some var
;;

let presence_bound_var = function
  | At_most presence | At_least presence ->
    (match canonical_presence presence with
     | PVar var -> Some var
     | Present | Absent -> None)
  | If_present _ -> None
;;

module Link_pairs = struct
  type 'a t = 'a list Int.Table.t option ref

  let create () = ref None

  let seen t id other =
    let table =
      match !t with
      | Some table -> table
      | None ->
        let table = Int.Table.create () in
        t := Some table;
        table
    in
    if List.exists (Hashtbl.find_multi table id) ~f:(phys_equal other)
    then true
    else (
      Hashtbl.add_multi table ~key:id ~data:other;
      false)
  ;;

  let ty t ty1 ty2 =
    match ty1, ty2 with
    | TVar { id; state = Link _; _ }, other
    | other, TVar { id; state = Link _; _ } -> seen t id other
    | _ -> false
  ;;

  let row t row1 row2 =
    match row1, row2 with
    | RVar { id; state = Link _; _ }, other
    | other, RVar { id; state = Link _; _ } -> seen t id other
    | _ -> false
  ;;
end

let rec same_ty' ((tys, _) as pairs) ty1 ty2 =
  phys_equal ty1 ty2
  ||
  let left = canonical_ty ty1 in
  let right = canonical_ty ty2 in
  phys_equal left right
  || (match left, right with
      | TRecord _, TRecord _ | TVariant _, TVariant _ -> false
      | _ -> Link_pairs.ty tys ty1 ty2)
  ||
  match left, right with
  | TVar var1, TVar var2 -> var1.id = var2.id
  | TCon (name1, args1), TCon (name2, args2) ->
    String.equal name1 name2 && List.equal (same_arg pairs) args1 args2
  | TArrow (a1, b1, m1), TArrow (a2, b2, m2) ->
    Arrow_modes.same m1 m2 && same_ty' pairs a1 a2 && same_ty' pairs b1 b2
  | TTuple tys1, TTuple tys2 -> List.equal (same_ty' pairs) tys1 tys2
  | TCode c1, TCode c2 ->
    same_ty' pairs c1.inner c2.inner && same_row' pairs c1.context c2.context
  | TRecord { id = id1; _ }, TRecord { id = id2; _ }
  | TVariant { id = id1; _ }, TVariant { id = id2; _ } -> id1 = id2
  | ( (TVar _ | TCon _ | TArrow _ | TTuple _ | TCode _ | TRecord _ | TVariant _)
    , _ ) -> false

and same_row' ((_, rows) as pairs) row1 row2 =
  phys_equal row1 row2
  || Link_pairs.row rows row1 row2
  ||
  match canonical_row row1, canonical_row row2 with
  | REmpty, REmpty -> true
  | RVar var1, RVar var2 -> var1.id = var2.id
  | RExtend (name1, p1, a1, ty1, rest1), RExtend (name2, p2, a2, ty2, rest2) ->
    String.equal name1 name2
    && same_presence p1 p2
    && same_presence a1 a2
    && same_ty' pairs ty1 ty2
    && same_row' pairs rest1 rest2
  | (REmpty | RVar _ | RExtend _), _ -> false

and same_presence p1 p2 =
  match canonical_presence p1, canonical_presence p2 with
  | Present, Present | Absent, Absent -> true
  | PVar var1, PVar var2 -> var1.id = var2.id
  | (Present | Absent | PVar _), _ -> false

and same_arg pairs arg1 arg2 =
  match arg1, arg2 with
  | ATy ty1, ATy ty2 -> same_ty' pairs ty1 ty2
  | ARow row1, ARow row2 -> same_row' pairs row1 row2
  | AMode mode1, AMode mode2 -> Mode_arg.same mode1 mode2
  | (ATy _ | ARow _ | AMode _), _ -> false
;;

let comparison () = Link_pairs.create (), Link_pairs.create ()
let same_ty ty1 ty2 = same_ty' (comparison ()) ty1 ty2
let same_row row1 row2 = same_row' (comparison ()) row1 row2

let same_bound ?(same_entry_ty = fun (_ : presence) -> same_ty) bound1 bound2 =
  let rec same_rows row1 row2 =
    match canonical_row row1, canonical_row row2 with
    | RExtend (name1, p1, a1, ty1, rest1), RExtend (name2, p2, a2, ty2, rest2)
      ->
      String.equal name1 name2
      && same_presence p1 p2
      && same_presence a1 a2
      && (same_ty ty1 ty2 || same_entry_ty p1 ty1 ty2)
      && same_rows rest1 rest2
    | row1, row2 -> same_row row1 row2
  in
  same_rows bound1.row bound2.row && Set.equal bound1.shadowed bound2.shadowed
;;

module Iter = struct
  type 'e t =
    { ty : guarded:bool -> (ty, Nothing.t) Var.t -> (unit, 'e) Result.t
    ; row : guarded:bool -> (row, bound) Var.t -> (unit, 'e) Result.t
    ; presence :
        guarded:bool -> (presence, presence_bound) Var.t -> (unit, 'e) Result.t
    ; mode : Mode.Var.t -> (unit, 'e) Result.t
    }
end

module Iter_start = struct
  type t =
    | Ty of ty @@ global
    | Row of row @@ global
end

module Visited = struct
  (* The first [Link]s, records and variants reached are followed without
     remembering them: most types are small, and visiting a variable again is
     harmless. Only past those is a table of their ids, which are distinct,
     needed for cycles. Whether each [Link] was only visited guarded: an
     unguarded visit covers a guarded one. *)
  type t =
    { mutable unremembered : int
    ; mutable table : bool Int.Table.t or_null
    }

  let table t =
    match t.table with
    | This table -> table
    | Null ->
      let table = Int.Table.create () in
      t.table <- This table;
      table
  ;;

  let enter_link t ~id ~guarded =
    if t.unremembered > 0
    then (
      t.unremembered <- t.unremembered - 1;
      true)
    else (
      let table = table t in
      match Hashtbl.find table id with
      | Some was_guarded when guarded || not was_guarded -> false
      | Some _ | None ->
        Hashtbl.set table ~key:id ~data:guarded;
        true)
  ;;

  let enter_node t id =
    if t.unremembered > 0
    then (
      t.unremembered <- t.unremembered - 1;
      true)
    else (
      let table = table t in
      if Hashtbl.mem table id
      then false
      else (
        Hashtbl.set table ~key:id ~data:true;
        true))
  ;;
end

let iterate
  (local_ (f : _ Iter.t))
  ~(local_ held : Mode.Var.t -> _)
  (local_ (start : Iter_start.t))
  =
  let visited = stack_ { Visited.unremembered = 64; table = Null } in
  let iter_modes (modes : Arrow_modes.t) =
    Arrow_modes.iter_result modes ~f:(fun var -> f.mode var) [@nontail]
  in
  let rec iter_ty ~guarded ty =
    match ty with
    | TVar ({ state = Link target; _ } as var) ->
      if Visited.enter_link visited ~id:var.id ~guarded
      then iter_ty ~guarded target
      else Ok ()
    | TVar var -> (f.ty ~guarded) var
    | TCon (_, args) -> iter_args ~guarded args
    | TArrow (ty1, ty2, modes) ->
      (match iter_modes modes with
       | Error _ as error -> error
       | Ok () ->
         (match iter_ty ~guarded ty1 with
          | Ok () -> iter_ty ~guarded ty2
          | Error _ as error -> error))
    | TTuple tys -> iter_tys ~guarded tys
    | TCode { inner; context } ->
      (match iter_ty ~guarded inner with
       | Ok () -> iter_row ~guarded context
       | Error _ as error -> error)
    | TRecord { id; row } | TVariant { id; row } ->
      if Visited.enter_node visited id
      then iter_row ~guarded:true row
      else Ok ()
  and iter_args ~guarded args =
    match args with
    | [] -> Ok ()
    | ATy ty :: args ->
      (match iter_ty ~guarded ty with
       | Ok () -> iter_args ~guarded args
       | Error _ as error -> error)
    | ARow row :: args ->
      (match iter_row ~guarded row with
       | Ok () -> iter_args ~guarded args
       | Error _ as error -> error)
    | AMode mode :: args ->
      (match Mode_arg.iter_result mode ~f:(fun var -> held var) with
       | Ok () -> iter_args ~guarded args
       | Error _ as error -> error)
  and iter_tys ~guarded tys =
    match tys with
    | [] -> Ok ()
    | ty :: tys ->
      (match iter_ty ~guarded ty with
       | Ok () -> iter_tys ~guarded tys
       | Error _ as error -> error)
  and iter_row ~guarded row =
    match row with
    | REmpty -> Ok ()
    | RExtend (_, presence, assigned, ty, rest) ->
      (match iter_presence ~guarded presence with
       | Error _ as error -> error
       | Ok () ->
         (match iter_presence ~guarded assigned with
          | Error _ as error -> error
          | Ok () ->
            (match iter_ty ~guarded ty with
             | Ok () -> iter_row ~guarded rest
             | Error _ as error -> error)))
    | RVar ({ state = Link target; _ } as var) ->
      if Visited.enter_link visited ~id:var.id ~guarded
      then iter_row ~guarded target
      else Ok ()
    | RVar var -> (f.row ~guarded) var
  and iter_presence ~guarded presence =
    match canonical_presence presence with
    | Present | Absent -> Ok ()
    | PVar var -> (f.presence ~guarded) var
  in
  match start with
  | Ty ty -> iter_ty ~guarded:false ty [@nontail]
  | Row row -> iter_row ~guarded:false row [@nontail]
;;

let iter_ty f ty =
  iterate f ~held:(fun var -> f.mode var) (stack_ Iter_start.Ty ty) [@nontail]
;;

let iter_row f row =
  iterate f ~held:(fun var -> f.mode var) (stack_ Iter_start.Row row) [@nontail]
;;

module Mapper = struct
  type t =
    { ty : (ty, Nothing.t) Var.t -> ty
    ; row : (row, bound) Var.t -> row
    ; presence : (presence, presence_bound) Var.t -> presence
    ; keeps : 'a 'bound. ('a, 'bound) Var.t -> bool
    ; id : unit -> Id.t
    ; mode : Mode.Var.t -> Mode.Var.t
    }
end

(* Whether what a [Link], record or variant points to has a variable not
   satisfying [keeps], computed once for each. Those of a strongly connected
   component (found as by Tarjan's algorithm) reach each other, so share the
   answer. *)
module Changes = struct
  module Visiting = struct
    type t =
      { index : int
      ; mutable changes : bool
      }
  end

  module State = struct
    type t =
      | Visiting of Visiting.t
      | Visited of bool
  end

  type t =
    { keeps : 'a 'bound. ('a, 'bound) Var.t -> bool
    ; mode : Mode.Var.t -> Mode.Var.t
    ; tys : State.t Int.Table.t
    ; rows : State.t Int.Table.t
    ; nodes : State.t Int.Table.t
    ; stack : (State.t Int.Table.t * Id.t * Visiting.t) Stack.t
    ; mutable next_index : int
    }

  let create (f : Mapper.t) =
    { keeps = f.keeps
    ; mode = f.mode
    ; tys = Int.Table.create ()
    ; rows = Int.Table.create ()
    ; nodes = Int.Table.create ()
    ; stack = Stack.create ()
    ; next_index = 0
    }
  ;;

  (* Whether there are changes, and the lowest index of a vertex still being
     visited reached. *)
  let none = false, Int.max_value

  let both (changes1, low1) (changes2, low2) =
    changes1 || changes2, min low1 low2
  ;;

  let var t var = t.keeps var |> not, Int.max_value

  let vertex t table id ~target =
    match Hashtbl.find table id with
    | Some (State.Visited changes) -> changes, Int.max_value
    | Some (Visiting { index; _ }) -> false, index
    | None ->
      let index = t.next_index in
      t.next_index <- index + 1;
      let visiting = { Visiting.index; changes = false } in
      Hashtbl.set table ~key:id ~data:(Visiting visiting);
      Stack.push t.stack (table, id, visiting);
      let changes, low = target () in
      visiting.changes <- changes;
      if low < index
      then changes, low
      else (
        let rec pop component =
          let ((table', id', _) as entry) = Stack.pop_exn t.stack in
          let component = entry :: component in
          if phys_equal table table' && id = id'
          then component
          else pop component
        in
        let component = pop [] in
        let changes =
          List.exists component ~f:(fun (_, _, visiting) -> visiting.changes)
        in
        List.iter component ~f:(fun (table, id, _) ->
          Hashtbl.set table ~key:id ~data:(Visited changes));
        changes, Int.max_value)
  ;;

  let rec ty t ty' =
    match ty' with
    | TVar ({ state = Link target; _ } as var') ->
      vertex t t.tys var'.id ~target:(fun () -> ty t target)
    | TVar var' -> var t var'
    | TCon (_, args) ->
      List.fold args ~init:none ~f:(fun acc -> function
        | ATy ty' -> both acc (ty t ty')
        | ARow row' -> both acc (row t row')
        | AMode mode ->
          both
            acc
            ( Mode_arg.exists mode ~f:(fun var ->
                not (phys_equal (t.mode var) var))
            , Int.max_value ))
    | TArrow (ty1, ty2, modes) ->
      let changes =
        Arrow_modes.exists modes ~f:(fun var ->
          not (phys_equal (t.mode var) var))
      in
      both (changes, Int.max_value) (both (ty t ty1) (ty t ty2))
    | TTuple tys ->
      List.fold tys ~init:none ~f:(fun acc ty' -> both acc (ty t ty'))
    | TCode { inner; context } -> both (ty t inner) (row t context)
    | TRecord { id; row = row' } | TVariant { id; row = row' } ->
      vertex t t.nodes id ~target:(fun () -> row t row')

  and row t row' =
    match row' with
    | REmpty -> none
    | RExtend (_, presence, assigned, ty', rest) ->
      let presence_var presence =
        match canonical_presence presence with
        | Present | Absent -> none
        | PVar var' -> var t var'
      in
      both
        (both (presence_var presence) (presence_var assigned))
        (both (ty t ty') (row t rest))
    | RVar ({ state = Link target; _ } as var') ->
      vertex t t.rows var'.id ~target:(fun () -> row t target)
    | RVar var' -> var t var'
  ;;
end

(* A record or variant copied, or being copied if there is no [result] yet:
   reaching it again then gives a [knot], linked to the result once known. *)
module Copy = struct
  type t =
    { mutable result : ty option
    ; mutable knot : (ty, Nothing.t) Var.t option
    }

  let memo table id ~new_id ~f =
    match Hashtbl.find table id with
    | Some { result = Some result; knot = _ } -> result
    | Some copy ->
      let knot =
        match copy.knot with
        | Some knot -> knot
        | None ->
          let knot = Var.create (new_id ()) Generic in
          copy.knot <- Some knot;
          knot
      in
      TVar knot
    | None ->
      let copy = { result = None; knot = None } in
      Hashtbl.set table ~key:id ~data:copy;
      let result = f () in
      copy.result <- Some result;
      Option.iter copy.knot ~f:(fun knot -> Var.set_state knot (Link result));
      result
  ;;
end

let mapper (f : Mapper.t) =
  let tys = Int.Table.create () in
  let rows = Int.Table.create () in
  let nodes = Int.Table.create () in
  let changes = Changes.create f in
  (* A changed [Link] is copied to a new [Link], created before its target is
     copied, so the copy shares through it, and closes cycles. *)
  let link table (var : _ Var.t) ~unchanged ~changes ~wrap ~map_target =
    match Hashtbl.find table var.id with
    | Some copy -> copy
    | None when not changes ->
      Hashtbl.set table ~key:var.id ~data:unchanged;
      unchanged
    | None ->
      let var' = Var.create (f.id ()) Generic in
      let copy = wrap var' in
      Hashtbl.set table ~key:var.id ~data:copy;
      Var.set_state var' (Link (map_target ()));
      copy
  in
  (* Unchanged parts are kept as they are. *)
  let rec map_ty ty =
    match ty with
    | TVar ({ state = Link target; _ } as var) ->
      link
        tys
        var
        ~unchanged:ty
        ~changes:(fst (Changes.ty changes ty))
        ~wrap:(fun var -> TVar var)
        ~map_target:(fun () -> map_ty target)
    | TVar var -> if f.keeps var then ty else f.ty var
    | TCon (name, args) ->
      let args' = List.map args ~f:map_arg in
      if List.for_all2_exn args args' ~f:phys_equal
      then ty
      else TCon (name, args')
    | TArrow (ty1, ty2, modes) ->
      let ty1' = map_ty ty1 in
      let ty2' = map_ty ty2 in
      let modes' = Arrow_modes.map modes ~f:f.mode in
      if phys_equal ty1 ty1' && phys_equal ty2 ty2' && phys_equal modes modes'
      then ty
      else TArrow (ty1', ty2', modes')
    | TTuple tys ->
      let tys' = List.map tys ~f:map_ty in
      if List.for_all2_exn tys tys' ~f:phys_equal then ty else TTuple tys'
    | TCode { inner; context } ->
      let inner' = map_ty inner in
      let context' = map_row context in
      if phys_equal inner inner' && phys_equal context context'
      then ty
      else TCode { inner = inner'; context = context' }
    | TRecord { id; row } | TVariant { id; row } ->
      Copy.memo nodes id ~new_id:f.id ~f:(fun () ->
        if not (fst (Changes.ty changes ty))
        then ty
        else (
          let row = map_row row in
          match ty with
          | TRecord _ -> TRecord { id = f.id (); row }
          | _ -> TVariant { id = f.id (); row }))
  and map_arg arg =
    match arg with
    | ATy ty ->
      let ty' = map_ty ty in
      if phys_equal ty ty' then arg else ATy ty'
    | ARow row ->
      let row' = map_row row in
      if phys_equal row row' then arg else ARow row'
    | AMode mode ->
      let mode' = Mode_arg.map mode ~f:f.mode in
      if phys_equal mode mode' then arg else AMode mode'
  and map_row row =
    match row with
    | REmpty -> row
    | RExtend (name, presence, assigned, ty, rest) ->
      let presence' = map_presence presence in
      let assigned' = map_presence assigned in
      let ty' = map_ty ty in
      let rest' = map_row rest in
      if phys_equal presence presence'
         && phys_equal assigned assigned'
         && phys_equal ty ty'
         && phys_equal rest rest'
      then row
      else RExtend (name, presence', assigned', ty', rest')
    | RVar ({ state = Link target; _ } as var) ->
      link
        rows
        var
        ~unchanged:row
        ~changes:(fst (Changes.row changes row))
        ~wrap:(fun var -> RVar var)
        ~map_target:(fun () -> map_row target)
    | RVar var -> if f.keeps var then row else f.row var
  and map_presence presence =
    match canonical_presence presence with
    | Present | Absent -> presence
    | PVar var -> if f.keeps var then presence else f.presence var
  in
  map_ty, map_row
;;

let map_ty f ty = fst (mapper f) ty
let map_row f row = snd (mapper f) row

let has_unbound_var ty =
  let unbound ~guarded:_ (var : _ Var.t) =
    match var.state with
    | Unbound _ -> Error ()
    | Generic | Link _ -> Ok ()
  in
  Result.is_error
    (iter_ty
       { ty = unbound
       ; row = unbound
       ; presence = unbound
       ; mode = (fun _ -> Ok ())
       }
       ty)
;;

(* Only the modes of an arrow that aren't the default of a declaration's. *)
let sexp_of_modes ({ arg; res; curry = _ } : Arrow_modes.t) =
  let side name : Arrow_modes.Side.t -> _ = function
    | Fixed const when Mode.Const.equal const Arrow_modes.default_const -> []
    | Fixed const -> [ [%sexp (name : string), (const : Mode.Const.t)] ]
    | Var _ -> []
    | Var_at { locality = Global; _ } -> []
    | Var_at { locality; _ } ->
      [ [%sexp (name : string), (locality : Mode.Locality.t)] ]
  in
  side "arg" arg @ side "res" res
;;

module Sexper = struct
  module Printing = struct
    type t =
      { depth : int
      ; mutable label : int option
      }
  end

  type t =
    { printing : Printing.t Int.Table.t
        (* Nodes and links being printed, labelled once reached again. *)
    ; nodes : Sexp.t Int.Table.t
    ; tys : Sexp.t Int.Table.t
    ; rows : Sexp.t Int.Table.t
    ; mutable depth : int
    ; mutable low : int
        (* The lowest depth of a record or variant being printed that the
           current part refers to. *)
    ; mutable next_label : int
    }

  let create () =
    { printing = Int.Table.create ()
    ; nodes = Int.Table.create ()
    ; tys = Int.Table.create ()
    ; rows = Int.Table.create ()
    ; depth = 0
    ; low = Int.max_value
    ; next_label = 0
    }
  ;;

  (* Memoizes the sexp of a part unless it refers to a record or variant being
     printed around it, as it would then print differently elsewhere. *)
  let closed t table key ~sexp =
    match Hashtbl.find table key with
    | Some sexp -> sexp
    | None ->
      let outer_low = t.low in
      let depth = t.depth + 1 in
      t.low <- Int.max_value;
      let sexp = sexp () in
      if t.low >= depth then Hashtbl.set table ~key ~data:sexp;
      t.low <- (if t.low >= depth then outer_low else min outer_low t.low);
      sexp
  ;;

  let cycle t table id ~sexp =
    match Hashtbl.find t.printing id with
    | Some printing ->
      t.low <- min t.low printing.depth;
      let label =
        match printing.label with
        | Some label -> label
        | None ->
          let next = t.next_label in
          t.next_label <- next + 1;
          printing.label <- Some next;
          next
      in
      [%sexp "Cycle_ref", (label : int)]
    | None ->
      closed t table id ~sexp:(fun () ->
        t.depth <- t.depth + 1;
        let printing = { Printing.depth = t.depth; label = None } in
        Hashtbl.set t.printing ~key:id ~data:printing;
        let sexp = sexp () in
        Hashtbl.remove t.printing id;
        t.depth <- t.depth - 1;
        match printing.label with
        | None -> sexp
        | Some label -> [%sexp "Cycle", (label : int), (sexp : Sexp.t)])
  ;;

  let var t table (var : _ Var.t) ~structural ~sexp_of_link : Sexp.t =
    match var.state with
    | Unbound rank -> [%sexp "Unbound", (var.id : Id.t), (rank : Rank.t)]
    | Generic -> [%sexp "Generic", (var.id : Id.t)]
    | Link target ->
      let memo = if structural target then closed else cycle in
      memo t table var.id ~sexp:(fun () ->
        [%sexp "Link", (sexp_of_link target : Sexp.t)])
  ;;

  let rec ty t ty' : Sexp.t =
    match ty' with
    | TCon (name, args) ->
      List [ Atom "TCon"; Atom name; List (List.map args ~f:(arg t)) ]
    | TArrow (ty1, ty2, modes) ->
      Sexp.List
        ([ Sexp.Atom "TArrow"; ty t ty1; ty t ty2 ] @ sexp_of_modes modes)
    | TTuple tys -> List [ Atom "TTuple"; List (List.map tys ~f:(ty t)) ]
    | TCode { inner; context } ->
      List
        [ Atom "TCode"
        ; List [ Atom "inner"; ty t inner ]
        ; List [ Atom "context"; row t context ]
        ]
    | TRecord { id; row = r } -> node t id ~name:"TRecord" r
    | TVariant { id; row = r } -> node t id ~name:"TVariant" r
    | TVar v ->
      let structural target =
        match canonical_ty target with
        | TRecord _ | TVariant _ -> true
        | _ -> false
      in
      List [ Atom "TVar"; var t t.tys v ~structural ~sexp_of_link:(ty t) ]

  and node t id ~name r =
    cycle t t.nodes id ~sexp:(fun () -> List [ Atom name; row t r ])

  and row t row' : Sexp.t =
    match row' with
    | REmpty -> Atom "REmpty"
    | RExtend (name, p, assigned, ty', rest) ->
      (* Only [Present] is shown, as unknown is the usual case in contexts. *)
      let assigned : Sexp.t list =
        match canonical_presence assigned with
        | Present -> [ Sexp.Atom "Assigned" ]
        | Absent | PVar _ -> []
      in
      Sexp.List
        ([ Sexp.Atom "RExtend"; Atom name; presence p ]
         @ assigned
         @ [ ty t ty'; row t rest ])
    | RVar v ->
      List
        [ Atom "RVar"
        ; var t t.rows v ~structural:(fun _ -> false) ~sexp_of_link:(row t)
        ]

  and presence p : Sexp.t =
    match p with
    | Present -> Atom "Present"
    | Absent -> Atom "Absent"
    | PVar v ->
      List
        [ Atom "PVar"
        ; Var.sexp_of_t presence (fun (_ : presence_bound) -> Sexp.List []) v
        ]

  and arg t arg' : Sexp.t =
    match arg' with
    | ATy ty' -> List [ Atom "ATy"; ty t ty' ]
    | ARow row' -> List [ Atom "ARow"; row t row' ]
    | AMode mode -> List [ Atom "AMode"; Mode_arg.sexp_of_t mode ]
  ;;
end

let sexp_of_ty ty = Sexper.ty (Sexper.create ()) ty
let sexp_of_row row = Sexper.row (Sexper.create ()) row
let sexp_of_presence = Sexper.presence
let sexp_of_arg arg = Sexper.arg (Sexper.create ()) arg

let sexp_of_presence_bound = function
  | At_most presence -> [%sexp At_most (sexp_of_presence presence : Sexp.t)]
  | At_least presence -> [%sexp At_least (sexp_of_presence presence : Sexp.t)]
  | If_present (ty1, ty2) -> [%sexp If_present (ty1 : ty), (ty2 : ty)]
;;

let sexp_of_bound { row; shadowed } =
  [%sexp { row : row; shadowed : String.Set.t }]
;;

module Printer = struct
  module Bounded = struct
    type t =
      | Bounds of (unit -> string list)
      | Kept of kept
  end

  type t =
    { type_name : string -> string
    ; names : int Int.Table.t
    ; mutable next_index : int
    ; cycle_names : string Int.Table.t
        (* The names of the records and variants reached again within
           themselves, by id, kept across types for consistency. *)
    ; printing : bool ref Int.Table.t
        (* The records and variants being printed, and whether they were reached
           again within themselves. *)
    ; introduced : string Int.Table.t
        (* The records and variants whose name the type being printed has
           introduced, which later occurrences use. *)
    ; mutable recursive : (ty * string) option
        (* The body [recursive] prints, and the name of its occurrences. *)
    ; bounded : Bounded.t Queue.t
        (* The bounds of the variables reached, in order of appearance, to print
           after the types. *)
    ; kept : Int.Hash_set.t
    ; elided : Int.Hash_set.t
        (* The assignment variables written as an unmarked entry, as each is the
           only occurrence of an unconstrained one, so reads back as it. *)
    ; mutable counting : int Int.Table.t option
        (* Set while rendering only to count how often each presence variable is
           named. *)
    ; modes : Mode_printing.t option (* With [create ~modes:true]. *)
    ; pointer_free : string Queue.t (* Type variables annotated so. *)
    ; layout_names : string Int.Table.t
        (* The names of the variables whose layouts are these, by id. *)
    ; mode_names : string Int.Table.t
    ; name_mode_args : bool
        (* Without [modes], whether a mode argument not known is named rather
           than left out as if the default. *)
    ; declaration : bool
    (* Whether this prints a declaration, whose mode variables are its
       parameters. *)
    }

  let create
    ?(type_name = fun name -> Prelude_path.to_string_hum name)
    ?(modes = false)
    ?(name_mode_args = false)
    ?(declaration = false)
    ()
    =
    { type_name
    ; names = Int.Table.create ()
    ; next_index = 0
    ; cycle_names = Int.Table.create ()
    ; printing = Int.Table.create ()
    ; introduced = Int.Table.create ()
    ; recursive = None
    ; bounded = Queue.create ()
    ; kept = Int.Hash_set.create ()
    ; elided = Int.Hash_set.create ()
    ; counting = None
    ; modes = Option.some_if modes (Mode_printing.create ())
    ; pointer_free = Queue.create ()
    ; layout_names = Int.Table.create ()
    ; mode_names = Int.Table.create ()
    ; name_mode_args
    ; declaration
    }
  ;;

  let name index ~weak =
    let letter = Char.of_int_exn (Char.to_int 'a' + (index % 26)) in
    let suffix = if index >= 26 then Int.to_string (index / 26) else "" in
    sprintf "'%s%c%s" (if weak then "_" else "") letter suffix
  ;;

  let next_index t =
    let index = t.next_index in
    t.next_index <- index + 1;
    index
  ;;

  let var_name t (var : _ Var.t) =
    let index =
      Hashtbl.find_or_add t.names var.id ~default:(fun () -> next_index t)
    in
    let weak =
      match var.state with
      | Unbound _ -> true
      | Generic -> false
      | Link _ -> assert false
    in
    name index ~weak
  ;;

  (* Weak if it has a weak variable. *)
  let cycle_name t id node =
    Hashtbl.find_or_add t.cycle_names id ~default:(fun () ->
      name (next_index t) ~weak:(has_unbound_var node))
  ;;

  let bounded_var_name t (var : _ Var.t) ~prefix ~bounds =
    let first_appearance = not (Hashtbl.mem t.names var.id) in
    let name = prefix ^ var_name t var in
    if first_appearance
    then (
      Option.iter (Var.layout var) ~f:(fun layout ->
        Hashtbl.set
          t.layout_names
          ~key:(Mode.Layout.id layout)
          ~data:(var_name t var);
        if Option.is_some (Mode.Layout.bound layout)
        then Queue.enqueue t.pointer_free name);
      if not (List.is_empty var.bounds)
      then Queue.enqueue t.bounded (Bounds (fun () -> bounds name));
      List.iter var.kept ~f:(fun kept ->
        if Hash_set.strict_add t.kept (kept_id kept)
           |> Hash_set.Ok_or_duplicate.is_ok
        then Queue.enqueue t.bounded (Kept kept)));
    name
  ;;

  let rec row_entries row =
    match canonical_row row with
    | REmpty -> [], None
    | RExtend (name, presence, assigned, ty, rest) ->
      let entries, tail = row_entries rest in
      (name, presence, assigned, ty) :: entries, tail
    | RVar var -> [], Some var
  ;;

  (* An absent entry of a closed row is implied, unless it shadows another. *)
  let rec drop_implied_absent entries =
    match entries with
    | [] -> []
    | ((name, presence, _, _) as entry) :: rest ->
      let rest' = drop_implied_absent rest in
      (match canonical_presence presence with
       | Absent
         when not
                (List.exists rest ~f:(fun (name', _, _, _) ->
                   String.equal name name')) -> rest'
       | Present | Absent | PVar _ -> entry :: rest')
  ;;

  let assigned_marker t assigned ~presence_var_name =
    match canonical_presence assigned with
    | Present -> "mut "
    | Absent -> "immut "
    | PVar var ->
      if Hash_set.mem t.elided var.id
      then ""
      else sprintf "mut?%s " (presence_var_name t var)
  ;;

  let sorted_row_entries
    t
    row
    ~context
    ~label
    ~entry
    ~row_var_name
    ~presence_var_name
    =
    let entries, tail = row_entries row in
    let entries =
      (match tail with
       | None -> drop_implied_absent entries
       | Some _ -> entries)
      |> List.stable_sort ~compare:(fun (a, _, _, _) (b, _, _, _) ->
        String.compare a b)
      |> List.map ~f:(fun (name, presence, assigned, ty) ->
        let name = label name in
        let entry name ty =
          entry
            name
            ty
            ~assigned:
              (match context, canonical_presence assigned with
               | true, _ -> assigned_marker t assigned ~presence_var_name
               | false, Present -> "mut "
               | false, (Absent | PVar _) -> "")
        in
        match canonical_presence presence with
        | Present -> entry name ty
        | Absent -> "-" ^ name
        | PVar var -> entry (sprintf "%s?%s" name (presence_var_name t var)) ty)
    in
    entries, Option.map tail ~f:(row_var_name t)
  ;;

  let render_cycle t id ~name ~render =
    match Hashtbl.find t.introduced id, Hashtbl.find t.printing id with
    | Some name, _ -> name
    | None, Some reached ->
      reached := true;
      name ()
    | None, None ->
      let reached = ref false in
      Hashtbl.set t.printing ~key:id ~data:reached;
      let body = render () in
      Hashtbl.remove t.printing id;
      if !reached
      then (
        let name = name () in
        Hashtbl.set t.introduced ~key:id ~data:name;
        sprintf "(%s as %s)" body name)
      else body
  ;;

  let axis_atom
    (type a)
    (axis : a Mode.Axis.t)
    (point : a)
    ~(default : a option)
    =
    let module C = (val Mode.Axis.chain axis) in
    if Option.exists default ~f:(C.equal point)
    then []
    else [ Sexp.to_string (C.sexp_of_t point) |> String.lowercase ]
  ;;

  (* The atoms of the axes other than [default]'s, or of every axis, locality
     first: [@ local portable gc]. *)
  let const_atoms (const : Mode.Const.t) ~(default : Mode.Const.t option) =
    axis_atom
      Locality
      const.locality
      ~default:(Option.map default ~f:(fun (d : Mode.Const.t) -> d.locality))
    @ axis_atom
        Stage
        const.stage
        ~default:(Option.map default ~f:(fun (d : Mode.Const.t) -> d.stage))
    @ axis_atom
        Provenance
        const.provenance
        ~default:
          (Option.map default ~f:(fun (d : Mode.Const.t) -> d.provenance))
  ;;

  let locality_atom locality =
    Sexp.to_string (Mode.Locality.sexp_of_t locality) |> String.lowercase
  ;;

  (* Where modes aren't printed in full, what restricts a side's uses: its
     locality unless [global], and [external] if its variable is known to be. As
     [@ local external] would read back. *)
  let var_at_atoms var locality =
    (if Mode.Locality.equal locality Global
     then []
     else [ locality_atom locality ])
    @
    if Mode.Provenance.equal (Mode.Var.lower var).provenance External
    then [ "external" ]
    else []
  ;;

  let lower_atoms var =
    match var_at_atoms var (Mode.Var.lower var).locality with
    | [] -> None
    | atoms -> Some (String.concat ~sep:" " atoms)
  ;;

  (* A side's variable printed as the constant it is fixed to, as [Fixed]. *)
  let effective t (side : Arrow_modes.Side.t) : Arrow_modes.Side.t =
    match Arrow_modes.Side.var side, t.modes, t.counting with
    | Some var, Some modes, None ->
      (match Mode_printing.side modes var with
       | `Name _ -> side
       | `Atoms atoms ->
         let point axis =
           List.Assoc.find atoms axis ~equal:Mode.Axis.Any.equal
           |> Option.value ~default:0
         in
         Fixed
           { stage = Mode.Stage.of_index_exn (point (T Stage))
           ; provenance = Mode.Provenance.of_index_exn (point (T Provenance))
           ; locality =
               (match side with
                | Var_at { locality; _ } -> locality
                | Fixed _ | Var _ ->
                  Mode.Locality.of_index_exn (point (T Locality)))
           })
    | (Some _ | None), _, _ -> side
  ;;

  let is_default_side t side =
    match effective t side with
    | Fixed const ->
      List.is_empty
        (const_atoms const ~default:(Some Arrow_modes.default_const))
    | Var var ->
      (match t.modes with
       | None -> (not t.declaration) && Option.is_none (lower_atoms var)
       | Some _ -> false)
    | Var_at { var; locality } ->
      (match t.modes with
       | None ->
         (not t.declaration) && List.is_empty (var_at_atoms var locality)
       | Some _ -> false)
  ;;

  let mode_var_name t var =
    Hashtbl.find_or_add t.mode_names (Mode.Var.id var) ~default:(fun () ->
      sprintf "'m%d" (Hashtbl.length t.mode_names))
  ;;

  let mode_side t side ~position =
    match effective t side with
    | Fixed const ->
      (match const_atoms const ~default:(Some Arrow_modes.default_const) with
       | [] -> ""
       | atoms -> " @ " ^ String.concat ~sep:" " atoms)
    | Var var ->
      (match t.modes, t.counting with
       | None, _ when t.declaration -> " @ " ^ mode_var_name t var
       | None, _ ->
         Option.value_map (lower_atoms var) ~default:"" ~f:(fun atom ->
           " @ " ^ atom)
       | Some modes, Some _ ->
         Mode_printing.count
           modes
           var
           ~side:
             (match position with
              | `Arg -> `Arg
              | `Res -> `Res);
         " @ _"
       | Some modes, None ->
         (match Mode_printing.side modes var with
          | `Name name -> " @ " ^ name
          | `Atoms _ -> assert false))
    | Var_at { var; locality } ->
      (* In a declaration, a mode parameter alone is [global]. *)
      let at name =
        " @ "
        ^ (if t.declaration && Mode.Locality.equal locality Global
           then ""
           else locality_atom locality ^ " ")
        ^ name
      in
      (match t.modes, t.counting with
       | None, _ when t.declaration -> at (mode_var_name t var)
       | None, _ ->
         (match var_at_atoms var locality with
          | [] -> ""
          | atoms -> " @ " ^ String.concat ~sep:" " atoms)
       | Some modes, Some _ ->
         Mode_printing.count
           modes
           var
           ~side:
             (match position with
              | `Arg -> `Mode_arg
              | `Res -> `Res_at);
         at "_"
       | Some modes, None ->
         (match Mode_printing.side modes var with
          | `Name name -> at name
          | `Atoms _ -> assert false))
  ;;

  (* A mode argument, unless the default ([ptr[t]]'s [nonportable gc]). *)
  let held_mode t (held : Mode_arg.t) =
    let atom provenance =
      Some
        (Sexp.to_string (Mode.Provenance.sexp_of_t provenance)
         |> String.lowercase)
    in
    let const_atoms ({ stage; provenance } : Mode_arg.Const.t) =
      let default = Mode_arg.Const.default in
      match
        axis_atom Stage stage ~default:(Some default.stage)
        @ axis_atom Provenance provenance ~default:(Some default.provenance)
      with
      | [] -> None
      | atoms -> Some (String.concat ~sep:" " atoms)
    in
    match held, Mode_arg.known held, t.modes with
    | Fixed const, _, _ | Var _, Some const, None -> const_atoms const
    | Var var, _, Some modes ->
      (match t.counting with
       | Some _ ->
         Mode_printing.count modes var ~side:`Mode_arg;
         Some "_"
       | None ->
         (match Mode_printing.side modes var with
          | `Name name -> Some name
          | `Atoms atoms ->
            let point axis =
              List.Assoc.find atoms axis ~equal:Mode.Axis.Any.equal
              |> Option.value ~default:0
            in
            const_atoms
              { stage = Mode.Stage.of_index_exn (point (T Stage))
              ; provenance = Mode.Provenance.of_index_exn (point (T Provenance))
              }))
    | Var var, None, None ->
      if Mode.Provenance.equal (Mode.Var.lower var).provenance External
      then atom External
      else if Mode.Provenance.equal (Mode.Var.upper var).provenance Static
      then atom Static
      else if t.name_mode_args
      then Some (mode_var_name t var)
      else None
  ;;

  (* A mode argument other than [ptr]'s, which is always written. *)
  let mode_arg_to_string t (mode : Mode_arg.t) =
    match held_mode t mode, mode with
    | Some printed, _ -> printed
    | None, Fixed _ -> "gc"
    | None, Var var ->
      if Option.is_some t.modes then "gc" else mode_var_name t var
  ;;

  let side_locality t side : Mode.Locality.t =
    match effective t side with
    | Fixed const -> const.locality
    | Var var -> (Mode.Var.lower var).locality
    | Var_at { locality; _ } -> locality
  ;;

  (* A declared curried arrow's result defaults to as local as the arguments
     before it, so a less local result says so. *)
  let curried_global t ~curried (side : Arrow_modes.Side.t) =
    (not (Mode.Locality.equal curried Global))
    && Mode.Locality.equal (side_locality t side) Global
    &&
    match effective t side, t.modes with
    | Var _, Some _ -> false
    | Var _, None | Fixed _, _ -> true
    (* Its locality is written, or, a mode parameter's, is [global] anyway. *)
    | Var_at _, Some _ -> false
    | Var_at _, None -> not t.declaration
  ;;

  let rec ty_to_string ?curried t ~prec ty =
    let canonical = canonical_ty ty in
    match canonical, t.recursive with
    | ty, Some (body, name) when phys_equal ty body -> name
    | (TRecord { id; _ } | TVariant { id; _ }), _ ->
      render_cycle
        t
        id
        ~name:(fun () -> cycle_name t id canonical)
        ~render:(fun () -> structure t ~prec canonical)
    | _ ->
      (match ty with
       | TVar { id; state = Link _; _ } ->
         render_cycle
           t
           id
           ~name:(fun () -> cycle_name t id canonical)
           ~render:(fun () -> structure ?curried t ~prec canonical)
       | _ -> structure ?curried t ~prec canonical)

  and structure ?(curried = Mode.Locality.Global) t ~prec ty =
    let parens_if cond s = if cond then "(" ^ s ^ ")" else s in
    match canonical_ty ty with
    | TCon (name, []) -> t.type_name name
    | TCon (name, [ ATy pointee; AMode held ]) when String.equal name ptr_name
      ->
      (match held_mode t held with
       | None ->
         sprintf "%s[%s]" (t.type_name name) (ty_to_string t ~prec:0 pointee)
       | Some mode ->
         sprintf
           "%s[%s @@ %s]"
           (t.type_name name)
           (ty_to_string t ~prec:1 pointee)
           mode)
    | TCon (name, args) ->
      sprintf
        "%s[%s]"
        (t.type_name name)
        (List.map args ~f:(function
           | ATy ty -> ty_to_string t ~prec:0 ty
           | ARow row -> record_to_string t row
           | AMode mode -> mode_arg_to_string t mode)
         |> String.concat ~sep:", ")
    | TArrow (ty1, ty2, modes) ->
      let ty1 =
        ty_to_string t ~prec:1 ty1 ^ mode_side t modes.arg ~position:`Arg
      in
      let curried =
        match canonical_ty ty2 with
        | TArrow _ ->
          (* As read back: a named argument adds nothing to the default. *)
          (match effective t modes.arg, t.modes with
           | Var _, Some _ -> curried
           | (Fixed _ | Var _ | Var_at _), _ ->
             Mode.Locality.join curried (side_locality t modes.arg))
        | _ -> Global
      in
      let global = curried_global t ~curried modes.res in
      let ty2 =
        ty_to_string
          t
          ~curried
          ~prec:(if is_default_side t modes.res && not global then 0 else 1)
          ty2
      in
      let res =
        match mode_side t modes.res ~position:`Res, global with
        | side, false -> side
        | "", true -> " @ global"
        | side, true when String.is_prefix side ~prefix:" @ '" -> side
        | side, true -> side ^ " global"
      in
      parens_if (prec > 0) (sprintf "%s -> %s%s" ty1 ty2 res)
    | TTuple [] -> "()"
    | TTuple tys ->
      parens_if
        (prec > 1)
        (List.map tys ~f:(ty_to_string t ~prec:2) |> String.concat ~sep:" * ")
    | TCode { inner; context } ->
      let inner = ty_to_string t ~prec:0 inner in
      sprintf
        "<%s>%s"
        inner
        (row_to_string t context ~label:Fn.id ~context:true)
    | TRecord { row; _ } -> record_to_string t row
    | TVariant { row; _ } -> variant_to_string t row
    | TVar var -> bounded_var_name t var ~prefix:"" ~bounds:(fun _ -> [])

  and variant_to_string t row =
    let entries, tail =
      sorted_row_entries
        t
        row
        ~context:false
        ~label:(fun tag -> "`" ^ tag)
        ~row_var_name
        ~presence_var_name
        ~entry:(fun tag ty ~assigned:_ ->
          match canonical_ty ty with
          | TCon ("unit", []) -> tag
          | _ -> sprintf "%s of %s" tag (ty_to_string t ~prec:0 ty))
    in
    let entries = String.concat entries ~sep:" | " in
    match entries, tail with
    | entries, None -> "[" ^ entries ^ "]"
    | "", Some tail -> "[" ^ tail ^ "]"
    | entries, Some tail -> "[" ^ entries ^ "; " ^ tail ^ "]"

  and row_var_name t var =
    bounded_var_name t var ~prefix:".." ~bounds:(fun name ->
      List.map var.bounds ~f:(fun bound ->
        sprintf
          "%s <= %s"
          name
          (row_to_string t bound.row ~label:Fn.id ~context:true)))

  (* A bound is printed with its lower variable, unless only the upper one keeps
     it, as a generic variable does for a variable it is not generalized with. A
     lower variable not printed otherwise is named here, so prints its bounds
     later. *)
  and presence_var_name t (var : (presence, presence_bound) Var.t) =
    Option.iter t.counting ~f:(fun counts -> Hashtbl.incr counts var.id);
    let others side =
      List.filter_map var.bounds ~f:(fun bound ->
        match side, bound with
        | `Upper, At_most other | `Lower, At_least other ->
          (match canonical_presence other with
           | PVar other when not (phys_equal other var) -> Some other
           | PVar _ | Present | Absent -> None)
        | `Upper, (At_least _ | If_present _)
        | `Lower, (At_most _ | If_present _) -> None)
      |> List.dedup_and_sort ~compare:(fun (a : _ Var.t) b ->
        Int.compare a.id b.id)
    in
    let keeps_bound_to_var (lower : (presence, presence_bound) Var.t) =
      List.exists lower.bounds ~f:(function
        | At_most bound ->
          (match canonical_presence bound with
           | PVar bound -> phys_equal bound var
           | Present | Absent -> false)
        | At_least _ | If_present _ -> false)
    in
    bounded_var_name t var ~prefix:"" ~bounds:(fun name ->
      let uppers =
        List.map (others `Upper) ~f:(fun upper ->
          sprintf "%s <= %s" name (presence_var_name t upper))
      in
      let lowers =
        List.filter (others `Lower) ~f:(fun lower ->
          if keeps_bound_to_var lower
          then (
            if not (Hashtbl.mem t.names lower.id)
            then ignore (presence_var_name t lower : string);
            false)
          else true)
        |> List.map ~f:(fun lower ->
          sprintf "%s <= %s" (presence_var_name t lower) name)
      in
      let equalities =
        List.filter_map var.bounds ~f:(function
          | If_present (ty1, ty2) when not (same_ty ty1 ty2) ->
            Some (equality t name ty1 ty2)
          | If_present _ | At_most _ | At_least _ -> None)
      in
      uppers @ lowers @ equalities)

  and equality t presence ty1 ty2 =
    sprintf
      "%s => %s = %s"
      presence
      (ty_to_string t ~prec:0 ty1)
      (ty_to_string t ~prec:0 ty2)

  and record_to_string t row =
    row_to_string t row ~label:(fun name -> "`" ^ name) ~context:false

  and row_to_string t row ~label ~context =
    match row with
    | RVar { id; state = Link _; _ } ->
      render_cycle
        t
        id
        ~name:(fun () ->
          Hashtbl.find_or_add t.cycle_names id ~default:(fun () ->
            ".." ^ name (next_index t) ~weak:false))
        ~render:(fun () -> row_structure t row ~label ~context)
    | _ -> row_structure t row ~label ~context

  and row_structure t row ~label ~context =
    let entries, tail =
      sorted_row_entries
        t
        row
        ~context
        ~label
        ~row_var_name
        ~presence_var_name
        ~entry:(fun name ty ~assigned ->
          sprintf "%s : %s%s" name assigned (ty_to_string t ~prec:0 ty))
    in
    "{" ^ String.concat ~sep:"; " (entries @ Option.to_list tail) ^ "}"
  ;;

  let rec bounds t =
    match Queue.dequeue t.bounded with
    | None -> []
    | Some (Bounds bounds_of_var) ->
      let these = bounds_of_var () in
      these @ bounds t
    | Some (Kept (Row_bound { lower; bound; _ })) ->
      let lower =
        match canonical_row (RVar lower) with
        | RVar var -> row_var_name t var
        | row -> row_to_string t row ~label:Fn.id ~context:true
      in
      let this =
        sprintf
          "%s <= %s"
          lower
          (row_to_string t bound.row ~label:Fn.id ~context:true)
      in
      this :: bounds t
    | Some (Kept (If_present_kept { ty1; ty2; _ })) when same_ty ty1 ty2 ->
      bounds t
    | Some (Kept (If_present_kept { presence; ty1; ty2; _ })) ->
      (match canonical_presence (PVar presence) with
       | PVar var ->
         let this = equality t (presence_var_name t var) ty1 ty2 in
         this :: bounds t
       | Present ->
         let this =
           sprintf
             "%s = %s"
             (ty_to_string t ~prec:0 ty1)
             (ty_to_string t ~prec:0 ty2)
         in
         this :: bounds t
       | Absent -> bounds t)
  ;;

  (* Renders with [render] once, on a copy, to find the assignment variables
     named once, so in no bound either, to elide, then for real. The copy prints
     the bounds it reaches, not those queued before, which print with [t]. *)
  let eliding t ~render =
    let counts = Int.Table.create () in
    let copy =
      { t with
        names = Hashtbl.copy t.names
      ; cycle_names = Hashtbl.copy t.cycle_names
      ; printing = Hashtbl.copy t.printing
      ; introduced = Hashtbl.copy t.introduced
      ; bounded = Queue.create ()
      ; pointer_free = Queue.create ()
      ; kept = Hash_set.copy t.kept
      ; elided = Hash_set.copy t.elided
      ; counting = Some counts
      }
    in
    ignore (render copy : string);
    ignore (bounds copy : string list);
    (* A declaration's mode variables are its parameters, related to nothing, so
       print by name, and there is no [with] to settle. *)
    if not t.declaration
    then Option.iter t.modes ~f:(fun modes -> Mode_printing.settle modes);
    Hashtbl.iteri counts ~f:(fun ~key:id ~data:count ->
      if count = 1 && not (Hashtbl.mem t.names id) then Hash_set.add t.elided id);
    render t
  ;;

  let ty ?(atomic = false) t ty =
    eliding t ~render:(fun t ->
      Hashtbl.clear t.introduced;
      ty_to_string t ~prec:(if atomic then 2 else 0) ty)
  ;;

  let mode_arg t mode = eliding t ~render:(fun t -> mode_arg_to_string t mode)

  let declaration t name params =
    eliding t ~render:(fun t ->
      Hashtbl.clear t.introduced;
      match params with
      | [] -> t.type_name name
      | params ->
        List.map params ~f:(function
          | ATy ty -> ty_to_string t ~prec:0 ty
          | ARow row -> record_to_string t row
          | AMode mode -> mode_arg_to_string t mode ^ " : mode")
        |> String.concat ~sep:", "
        |> sprintf "%s[%s]" (t.type_name name))
  ;;

  let context t row =
    eliding t ~render:(fun t ->
      Hashtbl.clear t.introduced;
      row_to_string t row ~label:Fn.id ~context:true)
  ;;

  let recursive t ty ~name =
    eliding t ~render:(fun t ->
      Hashtbl.clear t.introduced;
      let body = canonical_ty ty in
      t.recursive <- Some (body, name);
      let printed = structure t ~prec:0 body in
      t.recursive <- None;
      printed)
  ;;

  let where t =
    let modes =
      match t.modes with
      | None -> []
      | Some modes ->
        (* A guard not of a variable printed is of one not in the type. *)
        let layout_name layout =
          Hashtbl.find_or_add
            t.layout_names
            (Mode.Layout.id layout)
            ~default:(fun () -> name (next_index t) ~weak:true)
        in
        List.map (Queue.to_list t.pointer_free) ~f:(fun name ->
          name ^ " : pointer_free")
        @ Mode_printing.constraints modes ~layout_name
    in
    (match bounds t with
     | [] -> ""
     | bounds -> " where " ^ String.concat ~sep:", " bounds)
    ^
    match modes with
    | [] -> ""
    | modes -> " with " ^ String.concat ~sep:" and " modes
  ;;
end

let to_string_hum ty =
  let printer = Printer.create () in
  let ty = Printer.ty printer ty in
  ty ^ Printer.where printer
;;

module Snapshot = struct
  type t =
    { tys : (ty, Nothing.t) var Int.Table.t
    ; rows : (row, bound) var Int.Table.t
    ; presences : (presence, presence_bound) var Int.Table.t
    }

  let create () =
    { tys = Int.Table.create ()
    ; rows = Int.Table.create ()
    ; presences = Int.Table.create ()
    }
  ;;

  (* Shares the copy of each variable, so cycles through links end. *)
  let var table (var : (_, _) var) ~copy =
    match Hashtbl.find table var.id with
    | Some copied -> copied
    | None ->
      let copied = { var with state = Generic } in
      Hashtbl.set table ~key:var.id ~data:copied;
      copied.state
      <- (match var.state with
          | Link linked -> Link (copy linked)
          | (Unbound _ | Generic) as state -> state);
      copied
  ;;

  let rec ty t = function
    | TCon (name, args) -> TCon (name, List.map args ~f:(arg t))
    | TArrow (a, b, modes) -> TArrow (ty t a, ty t b, modes)
    | TTuple tys -> TTuple (List.map tys ~f:(ty t))
    | TCode { inner; context } ->
      TCode { inner = ty t inner; context = row t context }
    | TRecord { id; row = r } -> TRecord { id; row = row t r }
    | TVariant { id; row = r } -> TVariant { id; row = row t r }
    | TVar v -> TVar (var t.tys v ~copy:(ty t))

  and row t = function
    | REmpty -> REmpty
    | RExtend (label, p, assigned, entry, rest) ->
      RExtend (label, presence t p, presence t assigned, ty t entry, row t rest)
    | RVar v -> RVar (var t.rows v ~copy:(row t))

  and presence t = function
    | (Present | Absent) as p -> p
    | PVar v -> PVar (var t.presences v ~copy:(presence t))

  and arg t = function
    | ATy a -> ATy (ty t a)
    | ARow r -> ARow (row t r)
    | AMode _ as mode -> mode
  ;;
end
