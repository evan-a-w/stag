open! Core

type id = int [@@deriving sexp, compare, equal]
type rank = int [@@deriving sexp, compare, equal]

module Var = struct
  type 'a state =
    | Unbound of rank
    | Generic
    | Link of 'a

  type ('a, 'bound) t =
    { id : id
    ; mutable state : 'a state
    ; mutable bounds : 'bound list
    }

  let create id state = { id; state; bounds = [] }

  let rank_exn t =
    match t.state with
    | Unbound rank -> rank
    | Generic | Link _ ->
      raise_s [%message "Type.Var.rank_exn: variable not unbound" (t.id : id)]
  ;;

  let sexp_of_t sexp_of_a _ t =
    match t.state with
    | Unbound rank -> [%sexp "Unbound", (t.id : id), (rank : rank)]
    | Generic -> [%sexp "Generic", (t.id : id)]
    | Link a -> [%sexp "Link", (a : a)]
  ;;

  let rec canonical x ~var =
    match var x with
    | Some ({ state = Link x'; _ } as t) ->
      let x'' = canonical x' ~var in
      t.state <- Link x'';
      x''
    | Some { state = Unbound _ | Generic; _ } | None -> x
  ;;
end

type ty =
  | TCon of string * arg list
  | TArrow of ty * ty
  | TTuple of ty list
  | TCode of
      { inner : ty
      ; context : row
      }
  | TRecord of row
  | TVariant of row
  | TVar of (ty, Nothing.t) Var.t

and row =
  | REmpty
  | RExtend of string * presence * ty * row
  | RVar of (row, bound) Var.t

and presence =
  | Present
  | Absent
  | PVar of (presence, Nothing.t) Var.t

and arg =
  | ATy of ty
  | ARow of row

and bound =
  { row : row
  ; shadowed : String.Set.t
  }

let int = TCon ("int", [])
let bool = TCon ("bool", [])
let string = TCon ("string", [])
let char = TCon ("char", [])
let unit = TCon ("unit", [])

let canonical_ty =
  Var.canonical ~var:(function
    | TVar var -> Some var
    | TCon _ | TArrow _ | TTuple _ | TCode _ | TRecord _ | TVariant _ -> None)
;;

let canonical_row =
  Var.canonical ~var:(function
    | RVar var -> Some var
    | REmpty | RExtend _ -> None)
;;

let canonical_presence =
  Var.canonical ~var:(function
    | PVar var -> Some var
    | Present | Absent -> None)
;;

let rec tail_var row =
  match row with
  | REmpty -> None
  | RExtend (_, _, _, row') | RVar { state = Link row'; _ } -> tail_var row'
  | RVar var -> Some var
;;

module Node = struct
  (* Every cycle passes through a [Link] and a [TRecord] or [TVariant], as the
     other nodes are immutable and unification keeps cycles guarded. *)
  let same ty1 ty2 =
    phys_equal ty1 ty2
    ||
    match ty1, ty2 with
    | TRecord row1, TRecord row2 | TVariant row1, TVariant row2 ->
      (match canonical_row row1, canonical_row row2 with
       | (RExtend _ as row1), row2 -> phys_equal row1 row2
       | (REmpty | RVar _), _ -> false)
    | _ -> false
  ;;

  (* Hashes the first few immutable nodes, with variables by id, so is stable
     under binding variables. *)
  module Skeleton_hash = struct
    let combine hash x = (hash * 65599) + x

    let rec ty budget hash ty' =
      if !budget <= 0
      then hash
      else (
        decr budget;
        match ty' with
        | TVar var -> combine hash var.id
        | TCon (name, args) ->
          List.fold
            args
            ~init:(combine hash (String.hash name))
            ~f:(fun hash -> function
              | ATy ty' -> ty budget hash ty'
              | ARow row' -> row budget hash row')
        | TArrow (ty1, ty2) -> ty budget (ty budget (combine hash 1) ty1) ty2
        | TTuple tys -> List.fold tys ~init:(combine hash 2) ~f:(ty budget)
        | TCode { inner; context } ->
          row budget (ty budget (combine hash 3) inner) context
        | TRecord row' -> row budget (combine hash 4) row'
        | TVariant row' -> row budget (combine hash 5) row')

    and row budget hash row' =
      if !budget <= 0
      then hash
      else (
        decr budget;
        match row' with
        | REmpty -> combine hash 6
        | RExtend (name, presence', ty', rest) ->
          let hash = presence (combine hash (String.hash name)) presence' in
          row budget (ty budget hash ty') rest
        | RVar var -> combine hash var.id)

    and presence hash presence' =
      match presence' with
      | Present -> combine hash 7
      | Absent -> combine hash 8
      | PVar var -> combine hash var.id
    ;;

    let budget () = ref 16
  end

  let phys_hash node = Skeleton_hash.ty (Skeleton_hash.budget ()) 0 node

  let hash ty =
    match ty with
    | TRecord row | TVariant row ->
      (match canonical_row row with
       | RExtend _ as row -> Skeleton_hash.row (Skeleton_hash.budget ()) 0 row
       | REmpty | RVar _ -> phys_hash ty)
    | _ -> phys_hash ty
  ;;

  module Table = Stdlib.Hashtbl.Make (struct
      type t = ty

      let equal = same
      let hash = hash
    end)

  module Pair_set = struct
    module Table = Stdlib.Hashtbl.Make (struct
        type t = ty * ty

        let equal (ty1, ty2) (ty1', ty2') =
          (phys_equal ty1 ty1' && phys_equal ty2 ty2')
          || (phys_equal ty1 ty2' && phys_equal ty2 ty1')
        ;;

        let hash (ty1, ty2) =
          let hash1 = phys_hash ty1 in
          let hash2 = phys_hash ty2 in
          Hashtbl.hash (min hash1 hash2, max hash1 hash2)
        ;;
      end)

    type t = unit Table.t

    let create () = Table.create 16

    let add t ty1 ty2 =
      if Table.mem t (ty1, ty2)
      then `Already_present
      else (
        Table.replace t (ty1, ty2) ();
        `Added)
    ;;
  end
end

module Iter = struct
  type 'e t =
    { ty : guarded:bool -> (ty, Nothing.t) Var.t -> (unit, 'e) Result.t
    ; row : guarded:bool -> (row, bound) Var.t -> (unit, 'e) Result.t
    ; presence :
        guarded:bool -> (presence, Nothing.t) Var.t -> (unit, 'e) Result.t
    }
end

let iterator (f : _ Iter.t) =
  (* Whether each [Link] was only visited guarded: an unguarded visit covers a
     guarded one. *)
  let tys = Int.Table.create () in
  let rows = Int.Table.create () in
  let link table (var : _ Var.t) ~guarded ~visit =
    match Hashtbl.find table var.id with
    | Some was_guarded when guarded || not was_guarded -> Ok ()
    | Some _ | None ->
      Hashtbl.set table ~key:var.id ~data:guarded;
      visit ()
  in
  let rec iter_ty ~guarded ty =
    match ty with
    | TVar ({ state = Link target; _ } as var) ->
      link tys var ~guarded ~visit:(fun () -> iter_ty ~guarded target)
    | TVar var -> f.ty ~guarded var
    | TCon (_, args) ->
      List.fold_result args ~init:() ~f:(fun () -> function
        | ATy ty -> iter_ty ~guarded ty
        | ARow row -> iter_row ~guarded row)
    | TArrow (ty1, ty2) ->
      let%bind.Result () = iter_ty ~guarded ty1 in
      iter_ty ~guarded ty2
    | TTuple tys ->
      List.fold_result tys ~init:() ~f:(fun () -> iter_ty ~guarded)
    | TCode { inner; context } ->
      let%bind.Result () = iter_ty ~guarded inner in
      iter_row ~guarded context
    | TRecord row | TVariant row -> iter_row ~guarded:true row
  and iter_row ~guarded row =
    match row with
    | REmpty -> Ok ()
    | RExtend (_, presence, ty, rest) ->
      let%bind.Result () = iter_presence ~guarded presence in
      let%bind.Result () = iter_ty ~guarded ty in
      iter_row ~guarded rest
    | RVar ({ state = Link target; _ } as var) ->
      link rows var ~guarded ~visit:(fun () -> iter_row ~guarded target)
    | RVar var -> f.row ~guarded var
  and iter_presence ~guarded presence =
    match canonical_presence presence with
    | Present | Absent -> Ok ()
    | PVar var -> f.presence ~guarded var
  in
  iter_ty ~guarded:false, iter_row ~guarded:false
;;

let iter_ty f ty = fst (iterator f) ty
let iter_row f row = snd (iterator f) row

module Mapper = struct
  type t =
    { ty : (ty, Nothing.t) Var.t -> ty
    ; row : (row, bound) Var.t -> row
    ; presence : (presence, Nothing.t) Var.t -> presence
    ; keeps : 'a 'bound. ('a, 'bound) Var.t -> bool
    ; id : unit -> id
    }
end

(* Whether what a [Link] points to has a variable not satisfying [keeps],
   computed once per [Link]. The [Link]s of a strongly connected component
   (found as by Tarjan's algorithm) reach each other, so share the answer. *)
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
    ; tys : State.t Int.Table.t
    ; rows : State.t Int.Table.t
    ; stack : (State.t Int.Table.t * id * Visiting.t) Stack.t
    ; mutable next_index : int
    }

  let create (f : Mapper.t) =
    { keeps = f.keeps
    ; tys = Int.Table.create ()
    ; rows = Int.Table.create ()
    ; stack = Stack.create ()
    ; next_index = 0
    }
  ;;

  (* Whether there are changes, and the lowest index of a [Link] still being
     visited reached. *)
  let none = false, Int.max_value

  let both (changes1, low1) (changes2, low2) =
    changes1 || changes2, min low1 low2
  ;;

  let var t var = t.keeps var |> not, Int.max_value

  let link t table (var : _ Var.t) ~target =
    match Hashtbl.find table var.id with
    | Some (State.Visited changes) -> changes, Int.max_value
    | Some (Visiting { index; _ }) -> false, index
    | None ->
      let index = t.next_index in
      t.next_index <- index + 1;
      let visiting = { Visiting.index; changes = false } in
      Hashtbl.set table ~key:var.id ~data:(Visiting visiting);
      Stack.push t.stack (table, var.id, visiting);
      let changes, low = target () in
      visiting.changes <- changes;
      if low < index
      then changes, low
      else (
        let rec pop component =
          let ((table', id, _) as entry) = Stack.pop_exn t.stack in
          let component = entry :: component in
          if phys_equal table table' && id = var.id
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
      link t t.tys var' ~target:(fun () -> ty t target)
    | TVar var' -> var t var'
    | TCon (_, args) ->
      List.fold args ~init:none ~f:(fun acc -> function
        | ATy ty' -> both acc (ty t ty')
        | ARow row' -> both acc (row t row'))
    | TArrow (ty1, ty2) -> both (ty t ty1) (ty t ty2)
    | TTuple tys ->
      List.fold tys ~init:none ~f:(fun acc ty' -> both acc (ty t ty'))
    | TCode { inner; context } -> both (ty t inner) (row t context)
    | TRecord row' | TVariant row' -> row t row'

  and row t row' =
    match row' with
    | REmpty -> none
    | RExtend (_, presence, ty', rest) ->
      let presence =
        match canonical_presence presence with
        | Present | Absent -> none
        | PVar var' -> var t var'
      in
      both presence (both (ty t ty') (row t rest))
    | RVar ({ state = Link target; _ } as var') ->
      link t t.rows var' ~target:(fun () -> row t target)
    | RVar var' -> var t var'
  ;;
end

(* A record or variant copied, or being copied if there is no [result] yet. *)
module Copy = struct
  type t =
    { mutable result : ty option
    ; mutable knot : (ty, Nothing.t) Var.t option
    }
end

let mapper (f : Mapper.t) =
  let tys = Int.Table.create () in
  let rows = Int.Table.create () in
  let memo table (var : _ Var.t) ~f =
    match Hashtbl.find table var.id with
    | Some result -> result
    | None ->
      let result = f () in
      Hashtbl.set table ~key:var.id ~data:result;
      result
  in
  let changes = Changes.create f in
  let copies = Node.Table.create 16 in
  (* Unchanged parts are kept as they are. *)
  let rec map_ty ty =
    match ty with
    | TVar ({ state = Link target; _ } as var) ->
      memo tys var ~f:(fun () ->
        if fst (Changes.ty changes ty) then map_ty target else ty)
    | TVar var -> if f.keeps var then ty else f.ty var
    | TCon (name, args) ->
      let args' = List.map args ~f:map_arg in
      if List.for_all2_exn args args' ~f:phys_equal
      then ty
      else TCon (name, args')
    | TArrow (ty1, ty2) ->
      let ty1' = map_ty ty1 in
      let ty2' = map_ty ty2 in
      if phys_equal ty1 ty1' && phys_equal ty2 ty2'
      then ty
      else TArrow (ty1', ty2')
    | TTuple tys ->
      let tys' = List.map tys ~f:map_ty in
      if List.for_all2_exn tys tys' ~f:phys_equal then ty else TTuple tys'
    | TCode { inner; context } ->
      let inner' = map_ty inner in
      let context' = map_row context in
      if phys_equal inner inner' && phys_equal context context'
      then ty
      else TCode { inner = inner'; context = context' }
    | TRecord row | TVariant row ->
      (match Node.Table.find_opt copies ty with
       | Some { Copy.result = Some result; _ } -> result
       | Some copy ->
         let knot =
           match copy.knot with
           | Some knot -> knot
           | None ->
             let knot = Var.create (f.id ()) Generic in
             copy.knot <- Some knot;
             knot
         in
         TVar knot
       | None ->
         let copy = { Copy.result = None; knot = None } in
         Node.Table.replace copies ty copy;
         let row' = map_row row in
         let result =
           if phys_equal row row'
           then ty
           else (
             match ty with
             | TRecord _ -> TRecord row'
             | _ -> TVariant row')
         in
         copy.result <- Some result;
         Option.iter copy.knot ~f:(fun knot -> knot.state <- Link result);
         result)
  and map_arg arg =
    match arg with
    | ATy ty ->
      let ty' = map_ty ty in
      if phys_equal ty ty' then arg else ATy ty'
    | ARow row ->
      let row' = map_row row in
      if phys_equal row row' then arg else ARow row'
  and map_row row =
    match row with
    | REmpty -> row
    | RExtend (name, presence, ty, rest) ->
      let presence' = map_presence presence in
      let ty' = map_ty ty in
      let rest' = map_row rest in
      if
        phys_equal presence presence'
        && phys_equal ty ty'
        && phys_equal rest rest'
      then row
      else RExtend (name, presence', ty', rest')
    | RVar ({ state = Link target; _ } as var) ->
      memo rows var ~f:(fun () ->
        if fst (Changes.row changes row) then map_row target else row)
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
    (iter_ty { ty = unbound; row = unbound; presence = unbound } ty)
;;

module Sexper = struct
  type t =
    { cycles : int option ref Node.Table.t
      (* The records and variants being printed, labelled once a cycle
         reaches them again. *)
    ; mutable next_label : int
    }

  let create () = { cycles = Node.Table.create 16; next_label = 0 }

  let var (var : _ Var.t) ~sexp_of_link =
    match var.state with
    | Unbound rank -> [%sexp "Unbound", (var.id : id), (rank : rank)]
    | Generic -> [%sexp "Generic", (var.id : id)]
    | Link target -> [%sexp "Link", (sexp_of_link target : Sexp.t)]
  ;;

  let rec ty t ty' : Sexp.t =
    match ty' with
    | TCon (name, args) ->
      List [ Atom "TCon"; Atom name; List (List.map args ~f:(arg t)) ]
    | TArrow (ty1, ty2) -> List [ Atom "TArrow"; ty t ty1; ty t ty2 ]
    | TTuple tys -> List [ Atom "TTuple"; List (List.map tys ~f:(ty t)) ]
    | TCode { inner; context } ->
      List
        [ Atom "TCode"
        ; List [ Atom "inner"; ty t inner ]
        ; List [ Atom "context"; row t context ]
        ]
    | TRecord r ->
      cycle t ty' (fun () : Sexp.t -> List [ Atom "TRecord"; row t r ])
    | TVariant r ->
      cycle t ty' (fun () : Sexp.t -> List [ Atom "TVariant"; row t r ])
    | TVar v -> List [ Atom "TVar"; var v ~sexp_of_link:(ty t) ]

  and cycle t node sexp =
    match Node.Table.find_opt t.cycles node with
    | Some label ->
      let label =
        match !label with
        | Some label -> label
        | None ->
          let next = t.next_label in
          t.next_label <- next + 1;
          label := Some next;
          next
      in
      [%sexp "Cycle_ref", (label : int)]
    | None ->
      let label = ref None in
      Node.Table.replace t.cycles node label;
      let sexp = sexp () in
      Node.Table.remove t.cycles node;
      (match !label with
       | None -> sexp
       | Some label -> [%sexp "Cycle", (label : int), (sexp : Sexp.t)])

  and row t row' : Sexp.t =
    match row' with
    | REmpty -> Atom "REmpty"
    | RExtend (name, p, ty', rest) ->
      List [ Atom "RExtend"; Atom name; presence p; ty t ty'; row t rest ]
    | RVar v -> List [ Atom "RVar"; var v ~sexp_of_link:(row t) ]

  and presence p : Sexp.t =
    match p with
    | Present -> Atom "Present"
    | Absent -> Atom "Absent"
    | PVar v -> List [ Atom "PVar"; Var.sexp_of_t presence Nothing.sexp_of_t v ]

  and arg t arg' : Sexp.t =
    match arg' with
    | ATy ty' -> List [ Atom "ATy"; ty t ty' ]
    | ARow row' -> List [ Atom "ARow"; row t row' ]
  ;;
end

let sexp_of_ty ty = Sexper.ty (Sexper.create ()) ty
let sexp_of_row row = Sexper.row (Sexper.create ()) row
let sexp_of_presence = Sexper.presence
let sexp_of_arg arg = Sexper.arg (Sexper.create ()) arg

let sexp_of_bound { row; shadowed } =
  [%sexp { row : row; shadowed : String.Set.t }]
;;

module Printer = struct
  type t =
    { type_name : string -> string
    ; names : int Int.Table.t
    ; mutable next_index : int
    ; cycles : string option ref Node.Table.t
      (* The records and variants being printed, named once a cycle reaches
         them again, and those printed with their name, which later
         occurrences use. *)
    ; bounded : (string * (row, bound) Var.t) Queue.t
      (* Bounded row variables, in order of appearance, to print their
         bounds after the types. *)
    }

  let create ?(type_name = Fn.id) () =
    { type_name
    ; names = Int.Table.create ()
    ; next_index = 0
    ; cycles = Node.Table.create 16
    ; bounded = Queue.create ()
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
  let cycle_name t cycle node =
    match !cycle with
    | Some name -> name
    | None ->
      let name = name (next_index t) ~weak:(has_unbound_var node) in
      cycle := Some name;
      name
  ;;

  let row_var_name t (var : _ Var.t) =
    let first_appearance = not (Hashtbl.mem t.names var.id) in
    let name = ".." ^ var_name t var in
    if first_appearance && not (List.is_empty var.bounds)
    then Queue.enqueue t.bounded (name, var);
    name
  ;;

  let rec row_entries row =
    match canonical_row row with
    | REmpty -> [], None
    | RExtend (name, presence, ty, rest) ->
      let entries, tail = row_entries rest in
      (name, presence, ty) :: entries, tail
    | RVar var -> [], Some var
  ;;

  let sorted_row_entries t row ~label ~entry =
    let entries, tail = row_entries row in
    let entries =
      List.stable_sort entries ~compare:(fun (a, _, _) (b, _, _) ->
        String.compare a b)
      |> List.map ~f:(fun (name, presence, ty) ->
        let name = label name in
        match canonical_presence presence with
        | Present -> entry name ty
        | Absent -> "-" ^ name
        | PVar var -> entry (sprintf "%s?%s" name (var_name t var)) ty)
    in
    entries, Option.map tail ~f:(row_var_name t)
  ;;

  let rec ty_to_string t ~prec ty =
    let ty = canonical_ty ty in
    match Node.Table.find_opt t.cycles ty with
    | Some cycle -> cycle_name t cycle ty
    | None ->
      (match ty with
       | TRecord _ | TVariant _ ->
         let cycle = ref None in
         Node.Table.replace t.cycles ty cycle;
         let body = structure t ~prec ty in
         (match !cycle with
          | None ->
            Node.Table.remove t.cycles ty;
            body
          | Some name -> sprintf "(%s as %s)" body name)
       | _ -> structure t ~prec ty)

  and structure t ~prec ty =
    let parens_if cond s = if cond then "(" ^ s ^ ")" else s in
    match canonical_ty ty with
    | TCon (name, []) -> t.type_name name
    | TCon (name, args) ->
      sprintf
        "%s[%s]"
        (t.type_name name)
        (List.map args ~f:(function
           | ATy ty -> ty_to_string t ~prec:0 ty
           | ARow row -> row_to_string t row)
         |> String.concat ~sep:", ")
    | TArrow (ty1, ty2) ->
      let ty1 = ty_to_string t ~prec:1 ty1 in
      let ty2 = ty_to_string t ~prec:0 ty2 in
      parens_if (prec > 0) (sprintf "%s -> %s" ty1 ty2)
    | TTuple [] -> "()"
    | TTuple tys ->
      parens_if
        (prec > 1)
        (List.map tys ~f:(ty_to_string t ~prec:2) |> String.concat ~sep:" * ")
    | TCode { inner; context } ->
      let inner = ty_to_string t ~prec:0 inner in
      sprintf "<%s>%s" inner (row_to_string t context)
    | TRecord row -> row_to_string t row
    | TVariant row -> variant_to_string t row
    | TVar var -> var_name t var

  and variant_to_string t row =
    let entries, tail =
      sorted_row_entries
        t
        row
        ~label:(fun tag -> "`" ^ tag)
        ~entry:(fun tag ty ->
          match canonical_ty ty with
          | TCon ("unit", []) -> tag
          | ty -> sprintf "%s of %s" tag (ty_to_string t ~prec:0 ty))
    in
    let entries = String.concat entries ~sep:" | " in
    match entries, tail with
    | entries, None -> "[" ^ entries ^ "]"
    | "", Some tail -> "[" ^ tail ^ "]"
    | entries, Some tail -> "[" ^ entries ^ "; " ^ tail ^ "]"

  and row_to_string t row =
    let entries, tail =
      sorted_row_entries t row ~label:Fn.id ~entry:(fun name ty ->
        sprintf "%s : %s" name (ty_to_string t ~prec:0 ty))
    in
    "{" ^ String.concat ~sep:"; " (entries @ Option.to_list tail) ^ "}"
  ;;

  let ty t ty = ty_to_string t ~prec:0 ty

  let recursive t ty ~name =
    let node = canonical_ty ty in
    Node.Table.replace t.cycles node (ref (Some name));
    let body = structure t ~prec:0 ty in
    Node.Table.remove t.cycles node;
    body
  ;;

  let rec bounds t =
    match Queue.dequeue t.bounded with
    | None -> []
    | Some (name, var) ->
      let these =
        List.map var.bounds ~f:(fun bound ->
          sprintf "%s <= %s" name (row_to_string t bound.row))
      in
      these @ bounds t
  ;;

  let where t =
    match bounds t with
    | [] -> ""
    | bounds -> " where " ^ String.concat ~sep:", " bounds
  ;;
end

let to_string_hum ty =
  let printer = Printer.create () in
  let ty = Printer.ty printer ty in
  ty ^ Printer.where printer
;;
