open! Import

module Counterexample = struct
  type t = Syntax.Pattern.t

  let to_string t = Printer.pattern t
  let sexp_of_t t = Sexp.Atom (to_string t)
  let make desc : t = Located.create desc
  let any = make Any
  let name name = Located.create (Syntax.Decl_path.Path (Syntax.Path.name name))

  let is_any (t : t) =
    match t.txt with
    | Any -> true
    | _ -> false
  ;;
end

module Result = struct
  type t =
    { missing : Counterexample.t option
    ; unused : int list
    }
  [@@deriving sexp_of]
end

(* The tags a polymorphic variant may have, each once, and whether it may have
   others. *)
module Variant_row = struct
  type t =
    { tags : (string * Type.ty) list
    ; open_ : bool
    }

  let of_ty ty =
    match Type.canonical_ty ty with
    | TVariant { row; _ } ->
      let rec loop row ~seen ~tags =
        match Type.canonical_row row with
        | Type.REmpty -> { tags = List.rev tags; open_ = false }
        | RVar _ -> { tags = List.rev tags; open_ = true }
        | RExtend (tag, presence, _, ty, rest) ->
          let shadowed = Set.mem seen tag in
          let seen = Set.add seen tag in
          (match shadowed, Type.canonical_presence presence with
           | true, _ | false, Absent -> loop rest ~seen ~tags
           | false, (Present | PVar _) ->
             loop rest ~seen ~tags:((tag, ty) :: tags))
      in
      loop row ~seen:String.Set.empty ~tags:[]
    | _ -> { tags = []; open_ = true }
  ;;

  let has_unit_arg t tag =
    match List.Assoc.find t.tags tag ~equal:String.equal with
    | Some ty -> Type.same_ty ty Type.unit
    | None -> false
  ;;
end

(* What a pattern requires of the outermost constructor of a value. *)
module Head = struct
  type t =
    | Constructor of
        { constructor : Typed.Constructor_ref.t
        ; arity : int
        }
    | Lit of Syntax.Lit.t
    | Tuple of int
    | Deref
    | Record of Typed.Record_type.t
    | Poly_variant of
        { tag : string
        ; ty : Type.ty
        }
    | Poly_record of string list
    (** The labels of every pattern of the column. *)

  let equal a b =
    match a, b with
    | Constructor a, Constructor b ->
      String.equal a.constructor.name b.constructor.name
    | Lit a, Lit b -> Syntax.Lit.equal a b
    | Poly_variant a, Poly_variant b -> String.equal a.tag b.tag
    | Tuple _, Tuple _
    | Deref, Deref
    | Record _, Record _
    | Poly_record _, Poly_record _ -> true
    | ( ( Constructor _
        | Lit _
        | Tuple _
        | Deref
        | Record _
        | Poly_variant _
        | Poly_record _ )
      , _ ) -> false
  ;;

  let arity = function
    | Constructor { arity; _ } -> arity
    | Lit _ -> 0
    | Tuple arity -> arity
    | Deref | Poly_variant _ -> 1
    | Record record -> Array.length record.fields
    | Poly_record labels -> List.length labels
  ;;

  let counterexample t (args : Counterexample.t list) ~name : Counterexample.t =
    let short = name in
    let relevant fields =
      List.filter fields ~f:(fun (_, arg) -> not (Counterexample.is_any arg))
    in
    match t, args with
    | Constructor { constructor; _ }, args ->
      Counterexample.make
        (Construct (Counterexample.name (short constructor.name), args))
    | Lit lit, _ -> Counterexample.make (Lit lit)
    | Tuple _, args -> Counterexample.make (Tuple args)
    | Deref, [ arg ] -> Counterexample.make (Deref arg)
    | Poly_variant { tag; ty }, [ arg ] ->
      let arg =
        match arg.txt with
        | Lit Unit -> None
        | Any when Variant_row.has_unit_arg (Variant_row.of_ty ty) tag -> None
        | _ -> Some arg
      in
      Counterexample.make (Poly_variant (Located.create tag, arg))
    | Record record, args ->
      (match
         relevant
           (List.zip_exn
              (Array.to_list record.fields |> List.map ~f:short)
              args)
       with
       | [] -> Counterexample.any
       | fields ->
         Counterexample.make
           (Record
              (List.map fields ~f:(fun (field, arg) ->
                 Counterexample.name field, arg))))
    | Poly_record labels, args ->
      (match relevant (List.zip_exn labels args) with
       | [] -> Counterexample.any
       | fields ->
         Counterexample.make
           (Poly_record
              { fields =
                  List.map fields ~f:(fun (label, arg) ->
                    Located.create label, arg)
              ; rest = Open
              }))
    | (Deref | Poly_variant _), _ ->
      raise_s [%message "Exhaustiveness: arity mismatch"]
  ;;
end

module Pattern = struct
  type t =
    | Wild
    | Or of t * t
    | Con of Head.t * t list
    | Poly_record of (string * t) list

  let rec of_typed ({ desc; ty; _ } : Typed.Pattern.t) =
    match desc with
    | Any | Var _ | Error _ -> Wild
    | Or (a, b) -> Or (of_typed a, of_typed b)
    | Lit lit -> Con (Lit lit, [])
    | Tuple ps -> Con (Tuple (List.length ps), List.map ps ~f:of_typed)
    | Deref p -> Con (Deref, [ of_typed p ])
    | Construct (constructor, args) ->
      Con
        ( Constructor { constructor; arity = List.length args }
        , List.map args ~f:of_typed )
    | Record [] -> Wild
    | Record (((field : Typed.Field_ref.t), _) :: _ as fields) ->
      let args = Array.create ~len:(Array.length field.record.fields) Wild in
      List.iter fields ~f:(fun ((field : Typed.Field_ref.t), p) ->
        args.(field.index) <- of_typed p);
      Con (Record field.record, Array.to_list args)
    | Poly_variant (tag, arg) ->
      Con
        ( Poly_variant { tag; ty }
        , [ Option.value_map arg ~default:(Con (Lit Unit, [])) ~f:of_typed ] )
    | Poly_record { fields; rest = _ } ->
      Poly_record (List.map fields ~f:(fun (label, p) -> label, of_typed p))
  ;;

  let wilds n = List.init n ~f:(fun _ -> Wild)
end

module Matrix = struct
  type t = Pattern.t list list

  (* The distinct heads of the first column, in order. *)
  let heads (column : Pattern.t list) =
    let rec add (heads, labels) : Pattern.t -> _ = function
      | Wild -> heads, labels
      | Or (a, b) -> add (add (heads, labels) a) b
      | Con (head, _) ->
        if List.mem heads head ~equal:Head.equal
        then heads, labels
        else head :: heads, labels
      | Poly_record fields ->
        heads, Some (List.map fields ~f:fst @ Option.value labels ~default:[])
    in
    let heads, labels = List.fold column ~init:([], None) ~f:add in
    let heads = List.rev heads in
    match labels with
    | None -> heads
    | Some labels ->
      heads
      @ [ Head.Poly_record (List.dedup_and_sort labels ~compare:String.compare)
        ]
  ;;

  (* The arguments of each alternative of [p] matching [head]. *)
  let rec args head (p : Pattern.t) =
    let arity = Head.arity head in
    match p, head with
    | Wild, _ -> [ Pattern.wilds arity ]
    | Or (a, b), _ -> args head a @ args head b
    | Con (head', args), _ when Head.equal head head' ->
      if List.length args = arity then [ args ] else [ Pattern.wilds arity ]
    | Poly_record fields, Poly_record labels ->
      [ List.map labels ~f:(fun label ->
          List.Assoc.find fields label ~equal:String.equal
          |> Option.value ~default:Pattern.Wild)
      ]
    | (Con _ | Poly_record _), _ -> []
  ;;

  let specialize head (t : t) : t =
    List.concat_map t ~f:(function
      | [] -> []
      | p :: rest -> List.map (args head p) ~f:(fun args -> args @ rest))
  ;;

  let default (t : t) : t =
    let rec rows (p : Pattern.t) rest =
      match p with
      | Wild -> [ rest ]
      | Or (a, b) -> rows a rest @ rows b rest
      | Con _ | Poly_record _ -> []
    in
    List.concat_map t ~f:(function
      | [] -> []
      | p :: rest -> rows p rest)
  ;;
end

module Signature = struct
  let missing_lit (lits : Syntax.Lit.t list) (sample : Syntax.Lit.t)
    : Syntax.Lit.t option
    =
    let absent lit = not (List.mem lits lit ~equal:Syntax.Lit.equal) in
    let first candidates = Sequence.find_exn candidates ~f:absent in
    match sample with
    | Unit -> None
    | Bool _ -> List.find [ Syntax.Lit.Bool true; Bool false ] ~f:absent
    | Int _ ->
      Some
        (first
           (Sequence.map (Sequence.range 0 Int.max_value) ~f:Syntax.Lit.int))
    | Char _ ->
      Some
        (first
           (Sequence.append
              (Sequence.range (Char.to_int 'a') (Char.to_int 'z' + 1))
              (Sequence.range 0 256)
            |> Sequence.map ~f:(fun c -> Syntax.Lit.Char (Char.of_int_exn c))))
    | String _ ->
      Some
        (first
           (Sequence.map (Sequence.range 0 Int.max_value) ~f:(fun n ->
              Syntax.Lit.String (String.make n 'a'))))
  ;;

  (* [None] if [heads], those of a column, has every constructor of its type,
     else a value of the type none of them match. *)
  let missing ~variant_constructors ~name (heads : Head.t list)
    : Counterexample.t option
    =
    let mem head = List.mem heads head ~equal:Head.equal in
    match heads with
    | [] -> Some Counterexample.any
    | (Tuple _ | Deref | Record _ | Poly_record _) :: _ -> None
    | Lit sample :: _ ->
      let lits =
        List.filter_map heads ~f:(function
          | Lit lit -> Some lit
          | _ -> None)
      in
      missing_lit lits sample
      |> Option.map ~f:(fun lit -> Counterexample.make (Lit lit))
    | Constructor { constructor; _ } :: _ ->
      if Typed.Constructor_ref.is_exception constructor
      then Some Counterexample.any
      else (
        match variant_constructors constructor.type_name with
        | None -> Some Counterexample.any
        | Some constructors ->
          List.find_map constructors ~f:(fun (constructor, arity) ->
            let head = Head.Constructor { constructor; arity } in
            if mem head
            then None
            else
              Some
                (Head.counterexample
                   ~name
                   head
                   (List.init arity ~f:(fun _ -> Counterexample.any)))))
    | Poly_variant { ty; _ } :: _ ->
      let row = Variant_row.of_ty ty in
      (match
         List.find row.tags ~f:(fun (tag, _) ->
           not (mem (Poly_variant { tag; ty })))
       with
       | Some (tag, _) ->
         Some
           (Head.counterexample
              ~name
              (Poly_variant { tag; ty })
              [ Counterexample.any ])
       | None -> Option.some_if row.open_ Counterexample.any)
  ;;
end

module Context = struct
  type t =
    { variant_constructors :
        string -> (Typed.Constructor_ref.t * int) list option
    ; name : string -> string
    }

  let column (rows : Matrix.t) = List.filter_map rows ~f:List.hd

  (* Whether a value matches [q] but no row of [rows]. *)
  let rec useful t (rows : Matrix.t) (q : Pattern.t list) =
    match q with
    | _ when List.is_empty rows -> true
    | [] -> false
    | Or (a, b) :: rest -> useful t rows (a :: rest) || useful t rows (b :: rest)
    | ((Con _ | Poly_record _) as p) :: rest ->
      let head =
        match p with
        | Con (head, _) -> head
        | _ -> List.last_exn (Matrix.heads (p :: column rows))
      in
      let rows = Matrix.specialize head rows in
      List.exists (Matrix.args head p) ~f:(fun args ->
        useful t rows (args @ rest))
    | Wild :: rest ->
      let heads = Matrix.heads (column rows) in
      (match
         Signature.missing
           ~variant_constructors:t.variant_constructors
           ~name:t.name
           heads
       with
       | None ->
         List.exists heads ~f:(fun head ->
           useful
             t
             (Matrix.specialize head rows)
             (Pattern.wilds (Head.arity head) @ rest))
       | Some _ -> useful t (Matrix.default rows) rest)
  ;;

  (* [n] patterns, one per column, of a value no row of [rows] matches. *)
  let rec counterexample t (rows : Matrix.t) n =
    if List.is_empty rows
    then Some (List.init n ~f:(fun _ -> Counterexample.any))
    else if n = 0
    then None
    else (
      let heads = Matrix.heads (column rows) in
      match
        Signature.missing
          ~variant_constructors:t.variant_constructors
          ~name:t.name
          heads
      with
      | None ->
        List.find_map heads ~f:(fun head ->
          let arity = Head.arity head in
          counterexample t (Matrix.specialize head rows) (arity + n - 1)
          |> Option.map ~f:(fun patterns ->
            let #(args, rest) = List.split_n patterns arity in
            Head.counterexample head args ~name:t.name :: rest))
      | Some missing ->
        counterexample t (Matrix.default rows) (n - 1)
        |> Option.map ~f:(fun rest -> missing :: rest))
  ;;

  let unused t patterns =
    List.foldi patterns ~init:([], []) ~f:(fun i (rows, unused) p ->
      let row = [ Pattern.of_typed p ] in
      row :: rows, if useful t rows row then unused else i :: unused)
    |> snd
    |> List.rev
  ;;
end

let is_empty_variant ~variant_constructors scrutinee =
  match Type.canonical_ty scrutinee with
  | TCon (name, _) ->
    (match variant_constructors name with
     | Some [] -> true
     | Some (_ :: _) | None -> false)
  | TVariant _ ->
    (match Variant_row.of_ty scrutinee with
     | { tags = []; open_ = false } -> true
     | { tags = _ :: _; _ } | { open_ = true; _ } -> false)
  | _ -> false
;;

let check_match ~variant_constructors ~name ~scrutinee patterns =
  let t = { Context.variant_constructors; name } in
  let missing =
    if List.is_empty patterns
       && is_empty_variant ~variant_constructors scrutinee
    then None
    else
      Context.counterexample
        t
        (List.map patterns ~f:(fun p -> [ Pattern.of_typed p ]))
        1
      |> Option.map ~f:List.hd_exn
  in
  { Result.missing; unused = Context.unused t patterns }
;;

let check_exception_arms ~variant_constructors patterns =
  Context.unused { variant_constructors; name = Fn.id } patterns
;;
