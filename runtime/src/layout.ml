open! Import

module Path = struct
  module Step = struct
    type t =
      | Field of int
      | Case of int
      | Index of int
    [@@deriving sexp_of]
  end

  type t = Step.t list [@@deriving sexp_of]
end

module Leaf = struct
  type t =
    | Scalar of
        { scalar_offset : int
        ; scalar : Shape.Scalar.t
        }
    | Ptr of { ptr_index : int }
  [@@deriving sexp_of]

  let bytes = function
    | Scalar { scalar; _ } -> Shape.Scalar.bytes scalar
    | Ptr _ -> 8
  ;;

  let shift t ~scalar_offset:by_scalar ~ptr_index:by_ptr =
    match t with
    | Scalar { scalar_offset; scalar } ->
      Scalar { scalar_offset = scalar_offset + by_scalar; scalar }
    | Ptr { ptr_index } -> Ptr { ptr_index = ptr_index + by_ptr }
  ;;
end

(* Values of a leaf of [bytes], modulo 2^(8 * bytes); [None] for 8 bytes, whose
   values ints only cover in part. *)
let modulus bytes = if bytes >= 8 then None else Some (1 lsl (8 * bytes))

let wrap ~bytes v =
  match modulus bytes with
  | None -> v
  | Some m -> ((v % m) + m) % m
;;

module Niche = struct
  type t =
    { leaf : Leaf.t
    ; start : int
    ; available : int
    }
  [@@deriving sexp_of]

  let shift t ~scalar_offset ~ptr_index =
    { t with leaf = Leaf.shift t.leaf ~scalar_offset ~ptr_index }
  ;;

  let below_4096 leaf = { leaf; start = 0; available = 4096 }

  (* What is left after taking [count] values. *)
  let take t ~count =
    if t.available = count
    then None
    else
      Some
        { t with
          start = wrap ~bytes:(Leaf.bytes t.leaf) (t.start + count)
        ; available = t.available - count
        }
  ;;

  let available = function
    | None -> 0
    | Some t -> t.available
  ;;
end

module Discriminant = struct
  type t =
    | Uninhabited
    | Single of { case : int }
    | Tag of Leaf.t
    | Niche of
        { leaf : Leaf.t
        ; untagged : int
        ; first : int
        ; last : int
        ; start : int
        }
  [@@deriving sexp_of]
end

type t =
  { scalar_bytes : int
  ; scalar_align : int
  ; ptr_words : int
  ; inhabited : bool
  ; niche : Niche.t option [@sexp.option]
  ; desc : desc
  }

and desc =
  | Scalar of Shape.Scalar.t
  | Ptr
  | Struct of
      { repr : Shape.Repr.t
      ; places : place array
      ; reserved : int
      }
  | Union of
      { cases : place array
      ; discriminant : Discriminant.t
      }
  | Array of
      { elt : t
      ; length : int
      }

and place =
  { scalar_offset : int
  ; ptr_index : int
  ; layout : t
  }
[@@deriving sexp_of]

module Place = struct
  type nonrec t = place =
    { scalar_offset : int
    ; ptr_index : int
    ; layout : t
    }
  [@@deriving sexp_of]
end

module Element = struct
  type nonrec t =
    { scalar_stride : int
    ; ptr_stride : int
    ; layout : t
    }
  [@@deriving sexp_of]
end

module Write_plan = struct
  type t =
    { ptr_index : int
    ; ptr_words : int
    ; discriminant : (Leaf.t * int) option
    }
  [@@deriving sexp_of]
end

let scalar_bytes t = t.scalar_bytes
let scalar_align t = t.scalar_align
let ptr_words t = t.ptr_words
let inhabited t = t.inhabited
let niche t = t.niche
let size t = t.scalar_bytes + (8 * t.ptr_words)
let at_start layout = { scalar_offset = 0; ptr_index = 0; layout }

exception Invalid of Sexp.t

let invalid sexp = raise (Invalid sexp)

let scalar (scalar : Shape.Scalar.t) ~valid =
  let bytes = Shape.Scalar.bytes scalar in
  let leaf = Leaf.Scalar { scalar_offset = 0; scalar } in
  let niche : Niche.t option =
    match valid, scalar with
    | None, Code -> Some (Niche.below_4096 leaf)
    | None, _ -> None
    | Some _, Code -> invalid [%message "a valid range on Code"]
    | Some (min, max), _ ->
      let fits =
        match modulus bytes with
        | None -> true
        | Some m -> max < m
      in
      if min < 0 || min > max || not fits
      then
        invalid
          [%message
            "invalid range" (scalar : Shape.Scalar.t) (min : int) (max : int)];
      let available =
        match modulus bytes with
        | Some m -> m - (max - min + 1)
        | None -> Int.max_value
      in
      if available = 0
      then None
      else Some { leaf; start = wrap ~bytes (max + 1); available }
  in
  { scalar_bytes = bytes
  ; scalar_align = bytes
  ; ptr_words = 0
  ; inhabited = true
  ; niche
  ; desc = Scalar scalar
  }
;;

let ptr =
  { scalar_bytes = 0
  ; scalar_align = 1
  ; ptr_words = 1
  ; inhabited = true
  ; niche = Some (Niche.below_4096 (Ptr { ptr_index = 0 }))
  ; desc = Ptr
  }
;;

let place_niche { scalar_offset; ptr_index; layout } =
  Option.map layout.niche ~f:(Niche.shift ~scalar_offset ~ptr_index)
;;

(* The first of the largest. *)
let largest_niche places =
  Array.fold places ~init:None ~f:(fun best place ->
    match place_niche place with
    | Some niche when niche.available > Niche.available best -> Some niche
    | _ -> best)
;;

(* [reserved] bytes at the start of the scalar part are left for a tag, which
   the fields are laid out around: smallest alignment first, to fill the padding
   after it, as Rust does. *)
let struct_with (repr : Shape.Repr.t) (fields : t array) ~reserved =
  let order =
    let indices = List.init (Array.length fields) ~f:Fn.id in
    let by_align i j =
      Int.compare fields.(i).scalar_align fields.(j).scalar_align
    in
    match repr with
    | C -> indices
    | Default when reserved > 0 -> List.stable_sort indices ~compare:by_align
    | Default -> List.stable_sort indices ~compare:(fun i j -> by_align j i)
  in
  let scalar_offsets = Array.create ~len:(Array.length fields) 0 in
  let end_, scalar_align =
    List.fold order ~init:(reserved, 1) ~f:(fun (end_, align) i ->
      let field = fields.(i) in
      let offset = round_up end_ ~align:field.scalar_align in
      scalar_offsets.(i) <- offset;
      offset + field.scalar_bytes, Int.max align field.scalar_align)
  in
  let ptr_words = ref 0 in
  let places =
    Array.mapi fields ~f:(fun i layout ->
      let ptr_index = !ptr_words in
      ptr_words := !ptr_words + layout.ptr_words;
      { scalar_offset = scalar_offsets.(i); ptr_index; layout })
  in
  (match repr with
   | C when !ptr_words > 0 -> invalid [%message "repr C with pointers"]
   | C | Default -> ());
  { scalar_bytes = round_up end_ ~align:scalar_align
  ; scalar_align
  ; ptr_words = !ptr_words
  ; inhabited = Array.for_all fields ~f:(fun field -> field.inhabited)
  ; niche = largest_niche places
  ; desc = Struct { repr; places; reserved }
  }
;;

let struct_ repr fields = struct_with repr (Array.of_list fields) ~reserved:0

let array elt ~length =
  if length < 0 then invalid [%message "negative array length" (length : int)];
  { scalar_bytes = length * elt.scalar_bytes
  ; scalar_align = elt.scalar_align
  ; ptr_words = length * elt.ptr_words
  ; inhabited = length = 0 || elt.inhabited
  ; niche = (if length > 0 then elt.niche else None)
  ; desc = Array { elt; length }
  }
;;

(* The layout of a union whose cases are at [places]. *)
let of_places places ~discriminant ~min_scalar_bytes ~min_align ~niche =
  let end_, scalar_align, ptr_words =
    Array.fold
      places
      ~init:(min_scalar_bytes, min_align, 0)
      ~f:(fun (end_, align, ptr_words) { scalar_offset; ptr_index; layout } ->
        ( Int.max end_ (scalar_offset + layout.scalar_bytes)
        , Int.max align layout.scalar_align
        , Int.max ptr_words (ptr_index + layout.ptr_words) ))
  in
  { scalar_bytes = round_up end_ ~align:scalar_align
  ; scalar_align
  ; ptr_words
  ; inhabited = Array.exists places ~f:(fun place -> place.layout.inhabited)
  ; niche
  ; desc = Union { cases = places; discriminant }
  }
;;

let tag_of_count count : Shape.Scalar.t =
  if count <= 1 lsl 8 then I8 else if count <= 1 lsl 16 then I16 else I32
;;

let tag_niche leaf ~count ~(tag : Shape.Scalar.t) : Niche.t option =
  let available = (1 lsl (8 * Shape.Scalar.bytes tag)) - count in
  if available > 0 then Some { leaf; start = count; available } else None
;;

(* A Default struct case is laid out around the tag, where that is smaller. *)
let after_tag layout ~tag_bytes =
  let plain =
    { scalar_offset = round_up tag_bytes ~align:layout.scalar_align
    ; ptr_index = 0
    ; layout
    }
  in
  match layout.desc with
  | Struct { repr = Default; places; reserved = 0 } ->
    let around =
      struct_with
        Default
        (Array.map places ~f:(fun place -> place.layout))
        ~reserved:tag_bytes
    in
    if around.scalar_bytes < plain.scalar_offset + layout.scalar_bytes
    then at_start around
    else plain
  | Scalar _ | Ptr | Struct _ | Union _ | Array _ -> plain
;;

let tagged cases ~present =
  let count = Array.length cases in
  let tag = tag_of_count count in
  let tag_bytes = Shape.Scalar.bytes tag in
  let leaf = Leaf.Scalar { scalar_offset = 0; scalar = tag } in
  let places =
    Array.mapi cases ~f:(fun i layout ->
      if present.(i) then after_tag layout ~tag_bytes else at_start layout)
  in
  of_places
    places
    ~discriminant:(Tag leaf)
    ~min_scalar_bytes:tag_bytes
    ~min_align:tag_bytes
    ~niche:(tag_niche leaf ~count ~tag)
;;

(* As C: a 32 bit tag, then every case at the same offset. *)
let c_tagged cases =
  let count = Array.length cases in
  let tag : Shape.Scalar.t = I32 in
  let tag_bytes = Shape.Scalar.bytes tag in
  let leaf = Leaf.Scalar { scalar_offset = 0; scalar = tag } in
  let align =
    Array.fold cases ~init:1 ~f:(fun acc case -> Int.max acc case.scalar_align)
  in
  let scalar_offset = round_up tag_bytes ~align in
  of_places
    (Array.map cases ~f:(fun layout -> { scalar_offset; ptr_index = 0; layout }))
    ~discriminant:(Tag leaf)
    ~min_scalar_bytes:tag_bytes
    ~min_align:tag_bytes
    ~niche:(tag_niche leaf ~count ~tag)
;;

(* With [untagged] stored as itself, the other present cases as values of its
   niche, placed so as not to overlap it. *)
let niche_filled_with cases ~present ~untagged =
  let niche = Option.value_exn cases.(untagged).niche in
  let others =
    List.filter
      (List.init (Array.length cases) ~f:Fn.id)
      ~f:(fun i -> present.(i) && i <> untagged)
  in
  let first = List.hd_exn others in
  let last = List.last_exn others in
  let used = last - first + 1 in
  if used > niche.available
  then None
  else (
    let places =
      Array.mapi cases ~f:(fun i layout ->
        if i = untagged || not present.(i)
        then at_start layout
        else (
          match niche.leaf with
          | Scalar { scalar_offset; scalar } ->
            { scalar_offset =
                (if layout.scalar_bytes <= scalar_offset
                 then 0
                 else
                   round_up
                     (scalar_offset + Shape.Scalar.bytes scalar)
                     ~align:layout.scalar_align)
            ; ptr_index = 0
            ; layout
            }
          | Ptr { ptr_index } ->
            { scalar_offset = 0
            ; ptr_index =
                (if layout.ptr_words <= ptr_index then 0 else ptr_index + 1)
            ; layout
            }))
    in
    Some
      (of_places
         places
         ~discriminant:
           (Niche
              { leaf = niche.leaf; untagged; first; last; start = niche.start })
         ~min_scalar_bytes:0
         ~min_align:1
         ~niche:(Niche.take niche ~count:used)))
;;

let key t = size t, -Niche.available t.niche

(* Every present case with a niche is tried as the untagged one: in a split
   layout the best need not be the largest, as Rust's is. *)
let niche_filled cases ~present =
  Array.foldi cases ~init:None ~f:(fun untagged best case ->
    if not (present.(untagged) && Option.is_some case.niche)
    then best
    else (
      match best, niche_filled_with cases ~present ~untagged with
      | _, None -> best
      | None, Some t -> Some t
      | Some best_t, Some t ->
        if [%compare: int * int] (key t) (key best_t) < 0 then Some t else best))
;;

let union (repr : Shape.Repr.t) cases =
  let cases = Array.of_list cases in
  match repr with
  | C ->
    if Array.is_empty cases then invalid [%message "repr C union with no cases"];
    if Array.exists cases ~f:(fun case -> case.ptr_words > 0)
    then invalid [%message "repr C with pointers"];
    c_tagged cases
  | Default ->
    (* An uninhabited case of no size is never stored, so takes no room. One
       with a size is laid out, as its fields may be written. *)
    let present =
      Array.map cases ~f:(fun case -> case.inhabited || size case > 0)
    in
    (match
       List.filter
         (List.init (Array.length cases) ~f:Fn.id)
         ~f:(fun i -> present.(i))
     with
     | [] ->
       { scalar_bytes = 0
       ; scalar_align = 1
       ; ptr_words = 0
       ; inhabited = false
       ; niche = None
       ; desc =
           Union
             { cases = Array.map cases ~f:at_start; discriminant = Uninhabited }
       }
     | [ case ] ->
       let layout = cases.(case) in
       { layout with
         desc =
           Union
             { cases = Array.map cases ~f:at_start
             ; discriminant = Single { case }
             }
       }
     | _ ->
       let tagged = tagged cases ~present in
       (match niche_filled cases ~present with
        | Some niche_filled
          when [%compare: int * int] (key niche_filled) (key tagged) < 0 ->
          niche_filled
        | Some _ | None -> tagged))
;;

let rec of_shape_exn : Shape.t -> t = function
  | Scalar { scalar = s; valid } -> scalar s ~valid
  | Ptr -> ptr
  | Struct { repr; fields } -> struct_ repr (List.map fields ~f:of_shape_exn)
  | Union { repr; cases } -> union repr (List.map cases ~f:of_shape_exn)
  | Array { elt; length } -> array (of_shape_exn elt) ~length
;;

let of_shape shape =
  match of_shape_exn shape with
  | t -> Ok t
  | exception Invalid sexp ->
    Or_error.error_s [%message "Layout.of_shape" ~_:(sexp : Sexp.t)]
;;

let step t (step : Path.Step.t) =
  let out_of_range () =
    raise_s [%message "Layout.place: no such step" (step : Path.Step.t) (t : t)]
  in
  match step, t.desc with
  | Field i, Struct { places; _ } when i >= 0 && i < Array.length places ->
    places.(i)
  | Case i, Union { cases; _ } when i >= 0 && i < Array.length cases ->
    cases.(i)
  | Index i, Array { elt; length } when i >= 0 && i < length ->
    { scalar_offset = i * elt.scalar_bytes
    ; ptr_index = i * elt.ptr_words
    ; layout = elt
    }
  | (Field _ | Case _ | Index _), _ -> out_of_range ()
;;

let place t path =
  List.fold path ~init:(at_start t) ~f:(fun outer s ->
    let inner = step outer.layout s in
    { scalar_offset = outer.scalar_offset + inner.scalar_offset
    ; ptr_index = outer.ptr_index + inner.ptr_index
    ; layout = inner.layout
    })
;;

let addressable t path =
  let { layout; _ } = place t path in
  (layout.ptr_words = 0 || layout.scalar_bytes = 0)
  &&
  match layout.desc with
  | Struct { reserved; _ } -> reserved = 0
  | Scalar _ | Ptr | Union _ | Array _ -> true
;;

let element t : Element.t option =
  match t.desc with
  | Array { elt; _ } ->
    Some
      { scalar_stride = elt.scalar_bytes
      ; ptr_stride = elt.ptr_words
      ; layout = elt
      }
  | Scalar _ | Ptr | Struct _ | Union _ -> None
;;

let discriminant t =
  match t.desc with
  | Union { discriminant; _ } -> Some discriminant
  | Scalar _ | Ptr | Struct _ | Array _ -> None
;;

let write_plan t path ~case : Write_plan.t =
  let at = place t path in
  let fail message =
    raise_s
      [%message
        "Layout.write_plan" message (path : Path.t) (case : int) (t : t)]
  in
  match at.layout.desc with
  | Union { cases; discriminant } ->
    if case < 0 || case >= Array.length cases then fail "no such case";
    let layout = cases.(case).layout in
    if (not layout.inhabited) && size layout = 0
    then fail "a case that is never stored";
    let shift leaf =
      Leaf.shift leaf ~scalar_offset:at.scalar_offset ~ptr_index:at.ptr_index
    in
    let discriminant =
      match discriminant with
      | Uninhabited -> fail "uninhabited"
      | Single { case = only } ->
        if case <> only then fail "a case that is never stored";
        None
      | Tag leaf -> Some (shift leaf, case)
      | Niche { leaf; untagged; first; last; start } ->
        if case = untagged
        then None
        else if case < first || case > last
        then fail "a case that is never stored"
        else
          Some (shift leaf, wrap ~bytes:(Leaf.bytes leaf) (start + case - first))
    in
    { ptr_index = at.ptr_index; ptr_words = at.layout.ptr_words; discriminant }
  | Scalar _ | Ptr | Struct _ | Array _ -> fail "not a union"
;;
