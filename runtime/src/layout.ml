open! Import

module Path = struct
  type step =
    | Field of int
    | Case of int
    | Index of int
  [@@deriving sexp_of]

  type t = step list [@@deriving sexp_of]
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
    | Single
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
  ; niche : Niche.t option [@sexp.option]
  ; desc : desc
  }

and desc =
  | Scalar of Shape.Scalar.t
  | Ptr
  | Struct of place array
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

let scalar_bytes t = t.scalar_bytes
let scalar_align t = t.scalar_align
let ptr_words t = t.ptr_words
let niche t = t.niche
let round_up n ~align = (n + align - 1) / align * align
let size t = t.scalar_bytes + (8 * t.ptr_words)

exception Invalid of Sexp.t

let invalid sexp = raise (Invalid sexp)

let scalar scalar ~valid =
  let bytes = Shape.Scalar.bytes scalar in
  let leaf = Leaf.Scalar { scalar_offset = 0; scalar } in
  let niche : Niche.t option =
    match valid, scalar with
    | None, Code -> Some (Niche.below_4096 leaf)
    | None, _ -> None
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
  ; niche
  ; desc = Scalar scalar
  }
;;

let ptr =
  { scalar_bytes = 0
  ; scalar_align = 1
  ; ptr_words = 1
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

let struct_ (repr : Shape.Repr.t) fields =
  let fields = Array.of_list fields in
  let order =
    let indices = List.init (Array.length fields) ~f:Fn.id in
    match repr with
    | C -> indices
    | Default ->
      List.stable_sort indices ~compare:(fun i j ->
        Int.compare fields.(j).scalar_align fields.(i).scalar_align)
  in
  let scalar_offsets = Array.create ~len:(Array.length fields) 0 in
  let end_, scalar_align =
    List.fold order ~init:(0, 1) ~f:(fun (end_, align) i ->
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
  ; niche = largest_niche places
  ; desc = Struct places
  }
;;

let array elt ~length =
  if length < 0 then invalid [%message "negative array length" (length : int)];
  { scalar_bytes = length * elt.scalar_bytes
  ; scalar_align = elt.scalar_align
  ; ptr_words = length * elt.ptr_words
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
  ; niche
  ; desc = Union { cases = places; discriminant }
  }
;;

let tagged cases =
  let count = Array.length cases in
  let tag : Shape.Scalar.t =
    if count <= 1 lsl 8 then I8 else if count <= 1 lsl 16 then I16 else I32
  in
  let tag_bytes = Shape.Scalar.bytes tag in
  let leaf = Leaf.Scalar { scalar_offset = 0; scalar = tag } in
  let places =
    Array.map cases ~f:(fun layout ->
      { scalar_offset = round_up tag_bytes ~align:layout.scalar_align
      ; ptr_index = 0
      ; layout
      })
  in
  let available = (1 lsl (8 * tag_bytes)) - count in
  of_places
    places
    ~discriminant:(Tag leaf)
    ~min_scalar_bytes:tag_bytes
    ~min_align:tag_bytes
    ~niche:
      (if available > 0 then Some { leaf; start = count; available } else None)
;;

let niche_filled cases =
  let largest =
    Array.fold cases ~init:0 ~f:(fun acc case -> Int.max acc (size case))
  in
  let untagged =
    Array.foldi cases ~init:None ~f:(fun i best case ->
      match best, case.niche with
      | _, None -> best
      | _, Some _ when size case < largest -> best
      | Some (_, (best_niche : Niche.t)), Some niche
        when niche.available <= best_niche.available -> best
      | _, Some niche -> Some (i, niche))
  in
  Option.bind untagged ~f:(fun (untagged, (niche : Niche.t)) ->
    let count = Array.length cases in
    let first = if untagged = 0 then 1 else 0 in
    let last = if untagged = count - 1 then count - 2 else count - 1 in
    let used = last - first + 1 in
    if used > niche.available
    then None
    else (
      let places =
        Array.mapi cases ~f:(fun i layout ->
          if i = untagged
          then { scalar_offset = 0; ptr_index = 0; layout }
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
                { leaf = niche.leaf
                ; untagged
                ; first
                ; last
                ; start = niche.start
                })
           ~min_scalar_bytes:0
           ~min_align:1
           ~niche:(Niche.take niche ~count:used))))
;;

let union cases =
  let cases = Array.of_list cases in
  match cases with
  | [||] ->
    { scalar_bytes = 0
    ; scalar_align = 1
    ; ptr_words = 0
    ; niche = None
    ; desc = Union { cases = [||]; discriminant = Uninhabited }
    }
  | [| layout |] ->
    { layout with
      desc =
        Union
          { cases = [| { scalar_offset = 0; ptr_index = 0; layout } |]
          ; discriminant = Single
          }
    }
  | _ ->
    let tagged = tagged cases in
    (match niche_filled cases with
     | None -> tagged
     | Some niche_filled ->
       let key t = size t, -Niche.available t.niche in
       if [%compare: int * int] (key niche_filled) (key tagged) < 0
       then niche_filled
       else tagged)
;;

let rec of_shape_exn : Shape.t -> t = function
  | Scalar { scalar = s; valid } -> scalar s ~valid
  | Ptr -> ptr
  | Struct { repr; fields } -> struct_ repr (List.map fields ~f:of_shape_exn)
  | Union cases -> union (List.map cases ~f:of_shape_exn)
  | Array { elt; length } -> array (of_shape_exn elt) ~length
;;

let of_shape shape =
  match of_shape_exn shape with
  | t -> Ok t
  | exception Invalid sexp ->
    Or_error.error_s [%message "Layout.of_shape" ~_:(sexp : Sexp.t)]
;;

let step t (step : Path.step) =
  let out_of_range () =
    raise_s [%message "Layout.place: no such step" (step : Path.step) (t : t)]
  in
  match step, t.desc with
  | Field i, Struct places when i >= 0 && i < Array.length places -> places.(i)
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
  List.fold
    path
    ~init:{ scalar_offset = 0; ptr_index = 0; layout = t }
    ~f:(fun outer s ->
      let inner = step outer.layout s in
      { scalar_offset = outer.scalar_offset + inner.scalar_offset
      ; ptr_index = outer.ptr_index + inner.ptr_index
      ; layout = inner.layout
      })
;;

let addressable t path =
  let { layout; _ } = place t path in
  layout.ptr_words = 0 || layout.scalar_bytes = 0
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
