open! Import
open Layout_helpers

(* Random shapes, laid out; random values of them, written into a model of
   memory (scalar bytes and pointer slots, initially garbage) by their layout,
   unions by [Layout.write_plan], and read back. Writing checks each place is in
   bounds and that no two parts of a value share memory (including
   discriminants), and reading that the discriminants tell the cases apart,
   without relying on what was there before. *)

module Value = struct
  type t =
    | Scalar of int
    | Ptr of int
    | Struct of t list
    | Union of int * t
    | Array of t list
  [@@deriving sexp_of, equal]
end

let ranged random (s : Shape.Scalar.t) =
  let bits = Int.min 62 (8 * Shape.Scalar.bytes s) in
  let top = (1 lsl bits) - 1 in
  match Random.State.int random 3 with
  | 0 -> scalar s ~valid:(0, Random.State.int random 3)
  | 1 ->
    let min = Random.State.int random (Int.min top 1000) in
    scalar s ~valid:(min, min + Random.State.int random 1000)
  | _ -> scalar s ~valid:(top - Random.State.int random 1000, top)
;;

let rec gen_shape random ~depth : Shape.t =
  let leaf () : Shape.t =
    match Random.State.int random 10 with
    | 0 -> ptr
    | 1 -> code
    | 2 -> scalar I8 ~valid:(0, Random.State.int random 3)
    | 3 ->
      let min = Random.State.int random 100 in
      scalar I8 ~valid:(min, min + Random.State.int random 100)
    | 4 ->
      if Random.State.bool random
      then scalar I32 ~valid:(0, 0x10FFFF)
      else
        (* Its niche, from near the top, wraps round to 0. *)
        scalar
          I8
          ~valid:
            (10 + Random.State.int random 50, 252 + Random.State.int random 3)
    | 5 ->
      ranged
        random
        (List.random_element_exn
           ~random_state:random
           [ Shape.Scalar.I16; I32; I64 ])
    | _ ->
      scalar
        (List.random_element_exn
           ~random_state:random
           [ Shape.Scalar.I8; I16; I32; I64; F32; F64 ])
  in
  let children n =
    List.init n ~f:(fun _ -> gen_shape random ~depth:(depth - 1))
  in
  let repr () : Shape.Repr.t =
    if Random.State.int random 6 = 0 then C else Default
  in
  if depth = 0
  then leaf ()
  else (
    match Random.State.int random 10 with
    | 0 | 1 | 2 -> leaf ()
    | 3 | 4 ->
      Struct { repr = repr (); fields = children (Random.State.int random 4) }
    | 5 | 6 | 7 ->
      Union { repr = repr (); cases = children (Random.State.int random 5) }
    | 8 when Random.State.bool random ->
      (* A leaf and empty cases: niche-filled in its leaf. *)
      union
        (leaf () :: List.init (1 + Random.State.int random 4) ~f:(fun _ -> unit))
    | 8 ->
      enum
        (1
         + Random.State.int random (if Random.State.bool random then 4 else 300)
        )
    | _ ->
      Array
        { elt = gen_shape random ~depth:(depth - 1)
        ; length = Random.State.int random 3
        })
;;

let scalar_bytes (scalar : Shape.Scalar.t) = Shape.Scalar.bytes scalar

(* Values that fit in an [int], including the edges of valid ranges. *)
let gen_scalar random (scalar : Shape.Scalar.t) ~valid =
  match valid, scalar with
  | Some (min, max), _ ->
    (match Random.State.int random 4 with
     | 0 -> min
     | 1 -> max
     | _ -> min + Random.State.int random (Int.min (max - min + 1) (1 lsl 30)))
  | None, Code -> 4096 + Random.State.int random 1_000_000
  | None, _ ->
    let bits = Int.min 30 (8 * scalar_bytes scalar) in
    (match Random.State.int random 4 with
     | 0 -> 0
     | 1 -> (1 lsl bits) - 1
     | _ -> Random.State.int random (1 lsl bits))
;;

let rec gen_value random (shape : Shape.t) : Value.t option =
  match shape with
  | Scalar { scalar; valid } -> Some (Scalar (gen_scalar random scalar ~valid))
  | Ptr -> Some (Ptr (4096 + (8 * Random.State.int random 1_000_000)))
  | Struct { fields; _ } ->
    Option.all (List.map fields ~f:(gen_value random))
    |> Option.map ~f:(fun v -> Value.Struct v)
  | Array { elt; length } ->
    Option.all (List.init length ~f:(fun _ -> gen_value random elt))
    |> Option.map ~f:(fun v -> Value.Array v)
  | Union { cases; _ } ->
    let inhabited =
      List.filter_mapi cases ~f:(fun i case ->
        Option.map (gen_value random case) ~f:(fun v -> Value.Union (i, v)))
    in
    List.random_element ~random_state:random inhabited
;;

module Memory_model = struct
  module Slot = struct
    type t =
      | Garbage
      | Nulled
      | Written of string
  end

  type t =
    { scalars : int array (** By byte. *)
    ; scalar_owner : string option array
    ; ptrs : int array
    ; ptr_slots : Slot.t array
    }

  let create random layout =
    let garbage () = Random.State.int random (1 lsl 30) in
    { scalars =
        Array.init (Layout.scalar_bytes layout) ~f:(fun _ ->
          Random.State.int random 256)
    ; scalar_owner = Array.create ~len:(Layout.scalar_bytes layout) None
    ; ptrs = Array.init (Layout.ptr_words layout) ~f:(fun _ -> garbage ())
    ; ptr_slots = Array.create ~len:(Layout.ptr_words layout) Slot.Garbage
    }
  ;;

  let write t (leaf : Layout.Leaf.t) value ~owner =
    match leaf with
    | Scalar { scalar_offset; scalar } ->
      for i = 0 to scalar_bytes scalar - 1 do
        let byte = scalar_offset + i in
        if byte >= Array.length t.scalars
        then raise_s [%message "scalar out of bounds" owner (byte : int)];
        (match t.scalar_owner.(byte) with
         | Some with_ -> raise_s [%message "overlap" owner with_]
         | None -> ());
        t.scalars.(byte) <- (value lsr (8 * i)) land 0xff;
        t.scalar_owner.(byte) <- Some owner
      done
    | Ptr { ptr_index } ->
      if ptr_index >= Array.length t.ptrs
      then raise_s [%message "ptr out of bounds" owner (ptr_index : int)];
      (match t.ptr_slots.(ptr_index) with
       | Written with_ -> raise_s [%message "overlap" owner with_]
       | Garbage | Nulled -> ());
      t.ptrs.(ptr_index) <- value;
      t.ptr_slots.(ptr_index) <- Written owner
  ;;

  let null t ~ptr_index ~ptr_words ~owner =
    for i = ptr_index to ptr_index + ptr_words - 1 do
      if i >= Array.length t.ptrs
      then raise_s [%message "null out of bounds" owner (i : int)];
      (match t.ptr_slots.(i) with
       | Written with_ -> raise_s [%message "nulled a written slot" owner with_]
       | Garbage | Nulled -> ());
      t.ptrs.(i) <- 0;
      t.ptr_slots.(i) <- Nulled
    done
  ;;

  (* Every pointer slot was written or nulled, so the GC never sees garbage. *)
  let check_no_garbage t =
    Array.iteri t.ptr_slots ~f:(fun i slot ->
      match (slot : Slot.t) with
      | Garbage -> raise_s [%message "garbage pointer slot" (i : int)]
      | Nulled | Written _ -> ())
  ;;

  let read t (leaf : Layout.Leaf.t) =
    match leaf with
    | Scalar { scalar_offset; scalar } ->
      List.init (scalar_bytes scalar) ~f:(fun i ->
        t.scalars.(scalar_offset + i) lsl (8 * i))
      |> List.fold ~init:0 ~f:( lor )
    | Ptr { ptr_index } -> t.ptrs.(ptr_index)
  ;;
end

let shift leaf ~(at : Layout.Place.t) =
  match (leaf : Layout.Leaf.t) with
  | Scalar { scalar_offset; scalar } ->
    Layout.Leaf.Scalar
      { scalar_offset = scalar_offset + at.scalar_offset; scalar }
  | Ptr { ptr_index } -> Ptr { ptr_index = ptr_index + at.ptr_index }
;;

let leaf_bytes (leaf : Layout.Leaf.t) =
  match leaf with
  | Scalar { scalar; _ } -> scalar_bytes scalar
  | Ptr _ -> 8
;;

let wrap leaf v =
  if leaf_bytes leaf >= 8 then v else v land ((1 lsl (8 * leaf_bytes leaf)) - 1)
;;

let rec encode memory root (shape : Shape.t) (value : Value.t) path =
  let at = Layout.place root path in
  let owner = path_to_string path in
  match shape, value with
  | Scalar { scalar; _ }, Scalar v ->
    Memory_model.write
      memory
      (Scalar { scalar_offset = at.scalar_offset; scalar })
      v
      ~owner
  | Ptr, Ptr v ->
    Memory_model.write memory (Ptr { ptr_index = at.ptr_index }) v ~owner
  | Struct { fields; _ }, Struct values ->
    List.iteri (List.zip_exn fields values) ~f:(fun i (field, value) ->
      encode memory root field value (path @ [ Field i ]))
  | Array { elt; _ }, Array values ->
    List.iteri values ~f:(fun i value ->
      encode memory root elt value (path @ [ Index i ]))
  | Union { cases; _ }, Union (case, value) ->
    let plan = Layout.write_plan root path ~case in
    Memory_model.null
      memory
      ~ptr_index:plan.ptr_index
      ~ptr_words:plan.ptr_words
      ~owner;
    encode memory root (List.nth_exn cases case) value (path @ [ Case case ]);
    Option.iter plan.discriminant ~f:(fun (leaf, v) ->
      Memory_model.write memory leaf v ~owner:(owner ^ " discriminant"))
  | _ -> raise_s [%message "value of another shape" (value : Value.t)]
;;

let rec decode memory root (shape : Shape.t) path : Value.t =
  let at = Layout.place root path in
  match shape with
  | Scalar { scalar; _ } ->
    Scalar
      (Memory_model.read
         memory
         (Scalar { scalar_offset = at.scalar_offset; scalar }))
  | Ptr -> Ptr (Memory_model.read memory (Ptr { ptr_index = at.ptr_index }))
  | Struct { fields; _ } ->
    Struct
      (List.mapi fields ~f:(fun i field ->
         decode memory root field (path @ [ Field i ])))
  | Array { elt; length } ->
    Array
      (List.init length ~f:(fun i ->
         decode memory root elt (path @ [ Index i ])))
  | Union { cases; _ } ->
    let case =
      match Option.value_exn (Layout.discriminant at.layout) with
      | Uninhabited -> raise_s [%message "decoded an uninhabited value"]
      | Single { case } -> case
      | Tag leaf -> Memory_model.read memory (shift leaf ~at)
      | Niche { leaf; untagged; first; last; start } ->
        let k = wrap leaf (Memory_model.read memory (shift leaf ~at) - start) in
        (* Unsigned: a negative [k] of an 8 byte leaf is out of range. *)
        if k >= 0 && k <= last - first && first + k <> untagged
        then first + k
        else untagged
    in
    Union
      (case, decode memory root (List.nth_exn cases case) (path @ [ Case case ]))
;;

(* The value at the root's niche is never one of the niche's. *)
let check_niche memory layout =
  Option.iter (Layout.niche layout) ~f:(fun { leaf; start; available } ->
    let k = wrap leaf (Memory_model.read memory leaf - start) in
    if k >= 0 && k < available
    then raise_s [%message "a valid value in the niche" (k : int)])
;;

(* Every place of every path of [shape] lies within [root], aligned. *)
let rec check_bounds root (shape : Shape.t) path =
  let at = Layout.place root path in
  let align = Layout.scalar_align at.layout in
  let fits =
    at.scalar_offset >= 0
    && at.scalar_offset + Layout.scalar_bytes at.layout
       <= Layout.scalar_bytes root
    && at.ptr_index >= 0
    && at.ptr_index + Layout.ptr_words at.layout <= Layout.ptr_words root
    && Layout.scalar_bytes at.layout % align = 0
    && at.scalar_offset % align = 0
    && Layout.scalar_align root % align = 0
  in
  if not fits then raise_s [%message "out of bounds" (path : Layout.Path.t)];
  match shape with
  | Scalar _ | Ptr -> ()
  | Struct { fields; _ } ->
    List.iteri fields ~f:(fun i field ->
      check_bounds root field (path @ [ Field i ]))
  | Union { cases; _ } ->
    List.iteri cases ~f:(fun i case ->
      check_bounds root case (path @ [ Case i ]))
  | Array { elt; length } ->
    List.init length ~f:Fn.id
    |> List.iter ~f:(fun i -> check_bounds root elt (path @ [ Index i ]))
;;

(* How many unions of [shape] are stored each way, for coverage. *)
let rec count_discriminants root (shape : Shape.t) path counts =
  match shape with
  | Scalar _ | Ptr -> ()
  | Struct { fields; _ } ->
    List.iteri fields ~f:(fun i field ->
      count_discriminants
        root
        field
        (path @ [ Layout.Path.Step.Field i ])
        counts)
  | Array { elt; length } ->
    if length > 0 then count_discriminants root elt (path @ [ Index 0 ]) counts
  | Union { repr; cases } ->
    let kind =
      match
        ( repr
        , Option.value_exn (Layout.discriminant (Layout.place root path).layout)
        )
      with
      | _, Uninhabited -> "uninhabited"
      | _, Single _ -> "single"
      | C, Tag _ -> "repr C tag"
      | _, Tag _ -> "tag"
      | _, Niche { leaf = Scalar _ as leaf; first; last; start; _ } ->
        if leaf_bytes leaf < 8
           && start + last - first >= 1 lsl (8 * leaf_bytes leaf)
        then "wrapping scalar niche"
        else "scalar niche"
      | _, Niche { leaf = Ptr _; _ } -> "ptr niche"
    in
    Hashtbl.incr counts kind;
    List.iteri cases ~f:(fun i case ->
      count_discriminants root case (path @ [ Case i ]) counts)
;;

let%expect_test "random shapes: in bounds, no overlap, discriminants round trip"
  =
  let random = Random.State.make [| 42 |] in
  let shapes = ref 0 in
  let values = ref 0 in
  let errors = String.Table.create () in
  let unions = String.Table.create () in
  for _ = 1 to 3000 do
    let shape = gen_shape random ~depth:(1 + Random.State.int random 4) in
    match Layout.of_shape shape with
    | Error error ->
      let error = Error.to_string_hum error in
      (match
         List.find
           [ "repr C with pointers"; "repr C union with no cases" ]
           ~f:(fun substring -> String.is_substring error ~substring)
       with
       | Some kind -> Hashtbl.incr errors kind
       | None -> raise_s [%message (error : string)])
    | Ok layout ->
      incr shapes;
      check_bounds layout shape [];
      count_discriminants layout shape [] unions;
      for _ = 1 to 10 do
        match gen_value random shape with
        | None -> ()
        | Some value ->
          incr values;
          let memory = Memory_model.create random layout in
          (match
             encode memory layout shape value [];
             Memory_model.check_no_garbage memory;
             check_niche memory layout;
             decode memory layout shape []
           with
           | decoded ->
             if not (Value.equal value decoded)
             then
               raise_s
                 [%message
                   "round trip"
                     (shape : Shape.t)
                     (value : Value.t)
                     (decoded : Value.t)]
           | exception exn ->
             raise_s
               [%message "" (shape : Shape.t) (value : Value.t) (exn : exn)])
      done
  done;
  print_s
    [%message
      (shapes : int ref)
        (values : int ref)
        (errors : int String.Table.t)
        (unions : int String.Table.t)];
  [%expect
    {|
    ((shapes 2827) (values 26180)
     (errors (("repr C union with no cases" 60) ("repr C with pointers" 113)))
     (unions
      (("ptr niche" 160) ("repr C tag" 170) ("scalar niche" 580) (single 364)
       (tag 646) (uninhabited 349) ("wrapping scalar niche" 5))))
    |}]
;;

let%expect_test "random blocks: lengths round trip, parts fit" =
  let random = Random.State.make [| 7 |] in
  let blocks = ref 0 in
  let kinds = String.Table.create () in
  let gen_layout () =
    match
      Layout.of_shape (gen_shape random ~depth:(Random.State.int random 3))
    with
    | Ok layout -> Some layout
    | Error _ -> None
  in
  for _ = 1 to 3000 do
    match gen_layout (), gen_layout () with
    | None, _ | _, None -> ()
    | Some prefix, Some elt ->
      let block = Block.variable ~prefix ~elt in
      incr blocks;
      let element = Option.value_exn (Block.element block) in
      let length = Random.State.int random 20 in
      let header = Block.header block ~length Unmarked in
      let kind = Sexp.to_string [%sexp (Header.kind header : Header.Kind.t)] in
      Hashtbl.incr
        kinds
        (if Block.has_hidden_length block then kind ^ " hidden" else kind);
      let fail message =
        raise_s
          [%message
            message
              (prefix : Layout.t)
              (elt : Layout.t)
              (length : int)
              (header : Header.t)]
      in
      if Block.has_hidden_length block
      then (
        if Block.prefix_offset block <> 8 then fail "prefix after the length")
      else if Block.length block header <> length
      then fail "length";
      let scalar_end =
        Block.elements_offset block + (length * element.scalar_stride)
      in
      let scalar_room =
        match Header.kind header with
        | Bytes -> Header.length header
        | Scanned | Free -> 8 * Header.scalar_words header
      in
      if scalar_end > scalar_room || scalar_room - scalar_end >= 8
      then fail "scalar part";
      if Block.prefix_offset block + Layout.scalar_bytes prefix
         > Block.elements_offset block
         || Block.elements_offset block % Layout.scalar_align elt <> 0
      then fail "elements offset";
      if Header.size_words header - Header.scalar_words header
         <> Layout.ptr_words prefix + (length * element.ptr_stride)
         && not (Header.Kind.equal (Header.kind header) Bytes)
      then fail "pointer words"
  done;
  print_s [%message (blocks : int ref) (kinds : int String.Table.t)];
  [%expect
    {|
    ((blocks 2841)
     (kinds
      ((Bytes 2022) ("Bytes hidden" 231) (Scanned 418) ("Scanned hidden" 170))))
    |}]
;;
