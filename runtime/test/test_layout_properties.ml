open! Import
open Layout_helpers

(* Random shapes, laid out; random values of them, encoded into a model of
   memory (scalar bytes and pointer slots) by their layout and decoded back.
   Encoding checks each place is in bounds and that no two parts of a value
   share memory (including discriminants), and decoding that the discriminants
   tell the cases apart. *)

module Value = struct
  type t =
    | Scalar of int
    | Ptr of int
    | Struct of t list
    | Union of int * t
    | Array of t list
  [@@deriving sexp_of, equal]
end

let rec gen_shape random ~depth : Shape.t =
  let leaf () : Shape.t =
    match Random.State.int random 8 with
    | 0 -> ptr
    | 1 -> code
    | 2 -> scalar I8 ~valid:(0, Random.State.int random 3)
    | 3 ->
      let min = Random.State.int random 100 in
      scalar I8 ~valid:(min, min + Random.State.int random 100)
    | 4 -> scalar I32 ~valid:(0, 0x10FFFF)
    | _ ->
      scalar
        (List.random_element_exn
           ~random_state:random
           [ Shape.Scalar.I8; I16; I32; I64; F32; F64 ])
  in
  let children n =
    List.init n ~f:(fun _ -> gen_shape random ~depth:(depth - 1))
  in
  if depth = 0
  then leaf ()
  else (
    match Random.State.int random 10 with
    | 0 | 1 | 2 -> leaf ()
    | 3 | 4 ->
      Struct
        { repr = (if Random.State.int random 6 = 0 then C else Default)
        ; fields = children (Random.State.int random 4)
        }
    | 5 | 6 | 7 -> Union (children (Random.State.int random 5))
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
     | _ -> min + Random.State.int random (max - min + 1))
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
  | Union cases ->
    let inhabited =
      List.filter_mapi cases ~f:(fun i case ->
        Option.map (gen_value random case) ~f:(fun v -> Value.Union (i, v)))
    in
    List.random_element ~random_state:random inhabited
;;

module Memory_model = struct
  type t =
    { scalars : int option array (** By byte, with the leaf that wrote it. *)
    ; ptrs : int option array
    ; scalar_owner : string array
    ; ptr_owner : string array
    }

  let create layout =
    { scalars = Array.create ~len:(Layout.scalar_bytes layout) None
    ; ptrs = Array.create ~len:(Layout.ptr_words layout) None
    ; scalar_owner = Array.create ~len:(Layout.scalar_bytes layout) ""
    ; ptr_owner = Array.create ~len:(Layout.ptr_words layout) ""
    }
  ;;

  let write t (leaf : Layout.Leaf.t) value ~owner =
    match leaf with
    | Scalar { scalar_offset; scalar } ->
      for i = 0 to scalar_bytes scalar - 1 do
        let byte = scalar_offset + i in
        if byte >= Array.length t.scalars
        then raise_s [%message "scalar out of bounds" owner (byte : int)];
        (match t.scalars.(byte) with
         | Some _ ->
           raise_s
             [%message "overlap" owner ~with_:(t.scalar_owner.(byte) : string)]
         | None -> ());
        t.scalars.(byte) <- Some ((value lsr (8 * i)) land 0xff);
        t.scalar_owner.(byte) <- owner
      done
    | Ptr { ptr_index } ->
      if ptr_index >= Array.length t.ptrs
      then raise_s [%message "ptr out of bounds" owner (ptr_index : int)];
      (match t.ptrs.(ptr_index) with
       | Some _ ->
         raise_s
           [%message "overlap" owner ~with_:(t.ptr_owner.(ptr_index) : string)]
       | None -> ());
      t.ptrs.(ptr_index) <- Some value;
      t.ptr_owner.(ptr_index) <- owner
  ;;

  (* Unwritten memory reads as zero, as unused pointer slots are null. *)
  let read t (leaf : Layout.Leaf.t) =
    match leaf with
    | Scalar { scalar_offset; scalar } ->
      List.init (scalar_bytes scalar) ~f:(fun i ->
        Option.value t.scalars.(scalar_offset + i) ~default:0 lsl (8 * i))
      |> List.fold ~init:0 ~f:( lor )
    | Ptr { ptr_index } -> Option.value t.ptrs.(ptr_index) ~default:0
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
  | Union cases, Union (case, value) ->
    encode memory root (List.nth_exn cases case) value (path @ [ Case case ]);
    let owner = owner ^ " discriminant" in
    (match Option.value_exn (Layout.discriminant at.layout) with
     | Uninhabited -> raise_s [%message "encoded an uninhabited value"]
     | Single -> ()
     | Tag leaf -> Memory_model.write memory (shift leaf ~at) case ~owner
     | Niche { leaf; untagged; first; last; start } ->
       if case <> untagged
       then (
         assert (first <= case && case <= last);
         Memory_model.write
           memory
           (shift leaf ~at)
           (wrap leaf (start + case - first))
           ~owner))
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
  | Union cases ->
    let case =
      match Option.value_exn (Layout.discriminant at.layout) with
      | Uninhabited -> raise_s [%message "decoded an uninhabited value"]
      | Single -> 0
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

(* Every place of every path of [shape] lies within [root]. *)
let rec check_bounds root (shape : Shape.t) path =
  let at = Layout.place root path in
  let fits =
    at.scalar_offset >= 0
    && at.scalar_offset + Layout.scalar_bytes at.layout
       <= Layout.scalar_bytes root
    && at.ptr_index >= 0
    && at.ptr_index + Layout.ptr_words at.layout <= Layout.ptr_words root
    && Layout.scalar_bytes at.layout % Layout.scalar_align at.layout = 0
  in
  if not fits then raise_s [%message "out of bounds" (path : Layout.Path.t)];
  match shape with
  | Scalar _ | Ptr -> ()
  | Struct { fields; _ } ->
    List.iteri fields ~f:(fun i field ->
      check_bounds root field (path @ [ Field i ]))
  | Union cases ->
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
      count_discriminants root field (path @ [ Layout.Path.Field i ]) counts)
  | Array { elt; length } ->
    if length > 0 then count_discriminants root elt (path @ [ Index 0 ]) counts
  | Union cases ->
    let kind =
      match
        Option.value_exn (Layout.discriminant (Layout.place root path).layout)
      with
      | Uninhabited -> "uninhabited"
      | Single -> "single"
      | Tag _ -> "tag"
      | Niche { leaf = Scalar _; _ } -> "scalar niche"
      | Niche { leaf = Ptr _; _ } -> "ptr niche"
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
  let repr_c_errors = ref 0 in
  let unions = String.Table.create () in
  for _ = 1 to 3000 do
    let shape = gen_shape random ~depth:(1 + Random.State.int random 4) in
    match Layout.of_shape shape with
    | Error error ->
      if not
           (String.is_substring (Error.to_string_hum error) ~substring:"repr C")
      then raise_s [%message (error : Error.t)];
      incr repr_c_errors
    | Ok layout ->
      incr shapes;
      check_bounds layout shape [];
      count_discriminants layout shape [] unions;
      for _ = 1 to 10 do
        match gen_value random shape with
        | None -> ()
        | Some value ->
          incr values;
          let memory = Memory_model.create layout in
          (match
             encode memory layout shape value [];
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
        (repr_c_errors : int ref)
        (unions : int String.Table.t)];
  [%expect
    {|
    ((shapes 2963) (values 26910) (repr_c_errors 37)
     (unions
      (("ptr niche" 256) ("scalar niche" 628) (single 512) (tag 961)
       (uninhabited 423))))
    |}]
;;
