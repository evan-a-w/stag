open! Import

let scalar ?valid scalar = Shape.Scalar { scalar; valid }
let i8 = scalar I8
let i16 = scalar I16
let i32 = scalar I32
let i64 = scalar I64
let f32 = scalar F32
let f64 = scalar F64
let code = scalar Code
let ptr = Shape.Ptr
let struct_ fields = Shape.Struct { repr = Default; fields }
let c_struct fields = Shape.Struct { repr = C; fields }
let unit = struct_ []
let enum n = Shape.Union (List.init n ~f:(fun _ -> unit))
let bool = enum 2
let option t = Shape.Union [ unit; t ]
let array elt ~length = Shape.Array { elt; length }
let layout shape = Or_error.ok_exn (Layout.of_shape shape)

let path_to_string path =
  if List.is_empty path
  then "."
  else
    String.concat
      (List.map path ~f:(function
        | Layout.Path.Field i -> sprintf ".%d" i
        | Case i -> sprintf "|%d" i
        | Index i -> sprintf "[%d]" i))
;;

let scalar_to_string scalar = Sexp.to_string [%sexp (scalar : Shape.Scalar.t)]

let leaf_to_string ?(scalar_base = 0) ?(ptr_base = 0) (leaf : Layout.Leaf.t) =
  match leaf with
  | Scalar { scalar_offset; scalar } ->
    sprintf "%s @%d" (scalar_to_string scalar) (scalar_base + scalar_offset)
  | Ptr { ptr_index } -> sprintf "ptr #%d" (ptr_base + ptr_index)
;;

let niche_to_string (niche : Layout.Niche.t option) =
  match niche with
  | None -> "no niche"
  | Some { leaf; start; available } ->
    sprintf "niche %s from %d (%d)" (leaf_to_string leaf) start available
;;

(* Every leaf of [shape] by path, with its place in [root], and every union's
   discriminant. *)
let rec lines root (shape : Shape.t) path =
  let { Layout.Place.scalar_offset; ptr_index; layout } =
    Layout.place root path
  in
  let here line = [ path_to_string path, line ] in
  match shape with
  | Scalar { scalar; _ } ->
    here (sprintf "%s @%d" (scalar_to_string scalar) scalar_offset)
  | Ptr -> here (sprintf "ptr #%d" ptr_index)
  | Struct { fields; _ } ->
    List.concat_mapi fields ~f:(fun i field ->
      lines root field (path @ [ Field i ]))
  | Array { elt; length } ->
    List.concat
      (List.init length ~f:(fun i -> lines root elt (path @ [ Index i ])))
  | Union cases ->
    let discriminant =
      match Option.value_exn (Layout.discriminant layout) with
      | Uninhabited -> "uninhabited"
      | Single -> "single"
      | Tag leaf ->
        sprintf
          "tag %s"
          (leaf_to_string leaf ~scalar_base:scalar_offset ~ptr_base:ptr_index)
      | Niche { leaf; untagged; first; last; start } ->
        sprintf
          "niche %s: |%d untagged, |%d..|%d from %d"
          (leaf_to_string leaf ~scalar_base:scalar_offset ~ptr_base:ptr_index)
          untagged
          first
          last
          start
    in
    here discriminant
    @ List.concat_mapi cases ~f:(fun i case ->
      lines root case (path @ [ Case i ]))
;;

let show shape =
  match Layout.of_shape shape with
  | Error error -> print_s [%sexp (error : Error.t)]
  | Ok layout ->
    printf
      "scalar %d (align %d), %d ptrs, %s\n"
      (Layout.scalar_bytes layout)
      (Layout.scalar_align layout)
      (Layout.ptr_words layout)
      (niche_to_string (Layout.niche layout));
    let lines = lines layout shape [] in
    let width =
      List.fold lines ~init:0 ~f:(fun acc (path, _) ->
        Int.max acc (String.length path))
    in
    List.iter lines ~f:(fun (path, line) ->
      printf "  %-*s  %s\n" width path line)
;;
