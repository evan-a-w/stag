open! Import

(* [code] identifies the morphism (its axes and [right]): comparing and
   composing go by it. *)
type t =
  { src : Axis.Any.t
  ; dst : Axis.Any.t
  ; src_index : int
  ; dst_index : int
  ; right : int array
  ; left : int array
  ; code : int
  }

let compare a b = Int.compare a.code b.code

let make_code ~src_index ~dst_index right =
  Array.fold
    right
    ~init:((src_index * 3) + dst_index)
    ~f:(fun acc p -> (acc * 8) + p)
;;

let create ~src ~dst ~right ~left =
  let src_size = Axis.Any.size src
  and dst_size = Axis.Any.size dst in
  assert (Array.length right = src_size && Array.length left = dst_size);
  for p = 0 to dst_size - 1 do
    for q = 0 to src_size - 1 do
      assert (Bool.equal (left.(p) <= q) (p <= right.(q)))
    done
  done;
  { src
  ; dst
  ; src_index = Axis.Any.index src
  ; dst_index = Axis.Any.index dst
  ; right
  ; left
  ; code =
      make_code
        ~src_index:(Axis.Any.index src)
        ~dst_index:(Axis.Any.index dst)
        right
  }
;;

let identity axis =
  let points = Array.init (Axis.Any.size axis) ~f:Fn.id in
  create ~src:axis ~dst:axis ~right:points ~left:points
;;

let const_is_static =
  let i = Axis.Stage.index
  and p = Axis.Provenance.index in
  create
    ~src:(T Stage)
    ~dst:(T Provenance)
    ~right:[| p Static; p External; p External |]
    ~left:[| i Const; i Portable; i Portable |]
;;

let runtime_is_gc =
  let i = Axis.Stage.index
  and p = Axis.Provenance.index in
  create
    ~src:(T Provenance)
    ~dst:(T Stage)
    ~right:[| i Const; i Nonportable; i Nonportable |]
    ~left:[| p Static; p Gc; p Gc |]
;;

let cross_portable =
  let i = Axis.Stage.index in
  create
    ~src:(T Stage)
    ~dst:(T Stage)
    ~right:[| i Const; i Nonportable; i Nonportable |]
    ~left:[| i Const; i Portable; i Portable |]
;;

let locality ~right ~left =
  let i = Axis.Locality.index in
  create
    ~src:(T Locality)
    ~dst:(T Locality)
    ~right:(Array.map right ~f:i)
    ~left:(Array.map left ~f:i)
;;

let borrow =
  locality
    ~right:[| Global; Local; Local |]
    ~left:[| Global; Regional; Regional |]
;;

let release =
  locality ~right:[| Global; Global; Local |] ~left:[| Global; Local; Local |]
;;

let src t = t.src
let dst t = t.dst

let is_identity t =
  t.src_index = t.dst_index && Array.for_alli t.right ~f:(fun x y -> x = y)
;;

let named =
  [ "const_is_static", const_is_static
  ; "runtime_is_gc", runtime_is_gc
  ; "cross_portable", cross_portable
  ; "borrow", borrow
  ; "release", release
  ]
;;

let sexp_of_t t =
  if is_identity t
  then Sexp.Atom "id"
  else (
    match List.find named ~f:(fun (_, named) -> compare t named = 0) with
    | Some (name, _) -> Atom name
    | None ->
      let point = Axis.Any.sexp_of_point t.dst in
      [%sexp
        { src = (t.src : Axis.Any.t)
        ; right = (Array.map t.right ~f:point : Sexp.t array)
        }])
;;

let is_inflationary t =
  t.src_index = t.dst_index && Array.for_alli t.right ~f:(fun x y -> x <= y)
;;

let le a b =
  a.src_index = b.src_index
  && a.dst_index = b.dst_index
  && Array.for_all2_exn a.right b.right ~f:( <= )
;;

let right t x = t.right.(x)
let left t x = t.left.(x)

module Code_pair = struct
  module T = struct
    type t = int * int [@@deriving compare, hash, sexp_of]
  end

  include T
  include Hashable.Make_plain (T)
end

let composed = Code_pair.Table.create ()

let compose m1 m2 =
  assert (m1.src_index = m2.dst_index);
  Hashtbl.find_or_add composed (m1.code, m2.code) ~default:(fun () ->
    let right = Array.map m2.right ~f:(fun y -> m1.right.(y)) in
    { src = m2.src
    ; dst = m1.dst
    ; src_index = m2.src_index
    ; dst_index = m1.dst_index
    ; right
    ; left = Array.map m1.left ~f:(fun x -> m2.left.(x))
    ; code = make_code ~src_index:m2.src_index ~dst_index:m1.dst_index right
    })
;;

let meet_with axis point =
  let size = Axis.Any.size axis in
  let top = size - 1 in
  create
    ~src:axis
    ~dst:axis
    ~right:(Array.init size ~f:(fun y -> if point <= y then top else y))
    ~left:(Array.init size ~f:(fun x -> Int.min x point))
;;

let code t = t.code
