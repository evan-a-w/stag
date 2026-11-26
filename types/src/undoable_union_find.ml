open! Import

type 'a t =
  { mutable parent : 'a t option
  ; mutable value : 'a
  ; mutable size : int
  }

let create value = { parent = None; value; size = 1 }

(* Without path compression, so that only [union] changes parents: union by size
   keeps paths logarithmic. *)
let rec root t =
  match t.parent with
  | None -> t
  | Some parent -> root parent
;;

let get t = (root t).value
let same_class t other = phys_equal (root t) (root other)

let undoably ~undo f =
  match
    Mode.Private.Undo.atomically (fun () ->
      Mode.Private.Undo.record undo;
      f ();
      Ok ())
  with
  | Ok () -> ()
  | Error (_ : Nothing.t) -> .
;;

let set t value =
  let root = root t in
  let old = root.value in
  undoably ~undo:(fun () -> root.value <- old) (fun () -> root.value <- value)
;;

let union t other ~f =
  let a = root t
  and b = root other in
  if not (phys_equal a b)
  then (
    let value = f a.value b.value in
    let child, parent = if a.size <= b.size then a, b else b, a in
    let size = parent.size
    and old = parent.value in
    undoably
      ~undo:(fun () ->
        child.parent <- None;
        parent.size <- size;
        parent.value <- old)
      (fun () ->
        child.parent <- Some parent;
        parent.size <- size + child.size;
        parent.value <- value))
;;
