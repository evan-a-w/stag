open! Import

type t =
  { mutable state : state
  ; mutable failed : bool
  }

and state =
  | Open
  | Merged of t
  | Abandoned
  | Finished

let create () = { state = Open; failed = false }

let rec root t =
  match t.state with
  | Merged into -> root into
  | Open | Abandoned | Finished -> t
;;

let failed t = (root t).failed
let fail t = (root t).failed <- true

let is_abandoned t =
  match (root t).state with
  | Abandoned -> true
  | Open | Merged _ | Finished -> false
;;

let is_open t =
  match (root t).state with
  | Open -> true
  | Merged _ | Abandoned | Finished -> false
;;

let set_state t state ~from =
  let t = root t in
  match t.state with
  | Open -> t.state <- state
  | Merged _ | Abandoned | Finished ->
    raise_s [%message "Component: not open" (from : string)]
;;

let merge t ~into =
  let t = root t
  and into = root into in
  if not (phys_equal t into)
  then (
    if t.failed then into.failed <- true;
    set_state t (Merged into) ~from:"merge")
;;

let abandon t = set_state t Abandoned ~from:"abandon"
let finish t = set_state t Finished ~from:"finish"
