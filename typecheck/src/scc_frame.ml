open! Import

module Kind = struct
  type 'frame t =
    | Toplevel
    | Code of { within : 'frame option }
end

type t =
  { id : int
  ; kind : t Kind.t
  ; component : Component.t
  ; mutable lowlink : int
  ; mutable pending : Toplevel_binding.t list
  ; mutable quoting : Quoted_use.t list
  ; mutable reopened : Type.ty list
  }

let create id kind =
  { id
  ; kind
  ; component = Component.create ()
  ; lowlink = id
  ; pending = []
  ; quoting = []
  ; reopened = []
  }
;;

let toplevel_rank = 1

let use_rank t =
  match t.kind with
  | Toplevel -> toplevel_rank
  | Code _ -> 0
;;

let add_pending t toplevel = t.pending <- toplevel :: t.pending
let add_quoting t use = t.quoting <- use :: t.quoting
let add_reopened t ty = t.reopened <- ty :: t.reopened

let clear t =
  t.pending <- [];
  t.quoting <- [];
  t.reopened <- []
;;

let depends_on t ~lowlink = t.lowlink <- Int.min t.lowlink lowlink

let absorb t ~child =
  if Component.is_open child.component
  then (
    depends_on t ~lowlink:child.lowlink;
    Component.merge child.component ~into:t.component;
    t.pending <- child.pending @ t.pending;
    t.quoting <- child.quoting @ t.quoting;
    t.reopened <- child.reopened @ t.reopened;
    clear child)
;;

let hand_over t =
  match t.kind with
  | Code { within = Some within } -> absorb within ~child:t
  | Code { within = None }
    when List.is_empty t.pending && List.is_empty t.quoting -> ()
  | Code { within = None } | Toplevel ->
    raise_s [%message "Scc_frame.hand_over" (t.id : int)]
;;
