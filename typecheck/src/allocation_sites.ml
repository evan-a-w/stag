open! Import

type t =
  { toplevel : Typed.Ident.t
  ; mutable sites : Mode_rules.Site.t list (** Not placed yet, latest first. *)
  ; mutable count : int
  ; placed : Typed.Allocation.t Queue.t
  }

let create toplevel =
  { toplevel; sites = []; count = 0; placed = Queue.create () }
;;

let add t site =
  t.sites <- site :: t.sites;
  t.count <- t.count + 1
;;

let mark t = t.count

let place t ~since ~ty ~local =
  let mine = List.take t.sites (t.count - since)
  and kept = List.drop t.sites (t.count - since) in
  t.sites <- kept;
  t.count <- since;
  List.iter
    (Mode_rules.placements (List.rev mine) ~ty)
    ~f:(fun ((site : Mode_rules.Site.t), placement, follows) ->
      Queue.enqueue
        t.placed
        { Typed.Allocation.owner = { toplevel = t.toplevel; local }
        ; loc = site.loc
        ; kind = site.kind
        ; placement
        ; follows
        })
;;

let placed t =
  List.stable_sort (Queue.to_list t.placed) ~compare:(fun a b ->
    Loc.compare a.loc b.loc)
;;
