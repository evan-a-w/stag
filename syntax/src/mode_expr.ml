open! Import

module Axis = struct
  type t =
    | Stage
    | Provenance
    | Locality
  [@@deriving sexp_of, compare, equal]

  let to_string = function
    | Stage -> "stage"
    | Provenance -> "provenance"
    | Locality -> "locality"
  ;;

  let of_string = function
    | "stage" -> Some Stage
    | "provenance" -> Some Provenance
    | "locality" -> Some Locality
    | _ -> None
  ;;
end

module Atom = struct
  type t =
    | Const
    | Portable
    | Nonportable
    | Static
    | Gc
    | External
    | Global
    | Regional
    | Local
  [@@deriving sexp, compare, equal, enumerate]

  let axis : t -> Axis.t = function
    | Const | Portable | Nonportable -> Stage
    | Static | Gc | External -> Provenance
    | Global | Regional | Local -> Locality
  ;;

  let to_string t = String.lowercase (Sexp.to_string (sexp_of_t t))

  let of_string =
    let table =
      String.Table.of_alist_exn (List.map all ~f:(fun t -> to_string t, t))
    in
    Hashtbl.find table
  ;;
end

type desc =
  | Var of string
  | Atoms of Atom.t list
[@@deriving sexp_of, compare, equal]

type t = desc Located.t [@@deriving sexp_of, compare, equal]

let to_string (t : t) =
  match t.txt with
  | Var "_" -> "_"
  | Var name -> "'" ^ name
  | Atoms atoms -> String.concat ~sep:" " (List.map atoms ~f:Atom.to_string)
;;

module Term = struct
  type mode = t [@@deriving sexp_of, compare, equal]

  let mode_string = to_string

  type t =
    | Mode of mode
    | Axis of Axis.t * mode
    | Meet of t * Atom.t
    | Release of t
    | Static_if_const of t
    | Const_if_static of t
  [@@deriving sexp_of, compare, equal]

  let rec to_string = function
    | Mode mode -> mode_string mode
    | Axis (axis, mode) ->
      sprintf "%s(%s)" (Axis.to_string axis) (mode_string mode)
    | Meet (t, atom) ->
      sprintf "meet(%s, %s)" (to_string t) (Atom.to_string atom)
    | Release t -> sprintf "release(%s)" (to_string t)
    | Static_if_const t -> sprintf "static_if_const(%s)" (to_string t)
    | Const_if_static t -> sprintf "const_if_static(%s)" (to_string t)
  ;;

  (* The axis of a term of one variable, on the side of [<=] its functions are
     on: [`Left] for [meet] and [release], [`Right] for the others. *)
  let rec axis t ~side =
    let open Result.Let_syntax in
    let inner ?(rest = "") t ~(on : Axis.t) ~name =
      let example = sprintf "`%s(%s('m)%s)`" name (Axis.to_string on) rest in
      match t with
      | Mode { txt = Var _; _ } ->
        Error
          (sprintf
             "the variable in `%s(...)` names its axis, as in %s"
             name
             example)
      | _ ->
        let%bind axis = axis t ~side in
        if Axis.equal axis on
        then Ok ()
        else
          Error
            (sprintf
               "`%s(...)` %s, but its term is on %s: as in %s"
               name
               (if String.is_empty rest
                then sprintf "is on %s" (Axis.to_string on)
                else
                  sprintf
                    "is on the axis of its mode%s, %s"
                    (String.drop_prefix rest 1)
                    (Axis.to_string on))
               (Axis.to_string axis)
               example)
    in
    let on ~(sides : [ `Left | `Right ]) ~name =
      if Poly.equal side sides
      then Ok ()
      else
        Error
          (sprintf
             "`%s(...)` is only on the %s of `<=`"
             name
             (match sides with
              | `Left -> "left"
              | `Right -> "right"))
    in
    match t with
    | Mode { txt = Var _; _ } ->
      Error "a mode variable here names its axis, as in `stage('m)`"
    | Mode { txt = Atoms _; _ } ->
      Error "a constraint on one axis relates two mode variables"
    | Axis (axis, { txt = Var _; _ }) -> Ok axis
    | Axis (axis, { txt = Atoms _; _ }) ->
      Error
        (sprintf
           "`%s(...)` takes a mode variable, as in `%s('m)`: a mode like \
            `portable` names its axis"
           (Axis.to_string axis)
           (Axis.to_string axis))
    | Meet (t, atom) ->
      let%bind () = on ~sides:`Left ~name:"meet" in
      let%map () =
        inner
          t
          ~on:(Atom.axis atom)
          ~name:"meet"
          ~rest:(", " ^ Atom.to_string atom)
      in
      Atom.axis atom
    | Release t ->
      let%bind () = on ~sides:`Left ~name:"release" in
      let%map () = inner t ~on:Locality ~name:"release" in
      Axis.Locality
    | Static_if_const t ->
      let%bind () = on ~sides:`Right ~name:"static_if_const" in
      let%map () = inner t ~on:Stage ~name:"static_if_const" in
      Axis.Provenance
    | Const_if_static t ->
      let%bind () = on ~sides:`Right ~name:"const_if_static" in
      let%map () = inner t ~on:Provenance ~name:"const_if_static" in
      Axis.Stage
  ;;
end

module Constraint = struct
  type t =
    | Submode of
        { lower : Term.t
        ; upper : Term.t
        }
    | Pointer_free of string
  [@@deriving sexp_of, compare, equal]

  let to_string = function
    | Pointer_free var -> var ^ " : pointer_free"
    | Submode { lower; upper } ->
      Term.to_string lower ^ " <= " ^ Term.to_string upper
  ;;

  let check_submode ~(lower : Term.t) ~(upper : Term.t) =
    match lower, upper with
    | Mode { txt = Atoms _; _ }, Mode { txt = Atoms _; _ } ->
      Error
        "a mode constraint relates a mode variable, as in `'m <= 'n` or `'m <= \
         portable`"
    | Mode _, Mode _ -> Ok ()
    | Mode mode, term | term, Mode mode ->
      let side = if phys_equal term lower then `Left else `Right in
      (match Term.axis term ~side, mode.txt with
       | Error message, _ -> Error message
       | Ok axis, Var _ ->
         Error
           (sprintf
              "both sides of a constraint on one axis name it: write `%s('n)` \
               for `'n`"
              (Axis.to_string axis))
       | Ok _, Atoms _ ->
         Error
           (sprintf
              "a bound is on the whole variable, as in `%s`"
              (match side with
               | `Left -> "'m <= " ^ Term.mode_string mode
               | `Right -> Term.mode_string mode ^ " <= 'm")))
    | _ ->
      let open Result.Let_syntax in
      let%bind lower_axis = Term.axis lower ~side:`Left in
      let%bind upper_axis = Term.axis upper ~side:`Right in
      if Axis.equal lower_axis upper_axis
      then Ok ()
      else
        Error
          (sprintf
             "the sides of `<=` are on different axes, %s and %s"
             (Axis.to_string lower_axis)
             (Axis.to_string upper_axis))
  ;;

  let check = function
    | Pointer_free _ -> Ok ()
    | Submode { lower; upper } -> check_submode ~lower ~upper
  ;;
end
