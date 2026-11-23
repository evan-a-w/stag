open! Import

module Policy = struct
  type t =
    | Allowed
    | Denied of
        { external_ : string
        ; library : string option
        }
  [@@deriving sexp_of, equal]

  let denial ~external_ ~library =
    match library with
    | Some library ->
      sprintf
        "The external %s can't be called at compile time: its library %s \
         doesn't allow C calls at compile time (add (compile_time_c_calls) to \
         its stanza)"
        external_
        library
    | None ->
      sprintf
        "The external %s can't be called at compile time: C calls at compile \
         time are only allowed in libraries that allow them"
        external_
  ;;
end

module Library = struct
  type t =
    { name : string
    ; allowed : bool
    }
end

type t =
  | All
  | Libraries of Library.t String.Map.t

let none = Libraries String.Map.empty
let all = All
let of_libraries libraries = Libraries libraries

let policy t ~external_ : Policy.t =
  match t with
  | All -> Allowed
  | Libraries _ when Option.is_some (Prelude_path.relative external_) -> Allowed
  | Libraries libraries ->
    let root =
      match String.lsplit2 external_ ~on:'.' with
      | Some (root, _) -> Some root
      | None -> None
    in
    (match Option.bind root ~f:(Map.find libraries) with
     | Some { allowed = true; _ } -> Allowed
     | Some { name; allowed = false } ->
       Denied { external_; library = Some name }
     | None -> Denied { external_; library = None })
;;
