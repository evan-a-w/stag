open! Import

module Name = struct
  type t =
    | Non_exhaustive
    | Unused_arm
  [@@deriving sexp, compare, equal, enumerate]

  let to_string = function
    | Non_exhaustive -> "non-exhaustive"
    | Unused_arm -> "unused-arm"
  ;;

  let of_string string =
    List.find all ~f:(fun t -> String.equal (to_string t) string)
  ;;
end

module Level = struct
  type t =
    | Off
    | Warn
    | Error
  [@@deriving sexp, compare, equal]

  let sign = function
    | Off -> '-'
    | Warn -> '+'
    | Error -> '@'
  ;;

  let of_sign = function
    | '-' -> Some Off
    | '+' -> Some Warn
    | '@' -> Some Error
    | _ -> None
  ;;
end

module Spec = struct
  module Item = struct
    module Target = struct
      type t =
        | All
        | Warning of Name.t
      [@@deriving sexp, compare, equal]

      let to_string = function
        | All -> "all"
        | Warning name -> Name.to_string name
      ;;

      let of_string = function
        | "all" -> Some All
        | name -> Option.map (Name.of_string name) ~f:(fun name -> Warning name)
      ;;
    end

    type t =
      { level : Level.t
      ; target : Target.t
      }
    [@@deriving sexp, compare, equal]

    let to_string { level; target } =
      String.of_char (Level.sign level) ^ Target.to_string target
    ;;

    let known =
      String.concat
        ~sep:", "
        (List.map Name.all ~f:(fun name -> sprintf "`%s`" (Name.to_string name)))
    ;;

    let of_string item =
      let signs = "`-name` (off), `+name` (a warning) or `@name` (an error)" in
      match Level.of_sign item.[0] with
      | None ->
        Error
          (sprintf
             "expected a sign before the warning `%s`: %s"
             item
             (String.substr_replace_all signs ~pattern:"name" ~with_:item))
      | Some level ->
        (match String.drop_prefix item 1 with
         | "" ->
           Error
             (sprintf
                "expected a warning's name after `%s`, as in %s"
                item
                signs)
         | name ->
           (match Target.of_string name with
            | Some target -> Ok { level; target }
            | None ->
              Error
                (sprintf
                   "unknown warning `%s`: the warnings are %s, and `all` names \
                    every one"
                   name
                   known)))
    ;;
  end

  type t = Item.t list [@@deriving sexp, compare, equal]

  let of_string string =
    match
      String.split_on_chars string ~on:[ ' '; '\t'; '\n'; '\r'; ',' ]
      |> List.filter ~f:(Fn.non String.is_empty)
    with
    | [] -> Error "a warning spec has at least one item, as in `-unused-arm`"
    | items -> Result.all (List.map items ~f:Item.of_string)
  ;;

  let to_string t = String.concat ~sep:" " (List.map t ~f:Item.to_string)
end

module Settings = struct
  type t = (Name.t * Level.t) list [@@deriving sexp_of]

  let set t name level =
    List.map t ~f:(fun (name', level') ->
      name', if Name.equal name name' then level else level')
  ;;

  let default =
    List.map Name.all ~f:(fun name ->
      ( name
      , match (name : Name.t) with
        | Non_exhaustive -> Level.Error
        | Unused_arm -> Warn ))
  ;;

  let apply t spec =
    List.fold spec ~init:t ~f:(fun t ({ level; target } : Spec.Item.t) ->
      match target with
      | All -> List.map t ~f:(fun (name, _) -> name, level)
      | Warning name -> set t name level)
  ;;

  let level t name = List.Assoc.find_exn t name ~equal:Name.equal
end
