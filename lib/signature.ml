open! Core

module Value = struct
  type t =
    { name : string
    ; scheme : Type.ty
    ; persistent : bool
    }
  [@@deriving sexp_of]
end

module Constructor = struct
  type t =
    { name : string
    ; args : Type.ty list
    }
  [@@deriving sexp_of]
end

module Field = struct
  type t =
    { name : string
    ; ty : Type.ty
    }
  [@@deriving sexp_of]
end

module Type_decl = struct
  module Kind = struct
    type t =
      | Variant of Constructor.t list
      | Record of Field.t list
      | Alias of Type.ty
      | Abstract
    [@@deriving sexp_of]
  end

  type t =
    { name : string
    ; params : Type.arg list
    ; kind : Kind.t
    }
  [@@deriving sexp_of]

  let to_string_hum printer t ~canonical_name =
    let ty = Type.Printer.ty printer in
    let head = ty (TCon (canonical_name, t.params)) in
    let body =
      match t.kind with
      | Abstract -> None
      | Variant [] -> Some "|"
      | Variant constructors ->
        List.map constructors ~f:(fun { name; args } ->
          match args with
          | [] -> name
          (* A tuple parenthesizes its components as constructor arguments
             need. *)
          | args -> sprintf "%s of %s" name (ty (TTuple args)))
        |> String.concat ~sep:" | "
        |> Some
      | Record fields ->
        List.map fields ~f:(fun { name; ty = field } ->
          sprintf "%s : %s" name (ty field))
        |> String.concat ~sep:"; "
        |> sprintf "{ %s }"
        |> Some
      | Alias body -> Some (Type.Printer.recursive printer body ~name:head)
    in
    match body with
    | None -> sprintf "type %s" head
    | Some body -> sprintf "type %s = %s" head body
  ;;
end

module Item = struct
  type t =
    | Value of Value.t
    | Type of Type_decl.t
    | Module of
        { name : string
        ; items : t list
        }
  [@@deriving sexp_of]
end

module Scope = struct
  type t =
    { path : string list
    ; types : String.Set.t
    ; modules : String.Set.t
    }

  let create path (items : Item.t list) =
    { path
    ; types =
        List.filter_map items ~f:(function
          | Type { name; _ } -> Some name
          | Value _ | Module _ -> None)
        |> String.Set.of_list
    ; modules =
        List.filter_map items ~f:(function
          | Module { name; _ } -> Some name
          | Value _ | Type _ -> None)
        |> String.Set.of_list
    }
  ;;

  let canonical_name t name = String.concat (t.path @ [ name ]) ~sep:"."

  (* Whether [written], resolved lexically from the innermost of [enclosing],
     names the type declared in module [target]. The type exists, so a walk
     reaching [target] succeeds, even for the builtins, which are not items. *)
  let resolves enclosing written ~target =
    let is_target t = [%equal: string list] t.path target in
    match written with
    | [] -> false
    | [ name ] ->
      List.find enclosing ~f:(fun t -> is_target t || Set.mem t.types name)
      |> Option.exists ~f:is_target
    | first :: rest ->
      List.find enclosing ~f:(fun t -> Set.mem t.modules first)
      |> Option.exists ~f:(fun t ->
        [%equal: string list]
          (t.path @ (first :: List.drop_last_exn rest))
          target)
  ;;

  let relative_name enclosing canonical_name =
    let path = String.split canonical_name ~on:'.' in
    let target = List.drop_last_exn path in
    List.init (List.length path) ~f:(fun dropped ->
      List.drop path (List.length path - 1 - dropped))
    |> List.find ~f:(resolves enclosing ~target)
    |> Option.value_map
         ~default:("." ^ canonical_name)
         ~f:(String.concat ~sep:".")
  ;;
end

let rec lines enclosing : Item.t -> _ =
  let scope = List.hd_exn enclosing in
  let line f =
    let printer =
      Type.Printer.create ~type_name:(Scope.relative_name enclosing) ()
    in
    let line = f printer in
    [ line ^ Type.Printer.where printer ]
  in
  function
  | Value { name; scheme; persistent = _ } ->
    line (fun printer ->
      sprintf "val %s : %s" name (Type.Printer.ty printer scheme))
  | Type decl ->
    line (fun printer ->
      Type_decl.to_string_hum
        printer
        decl
        ~canonical_name:(Scope.canonical_name scope decl.name))
  | Module { name; items = [] } -> [ sprintf "module %s : sig end" name ]
  | Module { name; items } ->
    let enclosing = Scope.create (scope.path @ [ name ]) items :: enclosing in
    (sprintf "module %s : sig" name
     :: List.concat_map items ~f:(fun item ->
       List.map (lines enclosing item) ~f:(fun line -> "  " ^ line)))
    @ [ "end" ]
;;

type t = Item.t list [@@deriving sexp_of]

let to_string_hum t =
  List.concat_map t ~f:(lines [ Scope.create [] t ])
  |> List.map ~f:(fun line -> line ^ "\n")
  |> String.concat
;;
