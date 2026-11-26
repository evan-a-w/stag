open! Import

module Arg = struct
  type t =
    | Int
    | Bool
    | Char
    | C_int of C_int.t
    | String
  [@@deriving sexp_of, equal, compare, hash]
end

module Result = struct
  type t =
    | Int
    | Bool
    | Char
    | Unit
    | C_int of C_int.t
  [@@deriving sexp_of, equal, compare, hash]
end

type t =
  { args : Arg.t list
  ; result : Result.t
  }
[@@deriving sexp_of, equal, compare, hash]

let arity t = Int.max 1 (List.length t.args)

module Problem = struct
  type t =
    | Not_a_function
    | Argument of Type.ty
    | Result of Type.ty
    | Unit_argument
    | Pointer of Type.ty
    | Pointer_result of Type.ty
  [@@deriving sexp_of]
end

let con_name ty =
  match Type.canonical_ty ty with
  | TCon (name, []) -> Some name
  | TCon (name, _)
    when String.equal name Type.ptr_name || String.equal name Type.box_name ->
    Some "ptr"
  | _ -> None
;;

let c_int ty = Option.bind (con_name ty) ~f:C_int.of_type_name

let rec arrows ty acc =
  match Type.canonical_ty ty with
  | TArrow (arg, res, _) -> arrows res (arg :: acc)
  | _ -> List.rev acc, ty
;;

let of_type ty : (t, Problem.t) Core.Result.t =
  let open Core.Result.Let_syntax in
  match arrows ty [] with
  | [], _ -> Error Not_a_function
  | args, result ->
    let%bind result =
      match c_int result, con_name result with
      | Some c_int, _ -> Ok (Result.C_int c_int)
      | None, Some "int" -> Ok Result.Int
      | None, Some "bool" -> Ok Result.Bool
      | None, Some "char" -> Ok Result.Char
      | None, Some "unit" -> Ok Result.Unit
      | None, Some ("string" | "ptr") -> Error (Problem.Pointer_result result)
      | None, _ -> Error (Problem.Result result)
    in
    (match args with
     | [ arg ] when Option.equal String.equal (con_name arg) (Some "unit") ->
       Ok { args = []; result }
     | args ->
       let%map args =
         List.map args ~f:(fun arg : (Arg.t, Problem.t) Core.Result.t ->
           match c_int arg, con_name arg with
           | Some c_int, _ -> Ok (C_int c_int)
           | None, Some "int" -> Ok Int
           | None, Some "bool" -> Ok Bool
           | None, Some "char" -> Ok Char
           | None, Some "string" -> Ok String
           | None, Some "ptr" -> Error (Pointer arg)
           | None, Some "unit" -> Error Unit_argument
           | None, _ -> Error (Argument arg))
         |> Core.Result.all
       in
       { args; result })
;;

let rec value_type ty =
  match Type.canonical_ty ty with
  | TArrow (arg, res, modes) ->
    let int_if_c ty = if Option.is_some (c_int ty) then Type.int else ty in
    Type.TArrow (int_if_c arg, value_type res, modes)
  | ty -> if Option.is_some (c_int ty) then Type.int else ty
;;
