open! Import

module Arg = struct
  type t =
    | Int
    | Bool
    | Char
    | String
  [@@deriving sexp_of, equal]
end

module Result = struct
  type t =
    | Int
    | Bool
    | Char
    | Unit
  [@@deriving sexp_of, equal]
end

type t =
  { args : Arg.t list
  ; result : Result.t
  }
[@@deriving sexp_of, equal]

let arity t = Int.max 1 (List.length t.args)

module Problem = struct
  type t =
    | Not_a_function
    | Argument of Type.ty
    | Result of Type.ty
    | Unit_argument
    | Pointer of Type.ty
  [@@deriving sexp_of]
end

let con_name ty =
  match Type.canonical_ty ty with
  | TCon (name, []) -> Some name
  | TCon ("ptr", _) -> Some "ptr"
  | _ -> None
;;

let of_type ty : (t, Problem.t) Core.Result.t =
  let rec arrows ty acc =
    match Type.canonical_ty ty with
    | TArrow (arg, res, _) -> arrows res (arg :: acc)
    | _ -> List.rev acc, ty
  in
  let open Core.Result.Let_syntax in
  match arrows ty [] with
  | [], _ -> Error Not_a_function
  | args, result ->
    let%bind result =
      match con_name result with
      | Some "int" -> Ok Result.Int
      | Some "bool" -> Ok Result.Bool
      | Some "char" -> Ok Result.Char
      | Some "unit" -> Ok Result.Unit
      | Some ("string" | "ptr") -> Error (Problem.Pointer result)
      | _ -> Error (Problem.Result result)
    in
    (match args with
     | [ arg ] when Option.equal String.equal (con_name arg) (Some "unit") ->
       Ok { args = []; result }
     | args ->
       let%map args =
         List.map args ~f:(fun arg : (Arg.t, Problem.t) Core.Result.t ->
           match con_name arg with
           | Some "int" -> Ok Int
           | Some "bool" -> Ok Bool
           | Some "char" -> Ok Char
           | Some ("string" | "ptr") -> Error (Pointer arg)
           | Some "unit" -> Error Unit_argument
           | _ -> Error (Argument arg))
         |> Core.Result.all
       in
       { args; result })
;;
