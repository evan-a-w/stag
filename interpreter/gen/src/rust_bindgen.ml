open! Import
module Ty = R.Type_expr

module Field = struct
  type t =
    { name : string
    ; index : int (** In the OCaml block. *)
    ; ty : Ty.t
    }
end

module Constructor = struct
  module Args = struct
    type t =
      | Tuple of Field.t list
      | Record of Field.t list
  end

  type t =
    { name : string
    ; constant : bool
    ; tag : int (** Among the constant constructors, or the others. *)
    ; size : int (** Of its block, skipped fields included. *)
    ; args : Args.t
    }
end

module Body = struct
  type t =
    | Alias of Ty.t
    | Record of Field.t list
    | Variant of Constructor.t list

  let field_types = function
    | Alias ty -> [ ty ]
    | Record fields -> List.map fields ~f:(fun (field : Field.t) -> field.ty)
    | Variant constructors ->
      List.concat_map constructors ~f:(fun (constructor : Constructor.t) ->
        match constructor.args with
        | Tuple fields | Record fields ->
          List.map fields ~f:(fun (field : Field.t) -> field.ty))
  ;;
end

module Item = struct
  type t =
    { decl : R.Decl.t
    ; key : Key.t
    ; rust : string list * string
    ; body : Body.t
    ; stored : bool
    ; rebuilt : bool (** Has [[@rust.skip]] fields, so is rebuilt by OCaml. *)
    }

  let has_skipped_fields (decl : R.Decl.t) =
    let skipped = List.exists ~f:(fun (field : R.Field.t) -> field.skip) in
    match decl.kind with
    | Alias _ -> false
    | Record fields -> skipped fields
    | Variant constructors ->
      List.exists constructors ~f:(fun (constructor : R.Constructor.t) ->
        match constructor.args with
        | Tuple _ -> false
        | Record fields -> skipped fields)
  ;;
end

(** Checks that [ty] is well-formed in [decl]. *)
let rec check ~(decls : R.Decl.t Key.Map.t) ~(decl : R.Decl.t) (ty : Ty.t) =
  let open Or_error.Let_syntax in
  let check = check ~decls ~decl in
  match ty with
  | Var name ->
    if List.mem decl.params name ~equal:String.equal
    then Ok ()
    else Or_error.error_s [%message "Unbound type variable" name]
  | Int | Bool | Char | Unit | String | Nothing -> Ok ()
  | List ty | Array ty | Option ty | Set ty -> check ty
  | Map (key, data) ->
    let%bind () = check key in
    check data
  | Tuple tys -> Or_error.all_unit (List.map tys ~f:check)
  | Decl (key, args) ->
    let%bind () = Or_error.all_unit (List.map args ~f:check) in
    (match Map.find decls key with
     | None ->
       Or_error.error_s
         [%message
           "Unknown type (is its library linked?)"
             ~type_:(Key.to_string key)
             ~in_:(Key.to_string decl.key)]
     | Some target ->
       if List.length target.params = List.length args
       then Ok ()
       else
         Or_error.error_s
           [%message
             "Wrong number of type arguments"
               ~type_:(Key.to_string key)
               ~in_:(Key.to_string decl.key)])
;;

let fields ~check (fields : R.Field.t list) =
  List.filter_mapi fields ~f:(fun index (field : R.Field.t) ->
    Option.some_if (not field.skip) (index, field))
  |> List.map ~f:(fun (index, (field : R.Field.t)) ->
    Or_error.map (check field.ty) ~f:(fun () ->
      { Field.name = field.name; index; ty = field.ty }))
  |> Or_error.all
;;

let body ~check (decl : R.Decl.t) =
  let open Or_error.Let_syntax in
  match decl.kind with
  | Alias ty ->
    let%map () = check ty in
    Body.Alias ty
  | Record record_fields ->
    let%map fields = fields ~check record_fields in
    Body.Record fields
  | Variant constructors ->
    let constant = ref 0 in
    let block = ref 0 in
    let%map constructors =
      List.map constructors ~f:(fun { name; args } ->
        let next counter =
          let tag = !counter in
          incr counter;
          tag
        in
        match args with
        | Tuple [] ->
          return
            { Constructor.name
            ; constant = true
            ; tag = next constant
            ; size = 0
            ; args = Tuple []
            }
        | Tuple tys ->
          let tag = next block in
          let%map fields =
            List.mapi tys ~f:(fun index ty ->
              let%map () = check ty in
              { Field.name = ""; index; ty })
            |> Or_error.all
          in
          { Constructor.name
          ; constant = false
          ; tag
          ; size = List.length tys
          ; args = Tuple fields
          }
        | Record record_fields ->
          let tag = next block in
          if List.exists record_fields ~f:(fun field -> field.mutable_)
          then
            Or_error.error_s
              [%message
                "Mutable fields of inline records are unsupported"
                  ~constructor:name]
          else (
            let%map fields = fields ~check record_fields in
            { Constructor.name
            ; constant = false
            ; tag
            ; size = List.length record_fields
            ; args = Record fields
            }))
      |> Or_error.all
    in
    Body.Variant constructors
;;

let rec params (ty : Ty.t) =
  match ty with
  | Var name -> [ name ]
  | Int | Bool | Char | Unit | String | Nothing -> []
  | Decl (_, tys) | Tuple tys -> List.concat_map tys ~f:params
  | List ty | Array ty | Option ty | Set ty -> params ty
  | Map (key, data) -> params key @ params data
;;

module Gen = struct
  type t =
    { items : Item.t Key.Map.t
    ; boxed : Key.t -> Ty.t -> bool
      (** Whether a field of this type, of the declaration of this key,
          must be boxed. *)
    }

  let stored t key = (Map.find_exn t.items key).stored

  let generics names =
    if List.is_empty names
    then ""
    else "<" ^ String.concat ~sep:", " (List.map names ~f:Rust_name.camel) ^ ">"
  ;;

  let rec ty t ~from (ty_ : Ty.t) =
    let ty = ty t ~from in
    match ty_ with
    | Var name -> Rust_name.camel name
    | Int -> "i64"
    | Bool -> "bool"
    | Char -> "u8"
    | Unit -> "()"
    | String -> "String"
    | Nothing -> "crate::Nothing"
    | List elt -> sprintf "Vec<%s>" (ty elt)
    | Array elt -> sprintf "Box<[%s]>" (ty elt)
    | Option elt -> sprintf "Option<%s>" (ty elt)
    | Set elt -> sprintf "crate::Set<%s>" (ty elt)
    | Map (key, data) -> sprintf "crate::Map<%s, %s>" (ty key) (ty data)
    | Tuple tys -> "(" ^ String.concat ~sep:", " (List.map tys ~f:ty) ^ ")"
    | Decl (key, args) ->
      let item = Map.find_exn t.items key in
      let args =
        if List.is_empty args
        then ""
        else "<" ^ String.concat ~sep:", " (List.map args ~f:ty) ^ ">"
      in
      let named = Rust_name.path ~from item.rust ^ args in
      if item.stored then sprintf "crate::Ref<%s>" named else named
  ;;

  let field_ty t ~from ~key (ty_ : Ty.t) =
    match t.boxed key ty_, ty_ with
    | false, _ -> ty t ~from ty_
    | true, Option inner -> sprintf "Option<Box<%s>>" (ty t ~from inner)
    | true, _ -> sprintf "Box<%s>" (ty t ~from ty_)
  ;;

  let convert (field : Field.t) =
    sprintf
      "crate::FromOcaml::from_ocaml(crate::raw::field(v, %d), cx)"
      field.index
  ;;

  let derive = "#[derive(Debug, Clone, PartialEq, Eq, Hash)]"

  let bounds (item : Item.t) ~trait =
    if List.is_empty item.decl.params
    then ""
    else
      "<"
      ^ String.concat
          ~sep:", "
          (List.map item.decl.params ~f:(fun param ->
             Rust_name.camel param ^ ": " ^ trait))
      ^ ">"
  ;;

  (** Builds [fields] of the block at [slot], each bound to [binding]. *)
  let fill_fields fields ~binding ~indent =
    List.map fields ~f:(fun (field : Field.t) ->
      sprintf
        "%slet built = crate::ToOcaml::to_ocaml(%s, cx);\n\
         %scx.set(slot, %d, built);\n"
        indent
        (binding field)
        indent
        field.index)
    |> String.concat
  ;;

  (** The [ToOcaml] and [Construct] impls of [item]. *)
  let to_ocaml (item : Item.t) ~name ~generics =
    let bounds = bounds item ~trait:"crate::ToOcaml" in
    let rebuild =
      if item.rebuilt
      then sprintf "Some(%S)" (R.Callback_name.rebuild item.key)
      else "None"
    in
    let shell, fill =
      match item.body with
      | Alias _ -> assert false
      | Record fields ->
        let size =
          match item.decl.kind with
          | Record fields -> List.length fields
          | Alias _ | Variant _ -> assert false
        in
        ( sprintf "        cx.block(%d, 0)" size
        , fill_fields
            fields
            ~binding:(fun field -> "&self." ^ Rust_name.field field.name)
            ~indent:"        " )
      | Variant constructors ->
        let binding (field : Field.t) =
          if String.is_empty field.name
          then sprintf "v%d" field.index
          else "v_" ^ Rust_name.field field.name
        in
        let pattern (constructor : Constructor.t) =
          let name = Rust_name.camel constructor.name in
          match constructor.args with
          | Tuple [] -> sprintf "Self::%s" name
          | Tuple fields ->
            sprintf
              "Self::%s(%s)"
              name
              (String.concat ~sep:", " (List.map fields ~f:binding))
          | Record fields ->
            sprintf
              "Self::%s { %s }"
              name
              (String.concat
                 ~sep:", "
                 (List.map fields ~f:(fun field ->
                    sprintf
                      "%s: %s"
                      (Rust_name.field field.name)
                      (binding field))
                  @ [ ".." ]))
        in
        let fields (constructor : Constructor.t) =
          match constructor.args with
          | Tuple fields | Record fields -> fields
        in
        ( sprintf
            "        match self {\n%s        }"
            (List.map constructors ~f:(fun constructor ->
               let name = Rust_name.camel constructor.name in
               if constructor.constant
               then
                 sprintf
                   "            Self::%s => crate::raw::immediate(%d),\n"
                   name
                   constructor.tag
               else
                 sprintf
                   "            Self::%s { .. } => cx.block(%d, %d),\n"
                   name
                   constructor.size
                   constructor.tag)
             |> String.concat)
        , sprintf
            "        match self {\n%s        }\n"
            (List.map constructors ~f:(fun constructor ->
               match fields constructor with
               | [] -> sprintf "            %s => {}\n" (pattern constructor)
               | fields ->
                 sprintf
                   "            %s => {\n%s            }\n"
                   (pattern constructor)
                   (fill_fields fields ~binding ~indent:"                "))
             |> String.concat) )
    in
    let uses_fields = not (List.is_empty (Body.field_types item.body)) in
    let shell_uses_cx =
      match item.body with
      | Variant constructors ->
        List.exists constructors ~f:(fun (constructor : Constructor.t) ->
          not constructor.constant)
      | Alias _ | Record _ -> true
    in
    String.concat
      ~sep:"\n"
      [ sprintf "impl%s crate::ToOcaml for %s%s {" bounds name generics
      ; "    unsafe fn to_ocaml(&self, cx: &mut crate::Builder) -> \
         crate::raw::Value {"
      ; "        cx.construct(self)"
      ; "    }"
      ; "}"
      ; ""
      ; sprintf "impl%s crate::Construct for %s%s {" bounds name generics
      ; sprintf "    const REBUILD: Option<&'static str> = %s;" rebuild
      ; ""
      ; sprintf
          "    unsafe fn shell(&self, %s: &mut crate::Builder) -> \
           crate::raw::Value {"
          (if shell_uses_cx then "cx" else "_cx")
      ; shell
      ; "    }"
      ; ""
      ; sprintf
          "    unsafe fn fill(&self, %s: crate::Slot, %s: &mut crate::Builder) \
           {"
          (if uses_fields then "slot" else "_slot")
          (if uses_fields then "cx" else "_cx")
      ; fill ^ "    }"
      ; "}"
      ; ""
      ]
  ;;

  let item t (item : Item.t) =
    let from = fst item.rust in
    let ty = ty ~from in
    let field_ty = field_ty ~from in
    let name = snd item.rust in
    let generics = generics item.decl.params in
    let doc = sprintf "/// OCaml: [%s]." (Key.to_string item.key) in
    let impl ~uses_cx body =
      let bounds =
        if List.is_empty item.decl.params
        then ""
        else
          "<"
          ^ String.concat
              ~sep:", "
              (List.map item.decl.params ~f:(fun param ->
                 Rust_name.camel param ^ ": crate::FromOcaml"))
          ^ ">"
      in
      sprintf
        "impl%s crate::FromOcaml for %s%s {\n\
        \    unsafe fn from_ocaml(v: crate::raw::Value, %s: &mut \
         crate::ArenaBuilder) -> Self {\n\
         %s\n\
        \    }\n\
         }\n"
        bounds
        name
        generics
        (if uses_cx then "cx" else "_cx")
        body
    in
    let uses_cx = not (List.is_empty (Body.field_types item.body)) in
    let impl = impl ~uses_cx in
    let struct_fields fields ~indent ~pub_ =
      List.map fields ~f:(fun (field : Field.t) ->
        sprintf
          "%s%s%s: %s,\n"
          indent
          (if pub_ then "pub " else "")
          (Rust_name.field field.name)
          (field_ty t ~key:item.key field.ty))
      |> String.concat
    in
    let init fields ~indent =
      List.map fields ~f:(fun (field : Field.t) ->
        sprintf
          "%s%s: %s,\n"
          indent
          (Rust_name.field field.name)
          (convert field))
      |> String.concat
    in
    let bad = sprintf "crate::raw::bad(%S, v)" (Key.to_string item.key) in
    match item.body with
    | Alias ty_ ->
      sprintf "%s\npub type %s%s = %s;\n" doc name generics (ty t ty_)
    | Record fields ->
      sprintf
        "%s\n%s\npub struct %s%s {\n%s}\n\n%s"
        doc
        derive
        name
        generics
        (struct_fields fields ~indent:"    " ~pub_:true)
        (impl
           (sprintf
              "        Self {\n%s        }"
              (init fields ~indent:"            ")))
      ^ "\n"
      ^ to_ocaml item ~name ~generics
    | Variant constructors ->
      let variant (constructor : Constructor.t) =
        let name = Rust_name.camel constructor.name in
        match constructor.args with
        | Tuple [] -> sprintf "    %s,\n" name
        | Tuple fields ->
          sprintf
            "    %s(%s),\n"
            name
            (String.concat
               ~sep:", "
               (List.map fields ~f:(fun field ->
                  field_ty t ~key:item.key field.ty)))
        | Record fields ->
          sprintf
            "    %s {\n%s    },\n"
            name
            (struct_fields fields ~indent:"        " ~pub_:false)
      in
      let case (constructor : Constructor.t) =
        let name = Rust_name.camel constructor.name in
        match constructor.args with
        | Tuple [] ->
          sprintf "                %d => Self::%s,\n" constructor.tag name
        | Tuple fields ->
          sprintf
            "                %d => Self::%s(\n%s                ),\n"
            constructor.tag
            name
            (List.map fields ~f:(fun field ->
               sprintf "                    %s,\n" (convert field))
             |> String.concat)
        | Record fields ->
          sprintf
            "                %d => Self::%s {\n%s                },\n"
            constructor.tag
            name
            (init fields ~indent:"                    ")
      in
      let matching ~constant ~scrutinee =
        let cases =
          List.filter constructors ~f:(fun (constructor : Constructor.t) ->
            Bool.equal constructor.constant constant)
        in
        if List.is_empty cases
        then sprintf "            %s" bad
        else
          sprintf
            "            match %s {\n%s                _ => %s,\n            }"
            scrutinee
            (List.map cases ~f:case |> String.concat)
            bad
      in
      sprintf
        "%s\n%s\npub enum %s%s {\n%s}\n\n%s"
        doc
        derive
        name
        generics
        (List.map constructors ~f:variant |> String.concat)
        (impl
           (sprintf
              "        if crate::raw::is_long(v) {\n\
               %s\n\
              \        } else {\n\
               %s\n\
              \        }"
              (matching ~constant:true ~scrutinee:"crate::raw::long(v)")
              (matching ~constant:false ~scrutinee:"crate::raw::tag(v)")))
      ^ "\n"
      ^ to_ocaml item ~name ~generics
  ;;
end

(** The declarations whose values a field of type [ty] contains inline. *)
let rec inline ~stored (ty : Ty.t) =
  match ty with
  | Var _ | Int | Bool | Char | Unit | String | Nothing -> []
  | List _ | Array _ | Map _ | Set _ -> []
  | Option ty -> inline ~stored ty
  | Tuple tys -> List.concat_map tys ~f:(inline ~stored)
  | Decl (key, args) ->
    if stored key then [] else key :: List.concat_map args ~f:(inline ~stored)
;;

let reachability (items : Item.t Key.Map.t) ~stored =
  let edges =
    Map.map items ~f:(fun item ->
      List.concat_map (Body.field_types item.body) ~f:(inline ~stored))
  in
  Map.map items ~f:(fun item ->
    let seen = Key.Hash_set.create () in
    let rec visit key =
      if not (Hash_set.mem seen key)
      then (
        Hash_set.add seen key;
        List.iter (Map.find_exn edges key) ~f:visit)
    in
    List.iter (Map.find_exn edges item.key) ~f:visit;
    seen)
;;

let rec subst (ty : Ty.t) ~by : Ty.t =
  match ty with
  | Var param -> by param
  | Int | Bool | Char | Unit | String | Nothing -> ty
  | List ty -> List (subst ty ~by)
  | Array ty -> Array (subst ty ~by)
  | Option ty -> Option (subst ty ~by)
  | Set ty -> Set (subst ty ~by)
  | Map (key, data) -> Map (subst key ~by, subst data ~by)
  | Tuple tys -> Tuple (List.map tys ~f:(subst ~by))
  | Decl (key, args) -> Decl (key, List.map args ~f:(subst ~by))
;;

(** Every [Decl] of a stored type in [tys], which must not have parameters,
    seeing through the aliases [alias] gives the bodies of. *)
let rec instances ~stored ~alias (ty : Ty.t) =
  match ty with
  | Var _ | Int | Bool | Char | Unit | String | Nothing -> Ok []
  | List ty | Array ty | Option ty | Set ty -> instances ~stored ~alias ty
  | Map (key, data) -> instances_of ~stored ~alias [ key; data ]
  | Tuple tys -> instances_of ~stored ~alias tys
  | Decl (key, args) ->
    let open Or_error.Let_syntax in
    let%bind inner = instances_of ~stored ~alias args in
    (match alias key args with
     | Some body ->
       let%map through = instances ~stored ~alias body in
       through @ inner
     | None ->
       if not (stored key)
       then Ok inner
       else if List.is_empty (List.concat_map args ~f:params)
       then Ok (ty :: inner)
       else
         Or_error.error_s
           [%message
             "Stored types must be instantiated without type parameters"
               ~type_:(Key.to_string key)])

and instances_of ~stored ~alias tys =
  List.map tys ~f:(instances ~stored ~alias)
  |> Or_error.all
  |> Or_error.map ~f:List.concat
;;

(* Qualified by modules at the top only: eg. [type_var_ty_nothing]. *)
let rec instance_name ?(qualified = false) (gen : Gen.t) (ty : Ty.t) =
  let instance_name = instance_name gen in
  match ty with
  | Var name -> name
  | Int -> "int"
  | Bool -> "bool"
  | Char -> "char"
  | Unit -> "unit"
  | String -> "string"
  | Nothing -> "nothing"
  | List ty -> instance_name ty ^ "_list"
  | Array ty -> instance_name ty ^ "_array"
  | Option ty -> instance_name ty ^ "_option"
  | Set ty -> instance_name ty ^ "_set"
  | Map (key, data) -> instance_name key ^ "_" ^ instance_name data ^ "_map"
  | Tuple tys -> String.concat ~sep:"_" (List.map tys ~f:instance_name)
  | Decl (key, args) ->
    let modules, name = (Map.find_exn gen.items key).rust in
    let modules =
      if qualified
      then List.map modules ~f:(String.chop_suffix_if_exists ~suffix:"_")
      else []
    in
    String.concat
      ~sep:"_"
      ((modules @ [ Rust_name.snake name ]) @ List.map args ~f:instance_name)
;;

let arena (gen : Gen.t) instances =
  let instances =
    List.map instances ~f:(fun ty ->
      instance_name ~qualified:true gen ty, Gen.ty gen ~from:[] ty)
    |> List.dedup_and_sort ~compare:[%compare: string * string]
  in
  let%map.Or_error () =
    List.sort_and_group instances ~compare:(fun (a, _) (b, _) ->
      String.compare a b)
    |> List.filter_map ~f:(function
      | [] | [ _ ] -> None
      | (field, _) :: _ as group ->
        Some
          (Error.create_s
             [%message
               "Same arena field"
                 field
                 ~types:(List.map group ~f:snd : string list)]))
    |> function
    | [] -> Ok ()
    | errors -> Error (Error.of_list errors)
  in
  let fields ~pub_ ~ty =
    List.map instances ~f:(fun (name, rust) ->
      sprintf "    %s%s: %s,\n" (if pub_ then "pub " else "") name (ty rust))
    |> String.concat
  in
  let stored =
    List.mapi instances ~f:(fun index (name, rust) ->
      let stored = String.chop_prefix_exn rust ~prefix:"crate::Ref<" in
      let stored = String.drop_suffix stored 1 in
      sprintf
        "impl crate::Stored for %s {\n\
        \    fn pending(builder: &mut ArenaBuilder) -> &mut \
         crate::Pending<Self> {\n\
        \        &mut builder.%s\n\
        \    }\n\
        \    fn store(arena: &Arena) -> &crate::Store<Self> {\n\
        \        &arena.%s\n\
        \    }\n\
        \    fn store_mut(arena: &mut Arena) -> &mut crate::Store<Self> {\n\
        \        &mut arena.%s\n\
        \    }\n\
         }\n\n\
         impl crate::Indexed for %s {\n\
        \    const STORE: usize = %d;\n\
         }\n"
        stored
        name
        name
        name
        stored
        index)
    |> String.concat ~sep:"\n"
  in
  let inner rust =
    String.drop_suffix (String.chop_prefix_exn rust ~prefix:"crate::Ref<") 1
  in
  sprintf
    "/// The stored values of a conversion, by [`crate::Ref`].\n\
     #[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]\n\
     pub struct Arena {\n\
     %s}\n\n\
     /// An [`Arena`] being converted.\n\
     #[derive(Default)]\n\
     pub struct ArenaBuilder {\n\
     %s}\n\n\
     impl ArenaBuilder {\n\
    \    pub fn finish(self) -> Arena {\n\
    \        Arena {\n\
     %s        }\n\
    \    }\n\
     }\n\n\
     impl Arena {\n\
    \    /// The lengths of its stores, by [`crate::Indexed::STORE`].\n\
    \    pub fn store_lens(&self) -> Vec<usize> {\n\
    \        vec![%s]\n\
    \    }\n\
     }\n\n\
     %s"
    (fields ~pub_:true ~ty:(fun rust -> sprintf "crate::Store<%s>" (inner rust)))
    (fields ~pub_:false ~ty:(fun rust ->
       sprintf "crate::Pending<%s>" (inner rust)))
    (List.map instances ~f:(fun (name, _) ->
       sprintf "            %s: self.%s.finish(),\n" name name)
     |> String.concat)
    (List.map instances ~f:(fun (name, _) -> sprintf "self.%s.len()" name)
     |> String.concat ~sep:", ")
    stored
;;

module Tree = struct
  type t =
    { items : string list
    ; children : t String.Map.t
    }

  let empty = { items = []; children = String.Map.empty }

  let rec add t path item =
    match path with
    | [] -> { t with items = t.items @ [ item ] }
    | first :: rest ->
      { t with
        children =
          Map.update t.children first ~f:(fun child ->
            add (Option.value child ~default:empty) rest item)
      }
  ;;

  let rec render t ~indent =
    let indent_lines text =
      String.split_lines text
      |> List.map ~f:(fun line ->
        if String.is_empty line then line else indent ^ line)
      |> String.concat ~sep:"\n"
    in
    let items = List.map t.items ~f:(fun item -> indent_lines item ^ "\n") in
    let children =
      Map.to_alist t.children
      |> List.map ~f:(fun (name, child) ->
        sprintf
          "%spub mod %s {\n%s%s}\n"
          indent
          name
          (render child ~indent:(indent ^ "    "))
          indent)
    in
    String.concat ~sep:"\n" (items @ children)
  ;;
end

(** Every map key and set element type in [ty]. *)
let rec keys (ty : Ty.t) =
  match ty with
  | Var _ | Int | Bool | Char | Unit | String | Nothing -> []
  | List ty | Array ty | Option ty -> keys ty
  | Set elt -> elt :: keys elt
  | Map (key, data) -> (key :: keys key) @ keys data
  | Tuple tys | Decl (_, tys) -> List.concat_map tys ~f:keys
;;

(** The [OcamlKey] impls of the declarations [keys] gives. *)
let ocaml_keys (gen : Gen.t) ~follow tys =
  List.filter_map tys ~f:(fun (ty : Ty.t) ->
    match ty with
    | Int | Bool | Char | String -> None
    | _ ->
      Some
        (match R.Type_expr.key_name ty with
         | None ->
           Or_error.error_s
             [%message "Unsupported map key or set element" (ty : Ty.t)]
         | Some name -> Ok (Gen.ty gen ~from:[] (follow ty), name)))
  |> Or_error.all
  |> Or_error.map ~f:(fun keys ->
    List.dedup_and_sort keys ~compare:(fun (a, _) (b, _) -> String.compare a b)
    |> List.map ~f:(fun (rust, name) ->
      sprintf
        "impl crate::OcamlKey for %s {\n\
        \    const MAP: &'static str = %S;\n\
        \    const SET: &'static str = %S;\n\
         }\n"
        rust
        (R.Callback_name.map name)
        (R.Callback_name.set name))
    |> String.concat ~sep:"\n")
;;

let generate ~rebuilt (decls : R.Decl.t list) =
  let open Or_error.Let_syntax in
  let rebuilt = Key.Set.of_list rebuilt in
  let%bind decls =
    match
      Key.Map.of_alist
        (List.map decls ~f:(fun (decl : R.Decl.t) -> decl.key, decl))
    with
    | `Ok decls -> Ok decls
    | `Duplicate_key key ->
      Or_error.error_s [%message "Registered twice" ~type_:(Key.to_string key)]
  in
  let%bind items =
    Map.to_alist decls
    |> List.map ~f:(fun (key, (decl : R.Decl.t)) ->
      let%bind body = body ~check:(check ~decls ~decl) decl in
      let mutable_ =
        match decl.kind with
        | Record fields -> List.exists fields ~f:(fun field -> field.mutable_)
        | Alias _ | Variant _ -> false
      in
      let%bind () =
        let used =
          List.concat_map (Body.field_types body) ~f:params
          |> String.Set.of_list
        in
        match
          List.filter decl.params ~f:(fun param -> not (Set.mem used param))
        with
        | [] -> Ok ()
        | unused ->
          Or_error.error_s
            [%message
              "Unused type parameters"
                ~type_:(Key.to_string key)
                (unused : string list)]
      in
      let%map () =
        let skipped = Item.has_skipped_fields decl in
        match skipped, Set.mem rebuilt key with
        | true, true | false, false -> Ok ()
        | true, false ->
          Or_error.error_s
            [%message
              "Declarations with [@rust.skip] fields need \
               Ppx_rust_runtime.register_rebuild"
                ~type_:(Key.to_string key)]
        | false, true ->
          Or_error.error_s
            [%message
              "Rebuilt declarations need [@rust.skip] fields"
                ~type_:(Key.to_string key)]
      in
      ( key
      , { Item.decl
        ; key
        ; rust =
            Rust_name.decl
              ~module_path:(List.drop_last_exn key)
              ~name:(List.last_exn key)
        ; body
        ; stored = decl.shared || mutable_
        ; rebuilt = Set.mem rebuilt key
        } ))
    |> Or_error.all
    |> Or_error.map ~f:Key.Map.of_alist_exn
  in
  let stored key = (Map.find_exn items key).stored in
  (* An alias of a declaration of the same Rust name, with the same
     parameters, is a re-export of it. *)
  let reexport (item : Item.t) =
    match item.body with
    | Alias (Decl (key, args)) ->
      let target = Map.find_exn items key in
      [%equal: string list * string] target.rust item.rust
      && Poly.equal
           args
           (List.map item.decl.params ~f:(fun param -> Ty.Var param))
    | _ -> false
  in
  let reexports, items = Map.partition_tf items ~f:reexport in
  let key_types =
    Map.data items
    |> List.concat_map ~f:(fun (item : Item.t) ->
      List.concat_map (Body.field_types item.body) ~f:keys)
  in
  let rec follow (ty : Ty.t) : Ty.t =
    match ty with
    | Var _ | Int | Bool | Char | Unit | String | Nothing -> ty
    | List ty -> List (follow ty)
    | Array ty -> Array (follow ty)
    | Option ty -> Option (follow ty)
    | Set ty -> Set (follow ty)
    | Map (key, data) -> Map (follow key, follow data)
    | Tuple tys -> Tuple (List.map tys ~f:follow)
    | Decl (key, args) ->
      let args = List.map args ~f:follow in
      let rec target key =
        match Map.find reexports key with
        | Some { body = Alias (Decl (key, _)); _ } -> target key
        | Some _ | None -> key
      in
      Decl (target key, args)
  in
  let follow_field (field : Field.t) = { field with ty = follow field.ty } in
  let items =
    Map.map items ~f:(fun (item : Item.t) ->
      let body : Body.t =
        match item.body with
        | Alias ty -> Alias (follow ty)
        | Record fields -> Record (List.map fields ~f:follow_field)
        | Variant constructors ->
          Variant
            (List.map constructors ~f:(fun (constructor : Constructor.t) ->
               { constructor with
                 args =
                   (match constructor.args with
                    | Tuple fields -> Tuple (List.map fields ~f:follow_field)
                    | Record fields -> Record (List.map fields ~f:follow_field))
               }))
      in
      { item with body })
  in
  let%bind () =
    Map.data items
    |> List.map ~f:(fun (item : Item.t) -> item.rust, item.key)
    |> List.sort_and_group ~compare:(fun (a, _) (b, _) ->
      [%compare: string list * string] a b)
    |> List.filter_map ~f:(function
      | [] | [ _ ] -> None
      | (rust, _) :: _ as group ->
        Some
          [%message
            "Same Rust name"
              ~rust:(Rust_name.path ~from:[] rust)
              ~types:
                (List.map group ~f:(fun (_, key) -> Key.to_string key)
                 : string list)])
    |> function
    | [] -> Ok ()
    | errors -> Error (Error.of_list (List.map errors ~f:Error.create_s))
  in
  let%bind () =
    Map.data items
    |> List.filter_map ~f:(fun (item : Item.t) ->
      let clash =
        match item.rust with
        | [], name -> Set.mem Rust_name.crate_types name
        | first :: _, _ -> Set.mem Rust_name.crate_modules first
      in
      Option.some_if
        clash
        (Error.create_s
           [%message
             "Rust name clashes with the crate's own"
               ~rust:(Rust_name.path ~from:[] item.rust)
               ~type_:(Key.to_string item.key)]))
    |> function
    | [] -> Ok ()
    | errors -> Error (Error.of_list errors)
  in
  let reaches = reachability items ~stored in
  let boxed key ty =
    List.exists (inline ~stored ty) ~f:(fun mentioned ->
      Hash_set.mem (Map.find_exn reaches mentioned) key)
  in
  let gen = { Gen.items; boxed } in
  (* Generic aliases are instantiated where they are used. *)
  let alias key args =
    let item = Map.find_exn items key in
    match item.body with
    | Alias body when not (List.is_empty item.decl.params) ->
      let by =
        List.zip_exn item.decl.params args
        |> String.Map.of_alist_exn
        |> Map.find_exn
      in
      Some (subst body ~by)
    | Alias _ | Record _ | Variant _ -> None
  in
  let%bind instances =
    Map.data items
    |> List.concat_map ~f:(fun (item : Item.t) ->
      match item.body with
      | Alias _ when not (List.is_empty item.decl.params) -> []
      | body -> Body.field_types body)
    |> instances_of ~stored ~alias
  in
  let tree =
    Map.data items
    |> List.fold ~init:Tree.empty ~f:(fun tree (item : Item.t) ->
      Tree.add tree (fst item.rust) (Gen.item gen item))
  in
  let%bind ocaml_keys = ocaml_keys gen ~follow key_types in
  let%map arena = arena gen instances in
  let tree = Tree.add tree [] arena in
  let tree = Tree.add tree [] ocaml_keys in
  let send_sync =
    Map.data items
    |> List.filter ~f:(fun (item : Item.t) -> List.is_empty item.decl.params)
    |> List.map ~f:(fun (item : Item.t) ->
      sprintf "    is_send_sync::<%s>();\n" (Rust_name.path ~from:[] item.rust))
    |> String.concat
  in
  let tree =
    Tree.add
      tree
      []
      (sprintf
         "#[allow(dead_code)]\n\
          fn assert_send_sync() {\n\
         \    fn is_send_sync<T: Send + Sync>() {}\n\
         \    is_send_sync::<Arena>();\n\
          %s}\n"
         send_sync)
  in
  "// Generated by interpreter/gen from the [@@deriving rust] declarations: do \
   not edit.\n\n"
  ^ Tree.render tree ~indent:""
;;
