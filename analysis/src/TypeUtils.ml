open SharedTypes

let debug_log_type_arg_context {env; type_args; type_params} =
  Printf.sprintf "Type arg context. env: %s, typeArgs: %s, typeParams: %s\n"
    (Debug.debug_print_env env)
    (type_args |> List.map Shared.type_to_string |> String.concat ", ")
    (type_params |> List.map Shared.type_to_string |> String.concat ", ")

(** Checks whether this type has any uninstantiated type parameters. *)
let rec has_tvar (ty : Types.type_expr) : bool =
  match ty.desc with
  | Tvar _ -> true
  | Tarrow (_, ty1, ty2, _) -> has_tvar ty1 || has_tvar ty2
  | Ttuple tyl -> List.exists has_tvar tyl
  | Tconstr (_, tyl, _) -> List.exists has_tvar tyl
  | Tobject (ty, _) -> has_tvar ty
  | Tfield (_, _, ty1, ty2) -> has_tvar ty1 || has_tvar ty2
  | Tnil -> false
  | Tlink ty -> has_tvar ty
  | Tsubst ty -> has_tvar ty
  | Tvariant {row_fields; _} ->
    List.exists
      (function
        | _, Types.Rpresent (Some ty) -> has_tvar ty
        | _, Reither (_, tyl, _, _) -> List.exists has_tvar tyl
        | _ -> false)
      row_fields
  | Tunivar _ -> true
  | Tpoly (ty, tyl) -> has_tvar ty || List.exists has_tvar tyl
  | Tpackage (_, _, tyl) -> List.exists has_tvar tyl

let find_type_via_loc ~full ~debug (loc : Location.t) =
  match References.get_loc_item ~full ~pos:(Pos.of_lexing loc.loc_end) ~debug with
  | Some {loc_type = Typed (_, typ_expr, _)} -> Some typ_expr
  | _ -> None

let rec path_from_type_expr (t : Types.type_expr) =
  match t.desc with
  | Tconstr (Pident {name = "function$"}, [t; _], _) -> path_from_type_expr t
  | Tconstr (path, _typeArgs, _)
  | Tlink {desc = Tconstr (path, _typeArgs, _)}
  | Tsubst {desc = Tconstr (path, _typeArgs, _)}
  | Tpoly ({desc = Tconstr (path, _typeArgs, _)}, []) ->
    Some path
  | _ -> None

let print_record_from_fields ?name (fields : field list) =
  (match name with
  | None -> ""
  | Some name -> "type " ^ name ^ " = ")
  ^ "{"
  ^ (fields
    |> List.map (fun f -> f.fname.txt ^ ": " ^ Shared.type_to_string f.typ)
    |> String.concat ", ")
  ^ "}"

let rec extracted_type_to_string ?(name_only = false) ?(inner = false) = function
  | Tuple (_, _, typ) | Tpolyvariant {type_expr = typ} | Tfunction {typ} ->
    if inner then
      try typ |> path_from_type_expr |> Option.get |> SharedTypes.path_ident_to_string
      with _ -> ""
    else Shared.type_to_string typ
  | Trecord {definition; fields} ->
    let name =
      match definition with
      | `TypeExpr typ -> (
        try
          typ |> path_from_type_expr |> Option.get |> SharedTypes.path_ident_to_string
        with _ -> "")
      | `NameOnly name -> name
    in
    if inner || name_only then name else print_record_from_fields ~name fields
  | Tbool _ -> "bool"
  | Tstring _ -> "string"
  | TtypeT _ -> "type t"
  | Tarray (_, TypeExpr inner_typ) ->
    "array<" ^ Shared.type_to_string inner_typ ^ ">"
  | Tarray (_, ExtractedType inner_typ) ->
    "array<" ^ extracted_type_to_string ~inner:true inner_typ ^ ">"
  | Toption (_, TypeExpr inner_typ) ->
    "option<" ^ Shared.type_to_string inner_typ ^ ">"
  | Tresult {ok_type; error_type} ->
    "result<" ^ Shared.type_to_string ok_type ^ ", "
    ^ Shared.type_to_string error_type
    ^ ">"
  | Toption (_, ExtractedType inner_typ) ->
    "option<" ^ extracted_type_to_string ~inner:true inner_typ ^ ">"
  | Tpromise (_, inner_typ) -> "promise<" ^ Shared.type_to_string inner_typ ^ ">"
  | Tvariant {variant_decl; variant_name} ->
    if inner || name_only then variant_name
    else Shared.decl_to_string variant_name variant_decl
  | TinlineRecord {fields} -> print_record_from_fields fields
  | Texn _ -> "exn"

let get_extracted_type maybe_res =
  match maybe_res with
  | None -> None
  | Some (extracted_type, _) -> Some extracted_type

let instantiate_type ~type_params ~type_args (t : Types.type_expr) =
  if type_params = [] || type_args = [] then t
  else
    let rec apply_sub tp ta t =
      match (tp, ta) with
      | t1 :: t_rest1, t2 :: t_rest2 ->
        if t1 = t then t2 else apply_sub t_rest1 t_rest2 t
      | [], _ | _, [] -> t
    in
    let rec loop (t : Types.type_expr) =
      match t.desc with
      | Tlink t -> loop t
      | Tvar _ -> apply_sub type_params type_args t
      | Tunivar _ -> t
      | Tconstr (path, args, memo) ->
        {t with desc = Tconstr (path, args |> List.map loop, memo)}
      | Tsubst t -> loop t
      | Tvariant rd -> {t with desc = Tvariant (row_desc rd)}
      | Tnil -> t
      | Tarrow (lbl, t1, t2, c) ->
        {t with desc = Tarrow (lbl, loop t1, loop t2, c)}
      | Ttuple tl -> {t with desc = Ttuple (tl |> List.map loop)}
      | Tobject (t, r) -> {t with desc = Tobject (loop t, r)}
      | Tfield (n, k, t1, t2) -> {t with desc = Tfield (n, k, loop t1, loop t2)}
      | Tpoly (t, []) -> loop t
      | Tpoly (t, tl) -> {t with desc = Tpoly (loop t, tl |> List.map loop)}
      | Tpackage (p, l, tl) ->
        {t with desc = Tpackage (p, l, tl |> List.map loop)}
    and row_desc (rd : Types.row_desc) =
      let row_fields =
        rd.row_fields |> List.map (fun (l, rf) -> (l, row_field rf))
      in
      let row_more = loop rd.row_more in
      let row_name =
        match rd.row_name with
        | None -> None
        | Some (p, tl) -> Some (p, tl |> List.map loop)
      in
      {rd with row_fields; row_more; row_name}
    and row_field (rf : Types.row_field) =
      match rf with
      | Rpresent None -> rf
      | Rpresent (Some t) -> Rpresent (Some (loop t))
      | Reither (b1, tl, b2, r) -> Reither (b1, tl |> List.map loop, b2, r)
      | Rabsent -> Rabsent
    in
    loop t

let instantiate_type2 ?(type_arg_context : type_arg_context option)
    (t : Types.type_expr) =
  match type_arg_context with
  | None | Some {type_args = []} | Some {type_params = []} -> t
  | Some {type_args; type_params} ->
    let rec apply_sub tp ta name =
      match (tp, ta) with
      | {Types.desc = Tvar (Some var_name)} :: t_rest1, t2 :: t_rest2 ->
        if var_name = name then t2 else apply_sub t_rest1 t_rest2 name
      | _ :: t_rest1, _ :: t_rest2 -> apply_sub t_rest1 t_rest2 name
      | [], _ | _, [] -> t
    in

    let rec loop (t : Types.type_expr) =
      match t.desc with
      | Tlink t -> loop t
      | Tvar (Some name) -> apply_sub type_params type_args name
      | Tvar _ -> t
      | Tunivar _ -> t
      | Tconstr (path, args, memo) ->
        {t with desc = Tconstr (path, args |> List.map loop, memo)}
      | Tsubst t -> loop t
      | Tvariant rd -> {t with desc = Tvariant (row_desc rd)}
      | Tnil -> t
      | Tarrow (lbl, t1, t2, c) ->
        {t with desc = Tarrow (lbl, loop t1, loop t2, c)}
      | Ttuple tl -> {t with desc = Ttuple (tl |> List.map loop)}
      | Tobject (t, r) -> {t with desc = Tobject (loop t, r)}
      | Tfield (n, k, t1, t2) -> {t with desc = Tfield (n, k, loop t1, loop t2)}
      | Tpoly (t, []) -> loop t
      | Tpoly (t, tl) -> {t with desc = Tpoly (loop t, tl |> List.map loop)}
      | Tpackage (p, l, tl) ->
        {t with desc = Tpackage (p, l, tl |> List.map loop)}
    and row_desc (rd : Types.row_desc) =
      let row_fields =
        rd.row_fields |> List.map (fun (l, rf) -> (l, row_field rf))
      in
      let row_more = loop rd.row_more in
      let row_name =
        match rd.row_name with
        | None -> None
        | Some (p, tl) -> Some (p, tl |> List.map loop)
      in
      {rd with row_fields; row_more; row_name}
    and row_field (rf : Types.row_field) =
      match rf with
      | Rpresent None -> rf
      | Rpresent (Some t) -> Rpresent (Some (loop t))
      | Reither (b1, tl, b2, r) -> Reither (b1, tl |> List.map loop, b2, r)
      | Rabsent -> Rabsent
    in
    loop t

let rec extract_record_type ~env ~package (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> extract_record_type ~env ~package t1
  | Tconstr (path, type_args, _) -> (
    match References.dig_constructor ~env ~package path with
    | Some (env, ({item = {kind = Record fields}} as typ)) ->
      let type_params = typ.item.decl.type_params in
      let fields =
        fields
        |> List.map (fun field ->
               let field_typ =
                 field.typ |> instantiate_type ~type_params ~type_args
               in
               {field with typ = field_typ})
      in
      Some (env, fields, typ)
    | Some
        ( env,
          {item = {decl = {type_manifest = Some t1; type_params = type_params}}}
        ) ->
      let t1 = t1 |> instantiate_type ~type_params ~type_args in
      extract_record_type ~env ~package t1
    | _ -> None)
  | _ -> None

let rec extract_object_type ~env ~package (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> extract_object_type ~env ~package t1
  | Tobject (t_obj, _) -> Some (env, t_obj)
  | Tconstr (path, type_args, _) -> (
    match References.dig_constructor ~env ~package path with
    | Some
        ( env,
          {item = {decl = {type_manifest = Some t1; type_params = type_params}}}
        ) ->
      let t1 = t1 |> instantiate_type ~type_params ~type_args in
      extract_object_type ~env ~package t1
    | _ -> None)
  | _ -> None

let rec extract_function_type ~env ~package typ =
  let rec loop ~env acc (t : Types.type_expr) =
    match t.desc with
    | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> loop ~env acc t1
    | Tarrow (label, t_arg, t_ret, _) -> loop ~env ((label, t_arg) :: acc) t_ret
    | Tconstr (Pident {name = "function$"}, [t; _], _) ->
      extract_function_type ~env ~package t
    | Tconstr (path, type_args, _) -> (
      match References.dig_constructor ~env ~package path with
      | Some
          ( env,
            {
              item = {decl = {type_manifest = Some t1; type_params = type_params}};
            } ) ->
        let t1 = t1 |> instantiate_type ~type_params ~type_args in
        loop ~env acc t1
      | _ -> (List.rev acc, t))
    | _ -> (List.rev acc, t)
  in
  loop ~env [] typ

let maybe_set_type_arg_ctx ?type_arg_context_from_type_manifest ~type_params ~type_args env
    =
  match type_arg_context_from_type_manifest with
  | Some type_arg_context_from_type_manifest -> Some type_arg_context_from_type_manifest
  | None ->
    let type_arg_context =
      if List.length type_params > 0 then Some {env; type_params; type_args}
      else None
    in
    (match type_arg_context with
    | None -> ()
    | Some type_arg_context ->
      if Debug.verbose () then
        Printf.printf "[#type_arg_ctx]--> setting new type arg ctx: %s"
          (debug_log_type_arg_context type_arg_context));
    type_arg_context

(* TODO(env-stuff) Maybe this could be removed entirely if we can guarantee that we don't have to look up functions from in here. *)
let rec extract_function_type2 ?type_arg_context ~env ~package typ =
  let rec loop ?type_arg_context ~env acc (t : Types.type_expr) =
    match t.desc with
    | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> loop ?type_arg_context ~env acc t1
    | Tarrow (label, t_arg, t_ret, _) ->
      loop ?type_arg_context ~env ((label, t_arg) :: acc) t_ret
    | Tconstr (Pident {name = "function$"}, [t; _], _) ->
      extract_function_type2 ?type_arg_context ~env ~package t
    | Tconstr (path, type_args, _) -> (
      match References.dig_constructor ~env ~package path with
      | Some
          ( env,
            {
              item = {decl = {type_manifest = Some t1; type_params = type_params}};
            } ) ->
        let type_arg_context = maybe_set_type_arg_ctx ~type_params ~type_args env in
        loop ?type_arg_context ~env acc t1
      | _ -> (List.rev acc, t, type_arg_context))
    | _ -> (List.rev acc, t, type_arg_context)
  in
  loop ?type_arg_context ~env [] typ

let rec extract_type ?(print_opening_debug = true)
    ?(type_arg_context : type_arg_context option)
    ?(type_arg_context_from_type_manifest : type_arg_context option) ~env ~package
    (t : Types.type_expr) =
  let maybe_set_type_arg_ctx = maybe_set_type_arg_ctx ?type_arg_context_from_type_manifest in
  if Debug.verbose () && print_opening_debug then
    Printf.printf
      "[extract_type]--> starting extraction of type: %s, in env: %s. Has type \
       arg ctx: %b\n"
      (Shared.type_to_string t) (Debug.debug_print_env env)
      (Option.is_some type_arg_context);
  (match type_arg_context with
  | None -> ()
  | Some type_arg_context ->
    if Debug.verbose () && print_opening_debug then
      Printf.printf "[extract_type]--> %s"
        (debug_log_type_arg_context type_arg_context));
  let instantiate_type = instantiate_type2 in
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) ->
    extract_type ?type_arg_context ~print_opening_debug:false ~env ~package t1
  | Tconstr (Path.Pident {name = "option"}, [payload_type_expr], _) ->
    Some (Toption (env, TypeExpr payload_type_expr), type_arg_context)
  | Tconstr (Path.Pident {name = "promise"}, [payload_type_expr], _) ->
    Some (Tpromise (env, payload_type_expr), type_arg_context)
  | Tconstr (Path.Pident {name = "array"}, [payload_type_expr], _) ->
    Some (Tarray (env, TypeExpr payload_type_expr), type_arg_context)
  | Tconstr (Path.Pident {name = "result"}, [ok_type; error_type], _) ->
    Some (Tresult {env; ok_type; error_type}, type_arg_context)
  | Tconstr (Path.Pident {name = "bool"}, [], _) ->
    Some (Tbool env, type_arg_context)
  | Tconstr (Path.Pident {name = "string"}, [], _) ->
    Some (Tstring env, type_arg_context)
  | Tconstr (Path.Pident {name = "exn"}, [], _) ->
    Some (Texn env, type_arg_context)
  | Tconstr (Pident {name = "function$"}, [t; _], _) -> (
    match extract_function_type2 ?type_arg_context t ~env ~package with
    | args, t_ret, type_arg_context when args <> [] ->
      Some
        ( Tfunction {env; args; typ = t; uncurried = true; return_type = t_ret},
          type_arg_context )
    | _args, _tRet, _typeArgContext -> None)
  | Tarrow _ -> (
    match extract_function_type2 ?type_arg_context t ~env ~package with
    | args, t_ret, type_arg_context when args <> [] ->
      Some
        ( Tfunction {env; args; typ = t; uncurried = false; return_type = t_ret},
          type_arg_context )
    | _args, _tRet, _typeArgContext -> None)
  | Tconstr (path, type_args, _) -> (
    if Debug.verbose () then
      Printf.printf "[extract_type]--> digging for type %s in %s\n"
        (Path.name path) (Debug.debug_print_env env);
    match References.dig_constructor ~env ~package path with
    | Some
        ( env_from_declaration,
          {item = {decl = {type_manifest = Some t1; type_params}}} ) ->
      if Debug.verbose () then
        print_endline "[extract_type]--> found type manifest";

      (* Type manifests inherit the last type args ctx that wasn't for a type manifest.
         This is because the manifest itself doesn't have type args and an env that can
         be used to instantiate. *)
      let type_arg_context =
        maybe_set_type_arg_ctx ~type_params:type_params ~type_args env
      in
      t1
      |> extract_type ?type_arg_context_from_type_manifest:type_arg_context
           ~env:env_from_declaration ~package
    | Some (env_from_item, {name; item = {decl; kind = Type.Variant constructors}})
      ->
      if Debug.verbose () then print_endline "[extract_type]--> found variant";
      let type_arg_context =
        maybe_set_type_arg_ctx ~type_params:decl.type_params ~type_args env
      in
      Some
        ( Tvariant
            {
              env = env_from_item;
              constructors;
              variant_name = name.txt;
              variant_decl = decl;
            },
          type_arg_context )
    | Some (env_from_declaration, {item = {kind = Record fields; decl}}) ->
      if Debug.verbose () then print_endline "[extract_type]--> found record";
      (* Need to create a new type arg context here because we're sending along a type expr that might have type vars. *)
      let type_arg_context =
        maybe_set_type_arg_ctx ~type_params:decl.type_params ~type_args env
      in
      Some
        ( Trecord {env = env_from_declaration; fields; definition = `TypeExpr t},
          type_arg_context )
    | Some (env_from_declaration, {item = {name = "t"; decl = {type_params}}}) ->
      let type_arg_context =
        maybe_set_type_arg_ctx ~type_params:type_params ~type_args env
      in
      Some (TtypeT {env = env_from_declaration; path}, type_arg_context)
    | None ->
      if Debug.verbose () then
        print_endline "[extract_type]--> found nothing when digging";
      None
    | _ ->
      if Debug.verbose () then
        print_endline "[extract_type]--> found something else when digging";
      None)
  | Ttuple expressions -> Some (Tuple (env, expressions, t), type_arg_context)
  | Tvariant {row_fields} ->
    let constructors =
      row_fields
      |> List.map (fun (label, field) ->
             {
               name = label;
               display_name = Utils.print_maybe_exotic_ident ~allow_uident:true label;
               args =
                 (* Multiple arguments are represented as a Ttuple, while a single argument is just the type expression itself. *)
                 (match field with
                 | Types.Rpresent (Some type_expr) -> (
                   match type_expr.desc with
                   | Ttuple args -> args
                   | _ -> [type_expr])
                 | _ -> []);
             })
    in
    Some (Tpolyvariant {env; constructors; type_expr = t}, type_arg_context)
  | Tvar (Some var_name) -> (
    if Debug.verbose () then
      Printf.printf
        "[extract_type]--> found type variable: '%s. Trying to instantiate %s"
        var_name
        (match type_arg_context with
        | None -> "with no type args ctx\n"
        | Some type_arg_context ->
          Printf.sprintf "with %s" (debug_log_type_arg_context type_arg_context));

    let instantiated = t |> instantiate_type ?type_arg_context in
    let rec extract_instantiated t =
      match t.Types.desc with
      | Tlink t1 | Tsubst t1 | Tpoly (t1, []) -> extract_instantiated t1
      | _ -> t
    in
    match extract_instantiated instantiated with
    | {desc = Tvar _} ->
      if Debug.verbose () then
        Printf.printf "[extract_type]--> could not instantiate '%s. Skipping.\n"
          var_name;
      None
    | _ ->
      if Debug.verbose () then
        Printf.printf
          "[extract_type]--> SUCCEEDED instantiation, new type is: %s\n"
          (Shared.type_to_string instantiated);

      (* Use the env from instantiation if we managed to instantiate the type param *)
      let next_env =
        match type_arg_context with
        | Some {env} -> env
        | None -> env
      in
      instantiated |> extract_type ?type_arg_context ~env:next_env ~package)
  | _ ->
    if Debug.verbose () then print_endline "[extract_type]--> miss";
    None

let find_return_type_of_function_at_loc loc ~(env : QueryEnv.t) ~full ~debug =
  match References.get_loc_item ~full ~pos:(loc |> Loc.end_) ~debug with
  | Some {loc_type = Typed (_, typ_expr, _)} -> (
    match extract_function_type ~env ~package:full.package typ_expr with
    | args, t_ret when args <> [] -> Some t_ret
    | _ -> None)
  | _ -> None

type builtin_type =
  | Array
  | Option
  | String
  | Int
  | Float
  | Promise
  | List
  | Result
  | Lazy
  | Char
  | RegExp

type pipe_completion_type =
  | Builtin of builtin_type * Types.type_expr
  | TypExpr of Types.type_expr

let get_builtin_from_type_path path =
  match path with
  | Path.Pident _ -> (
    match Path.name path with
    | "array" -> Some Array
    | "option" -> Some Option
    | "string" -> Some String
    | "int" -> Some Int
    | "float" -> Some Float
    | "promise" -> Some Promise
    | "list" -> Some List
    | "result" -> Some Result
    | "lazy_t" -> Some Lazy
    | "char" -> Some Char
    | _ -> None)
  | Pdot (Pdot (Pident m, "Re", _), "t", _) when Ident.name m = "Js" ->
    Some RegExp
  | Pdot (Pident id, "result", _) when Ident.name id = "Pervasives" ->
    Some Result
  | _ -> None

let rec dig_to_relevant_template_name_type ~env ~package ?(suffix = "")
    (t : Types.type_expr) =
  match t.desc with
  | Tlink t1 | Tsubst t1 | Tpoly (t1, []) ->
    dig_to_relevant_template_name_type ~suffix ~env ~package t1
  | Tconstr (Path.Pident {name = "option"}, [t1], _) ->
    dig_to_relevant_template_name_type ~suffix ~env ~package t1
  | Tconstr (Path.Pident {name = "array"}, [t1], _) ->
    dig_to_relevant_template_name_type ~suffix:"s" ~env ~package t1
  | Tconstr (path, _, _) -> (
    match References.dig_constructor ~env ~package path with
    | Some (env, {item = {decl = {type_manifest = Some typ}}}) ->
      dig_to_relevant_template_name_type ~suffix ~env ~package typ
    | _ -> (t, suffix, env))
  | _ -> (t, suffix, env)

let rec resolve_type_for_pipe_completion ~env ~package ~lhs_loc ~full
    (t : Types.type_expr) =
  let builtin =
    match t |> path_from_type_expr with
    | Some path -> path |> get_builtin_from_type_path
    | None -> None
  in
  match builtin with
  | Some builtin -> (env, Builtin (builtin, t))
  | None -> (
    (* If the type we're completing on is a type parameter, we won't be able to
       do completion unless we know what that type parameter is compiled as.
       This attempts to look up the compiled type for that type parameter by
       looking for compiled information at the loc of that expression. *)
    let typ_from_loc =
      match t with
      | {Types.desc = Tvar _} -> (
        match find_return_type_of_function_at_loc lhs_loc ~env ~full ~debug:false with
        | None -> None
        | Some typ_from_loc -> Some typ_from_loc)
      | _ -> None
    in
    match typ_from_loc with
    | Some typ_from_loc ->
      typ_from_loc |> resolve_type_for_pipe_completion ~lhs_loc ~env ~package ~full
    | None ->
      let rec dig_to_relevant_type ~env ~package (t : Types.type_expr) =
        match t.desc with
        | Tlink t1 | Tsubst t1 | Tpoly (t1, []) ->
          dig_to_relevant_type ~env ~package t1
        (* Don't descend into types named "t". Type t is a convention in the ReScript ecosystem. *)
        | Tconstr (path, _, _) when path |> Path.last = "t" -> (env, TypExpr t)
        | Tconstr (path, _, _) -> (
          match References.dig_constructor ~env ~package path with
          | Some (env, {item = {decl = {type_manifest = Some typ}}}) ->
            dig_to_relevant_type ~env ~package typ
          | _ -> (env, TypExpr t))
        | _ -> (env, TypExpr t)
      in
      dig_to_relevant_type ~env ~package t)

let extract_type_from_resolved_type (typ : Type.t) ~env ~full =
  match typ.kind with
  | Tuple items -> Some (Tuple (env, items, Ctype.newty (Ttuple items)))
  | Record fields ->
    Some (Trecord {env; fields; definition = `NameOnly typ.name})
  | Variant constructors ->
    Some
      (Tvariant
         {env; constructors; variant_name = typ.name; variant_decl = typ.decl})
  | Abstract _ | Open -> (
    match typ.decl.type_manifest with
    | None -> None
    | Some t -> t |> extract_type ~env ~package:full.package |> get_extracted_type)

(** The context we just came from as we resolve the nested structure. *)
type ctx = Rfield of string  (** A record field of name *)

let rec resolve_nested ?type_arg_context ~env ~full ~nested ?ctx
    (typ : completion_type) =
  let extract_type = extract_type ?type_arg_context in
  if Debug.verbose () then
    Printf.printf
      "[nested]--> running nested in env: %s. Has type arg ctx: %b\n"
      (Debug.debug_print_env env)
      (Option.is_some type_arg_context);
  (match type_arg_context with
  | None -> ()
  | Some type_arg_context ->
    if Debug.verbose () then
      Printf.printf "[nested]--> %s" (debug_log_type_arg_context type_arg_context));
  match nested with
  | [] ->
    if Debug.verbose () then
      print_endline "[nested]--> reached end of pattern, returning type";
    Some
      ( typ,
        env,
        (match ctx with
        | None -> None
        | Some (Rfield field_name) ->
          Some (Completable.CameFromRecordField field_name)),
        type_arg_context )
  | pattern_path :: nested -> (
    match (pattern_path, typ) with
    | Completable.NTupleItem {item_num}, Tuple (env, tuple_items, _) -> (
      if Debug.verbose () then
        print_endline "[nested]--> trying to move into tuple";
      match List.nth_opt tuple_items item_num with
      | None ->
        if Debug.verbose () then
          print_endline "[nested]--> tuple element not found";
        None
      | Some typ ->
        typ
        |> extract_type ~env ~package:full.package
        |> Utils.Option.flat_map (fun (typ, type_arg_context) ->
               typ |> resolve_nested ?type_arg_context ~env ~full ~nested))
    | ( NFollowRecordField {field_name},
        (TinlineRecord {env; fields} | Trecord {env; fields}) ) -> (
      if Debug.verbose () then
        print_endline "[nested]--> trying to move into record field";
      match
        fields
        |> List.find_opt (fun (field : field) -> field.fname.txt = field_name)
      with
      | None ->
        if Debug.verbose () then
          print_endline "[nested]--> did not find record field";
        None
      | Some {typ; optional} ->
        if Debug.verbose () then
          print_endline "[nested]--> found record field type";
        let typ = if optional then Utils.unwrap_if_option typ else typ in

        if Debug.verbose () then
          Printf.printf "[nested]--> extracting from type %s in env %s\n"
            (Shared.type_to_string typ) (Debug.debug_print_env env);
        typ
        |> extract_type ~env ~package:full.package
        |> Utils.Option.flat_map (fun (typ, type_arg_context) ->
               typ
               |> resolve_nested ?type_arg_context ~ctx:(Rfield field_name) ~env
                    ~full ~nested))
    | NRecordBody {seen_fields}, Trecord {env; definition = `TypeExpr type_expr}
      ->
      type_expr
      |> extract_type ~env ~package:full.package
      |> Option.map (fun (typ, type_arg_context) ->
             ( typ,
               env,
               Some (Completable.RecordField {seen_fields}),
               type_arg_context ))
    | ( NRecordBody {seen_fields},
        (Trecord {env; definition = `NameOnly _} as extracted_type) ) ->
      Some
        ( extracted_type,
          env,
          Some (Completable.RecordField {seen_fields}),
          type_arg_context )
    | NRecordBody {seen_fields}, TinlineRecord {env; fields} ->
      Some
        ( TinlineRecord {fields; env},
          env,
          Some (Completable.RecordField {seen_fields}),
          type_arg_context )
    | ( NVariantPayload {constructor_name = "Some"; item_num = 0},
        Toption (env, ExtractedType typ) ) ->
      if Debug.verbose () then
        print_endline "[nested]--> moving into option Some";
      typ |> resolve_nested ~env ~full ~nested
    | ( NVariantPayload {constructor_name = "Some"; item_num = 0},
        Toption (env, TypeExpr typ) ) ->
      if Debug.verbose () then
        print_endline "[nested]--> moving into option Some";
      typ
      |> extract_type ~env ~package:full.package
      |> Utils.Option.flat_map (fun (t, type_arg_context) ->
             t |> resolve_nested ?type_arg_context ~env ~full ~nested)
    | NVariantPayload {constructor_name = "Ok"; item_num = 0}, Tresult {ok_type} ->
      if Debug.verbose () then print_endline "[nested]--> moving into result Ok";
      ok_type
      |> extract_type ~env ~package:full.package
      |> Utils.Option.flat_map (fun (t, type_arg_context) ->
             t |> resolve_nested ?type_arg_context ~env ~full ~nested)
    | ( NVariantPayload {constructor_name = "Error"; item_num = 0},
        Tresult {error_type} ) ->
      if Debug.verbose () then
        print_endline "[nested]--> moving into result Error";
      error_type
      |> extract_type ~env ~package:full.package
      |> Utils.Option.flat_map (fun (t, type_arg_context) ->
             t |> resolve_nested ?type_arg_context ~env ~full ~nested)
    | NVariantPayload {constructor_name; item_num}, Tvariant {env; constructors}
      -> (
      if Debug.verbose () then
        Printf.printf
          "[nested]--> trying to move into variant payload $%i of constructor \
           '%s'\n"
          item_num constructor_name;
      match
        constructors
        |> List.find_opt (fun (c : Constructor.t) ->
               c.cname.txt = constructor_name)
      with
      | Some {args = Args args} -> (
        if Debug.verbose () then
          print_endline "[nested]--> found constructor (Args type)";
        match List.nth_opt args item_num with
        | None ->
          if Debug.verbose () then
            print_endline "[nested]--> did not find relevant args num";
          None
        | Some (typ, _) ->
          if Debug.verbose () then
            Printf.printf "[nested]--> found arg of type: %s\n"
              (Shared.type_to_string typ);

          typ
          |> extract_type ~env ~package:full.package
          |> Utils.Option.flat_map (fun (typ, type_arg_context) ->
                 if Debug.verbose () then
                   Printf.printf
                     "[nested]--> extracted %s, continuing descent of %i items\n"
                     (extracted_type_to_string typ)
                     (List.length nested);
                 typ |> resolve_nested ?type_arg_context ~env ~full ~nested))
      | Some {args = InlineRecord fields} when item_num = 0 ->
        if Debug.verbose () then
          print_endline "[nested]--> found constructor (inline record)";
        TinlineRecord {env; fields} |> resolve_nested ~env ~full ~nested
      | _ -> None)
    | ( NPolyvariantPayload {constructor_name; item_num},
        Tpolyvariant {env; constructors} ) -> (
      match
        constructors
        |> List.find_opt (fun (c : poly_variant_constructor) ->
               c.name = constructor_name)
      with
      | None -> None
      | Some constructor -> (
        match List.nth_opt constructor.args item_num with
        | None -> None
        | Some typ ->
          typ
          |> extract_type ~env ~package:full.package
          |> Utils.Option.flat_map (fun (typ, type_arg_context) ->
                 typ |> resolve_nested ?type_arg_context ~env ~full ~nested)))
    | NArray, Tarray (env, ExtractedType typ) ->
      typ |> resolve_nested ~env ~full ~nested
    | NArray, Tarray (env, TypeExpr typ) ->
      typ
      |> extract_type ~env ~package:full.package
      |> Utils.Option.flat_map (fun (typ, type_arg_context) ->
             typ |> resolve_nested ?type_arg_context ~env ~full ~nested)
    | _ -> None)

let find_type_of_record_field fields ~field_name =
  match
    fields |> List.find_opt (fun (field : field) -> field.fname.txt = field_name)
  with
  | None -> None
  | Some {typ; optional} ->
    let typ = if optional then Utils.unwrap_if_option typ else typ in
    Some typ

let find_type_of_constructor_arg constructors ~constructor_name ~payload_num ~env =
  match
    constructors
    |> List.find_opt (fun (c : Constructor.t) -> c.cname.txt = constructor_name)
  with
  | Some {args = Args args} -> (
    match List.nth_opt args payload_num with
    | None -> None
    | Some (typ, _) -> Some (TypeExpr typ))
  | Some {args = InlineRecord fields} when payload_num = 0 ->
    Some (ExtractedType (TinlineRecord {env; fields}))
  | _ -> None

let find_type_of_polyvariant_arg constructors ~constructor_name ~payload_num =
  match
    constructors
    |> List.find_opt (fun (c : poly_variant_constructor) ->
           c.name = constructor_name)
  with
  | Some {args} -> (
    match List.nth_opt args payload_num with
    | None -> None
    | Some typ -> Some typ)
  | None -> None

let rec resolve_nested_pattern_path (typ : inner_type) ~env ~full ~nested =
  if Debug.verbose () then print_endline "[nested_pattern_path]";
  let t =
    match typ with
    | TypeExpr t ->
      t |> extract_type ~env ~package:full.package |> get_extracted_type
    | ExtractedType t -> Some t
  in
  match nested with
  | [] -> None
  | [final_pattern_path] -> (
    match t with
    | None -> None
    | Some completion_type -> (
      match (final_pattern_path, completion_type) with
      | ( Completable.NFollowRecordField {field_name},
          (TinlineRecord {fields} | Trecord {fields}) ) -> (
        match fields |> find_type_of_record_field ~field_name with
        | None -> None
        | Some typ -> Some (TypeExpr typ, env))
      | NTupleItem {item_num}, Tuple (env, tuple_items, _) -> (
        match List.nth_opt tuple_items item_num with
        | None -> None
        | Some typ -> Some (TypeExpr typ, env))
      | NVariantPayload {constructor_name; item_num}, Tvariant {env; constructors}
        -> (
        match
          constructors
          |> find_type_of_constructor_arg ~constructor_name ~payload_num:item_num ~env
        with
        | Some typ -> Some (typ, env)
        | None -> None)
      | ( NPolyvariantPayload {constructor_name; item_num},
          Tpolyvariant {env; constructors} ) -> (
        match
          constructors
          |> find_type_of_polyvariant_arg ~constructor_name ~payload_num:item_num
        with
        | Some typ -> Some (TypeExpr typ, env)
        | None -> None)
      | ( NVariantPayload {constructor_name = "Some"; item_num = 0},
          Toption (env, typ) ) ->
        Some (typ, env)
      | ( NVariantPayload {constructor_name = "Ok"; item_num = 0},
          Tresult {env; ok_type} ) ->
        Some (TypeExpr ok_type, env)
      | ( NVariantPayload {constructor_name = "Error"; item_num = 0},
          Tresult {env; error_type} ) ->
        Some (TypeExpr error_type, env)
      | NArray, Tarray (env, typ) -> Some (typ, env)
      | _ -> None))
  | pattern_path :: nested -> (
    match t with
    | None -> None
    | Some completion_type -> (
      match (pattern_path, completion_type) with
      | ( Completable.NFollowRecordField {field_name},
          (TinlineRecord {env; fields} | Trecord {env; fields}) ) -> (
        match fields |> find_type_of_record_field ~field_name with
        | None -> None
        | Some typ ->
          typ
          |> extract_type ~env ~package:full.package
          |> get_extracted_type
          |> Utils.Option.flat_map (fun typ ->
                 ExtractedType typ
                 |> resolve_nested_pattern_path ~env ~full ~nested))
      | NTupleItem {item_num}, Tuple (env, tuple_items, _) -> (
        match List.nth_opt tuple_items item_num with
        | None -> None
        | Some typ ->
          typ
          |> extract_type ~env ~package:full.package
          |> get_extracted_type
          |> Utils.Option.flat_map (fun typ ->
                 ExtractedType typ
                 |> resolve_nested_pattern_path ~env ~full ~nested))
      | NVariantPayload {constructor_name; item_num}, Tvariant {env; constructors}
        -> (
        match
          constructors
          |> find_type_of_constructor_arg ~constructor_name ~payload_num:item_num ~env
        with
        | Some typ -> typ |> resolve_nested_pattern_path ~env ~full ~nested
        | None -> None)
      | ( NPolyvariantPayload {constructor_name; item_num},
          Tpolyvariant {env; constructors} ) -> (
        match
          constructors
          |> find_type_of_polyvariant_arg ~constructor_name ~payload_num:item_num
        with
        | Some typ ->
          TypeExpr typ |> resolve_nested_pattern_path ~env ~full ~nested
        | None -> None)
      | ( NVariantPayload {constructor_name = "Some"; item_num = 0},
          Toption (env, typ) ) ->
        typ |> resolve_nested_pattern_path ~env ~full ~nested
      | ( NVariantPayload {constructor_name = "Ok"; item_num = 0},
          Tresult {env; ok_type} ) ->
        TypeExpr ok_type |> resolve_nested_pattern_path ~env ~full ~nested
      | ( NVariantPayload {constructor_name = "Error"; item_num = 0},
          Tresult {env; error_type} ) ->
        TypeExpr error_type |> resolve_nested_pattern_path ~env ~full ~nested
      | NArray, Tarray (env, typ) ->
        typ |> resolve_nested_pattern_path ~env ~full ~nested
      | _ -> None))

let get_args ~env (t : Types.type_expr) ~full =
  let rec get_args_loop ~env (t : Types.type_expr) ~full ~current_argument_position
      =
    match t.desc with
    | Tlink t1
    | Tsubst t1
    | Tpoly (t1, [])
    | Tconstr (Pident {name = "function$"}, [t1; _], _) ->
      get_args_loop ~full ~env ~current_argument_position t1
    | Tarrow (Labelled l, t_arg, t_ret, _) ->
      (SharedTypes.Completable.Labelled l, t_arg)
      :: get_args_loop ~full ~env ~current_argument_position t_ret
    | Tarrow (Optional l, t_arg, t_ret, _) ->
      (Optional l, t_arg) :: get_args_loop ~full ~env ~current_argument_position t_ret
    | Tarrow (Nolabel, t_arg, t_ret, _) ->
      (Unlabelled {argument_position = current_argument_position}, t_arg)
      :: get_args_loop ~full ~env
           ~current_argument_position:(current_argument_position + 1)
           t_ret
    | Tconstr (path, type_args, _) -> (
      match References.dig_constructor ~env ~package:full.package path with
      | Some
          ( env,
            {
              item = {decl = {type_manifest = Some t1; type_params = type_params}};
            } ) ->
        let t1 = t1 |> instantiate_type ~type_params ~type_args in
        get_args_loop ~full ~env ~current_argument_position t1
      | _ -> [])
    | _ -> []
  in
  t |> get_args_loop ~env ~full ~current_argument_position:0

let type_is_unit (typ : Types.type_expr) =
  match typ.desc with
  | Tconstr (Pident id, _typeArgs, _)
  | Tlink {desc = Tconstr (Pident id, _typeArgs, _)}
  | Tsubst {desc = Tconstr (Pident id, _typeArgs, _)}
  | Tpoly ({desc = Tconstr (Pident id, _typeArgs, _)}, [])
    when Ident.name id = "unit" ->
    true
  | _ -> false

let rec context_path_from_core_type (core_type : Parsetree.core_type) =
  match core_type.ptyp_desc with
  | Ptyp_constr ({txt = Lident "option"}, [inner_typ]) ->
    inner_typ |> context_path_from_core_type
    |> Option.map (fun inner_typ -> Completable.CPOption inner_typ)
  | Ptyp_constr ({txt = Lident "array"}, [inner_typ]) ->
    Some (Completable.CPArray (inner_typ |> context_path_from_core_type))
  | Ptyp_constr (lid, _) ->
    Some
      (CPId
         {
           path = lid.txt |> Utils.flatten_long_ident;
           completion_context = Type;
           loc = lid.loc;
         })
  | _ -> None

let unwrap_completion_type_if_option (t : SharedTypes.completion_type) =
  match t with
  | Toption (_, ExtractedType unwrapped) -> unwrapped
  | _ -> t

module Codegen = struct
  let mk_fail_with_exp () =
    Ast_helper.Exp.apply
      (Ast_helper.Exp.ident {txt = Lident "failwith"; loc = Location.none})
      [(Nolabel, Ast_helper.Exp.constant (Pconst_string ("TODO", None)))]

  let mk_construct_pat ?payload name =
    Ast_helper.Pat.construct
      {Asttypes.txt = Longident.Lident name; loc = Location.none}
      payload

  let mk_tag_pat ?payload name = Ast_helper.Pat.variant name payload

  let any () = Ast_helper.Pat.any ()

  let rec extracted_type_to_exhaustive_patterns ~env ~full extracted_type =
    match extracted_type with
    | Tvariant v ->
      Some
        (v.constructors
        |> List.map (fun (c : SharedTypes.Constructor.t) ->
               mk_construct_pat
                 ?payload:
                   (match c.args with
                   | Args [] -> None
                   | _ -> Some (any ()))
                 c.cname.txt))
    | Tpolyvariant v ->
      Some
        (v.constructors
        |> List.map (fun (c : SharedTypes.poly_variant_constructor) ->
               mk_tag_pat
                 ?payload:
                   (match c.args with
                   | [] -> None
                   | _ -> Some (any ()))
                 c.display_name))
    | Toption (_, inner_type) ->
      let extracted_type =
        match inner_type with
        | ExtractedType t -> Some t
        | TypeExpr t ->
          extract_type t ~env ~package:full.package |> get_extracted_type
      in
      let expanded_branches =
        match extracted_type with
        | None -> []
        | Some extracted_type -> (
          match extracted_type_to_exhaustive_patterns ~env ~full extracted_type with
          | None -> []
          | Some patterns -> patterns)
      in
      Some
        ([
           mk_construct_pat "None";
           mk_construct_pat ~payload:(Ast_helper.Pat.any ()) "Some";
         ]
        @ (expanded_branches
          |> List.map (fun (pat : Parsetree.pattern) ->
                 mk_construct_pat ~payload:pat "Some")))
    | Tresult {ok_type; error_type} ->
      let extracted_ok_type =
        ok_type |> extract_type ~env ~package:full.package |> get_extracted_type
      in
      let extracted_error_type =
        error_type |> extract_type ~env ~package:full.package |> get_extracted_type
      in
      let expanded_ok_branches =
        match extracted_ok_type with
        | None -> []
        | Some extracted_type -> (
          match extracted_type_to_exhaustive_patterns ~env ~full extracted_type with
          | None -> []
          | Some patterns -> patterns)
      in
      let expanded_error_branches =
        match extracted_error_type with
        | None -> []
        | Some extracted_type -> (
          match extracted_type_to_exhaustive_patterns ~env ~full extracted_type with
          | None -> []
          | Some patterns -> patterns)
      in
      Some
        ((expanded_ok_branches
         |> List.map (fun (pat : Parsetree.pattern) ->
                mk_construct_pat ~payload:pat "Ok"))
        @ (expanded_error_branches
          |> List.map (fun (pat : Parsetree.pattern) ->
                 mk_construct_pat ~payload:pat "Error")))
    | Tbool _ -> Some [mk_construct_pat "true"; mk_construct_pat "false"]
    | _ -> None

  let extracted_type_to_exhaustive_cases ~env ~full extracted_type =
    let patterns = extracted_type_to_exhaustive_patterns ~env ~full extracted_type in

    match patterns with
    | None -> None
    | Some patterns ->
      Some
        (patterns
        |> List.map (fun (pat : Parsetree.pattern) ->
               Ast_helper.Exp.case pat (mk_fail_with_exp ())))
end

let get_path_relative_to_env ~debug ~(env : QueryEnv.t) ~env_from_item path =
  match path with
  | _ :: path_rev ->
    (* type path is relative to the completion environment
       express it from the root of the file *)
    let found, path_from_env =
      QueryEnv.path_from_env env_from_item (List.rev path_rev)
    in
    if debug then
      Printf.printf "CPPipe pathFromEnv:%s found:%b\n"
        (path_from_env |> String.concat ".")
        found;
    if path_from_env = [] then None
    else if
      env.file.module_name <> env_from_item.file.module_name && found
      (* If the module names are different, then one needs to qualify the path.
         But only if the path belongs to the env from completion *)
    then Some (env_from_item.file.module_name :: path_from_env)
    else Some path_from_env
  | _ -> None

let remove_opens_from_completion_path ~raw_opens ~package completion_path =
  let rec remove_raw_open raw_open module_path =
    match (raw_open, module_path) with
    | [_], _ -> Some module_path
    | s :: inner, first :: rest_path when s = first ->
      remove_raw_open inner rest_path
    | _ -> None
  in
  let rec remove_raw_opens raw_opens module_path =
    match raw_opens with
    | raw_open :: rest_opens -> (
      let new_module_path = remove_raw_opens rest_opens module_path in
      match remove_raw_open raw_open new_module_path with
      | None -> new_module_path
      | Some mp -> mp)
    | [] -> module_path
  in
  let completion_path_minus_opens =
    completion_path |> Utils.flatten_any_namespace_in_path
    |> remove_raw_opens package.opens
    |> remove_raw_opens raw_opens
  in
  completion_path_minus_opens

let path_to_element_props package =
  match package.generic_jsx_module with
  | None -> ["ReactDOM"; "domProps"]
  | Some g -> (g |> String.split_on_char '.') @ ["Elements"; "props"]
