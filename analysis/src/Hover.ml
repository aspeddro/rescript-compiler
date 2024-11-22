open SharedTypes

let show_module_top_level ~docstring ~is_type ~name (top_level : Module.item list) =
  let contents =
    top_level
    |> List.map (fun item ->
           match item.Module.kind with
           (* TODO pretty print module contents *)
           | Type ({decl}, rec_status) ->
             "  " ^ (decl |> Shared.decl_to_string ~rec_status item.name)
           | Module _ -> "  module " ^ item.name
           | Value typ ->
             "  let " ^ item.name ^ ": " ^ (typ |> Shared.type_to_string))
    (* TODO indent *)
    |> String.concat "\n"
  in
  let name = Utils.cut_after_dash name in
  let full =
    Markdown.code_block
      ("module "
      ^ (if is_type then "type " ^ name ^ " = " else name ^ ": ")
      ^ "{" ^ "\n" ^ contents ^ "\n}")
  in
  let doc =
    match docstring with
    | [] -> ""
    | _ :: _ -> "\n" ^ (docstring |> String.concat "\n") ^ "\n"
  in
  Some (doc ^ full)

let rec show_module ~docstring ~(file : File.t) ~package ~name
    (declared : Module.t Declared.t option) =
  match declared with
  | None ->
    show_module_top_level ~docstring ~is_type:false ~name file.structure.items
  | Some {item = Structure {items}; module_path} ->
    let is_type =
      match module_path with
      | ExportedModule {is_type} -> is_type
      | _ -> false
    in
    show_module_top_level ~docstring ~is_type ~name items
  | Some ({item = Constraint (_moduleItem, module_type_item)} as declared) ->
    (* show the interface *)
    show_module ~docstring ~file ~name ~package
      (Some {declared with item = module_type_item})
  | Some ({item = Ident path} as declared) -> (
    match References.resolve_module_reference ~file ~package declared with
    | None -> Some ("Unable to resolve module reference " ^ Path.name path)
    | Some (_, declared) -> show_module ~docstring ~file ~name ~package declared)

type extracted_type = {
  name: string;
  path: Path.t;
  decl: Types.type_declaration;
  env: SharedTypes.QueryEnv.t;
  loc: Warnings.loc;
}

let find_relevant_types_from_type ~file ~package typ =
  (* Expand definitions of types mentioned in typ.
     If typ itself is a record or variant, search its body *)
  let env = QueryEnv.from_file file in
  let env_to_search, types_to_search =
    match typ |> Shared.dig_constructor with
    | Some path -> (
      let label_declarations_types lds =
        lds |> List.map (fun (ld : Types.label_declaration) -> ld.ld_type)
      in
      match References.dig_constructor ~env ~package path with
      | None -> (env, [typ])
      | Some (env1, {item = {decl}}) -> (
        match decl.type_kind with
        | Type_record (lds, _) -> (env1, typ :: (lds |> label_declarations_types))
        | Type_variant cds ->
          ( env1,
            cds
            |> List.map (fun (cd : Types.constructor_declaration) ->
                   let from_args =
                     match cd.cd_args with
                     | Cstr_tuple ts -> ts
                     | Cstr_record lds -> lds |> label_declarations_types
                   in
                   typ
                   ::
                   (match cd.cd_res with
                   | None -> from_args
                   | Some t -> t :: from_args))
            |> List.flatten )
        | _ -> (env, [typ])))
    | None -> (env, [typ])
  in
  let from_constructor_path ~env path =
    match References.dig_constructor ~env ~package path with
    | None -> None
    | Some (env, {name = {txt}; extent_loc; item = {decl}}) ->
      if Utils.is_uncurried_internal path then None
      else Some {name = txt; env; loc = extent_loc; decl; path}
  in
  let constructors = Shared.find_type_constructors types_to_search in
  constructors |> List.filter_map (from_constructor_path ~env:env_to_search)

let expand_types ~file ~package ~supports_markdown_links typ =
  find_relevant_types_from_type typ ~file ~package
  |> List.map (fun {decl; env; loc; path} ->
         let link_to_type_definition_str =
           if supports_markdown_links then
             Markdown.go_to_definition_text ~env ~pos:loc.Warnings.loc_start
           else ""
         in
         Markdown.divider
         ^ (if supports_markdown_links then Markdown.spacing else "")
         ^ Markdown.code_block
             (decl
             |> Shared.decl_to_string ~print_name_as_is:true
                  (SharedTypes.path_ident_to_string path))
         ^ link_to_type_definition_str ^ "\n")

(* Produces a hover with relevant types expanded in the main type being hovered. *)
let hover_with_expanded_types ~file ~package ~supports_markdown_links typ =
  let type_string = Markdown.code_block (typ |> Shared.type_to_string) in
  type_string :: expand_types ~file ~package ~supports_markdown_links typ
  |> String.concat "\n"

(* Leverages autocomplete functionality to produce a hover for a position. This
   makes it (most often) work with unsaved content. *)
let get_hover_via_completions ~debug ~path ~pos ~current_file ~for_hover
    ~supports_markdown_links =
  match Completions.get_completions ~debug ~path ~pos ~current_file ~for_hover with
  | None -> None
  | Some (completions, ({file; package} as full), scope) -> (
    let raw_opens = Scope.get_raw_opens scope in
    match completions with
    | {kind = Label typ_string; docstring} :: _ ->
      let parts =
        docstring
        @ if typ_string = "" then [] else [Markdown.code_block typ_string]
      in

      Some (Protocol.stringify_hover (String.concat "\n\n" parts))
    | {kind = Field _; env; docstring} :: _ -> (
      let opens = CompletionBackEnd.get_opens ~debug ~raw_opens ~package ~env in
      match
        CompletionBackEnd.completions_get_type_env2 ~debug ~full ~raw_opens ~opens
          ~pos completions
      with
      | Some (typ, _env) ->
        let type_string =
          hover_with_expanded_types ~file ~package ~supports_markdown_links typ
        in
        let parts = docstring @ [type_string] in
        Some (Protocol.stringify_hover (String.concat "\n\n" parts))
      | None -> None)
    | {env} :: _ -> (
      let opens = CompletionBackEnd.get_opens ~debug ~raw_opens ~package ~env in
      match
        CompletionBackEnd.completions_get_type_env2 ~debug ~full ~raw_opens ~opens
          ~pos completions
      with
      | Some (typ, _env) ->
        let type_string =
          hover_with_expanded_types ~file ~package ~supports_markdown_links typ
        in
        Some (Protocol.stringify_hover type_string)
      | None -> None)
    | _ -> None)

let new_hover ~full:{file; package} ~supports_markdown_links loc_item =
  match loc_item.loc_type with
  | TypeDefinition (name, decl, _stamp) -> (
    let type_def = Markdown.code_block (Shared.decl_to_string name decl) in
    match decl.type_manifest with
    | None -> Some type_def
    | Some typ ->
      Some
        (type_def :: expand_types ~file ~package ~supports_markdown_links typ
        |> String.concat "\n"))
  | LModule (Definition (stamp, _tip)) | LModule (LocalReference (stamp, _tip))
    -> (
    match Stamps.find_module file.stamps stamp with
    | None -> None
    | Some md -> (
      match References.resolve_module_reference ~file ~package md with
      | None -> None
      | Some (file, declared) ->
        let name, docstring =
          match declared with
          | Some d -> (d.name.txt, d.docstring)
          | None -> (file.module_name, file.structure.docstring)
        in
        show_module ~docstring ~name ~file declared ~package))
  | LModule (GlobalReference (module_name, path, tip)) -> (
    match ProcessCmt.file_for_module ~package module_name with
    | None -> None
    | Some file -> (
      let env = QueryEnv.from_file file in
      match References.exported_for_tip ~env ~path ~package ~tip with
      | None -> None
      | Some (_env, _name, stamp) -> (
        match Stamps.find_module file.stamps stamp with
        | None -> None
        | Some md -> (
          match References.resolve_module_reference ~file ~package md with
          | None -> None
          | Some (file, declared) ->
            let name, docstring =
              match declared with
              | Some d -> (d.name.txt, d.docstring)
              | None -> (file.module_name, file.structure.docstring)
            in
            show_module ~docstring ~name ~file ~package declared))))
  | LModule NotFound -> None
  | TopLevelModule name -> (
    match ProcessCmt.file_for_module ~package name with
    | None -> None
    | Some file ->
      show_module ~docstring:file.structure.docstring ~name:file.module_name ~file
        ~package None)
  | Typed (_, _, Definition (_, (Field _ | Constructor _))) -> None
  | Constant t ->
    Some
      (Markdown.code_block
         (match t with
         | Const_int _ -> "int"
         | Const_char _ -> "char"
         | Const_string _ -> "string"
         | Const_float _ -> "float"
         | Const_int32 _ -> "int32"
         | Const_int64 _ -> "int64"
         | Const_bigint _ -> "bigint"))
  | Typed (_, t, loc_kind) ->
    let from_type ~docstring typ =
      ( hover_with_expanded_types ~file ~package ~supports_markdown_links typ,
        docstring )
    in
    let parts =
      match References.defined_for_loc ~file ~package loc_kind with
      | None ->
        let type_string, docstring = t |> from_type ~docstring:[] in
        type_string :: docstring
      | Some (docstring, res) -> (
        match res with
        | `Declared ->
          let type_string, docstring = t |> from_type ~docstring in
          type_string :: docstring
        | `Constructor {cname = {txt}; args; docstring} ->
          let type_string, docstring = t |> from_type ~docstring in
          let args_string =
            match args with
            | InlineRecord _ | Args [] -> ""
            | Args args ->
              args
              |> List.map (fun (t, _) -> Shared.type_to_string t)
              |> String.concat ", " |> Printf.sprintf "(%s)"
          in
          type_string :: Markdown.code_block (txt ^ args_string) :: docstring
        | `Field ->
          let type_string, docstring = t |> from_type ~docstring in
          type_string :: docstring)
    in
    Some (String.concat Markdown.divider parts)
