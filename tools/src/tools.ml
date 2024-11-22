open Analysis

type field_doc = {
  field_name: string;
  docstrings: string list;
  signature: string;
  optional: bool;
  deprecated: string option;
}

type constructor_payload = InlineRecord of {field_docs: field_doc list}

type constructor_doc = {
  constructor_name: string;
  docstrings: string list;
  signature: string;
  deprecated: string option;
  items: constructor_payload option;
}

type type_doc = {path: string; generic_parameters: type_doc list}
type value_signature = {parameters: type_doc list; return_type: type_doc}

type source = {filepath: string; line: int; col: int}

type doc_item_detail =
  | Record of {field_docs: field_doc list}
  | Variant of {constructor_docs: constructor_doc list}
  | Signature of value_signature

type doc_item =
  | Value of {
      id: string;
      docstring: string list;
      signature: string;
      name: string;
      deprecated: string option;
      detail: doc_item_detail option;
      source: source;
    }
  | Type of {
      id: string;
      docstring: string list;
      signature: string;
      name: string;
      deprecated: string option;
      detail: doc_item_detail option;
      source: source;
          (** Additional documentation for constructors and record fields, if available. *)
    }
  | Module of docs_for_module
  | ModuleType of {
      id: string;
      docstring: string list;
      deprecated: string option;
      name: string;
      source: source;
      items: doc_item list;
    }
  | ModuleAlias of {
      id: string;
      docstring: string list;
      name: string;
      source: source;
      items: doc_item list;
    }
and docs_for_module = {
  id: string;
  docstring: string list;
  deprecated: string option;
  name: string;
  moduletypeid: string option;
  source: source;
  items: doc_item list;
}

let stringify_docstrings docstrings =
  let open Protocol in
  docstrings
  |> List.map (fun docstring -> docstring |> String.trim |> wrap_in_quotes)
  |> array

let stringify_field_doc ~indentation (field_doc : field_doc) =
  let open Protocol in
  stringify_object ~indentation:(indentation + 1)
    [
      ("name", Some (wrap_in_quotes field_doc.field_name));
      ( "deprecated",
        match field_doc.deprecated with
        | Some d -> Some (wrap_in_quotes d)
        | None -> None );
      ("optional", Some (string_of_bool field_doc.optional));
      ("docstrings", Some (stringify_docstrings field_doc.docstrings));
      ("signature", Some (wrap_in_quotes field_doc.signature));
    ]

let stringify_constructor_payload ~indentation
    (constructor_payload : constructor_payload) =
  let open Protocol in
  match constructor_payload with
  | InlineRecord {field_docs} ->
    stringify_object ~indentation:(indentation + 1)
      [
        ("kind", Some (wrap_in_quotes "inlineRecord"));
        ( "fields",
          Some
            (field_docs
            |> List.map (stringify_field_doc ~indentation:(indentation + 1))
            |> array) );
      ]

let rec stringify_type_doc ~indentation (td : type_doc) : string =
  let open Protocol in
  let ps =
    match td.generic_parameters with
    | [] -> None
    | ts ->
      ts |> List.map (stringify_type_doc ~indentation:(indentation + 1))
      |> fun ts -> Some (array ts)
  in

  stringify_object ~indentation:(indentation + 1)
    [("path", Some (wrap_in_quotes td.path)); ("genericTypeParameters", ps)]

let stringify_detail ?(indentation = 0) (detail : doc_item_detail) =
  let open Protocol in
  match detail with
  | Record {field_docs} ->
    stringify_object ~start_on_newline:true ~indentation
      [
        ("kind", Some (wrap_in_quotes "record"));
        ( "items",
          Some
            (field_docs |> List.map (stringify_field_doc ~indentation) |> array)
        );
      ]
  | Variant {constructor_docs} ->
    stringify_object ~start_on_newline:true ~indentation
      [
        ("kind", Some (wrap_in_quotes "variant"));
        ( "items",
          Some
            (constructor_docs
            |> List.map (fun constructor_doc ->
                   stringify_object ~start_on_newline:true
                     ~indentation:(indentation + 1)
                     [
                       ( "name",
                         Some (wrap_in_quotes constructor_doc.constructor_name)
                       );
                       ( "deprecated",
                         match constructor_doc.deprecated with
                         | Some d -> Some (wrap_in_quotes d)
                         | None -> None );
                       ( "docstrings",
                         Some (stringify_docstrings constructor_doc.docstrings)
                       );
                       ( "signature",
                         Some (wrap_in_quotes constructor_doc.signature) );
                       ( "payload",
                         match constructor_doc.items with
                         | None -> None
                         | Some constructor_payload ->
                           Some
                             (stringify_constructor_payload
                                ~indentation:(indentation + 1)
                                constructor_payload) );
                     ])
            |> array) );
      ]
  | Signature {parameters; return_type} ->
    let ps =
      match parameters with
      | [] -> None
      | ps ->
        ps |> List.map (stringify_type_doc ~indentation:(indentation + 1))
        |> fun ps -> Some (array ps)
    in
    stringify_object ~start_on_newline:true ~indentation
      [
        ("kind", Some (wrap_in_quotes "signature"));
        ( "details",
          Some
            (stringify_object ~start_on_newline:false ~indentation
               [
                 ("parameters", ps);
                 ( "returnType",
                   Some (stringify_type_doc ~indentation return_type) );
               ]) );
      ]

let stringify_source ~indentation source =
  let open Protocol in
  stringify_object ~start_on_newline:false ~indentation
    [
      ("filepath", Some (source.filepath |> wrap_in_quotes));
      ("line", Some (source.line |> string_of_int));
      ("col", Some (source.col |> string_of_int));
    ]

let rec stringify_doc_item ?(indentation = 0) ~original_env (item : doc_item) =
  let open Protocol in
  match item with
  | Value {id; docstring; signature; name; deprecated; source; detail} ->
    stringify_object ~start_on_newline:true ~indentation
      [
        ("id", Some (wrap_in_quotes id));
        ("kind", Some (wrap_in_quotes "value"));
        ("name", Some (name |> wrap_in_quotes));
        ( "deprecated",
          match deprecated with
          | Some d -> Some (wrap_in_quotes d)
          | None -> None );
        ("signature", Some (signature |> String.trim |> wrap_in_quotes));
        ("docstrings", Some (stringify_docstrings docstring));
        ("source", Some (stringify_source ~indentation:(indentation + 1) source));
        ( "detail",
          match detail with
          | None -> None
          | Some detail ->
            Some (stringify_detail ~indentation:(indentation + 1) detail) );
      ]
  | Type {id; docstring; signature; name; deprecated; detail; source} ->
    stringify_object ~start_on_newline:true ~indentation
      [
        ("id", Some (wrap_in_quotes id));
        ("kind", Some (wrap_in_quotes "type"));
        ("name", Some (name |> wrap_in_quotes));
        ( "deprecated",
          match deprecated with
          | Some d -> Some (wrap_in_quotes d)
          | None -> None );
        ("signature", Some (signature |> wrap_in_quotes));
        ("docstrings", Some (stringify_docstrings docstring));
        ("source", Some (stringify_source ~indentation:(indentation + 1) source));
        ( "detail",
          match detail with
          | None -> None
          | Some detail ->
            Some (stringify_detail ~indentation:(indentation + 1) detail) );
      ]
  | Module m ->
    stringify_object ~start_on_newline:true ~indentation
      [
        ("id", Some (wrap_in_quotes m.id));
        ("name", Some (wrap_in_quotes m.name));
        ("kind", Some (wrap_in_quotes "module"));
        ( "deprecated",
          match m.deprecated with
          | Some d -> Some (wrap_in_quotes d)
          | None -> None );
        ( "moduletypeid",
          match m.moduletypeid with
          | Some path -> Some (wrap_in_quotes path)
          | None -> None );
        ("docstrings", Some (stringify_docstrings m.docstring));
        ( "source",
          Some (stringify_source ~indentation:(indentation + 1) m.source) );
        ( "items",
          Some
            (m.items
            |> List.map
                 (stringify_doc_item ~original_env ~indentation:(indentation + 1))
            |> array) );
      ]
  | ModuleType m ->
    stringify_object ~start_on_newline:true ~indentation
      [
        ("id", Some (wrap_in_quotes m.id));
        ("name", Some (wrap_in_quotes m.name));
        ("kind", Some (wrap_in_quotes "moduleType"));
        ( "deprecated",
          match m.deprecated with
          | Some d -> Some (wrap_in_quotes d)
          | None -> None );
        ("docstrings", Some (stringify_docstrings m.docstring));
        ( "source",
          Some (stringify_source ~indentation:(indentation + 1) m.source) );
        ( "items",
          Some
            (m.items
            |> List.map
                 (stringify_doc_item ~original_env ~indentation:(indentation + 1))
            |> array) );
      ]
  | ModuleAlias m ->
    stringify_object ~start_on_newline:true ~indentation
      [
        ("id", Some (wrap_in_quotes m.id));
        ("kind", Some (wrap_in_quotes "moduleAlias"));
        ("name", Some (wrap_in_quotes m.name));
        ("docstrings", Some (stringify_docstrings m.docstring));
        ( "source",
          Some (stringify_source ~indentation:(indentation + 1) m.source) );
        ( "items",
          Some
            (m.items
            |> List.map
                 (stringify_doc_item ~original_env ~indentation:(indentation + 1))
            |> array) );
      ]

and stringify_docs_for_module ?(indentation = 0) ~original_env
    (d : docs_for_module) =
  let open Protocol in
  stringify_object ~start_on_newline:true ~indentation
    [
      ("name", Some (wrap_in_quotes d.name));
      ( "deprecated",
        match d.deprecated with
        | Some d -> Some (wrap_in_quotes d)
        | None -> None );
      ("docstrings", Some (stringify_docstrings d.docstring));
      ("source", Some (stringify_source ~indentation:(indentation + 1) d.source));
      ( "items",
        Some
          (d.items
          |> List.map
               (stringify_doc_item ~original_env ~indentation:(indentation + 1))
          |> array) );
    ]

let field_to_field_doc (field : SharedTypes.field) : field_doc =
  {
    field_name = field.fname.txt;
    docstrings = field.docstring;
    optional = field.optional;
    signature = Shared.type_to_string field.typ;
    deprecated = field.deprecated;
  }

let type_detail typ ~env ~full =
  let open SharedTypes in
  match TypeUtils.extract_type_from_resolved_type ~env ~full typ with
  | Some (Trecord {fields}) ->
    Some (Record {field_docs = fields |> List.map field_to_field_doc})
  | Some (Tvariant {constructors}) ->
    Some
      (Variant
         {
           constructor_docs =
             constructors
             |> List.map (fun (c : Constructor.t) ->
                    {
                      constructor_name = c.cname.txt;
                      docstrings = c.docstring;
                      signature = CompletionBackEnd.show_constructor c;
                      deprecated = c.deprecated;
                      items =
                        (match c.args with
                        | InlineRecord fields ->
                          Some
                            (InlineRecord
                               {
                                 field_docs =
                                   fields |> List.map field_to_field_doc;
                               })
                        | _ -> None);
                    });
         })
  | _ -> None

(* split a list into two parts all the items except the last one and the last item *)
let split_last l =
  let rec splitLast' acc = function
    | [] -> failwith "splitLast: empty list"
    | [x] -> (List.rev acc, x)
    | x :: xs -> splitLast' (x :: acc) xs
  in
  splitLast' [] l

let path_to_string path =
  let buf = Buffer.create 64 in
  let rec aux = function
    | Path.Pident id -> Buffer.add_string buf (Ident.name id)
    | Path.Pdot (p, s, _) ->
      aux p;
      Buffer.add_char buf '.';
      Buffer.add_string buf s
    | Path.Papply (p1, p2) ->
      aux p1;
      Buffer.add_char buf '(';
      aux p2;
      Buffer.add_char buf ')'
  in
  aux path;
  Buffer.contents buf

let value_detail (typ : Types.type_expr) =
  let rec collect_signature_types (typ_desc : Types.type_desc) =
    match typ_desc with
    | Tlink t | Tsubst t | Tpoly (t, []) -> collect_signature_types t.desc
    | Tconstr (Path.Pident {name = "function$"}, [t; _], _) ->
      collect_signature_types t.desc
    | Tconstr (path, ts, _) -> (
      let p = path_to_string path in
      match ts with
      | [] -> [{path = p; generic_parameters = []}]
      | ts ->
        let ts =
          ts
          |> List.concat_map (fun (t : Types.type_expr) ->
                 collect_signature_types t.desc)
        in
        [{path = p; generic_parameters = ts}])
    | Tarrow (_, t1, t2, _) ->
      collect_signature_types t1.desc @ collect_signature_types t2.desc
    | Tvar None -> [{path = "_"; generic_parameters = []}]
    | _ -> []
  in
  match collect_signature_types typ.desc with
  | [] -> None
  | ts ->
    let parameters, return_type = split_last ts in
    Some (Signature {parameters; return_type})

let make_id module_path ~identifier =
  identifier :: module_path |> List.rev |> SharedTypes.ident

let get_source ~root_path ({loc_start} : Location.t) =
  let line, col = Pos.of_lexing loc_start in
  let filepath =
    Files.relpath root_path loc_start.pos_fname
    |> Files.split Filename.dir_sep
    |> String.concat "/"
  in
  {filepath; line = line + 1; col = col + 1}

let extract_docs ~entry_point_file ~debug =
  let path =
    match Filename.is_relative entry_point_file with
    | true -> Unix.realpath entry_point_file
    | false -> entry_point_file
  in
  if debug then Printf.printf "extracting docs for %s\n" path;
  let result =
    match
      FindFiles.is_implementation path = false
      && FindFiles.is_interface path = false
    with
    | false -> (
      let path =
        if FindFiles.is_implementation path then
          let path_as_resi =
            (path |> Filename.dirname) ^ "/"
            ^ (path |> Filename.basename |> Filename.chop_extension)
            ^ ".resi"
          in
          if Sys.file_exists path_as_resi then (
            if debug then
              Printf.printf "preferring found resi file for impl: %s\n"
                path_as_resi;
            path_as_resi)
          else path
        else path
      in
      match Cmt.load_full_cmt_from_path ~path with
      | None ->
        Error
          (Printf.sprintf
             "error: failed to generate doc for %s, try to build the project"
             path)
      | Some full ->
        let file = full.file in
        let structure = file.structure in
        let root_path = full.package.root_path in
        let open SharedTypes in
        let env = QueryEnv.from_file file in
        let rec extract_docs_for_module ?(module_path = [env.file.module_name])
            (structure : Module.structure) =
          {
            id = module_path |> List.rev |> ident;
            docstring = structure.docstring |> List.map String.trim;
            name = structure.name;
            moduletypeid = None;
            deprecated = structure.deprecated;
            source =
              {
                filepath =
                  (match root_path = "." with
                  | true -> file.uri |> Uri.to_path
                  | false ->
                    Files.relpath root_path (file.uri |> Uri.to_path)
                    |> Files.split Filename.dir_sep
                    |> String.concat "/");
                line = 1;
                col = 1;
              };
            items =
              structure.items
              |> List.filter_map (fun (item : Module.item) ->
                     let item =
                       {
                         item with
                         name = Ext_ident.unwrap_uppercase_exotic item.name;
                       }
                     in
                     let source = get_source ~root_path item.loc in
                     match item.kind with
                     | Value typ ->
                       Some
                         (Value
                            {
                              id = module_path |> make_id ~identifier:item.name;
                              docstring = item.docstring |> List.map String.trim;
                              signature =
                                "let " ^ item.name ^ ": "
                                ^ Shared.type_to_string typ;
                              name = item.name;
                              deprecated = item.deprecated;
                              detail = value_detail typ;
                              source;
                            })
                     | Type (typ, _) ->
                       Some
                         (Type
                            {
                              id = module_path |> make_id ~identifier:item.name;
                              docstring = item.docstring |> List.map String.trim;
                              signature =
                                typ.decl |> Shared.decl_to_string item.name;
                              name = item.name;
                              deprecated = item.deprecated;
                              detail = type_detail typ ~full ~env;
                              source;
                            })
                     | Module {type_ = Ident p; is_module_type = false} ->
                       (* module Whatever = OtherModule *)
                       let alias_to_module = p |> path_ident_to_string in
                       let id =
                         (module_path |> List.rev |> List.hd) ^ "." ^ item.name
                       in
                       let items, internal_docstrings =
                         match
                           ProcessCmt.file_for_module ~package:full.package
                             alias_to_module
                         with
                         | None -> ([], [])
                         | Some file ->
                           let docs =
                             extract_docs_for_module ~module_path:[id]
                               file.structure
                           in
                           (docs.items, docs.docstring)
                       in
                       Some
                         (ModuleAlias
                            {
                              id;
                              name = item.name;
                              source;
                              items;
                              docstring =
                                item.docstring @ internal_docstrings
                                |> List.map String.trim;
                            })
                     | Module {type_ = Structure m; is_module_type = false} ->
                       (* module Whatever = {} in res or module Whatever: {} in resi. *)
                       let module_path = m.name :: module_path in
                       let docs = extract_docs_for_module ~module_path m in
                       Some
                         (Module
                            {
                              id = module_path |> List.rev |> ident;
                              name = m.name;
                              moduletypeid = None;
                              docstring = item.docstring @ m.docstring;
                              deprecated = item.deprecated;
                              source;
                              items = docs.items;
                            })
                     | Module {type_ = Structure m; is_module_type = true} ->
                       (* module type Whatever = {} *)
                       let module_path = m.name :: module_path in
                       let docs = extract_docs_for_module ~module_path m in
                       Some
                         (ModuleType
                            {
                              id = module_path |> List.rev |> ident;
                              name = m.name;
                              docstring = item.docstring @ m.docstring;
                              deprecated = item.deprecated;
                              source;
                              items = docs.items;
                            })
                     | Module
                         {
                           type_ =
                             Constraint (Structure _impl, Structure interface);
                         } ->
                       (* module Whatever: { <interface> } = { <impl> }. Prefer the interface. *)
                       Some
                         (Module
                            (extract_docs_for_module
                               ~module_path:(interface.name :: module_path)
                               interface))
                     | Module {type_ = Constraint (Structure m, Ident p)} ->
                       (* module M: T = { <impl> }. Print M *)
                       let docs =
                         extract_docs_for_module
                           ~module_path:(m.name :: module_path) m
                       in
                       let ident_module_path = p |> Path.head |> Ident.name in

                       let module_type_id_path =
                         match
                           ProcessCmt.file_for_module ~package:full.package
                             ident_module_path
                           |> Option.is_none
                         with
                         | false -> []
                         | true -> [module_path |> List.rev |> List.hd]
                       in

                       Some
                         (Module
                            {
                              docs with
                              moduletypeid =
                                Some
                                  (make_id ~identifier:(Path.name p)
                                     module_type_id_path);
                            })
                     | _ -> None);
          }
        in
        let docs = extract_docs_for_module structure in
        Ok (stringify_docs_for_module ~original_env:env docs))
    | true ->
      Error
        (Printf.sprintf
           "error: failed to read %s, expected an .res or .resi file" path)
  in

  result

let extract_embedded ~extension_points ~filename =
  let {Res_driver.parsetree = structure} =
    Res_driver.parsing_engine.parse_implementation ~for_printer:false ~filename
  in
  let content = ref [] in
  let append item = content := item :: !content in
  let extension (iterator : Ast_iterator.iterator) (ext : Parsetree.extension) =
    (match ext with
    | ( {txt},
        PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ( {
                      pexp_loc;
                      pexp_desc = Pexp_constant (Pconst_string (contents, _));
                    },
                    _ );
            };
          ] )
      when extension_points |> List.exists (fun v -> v = txt) ->
      append (pexp_loc, txt, contents)
    | _ -> ());
    Ast_iterator.default_iterator.extension iterator ext
  in
  let iterator = {Ast_iterator.default_iterator with extension} in
  iterator.structure iterator structure;
  let open Analysis.Protocol in
  !content
  |> List.map (fun (loc, extension_name, contents) ->
         stringify_object
           [
             ("extensionName", Some (wrap_in_quotes extension_name));
             ("contents", Some (wrap_in_quotes contents));
             ( "loc",
               Some (Analysis.Utils.cmt_loc_to_range loc |> stringify_range) );
           ])
  |> List.rev |> array
