let completion ~debug ~path ~pos ~current_file =
  let completions =
    match
      Completions.get_completions ~debug ~path ~pos ~current_file ~for_hover:false
    with
    | None -> []
    | Some (completions, full, _) ->
      completions
      |> List.map (CompletionBackEnd.completion_to_item ~full)
      |> List.map Protocol.stringify_completion_item
  in
  completions |> Protocol.array |> print_endline

let completion_resolve ~path ~module_path =
  (* We ignore the internal module path as of now because there's currently
     no use case for it. But, if we wanted to move resolving documentation
     for regular modules and not just file modules to the completionResolve
     hook as well, it'd be easy to implement here. *)
  let module_name, _innerModulePath =
    match module_path |> String.split_on_char '.' with
    | [module_name] -> (module_name, [])
    | module_name :: rest -> (module_name, rest)
    | [] -> raise (Failure "Invalid module path.")
  in
  let docstring =
    match Cmt.load_full_cmt_from_path ~path with
    | None ->
      if Debug.verbose () then
        Printf.printf "[completion_resolve] Could not load cmt\n";
      Protocol.null
    | Some full -> (
      match ProcessCmt.file_for_module ~package:full.package module_name with
      | None ->
        if Debug.verbose () then
          Printf.printf "[completion_resolve] Did not find file for module %s\n"
            module_name;
        Protocol.null
      | Some file ->
        file.structure.docstring |> String.concat "\n\n"
        |> Protocol.wrap_in_quotes)
  in
  print_endline docstring

let inlayhint ~path ~pos ~max_length ~debug =
  let result =
    match Hint.inlay ~path ~pos ~max_length ~debug with
    | Some hints -> hints |> Protocol.array
    | None -> Protocol.null
  in
  print_endline result

let code_lens ~path ~debug =
  let result =
    match Hint.code_lens ~path ~debug with
    | Some lens -> lens |> Protocol.array
    | None -> Protocol.null
  in
  print_endline result

let hover ~path ~pos ~current_file ~debug ~supports_markdown_links =
  let result =
    match Cmt.load_full_cmt_from_path ~path with
    | None -> Protocol.null
    | Some full -> (
      match References.get_loc_item ~full ~pos ~debug with
      | None -> (
        if debug then
          Printf.printf
            "Nothing at that position. Now trying to use completion.\n";
        match
          Hover.get_hover_via_completions ~debug ~path ~pos ~current_file
            ~for_hover:true ~supports_markdown_links
        with
        | None -> Protocol.null
        | Some hover -> hover)
      | Some loc_item -> (
        let is_module =
          match loc_item.loc_type with
          | LModule _ | TopLevelModule _ -> true
          | TypeDefinition _ | Typed _ | Constant _ -> false
        in
        let uri_loc_opt = References.definition_for_loc_item ~full loc_item in
        let skip_zero =
          match uri_loc_opt with
          | None -> false
          | Some (_, loc) ->
            let is_interface = full.file.uri |> Uri.is_interface in
            let pos_is_zero {Lexing.pos_lnum; pos_bol; pos_cnum} =
              (not is_interface) && pos_lnum = 1 && pos_cnum - pos_bol = 0
            in
            (* Skip if range is all zero, unless it's a module *)
            (not is_module) && pos_is_zero loc.loc_start && pos_is_zero loc.loc_end
        in
        if skip_zero then Protocol.null
        else
          let hover_text = Hover.new_hover ~supports_markdown_links ~full loc_item in
          match hover_text with
          | None -> Protocol.null
          | Some s -> Protocol.stringify_hover s))
  in
  print_endline result

let signature_help ~path ~pos ~current_file ~debug ~allow_for_constructor_payloads =
  let result =
    match
      SignatureHelp.signature_help ~path ~pos ~current_file ~debug
        ~allow_for_constructor_payloads
    with
    | None ->
      {Protocol.signatures = []; active_signature = None; active_parameter = None}
    | Some res -> res
  in
  print_endline (Protocol.stringify_signature_help result)

let code_action ~path ~start_pos ~end_pos ~current_file ~debug =
  Xform.extract_code_actions ~path ~start_pos ~end_pos ~current_file ~debug
  |> CodeActions.stringify_code_actions |> print_endline

let definition ~path ~pos ~debug =
  let location_opt =
    match Cmt.load_full_cmt_from_path ~path with
    | None -> None
    | Some full -> (
      match References.get_loc_item ~full ~pos ~debug with
      | None -> None
      | Some loc_item -> (
        match References.definition_for_loc_item ~full loc_item with
        | None -> None
        | Some (uri, loc) when not loc.loc_ghost ->
          let is_interface = full.file.uri |> Uri.is_interface in
          let pos_is_zero {Lexing.pos_lnum; pos_bol; pos_cnum} =
            (* range is zero *)
            pos_lnum = 1 && pos_cnum - pos_bol = 0
          in
          let is_module =
            match loc_item.loc_type with
            | LModule _ | TopLevelModule _ -> true
            | TypeDefinition _ | Typed _ | Constant _ -> false
          in
          let skip_loc =
            (not is_module) && (not is_interface) && pos_is_zero loc.loc_start
            && pos_is_zero loc.loc_end
          in
          if skip_loc then None
          else
            Some
              {
                Protocol.uri = Files.canonicalize_uri uri;
                range = Utils.cmt_loc_to_range loc;
              }
        | Some _ -> None))
  in
  print_endline
    (match location_opt with
    | None -> Protocol.null
    | Some location -> location |> Protocol.stringify_location)

let type_definition ~path ~pos ~debug =
  let maybe_location =
    match Cmt.load_full_cmt_from_path ~path with
    | None -> None
    | Some full -> (
      match References.get_loc_item ~full ~pos ~debug with
      | None -> None
      | Some loc_item -> (
        match References.type_definition_for_loc_item ~full loc_item with
        | None -> None
        | Some (uri, loc) ->
          Some
            {
              Protocol.uri = Files.canonicalize_uri uri;
              range = Utils.cmt_loc_to_range loc;
            }))
  in
  print_endline
    (match maybe_location with
    | None -> Protocol.null
    | Some location -> location |> Protocol.stringify_location)

let references ~path ~pos ~debug =
  let all_locs =
    match Cmt.load_full_cmt_from_path ~path with
    | None -> []
    | Some full -> (
      match References.get_loc_item ~full ~pos ~debug with
      | None -> []
      | Some loc_item ->
        let all_references = References.all_references_for_loc_item ~full loc_item in
        all_references
        |> List.fold_left
             (fun acc {References.uri = uri2; loc_opt} ->
               let loc =
                 match loc_opt with
                 | Some loc -> loc
                 | None -> Uri.to_top_level_loc uri2
               in
               Protocol.stringify_location
                 {uri = Uri.to_string uri2; range = Utils.cmt_loc_to_range loc}
               :: acc)
             [])
  in
  print_endline
    (if all_locs = [] then Protocol.null
     else "[\n" ^ (all_locs |> String.concat ",\n") ^ "\n]")

let rename ~path ~pos ~new_name ~debug =
  let result =
    match Cmt.load_full_cmt_from_path ~path with
    | None -> Protocol.null
    | Some full -> (
      match References.get_loc_item ~full ~pos ~debug with
      | None -> Protocol.null
      | Some loc_item ->
        let all_references = References.all_references_for_loc_item ~full loc_item in
        let references_to_toplevel_modules =
          all_references
          |> Utils.filter_map (fun {References.uri = uri2; loc_opt} ->
                 if loc_opt = None then Some uri2 else None)
        in
        let references_to_items =
          all_references
          |> Utils.filter_map (function
               | {References.uri = uri2; loc_opt = Some loc} -> Some (uri2, loc)
               | {loc_opt = None} -> None)
        in
        let file_renames =
          references_to_toplevel_modules
          |> List.map (fun uri ->
                 let path = Uri.to_path uri in
                 let dir = Filename.dirname path in
                 let new_path =
                   Filename.concat dir (new_name ^ Filename.extension path)
                 in
                 let new_uri = Uri.from_path new_path in
                 Protocol.
                   {
                     old_uri = uri |> Uri.to_string;
                     new_uri = new_uri |> Uri.to_string;
                   })
        in
        let text_document_edits =
          let module StringMap = Misc.StringMap in
          let text_edits_by_uri =
            references_to_items
            |> List.map (fun (uri, loc) -> (Uri.to_string uri, loc))
            |> List.fold_left
                 (fun acc (uri, loc) ->
                   let text_edit =
                     Protocol.
                       {range = Utils.cmt_loc_to_range loc; new_text = new_name}
                   in
                   match StringMap.find_opt uri acc with
                   | None -> StringMap.add uri [text_edit] acc
                   | Some prev_edits ->
                     StringMap.add uri (text_edit :: prev_edits) acc)
                 StringMap.empty
          in
          StringMap.fold
            (fun uri edits acc ->
              let text_document_edit =
                Protocol.{text_document = {uri; version = None}; edits}
              in
              text_document_edit :: acc)
            text_edits_by_uri []
        in
        let file_renames_string =
          file_renames |> List.map Protocol.stringify_rename_file
        in
        let text_document_edits_string =
          text_document_edits |> List.map Protocol.stringify_text_document_edit
        in
        "[\n"
        ^ (file_renames_string @ text_document_edits_string |> String.concat ",\n")
        ^ "\n]")
  in
  print_endline result

let format ~path =
  if Filename.check_suffix path ".res" then
    let {Res_driver.parsetree = structure; comments; diagnostics} =
      Res_driver.parsing_engine.parse_implementation ~for_printer:true
        ~filename:path
    in
    if List.length diagnostics > 0 then ""
    else
      Res_printer.print_implementation
        ~width:Res_multi_printer.default_print_width ~comments structure
  else if Filename.check_suffix path ".resi" then
    let {Res_driver.parsetree = signature; comments; diagnostics} =
      Res_driver.parsing_engine.parse_interface ~for_printer:true ~filename:path
    in
    if List.length diagnostics > 0 then ""
    else
      Res_printer.print_interface ~width:Res_multi_printer.default_print_width
        ~comments signature
  else ""

let diagnostic_syntax ~path =
  print_endline (Diagnostics.document_syntax ~path |> Protocol.array)

let test ~path =
  Uri.strip_path := true;
  match Files.read_file path with
  | None -> assert false
  | Some text ->
    let lines = text |> String.split_on_char '\n' in
    let process_line i line =
      let create_current_file () =
        let current_file, cout =
          Filename.open_temp_file "def" ("txt." ^ Filename.extension path)
        in
        let remove_line_comment l =
          let len = String.length l in
          let rec loop i =
            if i + 2 <= len && l.[i] = '/' && l.[i + 1] = '/' then Some (i + 2)
            else if i + 2 < len && l.[i] = ' ' then loop (i + 1)
            else None
          in
          match loop 0 with
          | None -> l
          | Some index_after_comment ->
            String.make index_after_comment ' '
            ^ String.sub l index_after_comment (len - index_after_comment)
        in
        lines
        |> List.iteri (fun j l ->
               let line_to_output =
                 if j == i - 1 then remove_line_comment l else l
               in
               Printf.fprintf cout "%s\n" line_to_output);
        close_out cout;
        current_file
      in
      if Str.string_match (Str.regexp "^ *//[ ]*\\^") line 0 then
        let matched = Str.matched_string line in
        let len = line |> String.length in
        let mlen = String.length matched in
        let rest = String.sub line mlen (len - mlen) in
        let line = i - 1 in
        let col = mlen - 1 in
        if mlen >= 3 then (
          (match String.sub rest 0 3 with
          | "db+" -> Log.verbose := true
          | "db-" -> Log.verbose := false
          | "dv+" -> Debug.debug_level := Verbose
          | "dv-" -> Debug.debug_level := Off
          | "in+" -> Cfg.in_incremental_typechecking_mode := true
          | "in-" -> Cfg.in_incremental_typechecking_mode := false
          | "ve+" -> (
            let version = String.sub rest 3 (String.length rest - 3) in
            let version = String.trim version in
            if Debug.verbose () then
              Printf.printf "Setting version: %s\n" version;
            match String.split_on_char '.' version with
            | [major_raw; minor_raw] ->
              let version = (int_of_string major_raw, int_of_string minor_raw) in
              Packages.override_rescript_version := Some version
            | _ -> ())
          | "ve-" -> Packages.override_rescript_version := None
          | "def" ->
            print_endline
              ("Definition " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            definition ~path ~pos:(line, col) ~debug:true
          | "com" ->
            print_endline
              ("Complete " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let current_file = create_current_file () in
            completion ~debug:true ~path ~pos:(line, col) ~current_file;
            Sys.remove current_file
          | "cre" ->
            let module_path = String.sub rest 3 (String.length rest - 3) in
            let module_path = String.trim module_path in
            print_endline ("Completion resolve: " ^ module_path);
            completion_resolve ~path ~module_path
          | "dce" ->
            print_endline ("DCE " ^ path);
            Reanalyze.RunConfig.run_config.suppress <- ["src"];
            Reanalyze.RunConfig.run_config.unsuppress <-
              [Filename.concat "src" "dce"];
            DceCommand.command ()
          | "doc" ->
            print_endline ("DocumentSymbol " ^ path);
            DocumentSymbol.command ~path
          | "hig" ->
            print_endline ("Highlight " ^ path);
            SemanticTokens.command ~debug:true
              ~emitter:(SemanticTokens.Token.create_emitter ())
              ~path
          | "hov" ->
            print_endline
              ("Hover " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let current_file = create_current_file () in
            hover ~supports_markdown_links:true ~path ~pos:(line, col)
              ~current_file ~debug:true;
            Sys.remove current_file
          | "she" ->
            print_endline
              ("Signature help " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let current_file = create_current_file () in
            signature_help ~path ~pos:(line, col) ~current_file ~debug:true
              ~allow_for_constructor_payloads:true;
            Sys.remove current_file
          | "int" ->
            print_endline ("Create Interface " ^ path);
            let cmi_file =
              let open Filename in
              let ( ++ ) = concat in
              let name = chop_extension (basename path) ^ ".cmi" in
              let dir = dirname path in
              dir ++ parent_dir_name ++ "lib" ++ "bs" ++ "src" ++ name
            in
            Printf.printf "%s" (CreateInterface.command ~path ~cmi_file)
          | "ref" ->
            print_endline
              ("References " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            references ~path ~pos:(line, col) ~debug:true
          | "ren" ->
            let new_name = String.sub rest 4 (len - mlen - 4) in
            let () =
              print_endline
                ("Rename " ^ path ^ " " ^ string_of_int line ^ ":"
               ^ string_of_int col ^ " " ^ new_name)
            in
            rename ~path ~pos:(line, col) ~new_name ~debug:true
          | "typ" ->
            print_endline
              ("TypeDefinition " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            type_definition ~path ~pos:(line, col) ~debug:true
          | "xfm" ->
            let current_file = create_current_file () in
            (* +2 is to ensure that the character ^ points to is what's considered the end of the selection. *)
            let end_col = col + try String.index rest '^' + 2 with _ -> 0 in
            let end_pos = (line, end_col) in
            let start_pos = (line, col) in
            if start_pos = end_pos then
              print_endline
                ("Xform " ^ path ^ " " ^ string_of_int line ^ ":"
               ^ string_of_int col)
            else
              print_endline
                ("Xform " ^ path ^ " start: " ^ Pos.to_string start_pos
               ^ ", end: " ^ Pos.to_string end_pos);
            let code_actions =
              Xform.extract_code_actions ~path ~start_pos ~end_pos ~current_file
                ~debug:true
            in
            Sys.remove current_file;
            code_actions
            |> List.iter (fun {Protocol.title; edit = {document_changes}} ->
                   Printf.printf "Hit: %s\n" title;
                   document_changes
                   |> List.iter (fun dc ->
                          match dc with
                          | Protocol.TextDocumentEdit tde ->
                            Printf.printf "\nTextDocumentEdit: %s\n"
                              tde.text_document.uri;

                            tde.edits
                            |> List.iter (fun {Protocol.range; new_text} ->
                                   let indent =
                                     String.make range.start.character ' '
                                   in
                                   Printf.printf
                                     "%s\nnewText:\n%s<--here\n%s%s\n"
                                     (Protocol.stringify_range range)
                                     indent indent new_text)
                          | CreateFile cf ->
                            Printf.printf "\nCreateFile: %s\n" cf.uri))
          | "c-a" ->
            let hint = String.sub rest 3 (String.length rest - 3) in
            print_endline
              ("Codemod AddMissingCases" ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            Codemod.transform ~path ~pos:(line, col) ~debug:true
              ~typ:AddMissingCases ~hint
            |> print_endline
          | "dia" -> diagnostic_syntax ~path
          | "hin" ->
            (* Get all inlay Hint between line 1 and n.
               Don't get the first line = 0.
            *)
            let line_start = 1 in
            let line_end = 34 in
            print_endline
              ("Inlay Hint " ^ path ^ " " ^ string_of_int line_start ^ ":"
             ^ string_of_int line_end);
            inlayhint ~path ~pos:(line_start, line_end) ~max_length:"25"
              ~debug:false
          | "cle" ->
            print_endline ("Code Lens " ^ path);
            code_lens ~path ~debug:false
          | "ast" ->
            print_endline
              ("Dump AST " ^ path ^ " " ^ string_of_int line ^ ":"
             ^ string_of_int col);
            let current_file = create_current_file () in
            DumpAst.dump ~pos:(line, col) ~current_file;
            Sys.remove current_file
          | _ -> ());
          print_newline ())
    in
    lines |> List.iteri process_line
