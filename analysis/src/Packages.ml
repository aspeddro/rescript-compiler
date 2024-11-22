open SharedTypes

(* Creates the `pathsForModule` hashtbl, which maps a `moduleName` to it's `paths` (the ml/re, mli/rei, cmt, and cmti files) *)
let make_paths_for_module ~project_files_and_paths ~dependencies_files_and_paths =
  let paths_for_module = Hashtbl.create 30 in
  dependencies_files_and_paths
  |> List.iter (fun (mod_name, paths) ->
         Hashtbl.replace paths_for_module mod_name paths);
  project_files_and_paths
  |> List.iter (fun (mod_name, paths) ->
         Hashtbl.replace paths_for_module mod_name paths);
  paths_for_module

let override_rescript_version = ref None

let get_re_script_version () =
  match !override_rescript_version with
  | Some override_rescript_version -> override_rescript_version
  | None -> (
    (* TODO: Include patch stuff when needed *)
    let default_version = (11, 0) in
    try
      let value = Sys.getenv "RESCRIPT_VERSION" in
      let version =
        match value |> String.split_on_char '.' with
        | major :: minor :: _rest -> (
          match (int_of_string_opt major, int_of_string_opt minor) with
          | Some major, Some minor -> (major, minor)
          | _ -> default_version)
        | _ -> default_version
      in
      version
    with Not_found -> default_version)

let new_bs_package ~root_path =
  let rescript_json = Filename.concat root_path "rescript.json" in
  let bsconfig_json = Filename.concat root_path "bsconfig.json" in

  let parse_raw raw =
    let lib_bs =
      match !Cfg.is_doc_gen_from_compiler with
      | true -> BuildSystem.get_stdlib root_path
      | false -> BuildSystem.get_lib_bs root_path
    in
    match Json.parse raw with
    | Some config -> (
      let namespace = FindFiles.get_namespace config in
      let rescript_version = get_re_script_version () in
      let suffix =
        match config |> Json.get "suffix" with
        | Some (String suffix) -> suffix
        | _ -> ".js"
      in
      let uncurried =
        let ns = config |> Json.get "uncurried" in
        match (rescript_version, ns) with
        | (major, _), None when major >= 11 -> Some true
        | _, ns -> Option.bind ns Json.bool
      in
      let generic_jsx_module =
        let jsx_config = config |> Json.get "jsx" in
        match jsx_config with
        | Some jsx_config -> (
          match jsx_config |> Json.get "module" with
          | Some (String m) when String.lowercase_ascii m <> "react" -> Some m
          | _ -> None)
        | None -> None
      in
      let uncurried = uncurried = Some true in
      match lib_bs with
      | None -> None
      | Some lib_bs ->
        let cached = Cache.read_cache (Cache.target_file_from_lib_bs lib_bs) in
        let project_files, dependencies_files, paths_for_module =
          match cached with
          | Some cached ->
            ( cached.project_files,
              cached.dependencies_files,
              cached.paths_for_module )
          | None ->
            let dependencies_files_and_paths =
              match FindFiles.find_dependency_files root_path config with
              | None -> []
              | Some (_dependencyDirectories, dependencies_files_and_paths) ->
                dependencies_files_and_paths
            in
            let source_directories =
              FindFiles.get_source_directories ~include_dev:true ~base_dir:root_path
                config
            in
            let project_files_and_paths =
              FindFiles.find_project_files
                ~public:(FindFiles.get_public config)
                ~namespace ~path:root_path ~source_directories ~lib_bs
            in
            let paths_for_module =
              make_paths_for_module ~project_files_and_paths
                ~dependencies_files_and_paths
            in
            let project_files =
              project_files_and_paths |> List.map fst |> FileSet.of_list
            in
            let dependencies_files =
              dependencies_files_and_paths |> List.map fst |> FileSet.of_list
            in
            (project_files, dependencies_files, paths_for_module)
        in
        Some
          (let opens_from_namespace =
             match namespace with
             | None -> []
             | Some namespace ->
               let cmt = Filename.concat lib_bs namespace ^ ".cmt" in
               Hashtbl.replace paths_for_module namespace (Namespace {cmt});
               let path = [FindFiles.name_space_to_name namespace] in
               [path]
           in
           let opens_from_bsc_flags =
             let bind f x = Option.bind x f in
             match Json.get "bsc-flags" config |> bind Json.array with
             | Some l ->
               List.fold_left
                 (fun opens item ->
                   match item |> Json.string with
                   | None -> opens
                   | Some s -> (
                     let parts = String.split_on_char ' ' s in
                     match parts with
                     | "-open" :: name :: _ ->
                       let path = name |> String.split_on_char '.' in
                       path :: opens
                     | _ -> opens))
                 [] l
             | None -> []
           in
           let opens =
             ["Pervasives"; "JsxModules"] :: opens_from_namespace
             |> List.rev_append opens_from_bsc_flags
             |> List.map (fun path -> path @ ["place holder"])
           in
           {
             generic_jsx_module;
             suffix;
             rescript_version;
             root_path;
             project_files;
             dependencies_files;
             paths_for_module;
             opens;
             namespace;
             built_in_completion_modules =
               (if
                  opens_from_bsc_flags
                  |> List.find_opt (fun opn ->
                         match opn with
                         | ["RescriptCore"] -> true
                         | _ -> false)
                  |> Option.is_some
                then
                  {
                    array_module_path = ["Array"];
                    option_module_path = ["Option"];
                    string_module_path = ["String"];
                    int_module_path = ["Int"];
                    float_module_path = ["Float"];
                    promise_module_path = ["Promise"];
                    list_module_path = ["List"];
                    result_module_path = ["Result"];
                    exn_module_path = ["Exn"];
                    regexp_module_path = ["RegExp"];
                  }
                else if
                  opens_from_bsc_flags
                  |> List.find_opt (fun opn ->
                         match opn with
                         | ["Belt"] -> true
                         | _ -> false)
                  |> Option.is_some
                then
                  {
                    array_module_path = ["Array"];
                    option_module_path = ["Option"];
                    string_module_path = ["Js"; "String2"];
                    int_module_path = ["Int"];
                    float_module_path = ["Float"];
                    promise_module_path = ["Js"; "Promise"];
                    list_module_path = ["List"];
                    result_module_path = ["Result"];
                    exn_module_path = ["Js"; "Exn"];
                    regexp_module_path = ["Js"; "Re"];
                  }
                else
                  {
                    array_module_path = ["Js"; "Array2"];
                    option_module_path = ["Belt"; "Option"];
                    string_module_path = ["Js"; "String2"];
                    int_module_path = ["Belt"; "Int"];
                    float_module_path = ["Belt"; "Float"];
                    promise_module_path = ["Js"; "Promise"];
                    list_module_path = ["Belt"; "List"];
                    result_module_path = ["Belt"; "Result"];
                    exn_module_path = ["Js"; "Exn"];
                    regexp_module_path = ["Js"; "Re"];
                  });
             uncurried;
           }))
    | None -> None
  in

  match Files.read_file rescript_json with
  | Some raw -> parse_raw raw
  | None -> (
    Log.log ("Unable to read " ^ rescript_json);
    match Files.read_file bsconfig_json with
    | Some raw -> parse_raw raw
    | None ->
      Log.log ("Unable to read " ^ bsconfig_json);
      None)

let find_root ~uri packages_by_root =
  let path = Uri.to_path uri in
  let rec loop path =
    if path = "/" then None
    else if Hashtbl.mem packages_by_root path then Some (`Root path)
    else if
      Files.exists (Filename.concat path "rescript.json")
      || Files.exists (Filename.concat path "bsconfig.json")
    then Some (`Bs path)
    else
      let parent = Filename.dirname path in
      if parent = path then (* reached root *) None else loop parent
  in
  loop (if Sys.is_directory path then path else Filename.dirname path)

let get_package ~uri =
  let open SharedTypes in
  if Hashtbl.mem state.root_for_uri uri then
    Some (Hashtbl.find state.packages_by_root (Hashtbl.find state.root_for_uri uri))
  else
    match find_root ~uri state.packages_by_root with
    | None ->
      Log.log "No root directory found";
      None
    | Some (`Root root_path) ->
      Hashtbl.replace state.root_for_uri uri root_path;
      Some
        (Hashtbl.find state.packages_by_root (Hashtbl.find state.root_for_uri uri))
    | Some (`Bs root_path) -> (
      match new_bs_package ~root_path with
      | None -> None
      | Some package ->
        Hashtbl.replace state.root_for_uri uri package.root_path;
        Hashtbl.replace state.packages_by_root package.root_path package;
        Some package)
