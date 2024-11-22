open Common
module StringMap = Map_string

let bsconfig = "bsconfig.json"
let rescript_json = "rescript.json"

let read_file filename =
  try
    (* windows can't use open_in *)
    let chan = open_in_bin filename in
    let content = really_input_string chan (in_channel_length chan) in
    close_in_noerr chan;
    Some content
  with _ -> None

let rec find_project_root ~dir =
  let rescript_json_file = Filename.concat dir rescript_json in
  let bsconfig_file = Filename.concat dir bsconfig in
  if Sys.file_exists rescript_json_file || Sys.file_exists bsconfig_file then dir
  else
    let parent = dir |> Filename.dirname in
    if parent = dir then (
      prerr_endline
        ("Error: cannot find project root containing " ^ rescript_json ^ ".");
      assert false)
    else find_project_root ~dir:parent

let set_re_script_project_root =
  lazy
    (run_config.project_root <- find_project_root ~dir:(Sys.getcwd ());
     run_config.bsb_project_root <-
       (match Sys.getenv_opt "BSB_PROJECT_ROOT" with
       | None -> run_config.project_root
       | Some s -> s))

module Config = struct
  let read_suppress conf =
    match Json.get "suppress" conf with
    | Some (Array elements) ->
      let names =
        elements
        |> List.filter_map (fun (x : Json.t) ->
               match x with
               | String s -> Some s
               | _ -> None)
      in
      run_config.suppress <- names @ run_config.suppress
    | _ -> ()

  let read_unsuppress conf =
    match Json.get "unsuppress" conf with
    | Some (Array elements) ->
      let names =
        elements
        |> List.filter_map (fun (x : Json.t) ->
               match x with
               | String s -> Some s
               | _ -> None)
      in
      run_config.unsuppress <- names @ run_config.unsuppress
    | _ -> ()

  let read_analysis conf =
    match Json.get "analysis" conf with
    | Some (Array elements) ->
      elements
      |> List.iter (fun (x : Json.t) ->
             match x with
             | String "all" -> RunConfig.all ()
             | String "dce" -> RunConfig.dce ()
             | String "exception" -> RunConfig.exception_ ()
             | String "termination" -> RunConfig.termination ()
             | _ -> ())
    | _ ->
      (* if no "analysis" specified, default to dce *)
      RunConfig.dce ()

  let read_transitive conf =
    match Json.get "transitive" conf with
    | Some True -> RunConfig.transitive true
    | Some False -> RunConfig.transitive false
    | _ -> ()

  (* Read the config from rescript.json/bsconfig.json and apply it to runConfig and suppress and unsuppress *)
  let process_bsconfig () =
    Lazy.force set_re_script_project_root;
    let rescript_file = Filename.concat run_config.project_root rescript_json in
    let bsconfig_file = Filename.concat run_config.project_root bsconfig in

    let process_text text =
      match Json.parse text with
      | None -> ()
      | Some json -> (
        match Json.get "reanalyze" json with
        | Some conf ->
          read_suppress conf;
          read_unsuppress conf;
          read_analysis conf;
          read_transitive conf
        | None ->
          (* if no "analysis" specified, default to dce *)
          RunConfig.dce ())
    in

    match read_file rescript_file with
    | Some text -> process_text text
    | None -> (
      match read_file bsconfig_file with
      | Some text -> process_text text
      | None -> ())
end

(**
  * Handle namespaces in cmt files.
  * E.g. src/Module-Project.cmt becomes src/Module
  *)
let handle_namespace cmt =
  let cut_after_dash s =
    match String.index s '-' with
    | n -> ( try String.sub s 0 n with Invalid_argument _ -> s)
    | exception Not_found -> s
  in
  let no_dir = Filename.basename cmt = cmt in
  if no_dir then cmt |> Filename.remove_extension |> cut_after_dash
  else
    let dir = cmt |> Filename.dirname in
    let base =
      cmt |> Filename.basename |> Filename.remove_extension |> cut_after_dash
    in
    Filename.concat dir base

let get_module_name cmt = cmt |> handle_namespace |> Filename.basename

let read_dirs_from_config ~config_sources =
  let dirs = ref [] in
  let root = run_config.project_root in
  let rec process_dir ~subdirs dir =
    let abs_dir =
      match dir = "" with
      | true -> root
      | false -> Filename.concat root dir
    in
    if Sys.file_exists abs_dir && Sys.is_directory abs_dir then (
      dirs := dir :: !dirs;
      if subdirs then
        abs_dir |> Sys.readdir
        |> Array.iter (fun d -> process_dir ~subdirs (Filename.concat dir d)))
  in
  let rec process_source_item (source_item : Ext_json_types.t) =
    match source_item with
    | Str {str} -> str |> process_dir ~subdirs:false
    | Obj {map} -> (
      match StringMap.find_opt map "dir" with
      | Some (Str {str}) ->
        let subdirs =
          match StringMap.find_opt map "subdirs" with
          | Some (True _) -> true
          | Some (False _) -> false
          | _ -> false
        in
        str |> process_dir ~subdirs
      | _ -> ())
    | Arr {content = arr} -> arr |> Array.iter process_source_item
    | _ -> ()
  in
  (match config_sources with
  | Some source_item -> process_source_item source_item
  | None -> ());
  !dirs

let read_source_dirs ~config_sources =
  let source_dirs =
    ["lib"; "bs"; ".sourcedirs.json"]
    |> List.fold_left Filename.concat run_config.bsb_project_root
  in
  let dirs = ref [] in
  let read_dirs json =
    match json with
    | Ext_json_types.Obj {map} -> (
      match StringMap.find_opt map "dirs" with
      | Some (Arr {content = arr}) ->
        arr
        |> Array.iter (fun x ->
               match x with
               | Ext_json_types.Str {str} -> dirs := str :: !dirs
               | _ -> ());
        ()
      | _ -> ())
    | _ -> ()
  in
  if source_dirs |> Sys.file_exists then
    let json_opt = source_dirs |> Ext_json_parse.parse_json_from_file in
    match json_opt with
    | exception _ -> ()
    | json ->
      if run_config.bsb_project_root <> run_config.project_root then (
        read_dirs json;
        dirs := read_dirs_from_config ~config_sources)
      else read_dirs json
  else (
    if !Cli.debug then (
      Log_.item "Warning: can't find source dirs: %s\n" source_dirs;
      Log_.item "Types for cross-references will not be found.\n");
    dirs := read_dirs_from_config ~config_sources);
  !dirs
