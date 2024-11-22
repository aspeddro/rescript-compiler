open Common

let load_cmt_file cmt_file_path =
  let cmt_infos = Cmt_format.read_cmt cmt_file_path in
  let exclude_path source_file =
    !Cli.exclude_paths
    |> List.exists (fun prefix_ ->
           let prefix =
             match Filename.is_relative source_file with
             | true -> prefix_
             | false -> Filename.concat (Sys.getcwd ()) prefix_
           in
           String.length prefix <= String.length source_file
           &&
           try String.sub source_file 0 (String.length prefix) = prefix
           with Invalid_argument _ -> false)
  in
  match cmt_infos.cmt_annots |> FindSourceFile.cmt with
  | Some source_file when not (exclude_path source_file) ->
    if !Cli.debug then
      Log_.item "Scanning %s Source:%s@."
        (match !Cli.ci && not (Filename.is_relative cmt_file_path) with
        | true -> Filename.basename cmt_file_path
        | false -> cmt_file_path)
        (match !Cli.ci && not (Filename.is_relative source_file) with
        | true -> source_file |> Filename.basename
        | false -> source_file);
    FileReferences.add_file source_file;
    current_src := source_file;
    current_module := Paths.get_module_name source_file;
    current_module_name :=
      !current_module
      |> Name.create ~is_interface:(Filename.check_suffix !current_src "i");
    if run_config.dce then cmt_infos |> DeadCode.process_cmt ~cmt_file_path;
    if run_config.exception_ then cmt_infos |> Exception.process_cmt;
    if run_config.termination then cmt_infos |> Arnold.process_cmt
  | _ -> ()

let process_cmt_files ~cmt_root =
  let ( +++ ) = Filename.concat in
  match cmt_root with
  | Some root ->
    Cli.cmt_command := true;
    let rec walk_sub_dirs dir =
      let abs_dir =
        match dir = "" with
        | true -> root
        | false -> root +++ dir
      in
      let skip_dir =
        let base = Filename.basename dir in
        base = "node_modules" || base = "_esy"
      in
      if (not skip_dir) && Sys.file_exists abs_dir then
        if Sys.is_directory abs_dir then
          abs_dir |> Sys.readdir
          |> Array.iter (fun d -> walk_sub_dirs (dir +++ d))
        else if
          Filename.check_suffix abs_dir ".cmt"
          || Filename.check_suffix abs_dir ".cmti"
        then abs_dir |> load_cmt_file
    in
    walk_sub_dirs ""
  | None ->
    Lazy.force Paths.set_re_script_project_root;
    let lib_bs = run_config.project_root +++ ("lib" +++ "bs") in
    let source_dirs =
      Paths.read_source_dirs ~config_sources:None |> List.sort String.compare
    in
    source_dirs
    |> List.iter (fun source_dir ->
           let lib_bs_source_dir = Filename.concat lib_bs source_dir in
           let files =
             match Sys.readdir lib_bs_source_dir |> Array.to_list with
             | files -> files
             | exception Sys_error _ -> []
           in
           let cmt_files =
             files
             |> List.filter (fun x ->
                    Filename.check_suffix x ".cmt"
                    || Filename.check_suffix x ".cmti")
           in
           cmt_files |> List.sort String.compare
           |> List.iter (fun cmt_file ->
                  let cmt_file_path =
                    Filename.concat lib_bs_source_dir cmt_file
                  in
                  cmt_file_path |> load_cmt_file))

let run_analysis ~cmt_root =
  process_cmt_files ~cmt_root;
  if run_config.dce then (
    DeadException.force_delayed_items ();
    DeadOptionalArgs.force_delayed_items ();
    DeadCommon.report_dead ~check_optional_arg:DeadOptionalArgs.check;
    WriteDeadAnnotations.write ());
  if run_config.exception_ then Exception.Checks.do_checks ();
  if run_config.termination && !Common.Cli.debug then Arnold.report_stats ()

let run_analysis_and_report ~cmt_root =
  Log_.Color.setup ();
  if !Common.Cli.json then EmitJson.start ();
  run_analysis ~cmt_root;
  Log_.Stats.report ();
  Log_.Stats.clear ();
  if !Common.Cli.json then EmitJson.finish ()

let cli () =
  let analysis_kind_set = ref false in
  let cmt_root_ref = ref None in
  let usage = "reanalyze version " ^ Version.version in
  let version_and_exit () =
    print_endline usage;
    exit 0
      [@@raises exit]
  in
  let rec set_all cmt_root =
    RunConfig.all ();
    cmt_root_ref := cmt_root;
    analysis_kind_set := true
  and set_config () =
    Paths.Config.process_bsconfig ();
    analysis_kind_set := true
  and set_d_c_e cmt_root =
    RunConfig.dce ();
    cmt_root_ref := cmt_root;
    analysis_kind_set := true
  and set_exception cmt_root =
    RunConfig.exception_ ();
    cmt_root_ref := cmt_root;
    analysis_kind_set := true
  and set_termination cmt_root =
    RunConfig.termination ();
    cmt_root_ref := cmt_root;
    analysis_kind_set := true
  and speclist =
    [
      ("-all", Arg.Unit (fun () -> set_all None), "Run all the analyses.");
      ( "-all-cmt",
        String (fun s -> set_all (Some s)),
        "root_path Run all the analyses for all the .cmt files under the root \
         path" );
      ("-ci", Unit (fun () -> Cli.ci := true), "Internal flag for use in CI");
      ( "-config",
        Unit set_config,
        "Read the analysis mode from rescript.json/bsconfig.json" );
      ("-dce", Unit (fun () -> set_d_c_e None), "Eperimental DCE");
      ("-debug", Unit (fun () -> Cli.debug := true), "Print debug information");
      ( "-dce-cmt",
        String (fun s -> set_d_c_e (Some s)),
        "root_path Experimental DCE for all the .cmt files under the root path"
      );
      ( "-exception",
        Unit (fun () -> set_exception None),
        "Experimental exception analysis" );
      ( "-exception-cmt",
        String (fun s -> set_exception (Some s)),
        "root_path Experimental exception analysis for all the .cmt files \
         under the root path" );
      ( "-exclude-paths",
        String
          (fun s ->
            let paths = s |> String.split_on_char ',' in
            Common.Cli.exclude_paths :=
              paths @ Common.Cli.exclude_paths.contents),
        "comma-separated-path-prefixes Exclude from analysis files whose path \
         has a prefix in the list" );
      ( "-experimental",
        Set Common.Cli.experimental,
        "Turn on experimental analyses (this option is currently unused)" );
      ( "-externals",
        Set DeadCommon.Config.analyze_externals,
        "Report on externals in dead code analysis" );
      ("-json", Set Common.Cli.json, "Print reports in json format");
      ( "-live-names",
        String
          (fun s ->
            let names = s |> String.split_on_char ',' in
            Common.Cli.live_names := names @ Common.Cli.live_names.contents),
        "comma-separated-names Consider all values with the given names as live"
      );
      ( "-live-paths",
        String
          (fun s ->
            let paths = s |> String.split_on_char ',' in
            Common.Cli.live_paths := paths @ Common.Cli.live_paths.contents),
        "comma-separated-path-prefixes Consider all values whose path has a \
         prefix in the list as live" );
      ( "-suppress",
        String
          (fun s ->
            let names = s |> String.split_on_char ',' in
            run_config.suppress <- names @ run_config.suppress),
        "comma-separated-path-prefixes Don't report on files whose path has a \
         prefix in the list" );
      ( "-termination",
        Unit (fun () -> set_termination None),
        "Experimental termination analysis" );
      ( "-termination-cmt",
        String (fun s -> set_termination (Some s)),
        "root_path Experimental termination analysis for all the .cmt files \
         under the root path" );
      ( "-unsuppress",
        String
          (fun s ->
            let names = s |> String.split_on_char ',' in
            run_config.unsuppress <- names @ run_config.unsuppress),
        "comma-separated-path-prefixes Report on files whose path has a prefix \
         in the list, overriding -suppress (no-op if -suppress is not \
         specified)" );
      ("-version", Unit version_and_exit, "Show version information and exit");
      ("--version", Unit version_and_exit, "Show version information and exit");
      ( "-write",
        Set Common.Cli.write,
        "Write @dead annotations directly in the source files" );
    ]
  in
  Arg.parse speclist print_endline usage;
  if !analysis_kind_set = false then set_config ();
  let cmt_root = !cmt_root_ref in
  run_analysis_and_report ~cmt_root
[@@raises exit]

module RunConfig = RunConfig
module Log_ = Log_
