let doc_help =
  {|ReScript Tools

Output documentation to standard output

Usage: rescript-tools doc <FILE>

Example: rescript-tools doc ./path/to/EntryPointLib.res|}

let help =
  {|ReScript Tools

Usage: rescript-tools [command]

Commands:

doc <file>            Generate documentation
reanalyze             Reanalyze
-v, --version         Print version
-h, --help            Print help|}

let log_and_exit = function
  | Ok log ->
    Printf.printf "%s\n" log;
    exit 0
  | Error log ->
    Printf.eprintf "%s\n" log;
    exit 1

let version = Version.version

let main () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "doc" :: rest -> (
    match rest with
    | ["-h"] | ["--help"] -> log_and_exit (Ok doc_help)
    | [path] ->
      (* NOTE: Internal use to generate docs from compiler *)
      let () =
        match Sys.getenv_opt "FROM_COMPILER" with
        | Some "true" -> Analysis.Cfg.is_doc_gen_from_compiler := true
        | _ -> ()
      in
      log_and_exit (Tools.extract_docs ~entry_point_file:path ~debug:false)
    | _ -> log_and_exit (Error doc_help))
  | "reanalyze" :: _ ->
    let len = Array.length Sys.argv in
    for i = 1 to len - 2 do
      Sys.argv.(i) <- Sys.argv.(i + 1)
    done;
    Sys.argv.(len - 1) <- "";
    Reanalyze.cli ()
  | "extract-embedded" :: ext_point_names :: filename :: _ ->
    log_and_exit
      (Ok
         (Tools.extract_embedded
            ~extension_points:(ext_point_names |> String.split_on_char ',')
            ~filename))
  | ["-h"] | ["--help"] -> log_and_exit (Ok help)
  | ["-v"] | ["--version"] -> log_and_exit (Ok version)
  | _ -> log_and_exit (Error help)

let () = main ()
