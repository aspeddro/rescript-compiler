open Common

let check_prefix prefix_ =
  let prefix =
    match run_config.project_root = "" with
    | true -> prefix_
    | false -> Filename.concat run_config.project_root prefix_
  in
  let prefix_len = prefix |> String.length in
  fun source_dir ->
    try String.sub source_dir 0 prefix_len = prefix
    with Invalid_argument _ -> false

let suppress_source_dir =
  lazy
    (fun source_dir ->
      run_config.suppress
      |> List.exists (fun prefix -> check_prefix prefix source_dir))

let unsuppress_source_dir =
  lazy
    (fun source_dir ->
      run_config.unsuppress
      |> List.exists (fun prefix -> check_prefix prefix source_dir))

let pos_in_suppress (pos : Lexing.position) =
  pos.pos_fname |> Lazy.force suppress_source_dir

let pos_in_unsuppress (pos : Lexing.position) =
  pos.pos_fname |> Lazy.force unsuppress_source_dir

(** First suppress list, then override with unsuppress list *)
let filter pos = (not (pos_in_suppress pos)) || pos_in_unsuppress pos
