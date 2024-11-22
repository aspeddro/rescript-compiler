let namespaced_name namespace name =
  match namespace with
  | None -> name
  | Some namespace -> name ^ "-" ^ namespace

let ( /+ ) = Filename.concat

let get_bs_platform_dir root_path =
  match !Cfg.is_doc_gen_from_compiler with
  | false -> (
    let result =
      ModuleResolution.resolve_node_module_path ~start_path:root_path "rescript"
    in
    match result with
    | Some path -> Some path
    | None ->
      let message = "rescript could not be found" in
      Log.log message;
      None)
  | true -> Some root_path

let get_lib_bs root = Files.if_exists (root /+ "lib" /+ "bs")

let get_stdlib base =
  match get_bs_platform_dir base with
  | None -> None
  | Some bs_platform_dir -> Some (bs_platform_dir /+ "lib" /+ "ocaml")
