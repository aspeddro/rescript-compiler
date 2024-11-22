open Common
module NameMap = Map.Make (Name)

(* Keep track of the module path while traversing with Tast_mapper *)
type t = {aliases: Path.t NameMap.t; loc: Location.t; path: Path.t}

let initial = ({aliases = NameMap.empty; loc = Location.none; path = []} : t)
let current = (ref initial : t ref)
let init () = current := initial

let normalize_path ~aliases path =
  match path |> List.rev with
  | name :: rest_rev when rest_rev <> [] -> (
    match aliases |> NameMap.find_opt name with
    | None -> path
    | Some path1 ->
      let new_path = List.rev (path1 @ rest_rev) in
      if !Common.Cli.debug then
        Log_.item "Resolve Alias: %s to %s@."
          (path |> Common.Path.to_string)
          (new_path |> Common.Path.to_string);
      new_path)
  | _ -> path

let add_alias ~name ~path =
  let aliases = !current.aliases in
  let path_normalized = path |> normalize_path ~aliases in
  if !Common.Cli.debug then
    Log_.item "Module Alias: %s = %s@." (name |> Name.to_string)
      (Path.to_string path_normalized);
  current := {!current with aliases = NameMap.add name path_normalized aliases}

let resolve_alias path = path |> normalize_path ~aliases:!current.aliases
let get_current () = !current
let set_current p = current := p
