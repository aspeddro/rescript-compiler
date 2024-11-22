let ( /+ ) = Filename.concat

let rec resolve_node_module_path ~start_path name =
  let path = start_path /+ "node_modules" /+ name in
  if Files.exists path then Some path
  else if Filename.dirname start_path = start_path then None
  else resolve_node_module_path ~start_path:(Filename.dirname start_path) name
