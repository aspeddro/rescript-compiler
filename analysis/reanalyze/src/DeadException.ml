open DeadCommon
open Common

type item = {exception_path: Path.t; loc_from: Location.t}

let delayed_items = ref []
let declarations = Hashtbl.create 1

let add ~path ~loc ~(str_loc : Location.t) name =
  let exception_path = name :: path in
  Hashtbl.add declarations exception_path loc;
  name
  |> addDeclaration_ ~pos_end:str_loc.loc_end ~pos_start:str_loc.loc_start
       ~decl_kind:Exception ~module_loc:(ModulePath.get_current ()).loc ~path
       ~loc

let force_delayed_items () =
  let items = !delayed_items |> List.rev in
  delayed_items := [];
  items
  |> List.iter (fun {exception_path; loc_from} ->
         match Hashtbl.find_opt declarations exception_path with
         | None -> ()
         | Some loc_to ->
           add_value_reference ~add_file_reference:true ~loc_from ~loc_to)

let mark_as_used ~(loc_from : Location.t) ~(loc_to : Location.t) path_ =
  if loc_to.loc_ghost then
    (* Probably defined in another file, delay processing and check at the end *)
    let exception_path =
      path_ |> Path.from_path_t |> Path.module_to_implementation
    in
    delayed_items := {exception_path; loc_from} :: !delayed_items
  else add_value_reference ~add_file_reference:true ~loc_from ~loc_to
