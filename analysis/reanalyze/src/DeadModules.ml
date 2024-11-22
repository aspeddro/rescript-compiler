let active () =
  (* When transitive reporting is off, the only dead modules would be empty modules *)
  RunConfig.run_config.transitive

let table = Hashtbl.create 1

let mark_dead ~is_type ~loc path =
  if active () then
    let module_name = path |> Common.Path.to_module_name ~is_type in
    match Hashtbl.find_opt table module_name with
    | Some _ -> ()
    | _ -> Hashtbl.replace table module_name (false, loc)

let mark_live ~is_type ~(loc : Location.t) path =
  if active () then
    let module_name = path |> Common.Path.to_module_name ~is_type in
    match Hashtbl.find_opt table module_name with
    | None -> Hashtbl.replace table module_name (true, loc)
    | Some (false, loc) -> Hashtbl.replace table module_name (true, loc)
    | Some (true, _) -> ()

let check_module_dead ~file_name:pos_fname module_name =
  if active () then
    match Hashtbl.find_opt table module_name with
    | Some (false, loc) ->
      Hashtbl.remove table module_name;
      (* only report once *)
      let loc =
        if loc.loc_ghost then
          let pos =
            {Lexing.pos_fname; pos_lnum = 0; pos_bol = 0; pos_cnum = 0}
          in
          {Location.loc_start = pos; loc_end = pos; loc_ghost = false}
        else loc
      in
      Log_.warning ~loc
        (Common.DeadModule
           {
             message =
               Format.asprintf "@{<info>%s@} %s"
                 (module_name |> Name.to_interface |> Name.to_string)
                 "is a dead module as all its items are dead.";
           })
    | _ -> ()
