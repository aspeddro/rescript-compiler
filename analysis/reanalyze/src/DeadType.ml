(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

open Common
open DeadCommon

module TypeLabels = struct
  (* map from type path (for record/variant label) to its location *)

  let table = (Hashtbl.create 256 : (Path.t, Location.t) Hashtbl.t)
  let add path loc = Hashtbl.replace table path loc
  let find path = Hashtbl.find_opt table path
end

let add_type_reference ~pos_from ~pos_to =
  if !Common.Cli.debug then
    Log_.item "addTypeReference %s --> %s@."
      (pos_from |> pos_to_string)
      (pos_to |> pos_to_string);
  TypeReferences.add pos_to pos_from

module TypeDependencies = struct
  let delayed_items = ref []
  let add loc1 loc2 = delayed_items := (loc1, loc2) :: !delayed_items
  let clear () = delayed_items := []

  let process_type_dependency
      ( ({loc_start = pos_to; loc_ghost = ghost1} : Location.t),
        ({loc_start = pos_from; loc_ghost = ghost2} : Location.t) ) =
    if (not ghost1) && (not ghost2) && pos_to <> pos_from then
      add_type_reference ~pos_to ~pos_from

  let force_delayed_items () = List.iter process_type_dependency !delayed_items
end

let extend_type_dependencies (loc1 : Location.t) (loc2 : Location.t) =
  if loc1.loc_start <> loc2.loc_start then (
    if !Common.Cli.debug then
      Log_.item "extendTypeDependencies %s --> %s@."
        (loc1.loc_start |> pos_to_string)
        (loc2.loc_start |> pos_to_string);
    TypeDependencies.add loc1 loc2)

(* Type dependencies between Foo.re and Foo.rei *)
let add_type_dependencies_across_files ~path_to_type ~loc ~type_label_name =
  let is_interface = Filename.check_suffix !Common.current_src "i" in
  if not is_interface then (
    let path_1 = path_to_type |> Path.module_to_interface in
    let path_2 = path_1 |> Path.type_to_interface in
    let path1 = type_label_name :: path_1 in
    let path2 = type_label_name :: path_2 in
    match TypeLabels.find path1 with
    | None -> (
      match TypeLabels.find path2 with
      | None -> ()
      | Some loc2 ->
        extend_type_dependencies loc loc2;
        if not Config.report_types_dead_only_in_interface then
          extend_type_dependencies loc2 loc)
    | Some loc1 ->
      extend_type_dependencies loc loc1;
      if not Config.report_types_dead_only_in_interface then
        extend_type_dependencies loc1 loc)
  else
    let path_1 = path_to_type |> Path.module_to_implementation in
    let path1 = type_label_name :: path_1 in
    match TypeLabels.find path1 with
    | None -> ()
    | Some loc1 ->
      extend_type_dependencies loc1 loc;
      if not Config.report_types_dead_only_in_interface then
        extend_type_dependencies loc loc1

(* Add type dependencies between implementation and interface in inner module *)
let add_type_dependencies_inner_module ~path_to_type ~loc ~type_label_name =
  let path = type_label_name :: path_to_type in
  match TypeLabels.find path with
  | Some loc2 ->
    extend_type_dependencies loc loc2;
    if not Config.report_types_dead_only_in_interface then
      extend_type_dependencies loc2 loc
  | None -> TypeLabels.add path loc

let add_declaration ~(type_id : Ident.t) ~(type_kind : Types.type_kind) =
  let current_module_path = ModulePath.get_current () in
  let path_to_type =
    (type_id |> Ident.name |> Name.create)
    :: (current_module_path.path @ [!Common.current_module_name])
  in
  let process_type_label ?(pos_adjustment = Nothing) type_label_name ~decl_kind
      ~(loc : Location.t) =
    addDeclaration_ ~decl_kind ~path:path_to_type ~loc
      ~module_loc:current_module_path.loc ~pos_adjustment type_label_name;
    add_type_dependencies_across_files ~path_to_type ~loc ~type_label_name;
    add_type_dependencies_inner_module ~path_to_type ~loc ~type_label_name;
    TypeLabels.add (type_label_name :: path_to_type) loc
  in
  match type_kind with
  | Type_record (l, _) ->
    List.iter
      (fun {Types.ld_id; ld_loc} ->
        Ident.name ld_id |> Name.create
        |> process_type_label ~decl_kind:RecordLabel ~loc:ld_loc)
      l
  | Type_variant decls ->
    List.iteri
      (fun i {Types.cd_id; cd_loc; cd_args} ->
        let _handle_inline_records =
          match cd_args with
          | Cstr_record lbls ->
            List.iter
              (fun {Types.ld_id; ld_loc} ->
                Ident.name cd_id ^ "." ^ Ident.name ld_id
                |> Name.create
                |> process_type_label ~decl_kind:RecordLabel ~loc:ld_loc)
              lbls
          | Cstr_tuple _ -> ()
        in
        let pos_adjustment =
          (* In Res the variant loc can include the | and spaces after it *)
          if WriteDeadAnnotations.pos_language cd_loc.loc_start = Res then
            if i = 0 then FirstVariant else OtherVariant
          else Nothing
        in
        Ident.name cd_id |> Name.create
        |> process_type_label ~decl_kind:VariantCase ~loc:cd_loc ~pos_adjustment)
      decls
  | _ -> ()
