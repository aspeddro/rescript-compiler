(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

open Common

module PosSet = Set.Make (struct
  type t = Lexing.position

  let compare = compare
end)

module Config = struct
  (* Turn on type analysis *)
  let analyze_types = ref true
  let analyze_externals = ref false
  let report_underscore = false
  let report_types_dead_only_in_interface = false
  let recursive_debug = false
  let warn_on_circular_dependencies = false
end

module Current = struct
  let bindings = ref PosSet.empty
  let last_binding = ref Location.none

  (** max end position of a value reported dead *)
  let max_value_pos_end = ref Lexing.dummy_pos
end

let rec check_sub s1 s2 n =
  n <= 0
  || (try s1.[n] = s2.[n] with Invalid_argument _ -> false)
     && check_sub s1 s2 (n - 1)

let file_is_implementation_of s1 s2 =
  let n1 = String.length s1 and n2 = String.length s2 in
  n2 = n1 + 1 && check_sub s1 s2 (n1 - 1)

let live_annotation = "live"

module PosHash = struct
  include Hashtbl.Make (struct
    type t = Lexing.position

    let hash x =
      let s = Filename.basename x.Lexing.pos_fname in
      Hashtbl.hash (x.Lexing.pos_cnum, s)

    let equal (x : t) y = x = y
  end)

  let find_set h k = try find h k with Not_found -> PosSet.empty

  let add_set h k v =
    let set = find_set h k in
    replace h k (PosSet.add v set)
end

type decls = decl PosHash.t
(** all exported declarations *)

let decls = (PosHash.create 256 : decls)

module ValueReferences = struct
  (** all value references *)
  let table = (PosHash.create 256 : PosSet.t PosHash.t)

  let add pos_to pos_from = PosHash.add_set table pos_to pos_from
  let find pos = PosHash.find_set table pos
end

module TypeReferences = struct
  (** all type references *)
  let table = (PosHash.create 256 : PosSet.t PosHash.t)

  let add pos_to pos_from = PosHash.add_set table pos_to pos_from
  let find pos = PosHash.find_set table pos
end

let decl_get_loc decl =
  let loc_start =
    let offset =
      WriteDeadAnnotations.offset_of_pos_adjustment decl.pos_adjustment
    in
    let cnum_with_offset = decl.pos_start.pos_cnum + offset in
    if cnum_with_offset < decl.pos_end.pos_cnum then
      {decl.pos_start with pos_cnum = cnum_with_offset}
    else decl.pos_start
  in
  {Location.loc_start; loc_end = decl.pos_end; loc_ghost = false}

let add_value_reference ~add_file_reference ~(loc_from : Location.t)
    ~(loc_to : Location.t) =
  let last_binding = !Current.last_binding in
  let loc_from =
    match last_binding = Location.none with
    | true -> loc_from
    | false -> last_binding
  in
  if not loc_from.loc_ghost then (
    if !Cli.debug then
      Log_.item "addValueReference %s --> %s@."
        (loc_from.loc_start |> pos_to_string)
        (loc_to.loc_start |> pos_to_string);
    ValueReferences.add loc_to.loc_start loc_from.loc_start;
    if
      add_file_reference && (not loc_to.loc_ghost) && (not loc_from.loc_ghost)
      && loc_from.loc_start.pos_fname <> loc_to.loc_start.pos_fname
    then FileReferences.add loc_from loc_to)

let iter_files_from_roots_to_leaves iter_fun =
  (* For each file, the number of incoming references *)
  let inverse_references = (Hashtbl.create 1 : (string, int) Hashtbl.t) in
  (* For each number of incoming references, the files *)
  let references_by_number = (Hashtbl.create 1 : (int, FileSet.t) Hashtbl.t) in
  let get_num file_name =
    try Hashtbl.find inverse_references file_name with Not_found -> 0
  in
  let get_set num =
    try Hashtbl.find references_by_number num with Not_found -> FileSet.empty
  in
  let add_incoming_edge file_name =
    let old_num = get_num file_name in
    let new_num = old_num + 1 in
    let old_set_at_num = get_set old_num in
    let new_set_at_num = FileSet.remove file_name old_set_at_num in
    let old_set_at_new_num = get_set new_num in
    let new_set_at_new_num = FileSet.add file_name old_set_at_new_num in
    Hashtbl.replace inverse_references file_name new_num;
    Hashtbl.replace references_by_number old_num new_set_at_num;
    Hashtbl.replace references_by_number new_num new_set_at_new_num
  in
  let remove_incoming_edge file_name =
    let old_num = get_num file_name in
    let new_num = old_num - 1 in
    let old_set_at_num = get_set old_num in
    let new_set_at_num = FileSet.remove file_name old_set_at_num in
    let old_set_at_new_num = get_set new_num in
    let new_set_at_new_num = FileSet.add file_name old_set_at_new_num in
    Hashtbl.replace inverse_references file_name new_num;
    Hashtbl.replace references_by_number old_num new_set_at_num;
    Hashtbl.replace references_by_number new_num new_set_at_new_num
  in
  let add_edge from_file to_file =
    if FileReferences.exists from_file then add_incoming_edge to_file
  in
  let remove_edge from_file to_file =
    if FileReferences.exists from_file then remove_incoming_edge to_file
  in
  FileReferences.iter (fun from_file set ->
      if get_num from_file = 0 then
        Hashtbl.replace references_by_number 0
          (FileSet.add from_file (get_set 0));
      set |> FileSet.iter (fun to_file -> add_edge from_file to_file));
  while get_set 0 <> FileSet.empty do
    let files_with_no_incoming_references = get_set 0 in
    Hashtbl.remove references_by_number 0;
    files_with_no_incoming_references
    |> FileSet.iter (fun file_name ->
           iter_fun file_name;
           let references = FileReferences.find file_name in
           references
           |> FileSet.iter (fun to_file -> remove_edge file_name to_file))
  done;
  (* Process any remaining items in case of circular references *)
  references_by_number
  |> Hashtbl.iter (fun _num set ->
         if FileSet.is_empty set then ()
         else
           set
           |> FileSet.iter (fun file_name ->
                  let pos = {Lexing.dummy_pos with pos_fname = file_name} in
                  let loc =
                    {Location.none with loc_start = pos; loc_end = pos}
                  in
                  if Config.warn_on_circular_dependencies then
                    Log_.warning ~loc
                      (Circular
                         {
                           message =
                             Format.asprintf
                               "Results for %s could be inaccurate because of \
                                circular references"
                               file_name;
                         });
                  iter_fun file_name))

(** Keep track of the location of values annotated @genType or @dead *)
module ProcessDeadAnnotations = struct
  type annotated_as = GenType | Dead | Live

  let positions_annotated = PosHash.create 1
  let is_annotated_dead pos =
    PosHash.find_opt positions_annotated pos = Some Dead

  let is_annotated_gen_type_or_live pos =
    match PosHash.find_opt positions_annotated pos with
    | Some (Live | GenType) -> true
    | Some Dead | None -> false

  let is_annotated_gen_type_or_dead pos =
    match PosHash.find_opt positions_annotated pos with
    | Some (Dead | GenType) -> true
    | Some Live | None -> false

  let annotate_gen_type (pos : Lexing.position) =
    PosHash.replace positions_annotated pos GenType

  let annotate_dead (pos : Lexing.position) =
    PosHash.replace positions_annotated pos Dead

  let annotate_live (pos : Lexing.position) =
    PosHash.replace positions_annotated pos Live

  let process_attributes ~do_gen_type ~name ~pos attributes =
    let get_payload_fun f = attributes |> Annotation.get_attribute_payload f in
    let get_payload (x : string) =
      attributes |> Annotation.get_attribute_payload (( = ) x)
    in
    if
      do_gen_type
      && get_payload_fun Annotation.tag_is_one_of_the_gen_type_annotations
         <> None
    then pos |> annotate_gen_type;
    if get_payload WriteDeadAnnotations.dead_annotation <> None then
      pos |> annotate_dead;
    let name_is_in_live_names_or_paths () =
      !Cli.live_names |> List.mem name
      ||
      let fname =
        match Filename.is_relative pos.pos_fname with
        | true -> pos.pos_fname
        | false -> Filename.concat (Sys.getcwd ()) pos.pos_fname
      in
      let fname_len = String.length fname in
      !Cli.live_paths
      |> List.exists (fun prefix ->
             String.length prefix <= fname_len
             &&
             try String.sub fname 0 (String.length prefix) = prefix
             with Invalid_argument _ -> false)
    in
    if get_payload live_annotation <> None || name_is_in_live_names_or_paths ()
    then pos |> annotate_live;
    if attributes |> Annotation.is_ocaml_suppress_dead_warning then
      pos |> annotate_live

  let collect_export_locations ~do_gen_type =
    let super = Tast_mapper.default in
    let currently_disable_warnings = ref false in
    let value_binding self
        ({vb_attributes; vb_pat} as value_binding : Typedtree.value_binding) =
      (match vb_pat.pat_desc with
      | Tpat_var (id, {loc = {loc_start = pos}})
      | Tpat_alias ({pat_desc = Tpat_any}, id, {loc = {loc_start = pos}}) ->
        if !currently_disable_warnings then pos |> annotate_live;
        vb_attributes
        |> process_attributes ~do_gen_type ~name:(id |> Ident.name) ~pos
      | _ -> ());
      super.value_binding self value_binding
    in
    let type_kind toplevel_attrs self (type_kind : Typedtree.type_kind) =
      (match type_kind with
      | Ttype_record label_declarations ->
        label_declarations
        |> List.iter
             (fun ({ld_attributes; ld_loc} : Typedtree.label_declaration) ->
               toplevel_attrs @ ld_attributes
               |> process_attributes ~do_gen_type:false ~name:""
                    ~pos:ld_loc.loc_start)
      | Ttype_variant constructor_declarations ->
        constructor_declarations
        |> List.iter
             (fun
               ({cd_attributes; cd_loc; cd_args} :
                 Typedtree.constructor_declaration)
             ->
               let _process_inline_records =
                 match cd_args with
                 | Cstr_record flds ->
                   List.iter
                     (fun ({ld_attributes; ld_loc} :
                            Typedtree.label_declaration) ->
                       toplevel_attrs @ cd_attributes @ ld_attributes
                       |> process_attributes ~do_gen_type:false ~name:""
                            ~pos:ld_loc.loc_start)
                     flds
                 | Cstr_tuple _ -> ()
               in
               toplevel_attrs @ cd_attributes
               |> process_attributes ~do_gen_type:false ~name:""
                    ~pos:cd_loc.loc_start)
      | _ -> ());
      super.type_kind self type_kind
    in
    let type_declaration self (type_declaration : Typedtree.type_declaration) =
      let attributes = type_declaration.typ_attributes in
      let _ = type_kind attributes self type_declaration.typ_kind in
      type_declaration
    in
    let value_description self
        ({val_attributes; val_id; val_val = {val_loc = {loc_start = pos}}} as
         value_description :
          Typedtree.value_description) =
      if !currently_disable_warnings then pos |> annotate_live;
      val_attributes
      |> process_attributes ~do_gen_type ~name:(val_id |> Ident.name) ~pos;
      super.value_description self value_description
    in
    let structure_item self (item : Typedtree.structure_item) =
      (match item.str_desc with
      | Tstr_attribute attribute
        when [attribute] |> Annotation.is_ocaml_suppress_dead_warning ->
        currently_disable_warnings := true
      | _ -> ());
      super.structure_item self item
    in
    let structure self (structure : Typedtree.structure) =
      let old_disable_warnings = !currently_disable_warnings in
      super.structure self structure |> ignore;
      currently_disable_warnings := old_disable_warnings;
      structure
    in
    let signature_item self (item : Typedtree.signature_item) =
      (match item.sig_desc with
      | Tsig_attribute attribute
        when [attribute] |> Annotation.is_ocaml_suppress_dead_warning ->
        currently_disable_warnings := true
      | _ -> ());
      super.signature_item self item
    in
    let signature self (signature : Typedtree.signature) =
      let old_disable_warnings = !currently_disable_warnings in
      super.signature self signature |> ignore;
      currently_disable_warnings := old_disable_warnings;
      signature
    in
    {
      super with
      signature;
      signature_item;
      structure;
      structure_item;
      type_declaration;
      value_binding;
      value_description;
    }

  let structure ~do_gen_type structure =
    let collect_export_locations = collect_export_locations ~do_gen_type in
    structure
    |> collect_export_locations.structure collect_export_locations
    |> ignore

  let signature signature =
    let collect_export_locations = collect_export_locations ~do_gen_type:true in
    signature
    |> collect_export_locations.signature collect_export_locations
    |> ignore
end

let addDeclaration_ ?pos_end ?pos_start ~decl_kind ~path ~(loc : Location.t)
    ?(pos_adjustment = Nothing) ~module_loc (name : Name.t) =
  let pos = loc.loc_start in
  let pos_start =
    match pos_start with
    | Some pos_start -> pos_start
    | None -> pos
  in
  let pos_end =
    match pos_end with
    | Some pos_end -> pos_end
    | None -> loc.loc_end
  in
  (* a .cmi file can contain locations from other files.
     For instance:
         module M : Set.S with type elt = int
     will create value definitions whose location is in set.mli
  *)
  if
    (not loc.loc_ghost)
    && (!current_src = pos.pos_fname || !current_module == "*include*")
  then (
    if !Cli.debug then
      Log_.item "add%sDeclaration %s %s path:%s@."
        (decl_kind |> DeclKind.to_string)
        (name |> Name.to_string) (pos |> pos_to_string) (path |> Path.to_string);
    let decl =
      {
        decl_kind;
        module_loc;
        pos_adjustment;
        path = name :: path;
        pos;
        pos_end;
        pos_start;
        resolved_dead = None;
        report = true;
      }
    in
    PosHash.replace decls pos decl)

let add_value_declaration ?(is_toplevel = true) ~(loc : Location.t) ~module_loc
    ?(optional_args = OptionalArgs.empty) ~path ~side_effects name =
  name
  |> addDeclaration_
       ~decl_kind:(Value {is_toplevel; optional_args; side_effects})
       ~loc ~module_loc ~path

let emit_warning ~decl ~message dead_warning =
  let loc = decl |> decl_get_loc in
  let is_toplevel_value_with_side_effects decl =
    match decl.decl_kind with
    | Value {is_toplevel; side_effects} -> is_toplevel && side_effects
    | _ -> false
  in
  let should_write_line_annotation =
    (not (is_toplevel_value_with_side_effects decl))
    && Suppress.filter decl.pos
    && dead_warning <> IncorrectDeadAnnotation
  in
  let line_annotation =
    if should_write_line_annotation then
      WriteDeadAnnotations.add_line_annotation ~decl
    else None
  in
  decl.path
  |> Path.to_module_name ~is_type:(decl.decl_kind |> DeclKind.is_type)
  |> DeadModules.check_module_dead ~file_name:decl.pos.pos_fname;
  Log_.warning ~loc
    (DeadWarning
       {
         dead_warning;
         path = Path.without_head decl.path;
         message;
         line_annotation;
         should_write_line_annotation;
       })

module Decl = struct
  let is_value decl =
    match decl.decl_kind with
    | Value _ (* | Exception *) -> true
    | _ -> false

  let is_toplevel_value_with_side_effects decl =
    match decl.decl_kind with
    | Value {is_toplevel; side_effects} -> is_toplevel && side_effects
    | _ -> false

  let compare_using_dependencies ~ordered_files
      {
        decl_kind = kind1;
        path = _path1;
        pos =
          {
            pos_fname = fname1;
            pos_lnum = lnum1;
            pos_bol = bol1;
            pos_cnum = cnum1;
          };
      }
      {
        decl_kind = kind2;
        path = _path2;
        pos =
          {
            pos_fname = fname2;
            pos_lnum = lnum2;
            pos_bol = bol2;
            pos_cnum = cnum2;
          };
      } =
    let find_position fn = Hashtbl.find ordered_files fn [@@raises Not_found] in
    (* From the root of the file dependency DAG to the leaves.
       From the bottom of the file to the top. *)
    let position1, position2 =
      try (fname1 |> find_position, fname2 |> find_position)
      with Not_found -> (0, 0)
    in
    compare
      (position1, lnum2, bol2, cnum2, kind1)
      (position2, lnum1, bol1, cnum1, kind2)

  let compare_for_reporting
      {
        decl_kind = kind1;
        pos =
          {
            pos_fname = fname1;
            pos_lnum = lnum1;
            pos_bol = bol1;
            pos_cnum = cnum1;
          };
      }
      {
        decl_kind = kind2;
        pos =
          {
            pos_fname = fname2;
            pos_lnum = lnum2;
            pos_bol = bol2;
            pos_cnum = cnum2;
          };
      } =
    compare
      (fname1, lnum1, bol1, cnum1, kind1)
      (fname2, lnum2, bol2, cnum2, kind2)

  let is_inside_reported_value decl =
    let file_has_changed =
      !Current.max_value_pos_end.pos_fname <> decl.pos.pos_fname
    in
    let inside_reported_value =
      decl |> is_value && (not file_has_changed)
      && !Current.max_value_pos_end.pos_cnum > decl.pos.pos_cnum
    in
    if not inside_reported_value then
      if decl |> is_value then
        if
          file_has_changed
          || decl.pos_end.pos_cnum > !Current.max_value_pos_end.pos_cnum
        then Current.max_value_pos_end := decl.pos_end;
    inside_reported_value

  let report decl =
    let inside_reported_value = decl |> is_inside_reported_value in
    if decl.report then
      let name, message =
        match decl.decl_kind with
        | Exception ->
          (WarningDeadException, "is never raised or passed as value")
        | Value {side_effects} -> (
          let no_side_effects_or_underscore =
            (not side_effects)
            ||
            match decl.path with
            | hd :: _ -> hd |> Name.starts_with_underscore
            | [] -> false
          in
          ( (match not no_side_effects_or_underscore with
            | true -> WarningDeadValueWithSideEffects
            | false -> WarningDeadValue),
            match decl.path with
            | name :: _ when name |> Name.is_underscore ->
              "has no side effects and can be removed"
            | _ -> (
              "is never used"
              ^
              match not no_side_effects_or_underscore with
              | true -> " and could have side effects"
              | false -> "") ))
        | RecordLabel ->
          (WarningDeadType, "is a record label never used to read a value")
        | VariantCase ->
          (WarningDeadType, "is a variant case which is never constructed")
      in
      let has_ref_below () =
        let refs = ValueReferences.find decl.pos in
        let ref_is_below (pos : Lexing.position) =
          decl.pos.pos_fname <> pos.pos_fname
          || decl.pos.pos_cnum < pos.pos_cnum
             && (* not a function defined inside a function, e.g. not a callback *)
             decl.pos_end.pos_cnum < pos.pos_cnum
        in
        refs |> PosSet.exists ref_is_below
      in
      let should_emit_warning =
        (not inside_reported_value)
        && (match decl.path with
           | name :: _ when name |> Name.is_underscore ->
             Config.report_underscore
           | _ -> true)
        && (run_config.transitive || not (has_ref_below ()))
      in
      if should_emit_warning then (
        decl.path
        |> Path.to_module_name ~is_type:(decl.decl_kind |> DeclKind.is_type)
        |> DeadModules.check_module_dead ~file_name:decl.pos.pos_fname;
        emit_warning ~decl ~message name)
end

let decl_is_dead ~refs decl =
  let live_refs =
    refs
    |> PosSet.filter (fun p -> not (ProcessDeadAnnotations.is_annotated_dead p))
  in
  live_refs |> PosSet.cardinal = 0
  && not (ProcessDeadAnnotations.is_annotated_gen_type_or_live decl.pos)

let do_report_dead pos =
  not (ProcessDeadAnnotations.is_annotated_gen_type_or_dead pos)

let rec resolve_recursive_refs ~check_optional_arg ~dead_declarations ~level
    ~ordered_files ~refs ~refs_being_resolved decl : bool =
  match decl.pos with
  | _ when decl.resolved_dead <> None ->
    if Config.recursive_debug then
      Log_.item "recursiveDebug %s [%d] already resolved@."
        (decl.path |> Path.to_string)
        level;
    decl.pos |> ProcessDeadAnnotations.is_annotated_dead
  | _ when PosSet.mem decl.pos !refs_being_resolved ->
    if Config.recursive_debug then
      Log_.item "recursiveDebug %s [%d] is being resolved: assume dead@."
        (decl.path |> Path.to_string)
        level;
    true
  | _ ->
    if Config.recursive_debug then
      Log_.item "recursiveDebug resolving %s [%d]@."
        (decl.path |> Path.to_string)
        level;
    refs_being_resolved := PosSet.add decl.pos !refs_being_resolved;
    let all_deps_resolved = ref true in
    let new_refs =
      refs
      |> PosSet.filter (fun pos ->
             if pos = decl.pos then (
               if Config.recursive_debug then
                 Log_.item "recursiveDebug %s ignoring reference to self@."
                   (decl.path |> Path.to_string);
               false)
             else
               match PosHash.find_opt decls pos with
               | None ->
                 if Config.recursive_debug then
                   Log_.item "recursiveDebug can't find decl for %s@."
                     (pos |> pos_to_string);
                 true
               | Some x_decl ->
                 let x_refs =
                   match x_decl.decl_kind |> DeclKind.is_type with
                   | true -> TypeReferences.find pos
                   | false -> ValueReferences.find pos
                 in
                 let x_decl_is_dead =
                   x_decl
                   |> resolve_recursive_refs ~check_optional_arg
                        ~dead_declarations ~level:(level + 1) ~ordered_files
                        ~refs:x_refs ~refs_being_resolved
                 in
                 if x_decl.resolved_dead = None then all_deps_resolved := false;
                 not x_decl_is_dead)
    in
    let is_dead = decl |> decl_is_dead ~refs:new_refs in
    let is_resolved = (not is_dead) || !all_deps_resolved || level = 0 in
    if is_resolved then (
      decl.resolved_dead <- Some is_dead;
      if is_dead then (
        decl.path
        |> DeadModules.mark_dead
             ~is_type:(decl.decl_kind |> DeclKind.is_type)
             ~loc:decl.module_loc;
        if not (decl.pos |> do_report_dead) then decl.report <- false;
        dead_declarations := decl :: !dead_declarations;
        if not (Decl.is_toplevel_value_with_side_effects decl) then
          decl.pos |> ProcessDeadAnnotations.annotate_dead)
      else (
        check_optional_arg decl;
        decl.path
        |> DeadModules.mark_live
             ~is_type:(decl.decl_kind |> DeclKind.is_type)
             ~loc:decl.module_loc;
        if decl.pos |> ProcessDeadAnnotations.is_annotated_dead then
          emit_warning ~decl ~message:" is annotated @dead but is live"
            IncorrectDeadAnnotation);
      if !Cli.debug then
        let refs_string =
          new_refs |> PosSet.elements |> List.map pos_to_string
          |> String.concat ", "
        in
        Log_.item "%s %s %s: %d references (%s) [%d]@."
          (match is_dead with
          | true -> "Dead"
          | false -> "Live")
          (decl.decl_kind |> DeclKind.to_string)
          (decl.path |> Path.to_string)
          (new_refs |> PosSet.cardinal)
          refs_string level);
    is_dead

let report_dead ~check_optional_arg =
  let iter_decl_in_order ~dead_declarations ~ordered_files decl =
    let refs =
      match decl |> Decl.is_value with
      | true -> ValueReferences.find decl.pos
      | false -> TypeReferences.find decl.pos
    in
    resolve_recursive_refs ~check_optional_arg ~dead_declarations ~level:0
      ~ordered_files ~refs_being_resolved:(ref PosSet.empty) ~refs decl
    |> ignore
  in
  if !Cli.debug then (
    Log_.item "@.File References@.@.";
    let file_list = ref [] in
    FileReferences.iter (fun file files ->
        file_list := (file, files) :: !file_list);
    !file_list
    |> List.sort (fun (f1, _) (f2, _) -> String.compare f1 f2)
    |> List.iter (fun (file, files) ->
           Log_.item "%s -->> %s@."
             (file |> Filename.basename)
             (files |> FileSet.elements |> List.map Filename.basename
            |> String.concat ", ")));
  let declarations =
    PosHash.fold (fun _pos decl declarations -> decl :: declarations) decls []
  in
  let ordered_files = Hashtbl.create 256 in
  iter_files_from_roots_to_leaves
    (let current = ref 0 in
     fun file_name ->
       incr current;
       Hashtbl.add ordered_files file_name !current);
  let ordered_declarations =
    (* analyze in reverse order *)
    declarations
    |> List.fast_sort (Decl.compare_using_dependencies ~ordered_files)
  in
  let dead_declarations = ref [] in
  ordered_declarations
  |> List.iter (iter_decl_in_order ~ordered_files ~dead_declarations);
  let sorted_dead_declarations =
    !dead_declarations |> List.fast_sort Decl.compare_for_reporting
  in
  (* XXX *)
  sorted_dead_declarations |> List.iter Decl.report
