open DeadCommon

let process_signature ~do_values ~do_types (signature : Types.signature) =
  signature
  |> List.iter (fun sig_item ->
         DeadValue.process_signature_item ~do_values ~do_types
           ~module_loc:Location.none
           ~path:[!Common.current_module_name]
           sig_item)

let process_cmt ~cmt_file_path (cmt_infos : Cmt_format.cmt_infos) =
  (match cmt_infos.cmt_annots with
  | Interface signature ->
    ProcessDeadAnnotations.signature signature;
    process_signature ~do_values:true ~do_types:true signature.sig_type
  | Implementation structure ->
    let cmti_exists =
      Sys.file_exists ((cmt_file_path |> Filename.remove_extension) ^ ".cmti")
    in
    ProcessDeadAnnotations.structure ~do_gen_type:(not cmti_exists) structure;
    process_signature ~do_values:true ~do_types:false structure.str_type;
    let do_externals =
      (* This is already handled at the interface level, avoid issues in inconsistent locations
         https://github.com/BuckleScript/syntax/pull/54
         Ideally, the handling should be less location-based, just like other language aspects. *)
      false
    in
    DeadValue.process_structure ~do_types:true ~do_externals
      ~cmt_value_dependencies:cmt_infos.cmt_value_dependencies structure
  | _ -> ());
  DeadType.TypeDependencies.force_delayed_items ();
  DeadType.TypeDependencies.clear ()
