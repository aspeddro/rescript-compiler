(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

open DeadCommon

let check_any_value_binding_with_no_side_effects
    ({vb_pat = {pat_desc}; vb_expr = expr; vb_loc = loc} :
      Typedtree.value_binding) =
  match pat_desc with
  | Tpat_any when (not (SideEffects.check_expr expr)) && not loc.loc_ghost ->
    let name = "_" |> Name.create ~is_interface:false in
    let current_module_path = ModulePath.get_current () in
    let path = current_module_path.path @ [!Common.current_module_name] in
    name
    |> add_value_declaration ~path ~loc ~module_loc:current_module_path.loc
         ~side_effects:false
  | _ -> ()

let collect_value_binding super self (vb : Typedtree.value_binding) =
  let old_current_bindings = !Current.bindings in
  let old_last_binding = !Current.last_binding in
  check_any_value_binding_with_no_side_effects vb;
  let loc =
    match vb.vb_pat.pat_desc with
    | Tpat_var (id, {loc = {loc_start; loc_ghost} as loc})
    | Tpat_alias
        ({pat_desc = Tpat_any}, id, {loc = {loc_start; loc_ghost} as loc})
      when (not loc_ghost) && not vb.vb_loc.loc_ghost ->
      let name = Ident.name id |> Name.create ~is_interface:false in
      let optional_args =
        vb.vb_expr.exp_type |> DeadOptionalArgs.from_type_expr
        |> Common.OptionalArgs.from_list
      in
      let exists =
        match PosHash.find_opt decls loc_start with
        | Some {decl_kind = Value r} ->
          r.optional_args <- optional_args;
          true
        | _ -> false
      in
      let current_module_path = ModulePath.get_current () in
      let path = current_module_path.path @ [!Common.current_module_name] in
      let is_first_class_module =
        match vb.vb_expr.exp_type.desc with
        | Tpackage _ -> true
        | _ -> false
      in
      (if (not exists) && not is_first_class_module then
         (* This is never toplevel currently *)
         let is_toplevel = old_last_binding = Location.none in
         let side_effects = SideEffects.check_expr vb.vb_expr in
         name
         |> add_value_declaration ~is_toplevel ~loc
              ~module_loc:current_module_path.loc ~optional_args ~path ~side_effects);
      (match PosHash.find_opt decls loc_start with
      | None -> ()
      | Some decl ->
        (* Value bindings contain the correct location for the entire declaration: update final position.
           The previous value was taken from the signature, which only has positions for the id. *)
        let decl_kind =
          match decl.decl_kind with
          | Value vk ->
            Common.DeclKind.Value
              {vk with side_effects = SideEffects.check_expr vb.vb_expr}
          | dk -> dk
        in
        PosHash.replace decls loc_start
          {
            decl with
            decl_kind;
            pos_end = vb.vb_loc.loc_end;
            pos_start = vb.vb_loc.loc_start;
          });
      loc
    | _ -> !Current.last_binding
  in
  Current.bindings := PosSet.add loc.loc_start !Current.bindings;
  Current.last_binding := loc;
  let r = super.Tast_mapper.value_binding self vb in
  Current.bindings := old_current_bindings;
  Current.last_binding := old_last_binding;
  r

let process_optional_args ~exp_type ~(loc_from : Location.t) ~loc_to ~path args =
  if exp_type |> DeadOptionalArgs.has_optional_args then (
    let supplied = ref [] in
    let supplied_maybe = ref [] in
    args
    |> List.iter (fun (lbl, arg) ->
           let arg_is_supplied =
             match arg with
             | Some
                 {
                   Typedtree.exp_desc =
                     Texp_construct (_, {cstr_name = "Some"}, _);
                 } ->
               Some true
             | Some
                 {
                   Typedtree.exp_desc =
                     Texp_construct (_, {cstr_name = "None"}, _);
                 } ->
               Some false
             | Some _ -> None
             | None -> Some false
           in
           match lbl with
           | Asttypes.Optional s when not loc_from.loc_ghost ->
             if arg_is_supplied <> Some false then supplied := s :: !supplied;
             if arg_is_supplied = None then supplied_maybe := s :: !supplied_maybe
           | _ -> ());
    (!supplied, !supplied_maybe)
    |> DeadOptionalArgs.add_references ~loc_from ~loc_to ~path)

let rec collect_expr super self (e : Typedtree.expression) =
  let loc_from = e.exp_loc in
  (match e.exp_desc with
  | Texp_ident (_path, _, {Types.val_loc = {loc_ghost = false; _} as loc_to}) ->
    (* if Path.name _path = "rc" then assert false; *)
    if loc_from = loc_to && _path |> Path.name = "emptyArray" then (
      (* Work around lowercase jsx with no children producing an artifact `emptyArray`
         which is called from its own location as many things are generated on the same location. *)
      if !Common.Cli.debug then
        Log_.item "addDummyReference %s --> %s@."
          (Location.none.loc_start |> Common.pos_to_string)
          (loc_to.loc_start |> Common.pos_to_string);
      ValueReferences.add loc_to.loc_start Location.none.loc_start)
    else add_value_reference ~add_file_reference:true ~loc_from ~loc_to
  | Texp_apply
      ( {
          exp_desc =
            Texp_ident
              (path, _, {Types.val_loc = {loc_ghost = false; _} as loc_to});
          exp_type;
        },
        args ) ->
    args
    |> process_optional_args ~exp_type:exp_type
         ~loc_from:(loc_from : Location.t)
         ~loc_to ~path
  | Texp_let
      ( (* generated for functions with optional args *)
        Nonrecursive,
        [
          {
            vb_pat = {pat_desc = Tpat_var (id_arg, _)};
            vb_expr =
              {
                exp_desc =
                  Texp_ident
                    (path, _, {Types.val_loc = {loc_ghost = false; _} as loc_to});
                exp_type;
              };
          };
        ],
        {
          exp_desc =
            Texp_function
              {
                cases =
                  [
                    {
                      c_lhs = {pat_desc = Tpat_var (eta_arg, _)};
                      c_rhs =
                        {
                          exp_desc =
                            Texp_apply
                              ({exp_desc = Texp_ident (id_arg2, _, _)}, args);
                        };
                    };
                  ];
              };
        } )
    when Ident.name id_arg = "arg"
         && Ident.name eta_arg = "eta"
         && Path.name id_arg2 = "arg" ->
    args
    |> process_optional_args ~exp_type:exp_type
         ~loc_from:(loc_from : Location.t)
         ~loc_to ~path
  | Texp_field
      (_, _, {lbl_loc = {Location.loc_start = pos_to; loc_ghost = false}; _}) ->
    if !Config.analyze_types then
      DeadType.add_type_reference ~pos_to ~pos_from:loc_from.loc_start
  | Texp_construct
      ( _,
        {cstr_loc = {Location.loc_start = pos_to; loc_ghost} as loc_to; cstr_tag},
        _ ) ->
    (match cstr_tag with
    | Cstr_extension path -> path |> DeadException.mark_as_used ~loc_from ~loc_to
    | _ -> ());
    if !Config.analyze_types && not loc_ghost then
      DeadType.add_type_reference ~pos_to ~pos_from:loc_from.loc_start
  | Texp_record {fields} ->
    fields
    |> Array.iter (fun (_, record_label_definition) ->
           match record_label_definition with
           | Typedtree.Overridden (_, ({exp_loc} as e)) when exp_loc.loc_ghost
             ->
             (* Punned field in OCaml projects has ghost location in expression *)
             let e = {e with exp_loc = {exp_loc with loc_ghost = false}} in
             collect_expr super self e |> ignore
           | _ -> ())
  | _ -> ());
  super.Tast_mapper.expr self e

(*
  type k. is a locally abstract type
  https://caml.inria.fr/pub/docs/manual-ocaml/locallyabstract.html
  it is required because in ocaml >= 4.11 Typedtree.pattern and ADT is converted
  in a GADT
  https://github.com/ocaml/ocaml/commit/312253ce822c32740349e572498575cf2a82ee96
  in short: all branches of pattern matches aren't the same type.
  With this annotation we declare a new type for each branch to allow the
  function to be typed.
  *)
let collect_pattern : _ -> _ -> Typedtree.pattern -> Typedtree.pattern =
 fun super self pat ->
  let pos_from = pat.Typedtree.pat_loc.loc_start in
  (match pat.pat_desc with
  | Typedtree.Tpat_record (cases, _clodsedFlag) ->
    cases
    |> List.iter (fun (_loc, {Types.lbl_loc = {loc_start = pos_to}}, _pat) ->
           if !Config.analyze_types then
             DeadType.add_type_reference ~pos_from ~pos_to)
  | _ -> ());
  super.Tast_mapper.pat self pat

let rec get_signature (module_type : Types.module_type) =
  match module_type with
  | Mty_signature signature -> signature
  | Mty_functor (_, _mtParam, mt) -> get_signature mt
  | _ -> []

let rec process_signature_item ~do_types ~do_values ~module_loc ~path
    (si : Types.signature_item) =
  let old_module_path = ModulePath.get_current () in
  (match si with
  | Sig_type (id, t, _) when do_types ->
    if !Config.analyze_types then
      DeadType.add_declaration ~type_id:id ~type_kind:t.type_kind
  | Sig_value (id, {Types.val_loc = loc; val_kind = kind; val_type})
    when do_values ->
    if not loc.Location.loc_ghost then
      let is_primitive =
        match kind with
        | Val_prim _ -> true
        | _ -> false
      in
      if (not is_primitive) || !Config.analyze_externals then
        let optional_args =
          val_type |> DeadOptionalArgs.from_type_expr
          |> Common.OptionalArgs.from_list
        in

        (* if Ident.name id = "someValue" then
           Printf.printf "XXX %s\n" (Ident.name id); *)
        Ident.name id
        |> Name.create ~is_interface:false
        |> add_value_declaration ~loc ~module_loc ~optional_args ~path
             ~side_effects:false
  | Sig_module (id, {Types.md_type = module_type; md_loc = module_loc}, _)
  | Sig_modtype (id, {Types.mtd_type = Some module_type; mtd_loc = module_loc}) ->
    ModulePath.set_current
      {
        old_module_path with
        loc = module_loc;
        path = (id |> Ident.name |> Name.create) :: old_module_path.path;
      };
    let collect =
      match si with
      | Sig_modtype _ -> false
      | _ -> true
    in
    if collect then
      get_signature module_type
      |> List.iter
           (process_signature_item ~do_types ~do_values ~module_loc
              ~path:((id |> Ident.name |> Name.create) :: path))
  | _ -> ());
  ModulePath.set_current old_module_path

(* Traverse the AST *)
let traverse_structure ~do_types ~do_externals =
  let super = Tast_mapper.default in
  let expr self e = e |> collect_expr super self in
  let pat self p = p |> collect_pattern super self in
  let value_binding self vb = vb |> collect_value_binding super self in
  let structure_item self (structure_item : Typedtree.structure_item) =
    let old_module_path = ModulePath.get_current () in
    (match structure_item.str_desc with
    | Tstr_module {mb_expr; mb_id; mb_loc} -> (
      let has_interface =
        match mb_expr.mod_desc with
        | Tmod_constraint _ -> true
        | _ -> false
      in
      ModulePath.set_current
        {
          old_module_path with
          loc = mb_loc;
          path = (mb_id |> Ident.name |> Name.create) :: old_module_path.path;
        };
      if has_interface then
        match mb_expr.mod_type with
        | Mty_signature signature ->
          signature
          |> List.iter
               (process_signature_item ~do_types ~do_values:false
                  ~module_loc:mb_expr.mod_loc
                  ~path:
                    ((ModulePath.get_current ()).path
                    @ [!Common.current_module_name]))
        | _ -> ())
    | Tstr_primitive vd when do_externals && !Config.analyze_externals ->
      let current_module_path = ModulePath.get_current () in
      let path = current_module_path.path @ [!Common.current_module_name] in
      let exists =
        match PosHash.find_opt decls vd.val_loc.loc_start with
        | Some {decl_kind = Value _} -> true
        | _ -> false
      in
      let id = vd.val_id |> Ident.name in
      Printf.printf "Primitive %s\n" id;
      if
        (not exists) && id <> "unsafe_expr"
        (* see https://github.com/BuckleScript/bucklescript/issues/4532 *)
      then
        id
        |> Name.create ~is_interface:false
        |> add_value_declaration ~path ~loc:vd.val_loc
             ~module_loc:current_module_path.loc ~side_effects:false
    | Tstr_type (_recFlag, type_declarations) when do_types ->
      if !Config.analyze_types then
        type_declarations
        |> List.iter (fun (type_declaration : Typedtree.type_declaration) ->
               DeadType.add_declaration ~type_id:type_declaration.typ_id
                 ~type_kind:type_declaration.typ_type.type_kind)
    | Tstr_include {incl_mod; incl_type} -> (
      match incl_mod.mod_desc with
      | Tmod_ident (_path, _lid) ->
        let current_path =
          (ModulePath.get_current ()).path @ [!Common.current_module_name]
        in
        incl_type
        |> List.iter
             (process_signature_item ~do_types
                ~do_values:false (* TODO: also values? *)
                ~module_loc:incl_mod.mod_loc ~path:current_path)
      | _ -> ())
    | Tstr_exception {ext_id = id; ext_loc = loc} ->
      let path =
        (ModulePath.get_current ()).path @ [!Common.current_module_name]
      in
      let name = id |> Ident.name |> Name.create in
      name |> DeadException.add ~path ~loc ~str_loc:structure_item.str_loc
    | _ -> ());
    let result = super.structure_item self structure_item in
    ModulePath.set_current old_module_path;
    result
  in
  {super with expr; pat; structure_item; value_binding}

(* Merge a location's references to another one's *)
let process_value_dependency
    ( ({
         val_loc =
           {loc_start = {pos_fname = fn_to} as pos_to; loc_ghost = ghost1} as
           loc_to;
       } :
        Types.value_description),
      ({
         val_loc =
           {loc_start = {pos_fname = fn_from} as pos_from; loc_ghost = ghost2} as
           loc_from;
       } :
        Types.value_description) ) =
  if (not ghost1) && (not ghost2) && pos_to <> pos_from then (
    let add_file_reference = file_is_implementation_of fn_to fn_from in
    add_value_reference ~add_file_reference ~loc_from ~loc_to;
    DeadOptionalArgs.add_function_reference ~loc_from ~loc_to)

let process_structure ~cmt_value_dependencies ~do_types ~do_externals
    (structure : Typedtree.structure) =
  let traverse_structure = traverse_structure ~do_types ~do_externals in
  structure |> traverse_structure.structure traverse_structure |> ignore;
  let value_dependencies = cmt_value_dependencies |> List.rev in
  value_dependencies |> List.iter process_value_dependency
