let pos_to_string = Common.pos_to_string

module LocSet = Common.LocSet

module Values = struct
  let value_bindings_table =
    (Hashtbl.create 15 : (string, (Name.t, Exceptions.t) Hashtbl.t) Hashtbl.t)

  let current_file_table = ref (Hashtbl.create 1)

  let add ~name exceptions =
    let path = (name |> Name.create) :: (ModulePath.get_current ()).path in
    Hashtbl.replace !current_file_table (path |> Common.Path.to_name) exceptions

  let get_from_module ~module_name ~module_path (path_ : Common.Path.t) =
    let name = path_ @ module_path |> Common.Path.to_name in
    match
      Hashtbl.find_opt value_bindings_table
        (String.capitalize_ascii module_name)
    with
    | Some tbl -> Hashtbl.find_opt tbl name
    | None -> (
      match
        Hashtbl.find_opt value_bindings_table
          (String.uncapitalize_ascii module_name)
      with
      | Some tbl -> Hashtbl.find_opt tbl name
      | None -> None)

  let rec find_local ~module_name ~module_path path =
    match path |> get_from_module ~module_name ~module_path with
    | Some exceptions -> Some exceptions
    | None -> (
      match module_path with
      | [] -> None
      | _ :: rest_module_path ->
        path |> find_local ~module_name ~module_path:rest_module_path)

  let find_path ~module_name ~module_path path =
    let find_external ~external_module_name ~path_rev =
      path_rev |> List.rev
      |> get_from_module
           ~module_name:(external_module_name |> Name.to_string)
           ~module_path:[]
    in
    match path |> find_local ~module_name ~module_path with
    | None -> (
      (* Search in another file *)
      match path |> List.rev with
      | external_module_name :: path_rev -> (
        match (find_external ~external_module_name ~path_rev, path_rev) with
        | (Some _ as found), _ -> found
        | None, external_module_name2 :: path_rev2
          when !Common.Cli.cmt_command && path_rev2 <> [] ->
          (* Simplistic namespace resolution for dune namespace: skip the root of the path *)
          find_external ~external_module_name:external_module_name2
            ~path_rev:path_rev2
        | None, _ -> None)
      | [] -> None)
    | Some exceptions -> Some exceptions

  let new_cmt () =
    current_file_table := Hashtbl.create 15;
    Hashtbl.replace value_bindings_table !Common.current_module
      !current_file_table
end

module Event = struct
  type kind =
    | Catches of t list (* with | E => ... *)
    | Call of {callee: Common.Path.t; module_path: Common.Path.t} (* foo() *)
    | DoesNotRaise of
        t list (* DoesNotRaise(events) where events come from an expression *)
    | Raises  (** raise E *)

  and t = {exceptions: Exceptions.t; kind: kind; loc: Location.t}

  let rec print ppf event =
    match event with
    | {kind = Call {callee; module_path}; exceptions; loc} ->
      Format.fprintf ppf "%s Call(%s, modulePath:%s) %a@."
        (loc.loc_start |> pos_to_string)
        (callee |> Common.Path.to_string)
        (module_path |> Common.Path.to_string)
        (Exceptions.pp ~exn_table:None)
        exceptions
    | {kind = DoesNotRaise nested_events; loc} ->
      Format.fprintf ppf "%s DoesNotRaise(%a)@."
        (loc.loc_start |> pos_to_string)
        (fun ppf () ->
          nested_events |> List.iter (fun e -> Format.fprintf ppf "%a " print e))
        ()
    | {kind = Raises; exceptions; loc} ->
      Format.fprintf ppf "%s raises %a@."
        (loc.loc_start |> pos_to_string)
        (Exceptions.pp ~exn_table:None)
        exceptions
    | {kind = Catches nested_events; exceptions; loc} ->
      Format.fprintf ppf "%s Catches exceptions:%a nestedEvents:%a@."
        (loc.loc_start |> pos_to_string)
        (Exceptions.pp ~exn_table:None)
        exceptions
        (fun ppf () ->
          nested_events |> List.iter (fun e -> Format.fprintf ppf "%a " print e))
        ()

  let combine ~module_name events =
    if !Common.Cli.debug then (
      Log_.item "@.";
      Log_.item "Events combine: #events %d@." (events |> List.length));
    let exn_table = Hashtbl.create 1 in
    let extend_exn_table exn loc =
      match Hashtbl.find_opt exn_table exn with
      | Some loc_set -> Hashtbl.replace exn_table exn (LocSet.add loc loc_set)
      | None -> Hashtbl.replace exn_table exn (LocSet.add loc LocSet.empty)
    in
    let shrink_exn_table exn loc =
      match Hashtbl.find_opt exn_table exn with
      | Some loc_set ->
        Hashtbl.replace exn_table exn (LocSet.remove loc loc_set)
      | None -> ()
    in
    let rec loop exn_set events =
      match events with
      | ({kind = Raises; exceptions; loc} as ev) :: rest ->
        if !Common.Cli.debug then Log_.item "%a@." print ev;
        exceptions |> Exceptions.iter (fun exn -> extend_exn_table exn loc);
        loop (Exceptions.union exn_set exceptions) rest
      | ({kind = Call {callee; module_path}; loc} as ev) :: rest ->
        if !Common.Cli.debug then Log_.item "%a@." print ev;
        let exceptions =
          match callee |> Values.find_path ~module_name ~module_path with
          | Some exceptions -> exceptions
          | _ -> (
            match ExnLib.find callee with
            | Some exceptions -> exceptions
            | None -> Exceptions.empty)
        in
        exceptions |> Exceptions.iter (fun exn -> extend_exn_table exn loc);
        loop (Exceptions.union exn_set exceptions) rest
      | ({kind = DoesNotRaise nested_events; loc} as ev) :: rest ->
        if !Common.Cli.debug then Log_.item "%a@." print ev;
        let nested_exceptions = loop Exceptions.empty nested_events in
        (if Exceptions.is_empty nested_exceptions (* catch-all *) then
           let name =
             match nested_events with
             | {kind = Call {callee}} :: _ -> callee |> Common.Path.to_name
             | _ -> "expression" |> Name.create
           in
           Log_.warning ~loc
             (Common.ExceptionAnalysis
                {
                  message =
                    Format.asprintf
                      "@{<info>%s@} does not raise and is annotated with \
                       redundant @doesNotRaise"
                      (name |> Name.to_string);
                }));
        loop exn_set rest
      | ({kind = Catches nested_events; exceptions} as ev) :: rest ->
        if !Common.Cli.debug then Log_.item "%a@." print ev;
        if Exceptions.is_empty exceptions then loop exn_set rest
        else
          let nested_exceptions = loop Exceptions.empty nested_events in
          let new_raises = Exceptions.diff nested_exceptions exceptions in
          exceptions
          |> Exceptions.iter (fun exn ->
                 nested_events
                 |> List.iter (fun event -> shrink_exn_table exn event.loc));
          loop (Exceptions.union exn_set new_raises) rest
      | [] -> exn_set
    in
    let exn_set = loop Exceptions.empty events in
    (exn_set, exn_table)
end

module Checks = struct
  type check = {
    events: Event.t list;
    loc: Location.t;
    loc_full: Location.t;
    module_name: string;
    exn_name: string;
    exceptions: Exceptions.t;
  }

  type t = check list

  let checks = (ref [] : t ref)

  let add ~events ~exceptions ~loc ?(loc_full = loc) ~module_name exn_name =
    checks :=
      {events; exceptions; loc; loc_full; module_name; exn_name} :: !checks

  let do_check {events; exceptions; loc; loc_full; module_name; exn_name} =
    let raise_set, exn_table = events |> Event.combine ~module_name in
    let missing_annotations = Exceptions.diff raise_set exceptions in
    let redundant_annotations = Exceptions.diff exceptions raise_set in
    (if not (Exceptions.is_empty missing_annotations) then
       let description =
         Common.ExceptionAnalysisMissing
           {exn_name; exn_table; raise_set; missing_annotations; loc_full}
       in
       Log_.warning ~loc description);
    if not (Exceptions.is_empty redundant_annotations) then
      Log_.warning ~loc
        (Common.ExceptionAnalysis
           {
             message =
               (let raises_description ppf () =
                  if raise_set |> Exceptions.is_empty then
                    Format.fprintf ppf "raises nothing"
                  else
                    Format.fprintf ppf "might raise %a"
                      (Exceptions.pp ~exn_table:(Some exn_table))
                      raise_set
                in
                Format.asprintf
                  "@{<info>%s@} %a and is annotated with redundant @raises(%a)"
                  exn_name raises_description ()
                  (Exceptions.pp ~exn_table:None)
                  redundant_annotations);
           })

  let do_checks () = !checks |> List.rev |> List.iter do_check
end

let traverse_ast () =
  ModulePath.init ();
  let super = Tast_mapper.default in
  let current_id = ref "" in
  let current_events = ref [] in
  let exceptions_of_patterns patterns =
    patterns
    |> List.fold_left
         (fun acc desc ->
           match desc with
           | Typedtree.Tpat_construct ({txt}, _, _) ->
             Exceptions.add (Exn.from_lid txt) acc
           | _ -> acc)
         Exceptions.empty
  in
  let iter_expr self e = self.Tast_mapper.expr self e |> ignore in
  let iter_expr_opt self eo =
    match eo with
    | None -> ()
    | Some e -> e |> iter_expr self
  in
  let iter_pat self p = self.Tast_mapper.pat self p |> ignore in
  let iter_cases self cases =
    cases
    |> List.iter (fun case ->
           case.Typedtree.c_lhs |> iter_pat self;
           case.c_guard |> iter_expr_opt self;
           case.c_rhs |> iter_expr self)
  in
  let is_raise s = s = "Pervasives.raise" || s = "Pervasives.raise_notrace" in
  let raise_args args =
    match args with
    | [(_, Some {Typedtree.exp_desc = Texp_construct ({txt}, _, _)})] ->
      [Exn.from_lid txt] |> Exceptions.from_list
    | [(_, Some {Typedtree.exp_desc = Texp_ident _})] ->
      [Exn.from_string "genericException"] |> Exceptions.from_list
    | _ -> [Exn.from_string "TODO_from_raise1"] |> Exceptions.from_list
  in
  let does_not_raise attributes =
    attributes
    |> Annotation.get_attribute_payload (fun s ->
           s = "doesNotRaise" || s = "doesnotraise" || s = "DoesNoRaise"
           || s = "doesNotraise" || s = "doNotRaise" || s = "donotraise"
           || s = "DoNoRaise" || s = "doNotraise")
    <> None
  in
  let expr (self : Tast_mapper.mapper) (expr : Typedtree.expression) =
    let loc = expr.exp_loc in
    let is_does_no_raise = expr.exp_attributes |> does_not_raise in
    let old_events = !current_events in
    if is_does_no_raise then current_events := [];
    (match expr.exp_desc with
    | Texp_ident (callee_, _, _) ->
      let callee =
        callee_ |> Common.Path.from_path_t |> ModulePath.resolve_alias
      in
      let callee_name = callee |> Common.Path.to_name in
      if callee_name |> Name.to_string |> is_raise then
        Log_.warning ~loc
          (Common.ExceptionAnalysis
             {
               message =
                 Format.asprintf
                   "@{<info>%s@} can be analyzed only if called directly"
                   (callee_name |> Name.to_string);
             });
      current_events :=
        {
          Event.exceptions = Exceptions.empty;
          loc;
          kind = Call {callee; module_path = (ModulePath.get_current ()).path};
        }
        :: !current_events
    | Texp_apply
        ( {exp_desc = Texp_ident (atat, _, _)},
          [(_lbl1, Some {exp_desc = Texp_ident (callee, _, _)}); arg] )
      when (* raise @@ Exn(...) *)
           atat |> Path.name = "Pervasives.@@"
           && callee |> Path.name |> is_raise ->
      let exceptions = [arg] |> raise_args in
      current_events :=
        {Event.exceptions; loc; kind = Raises} :: !current_events;
      arg |> snd |> iter_expr_opt self
    | Texp_apply
        ( {exp_desc = Texp_ident (atat, _, _)},
          [arg; (_lbl1, Some {exp_desc = Texp_ident (callee, _, _)})] )
      when (*  Exn(...) |> raise *)
           atat |> Path.name = "Pervasives.|>"
           && callee |> Path.name |> is_raise ->
      let exceptions = [arg] |> raise_args in
      current_events :=
        {Event.exceptions; loc; kind = Raises} :: !current_events;
      arg |> snd |> iter_expr_opt self
    | Texp_apply (({exp_desc = Texp_ident (callee, _, _)} as e), args) ->
      let callee_name = Path.name callee in
      if callee_name |> is_raise then
        let exceptions = args |> raise_args in
        current_events :=
          {Event.exceptions; loc; kind = Raises} :: !current_events
      else e |> iter_expr self;
      args |> List.iter (fun (_, e_opt) -> e_opt |> iter_expr_opt self)
    | Texp_match (e, cases_ok, cases_exn, partial) ->
      let cases = cases_ok @ cases_exn in
      let exception_patterns =
        cases_exn
        |> List.map (fun (case : Typedtree.case) -> case.c_lhs.pat_desc)
      in
      let exceptions = exception_patterns |> exceptions_of_patterns in
      if exception_patterns <> [] then (
        let old_events = !current_events in
        current_events := [];
        e |> iter_expr self;
        current_events :=
          {Event.exceptions; loc; kind = Catches !current_events} :: old_events)
      else e |> iter_expr self;
      cases |> iter_cases self;
      if partial = Partial then
        current_events :=
          {
            Event.exceptions = [Exn.match_failure] |> Exceptions.from_list;
            loc;
            kind = Raises;
          }
          :: !current_events
    | Texp_try (e, cases) ->
      let exceptions =
        cases
        |> List.map (fun case -> case.Typedtree.c_lhs.pat_desc)
        |> exceptions_of_patterns
      in
      let old_events = !current_events in
      current_events := [];
      e |> iter_expr self;
      current_events :=
        {Event.exceptions; loc; kind = Catches !current_events} :: old_events;
      cases |> iter_cases self
    | _ -> super.expr self expr |> ignore);
    (if is_does_no_raise then
       let nested_events = !current_events in
       current_events :=
         {
           Event.exceptions = Exceptions.empty;
           loc;
           kind = DoesNotRaise nested_events;
         }
         :: old_events);
    expr
  in
  let get_exceptions_from_annotations attributes =
    let raises_annotation_payload =
      attributes
      |> Annotation.get_attribute_payload (fun s -> s = "raises" || s = "raise")
    in
    let rec get_exceptions payload =
      match payload with
      | Annotation.StringPayload s ->
        [Exn.from_string s] |> Exceptions.from_list
      | Annotation.ConstructPayload s when s <> "::" ->
        [Exn.from_string s] |> Exceptions.from_list
      | Annotation.IdentPayload s ->
        [Exn.from_string (s |> Longident.flatten |> String.concat ".")]
        |> Exceptions.from_list
      | Annotation.TuplePayload tuple ->
        tuple
        |> List.map (fun payload ->
               payload |> get_exceptions |> Exceptions.to_list)
        |> List.concat |> Exceptions.from_list
      | _ -> Exceptions.empty
    in
    match raises_annotation_payload with
    | None -> Exceptions.empty
    | Some payload -> payload |> get_exceptions
  in
  let toplevel_eval (self : Tast_mapper.mapper) (expr : Typedtree.expression)
      attributes =
    let old_id = !current_id in
    let old_events = !current_events in
    let name = "Toplevel expression" in
    current_id := name;
    current_events := [];
    let module_name = !Common.current_module in
    self.expr self expr |> ignore;
    Checks.add ~events:!current_events
      ~exceptions:(get_exceptions_from_annotations attributes)
      ~loc:expr.exp_loc ~module_name name;
    current_id := old_id;
    current_events := old_events
  in
  let structure_item (self : Tast_mapper.mapper)
      (structure_item : Typedtree.structure_item) =
    let old_module_path = ModulePath.get_current () in
    (match structure_item.str_desc with
    | Tstr_eval (expr, attributes) -> toplevel_eval self expr attributes
    | Tstr_module {mb_id; mb_loc} ->
      ModulePath.set_current
        {
          old_module_path with
          loc = mb_loc;
          path = (mb_id |> Ident.name |> Name.create) :: old_module_path.path;
        }
    | _ -> ());
    let result = super.structure_item self structure_item in
    ModulePath.set_current old_module_path;
    (match structure_item.str_desc with
    | Tstr_module {mb_id; mb_expr = {mod_desc = Tmod_ident (path_, _lid)}} ->
      ModulePath.add_alias
        ~name:(mb_id |> Ident.name |> Name.create)
        ~path:(path_ |> Common.Path.from_path_t)
    | _ -> ());
    result
  in
  let value_binding (self : Tast_mapper.mapper) (vb : Typedtree.value_binding) =
    let old_id = !current_id in
    let old_events = !current_events in
    let is_function =
      match vb.vb_expr.exp_desc with
      | Texp_function _ -> true
      | _ -> false
    in
    let is_toplevel = !current_id = "" in
    let process_binding name =
      current_id := name;
      current_events := [];
      let exceptions_from_annotations =
        get_exceptions_from_annotations vb.vb_attributes
      in
      exceptions_from_annotations |> Values.add ~name;
      let res = super.value_binding self vb in
      let module_name = !Common.current_module in
      let path = [name |> Name.create] in
      let exceptions =
        match
          path
          |> Values.find_path ~module_name
               ~module_path:(ModulePath.get_current ()).path
        with
        | Some exceptions -> exceptions
        | _ -> Exceptions.empty
      in
      Checks.add ~events:!current_events ~exceptions ~loc:vb.vb_pat.pat_loc
        ~loc_full:vb.vb_loc ~module_name name;
      current_id := old_id;
      current_events := old_events;
      res
    in
    match vb.vb_pat.pat_desc with
    | Tpat_any when is_toplevel && not vb.vb_loc.loc_ghost ->
      process_binding "_"
    | Tpat_construct ({txt}, _, _)
      when is_toplevel && (not vb.vb_loc.loc_ghost)
           && txt = Longident.Lident "()" ->
      process_binding "()"
    | Tpat_var (id, {loc = {loc_ghost}})
      when (is_function || is_toplevel)
           && (not loc_ghost) && not vb.vb_loc.loc_ghost ->
      process_binding (id |> Ident.name)
    | _ -> super.value_binding self vb
  in
  let open Tast_mapper in
  {super with expr; value_binding; structure_item}

let process_structure (structure : Typedtree.structure) =
  let traverse_ast = traverse_ast () in
  structure |> traverse_ast.structure traverse_ast |> ignore

let process_cmt (cmt_infos : Cmt_format.cmt_infos) =
  match cmt_infos.cmt_annots with
  | Interface _ -> ()
  | Implementation structure ->
    Values.new_cmt ();
    structure |> process_structure
  | _ -> ()
