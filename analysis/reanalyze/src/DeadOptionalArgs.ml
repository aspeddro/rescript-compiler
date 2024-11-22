open DeadCommon
open Common

let active () = true

type item = {
  pos_to: Lexing.position;
  arg_names: string list;
  arg_names_maybe: string list;
}

let delayed_items = (ref [] : item list ref)
let function_references = (ref [] : (Lexing.position * Lexing.position) list ref)

let add_function_reference ~(loc_from : Location.t) ~(loc_to : Location.t) =
  if active () then
    let pos_to = loc_to.loc_start in
    let pos_from = loc_from.loc_start in
    let should_add =
      match PosHash.find_opt decls pos_to with
      | Some {decl_kind = Value {optional_args}} ->
        not (OptionalArgs.is_empty optional_args)
      | _ -> false
    in
    if should_add then (
      if !Common.Cli.debug then
        Log_.item "OptionalArgs.addFunctionReference %s %s@."
          (pos_from |> pos_to_string) (pos_to |> pos_to_string);
      function_references := (pos_from, pos_to) :: !function_references)

let rec has_optional_args (texpr : Types.type_expr) =
  match texpr.desc with
  | _ when not (active ()) -> false
  | Tarrow (Optional _, _tFrom, _tTo, _) -> true
  | Tarrow (_, _tFrom, t_to, _) -> has_optional_args t_to
  | Tlink t -> has_optional_args t
  | Tsubst t -> has_optional_args t
  | _ -> false

let rec from_type_expr (texpr : Types.type_expr) =
  match texpr.desc with
  | _ when not (active ()) -> []
  | Tarrow (Optional s, _tFrom, t_to, _) -> s :: from_type_expr t_to
  | Tarrow (_, _tFrom, t_to, _) -> from_type_expr t_to
  | Tlink t -> from_type_expr t
  | Tsubst t -> from_type_expr t
  | _ -> []

let add_references ~(loc_from : Location.t) ~(loc_to : Location.t) ~path
    (arg_names, arg_names_maybe) =
  if active () then (
    let pos_to = loc_to.loc_start in
    let pos_from = loc_from.loc_start in
    delayed_items := {pos_to; arg_names; arg_names_maybe} :: !delayed_items;
    if !Common.Cli.debug then
      Log_.item
        "DeadOptionalArgs.addReferences %s called with optional argNames:%s \
         argNamesMaybe:%s %s@."
        (path |> Path.from_path_t |> Path.to_string)
        (arg_names |> String.concat ", ")
        (arg_names_maybe |> String.concat ", ")
        (pos_from |> pos_to_string))

let force_delayed_items () =
  let items = !delayed_items |> List.rev in
  delayed_items := [];
  items
  |> List.iter (fun {pos_to; arg_names; arg_names_maybe} ->
         match PosHash.find_opt decls pos_to with
         | Some {decl_kind = Value r} ->
           r.optional_args |> OptionalArgs.call ~arg_names ~arg_names_maybe
         | _ -> ());
  let f_refs = !function_references |> List.rev in
  function_references := [];
  f_refs
  |> List.iter (fun (pos_from, pos_to) ->
         match
           (PosHash.find_opt decls pos_from, PosHash.find_opt decls pos_to)
         with
         | Some {decl_kind = Value r_from}, Some {decl_kind = Value r_to} ->
           OptionalArgs.combine r_from.optional_args r_to.optional_args
         | _ -> ())

let check decl =
  match decl with
  | {decl_kind = Value {optional_args}}
    when active ()
         && not (ProcessDeadAnnotations.is_annotated_gen_type_or_live decl.pos) ->
    optional_args
    |> OptionalArgs.iter_unused (fun s ->
           Log_.warning ~loc:(decl |> decl_get_loc)
             (DeadOptional
                {
                  dead_optional = WarningUnusedArgument;
                  message =
                    Format.asprintf
                      "optional argument @{<info>%s@} of function @{<info>%s@} \
                       is never used"
                      s
                      (decl.path |> Path.without_head);
                }));
    optional_args
    |> OptionalArgs.iter_always_used (fun s n_calls ->
           Log_.warning ~loc:(decl |> decl_get_loc)
             (DeadOptional
                {
                  dead_optional = WarningRedundantOptionalArgument;
                  message =
                    Format.asprintf
                      "optional argument @{<info>%s@} of function @{<info>%s@} \
                       is always supplied (%d calls)"
                      s
                      (decl.path |> Path.without_head)
                      n_calls;
                }))
  | _ -> ()
