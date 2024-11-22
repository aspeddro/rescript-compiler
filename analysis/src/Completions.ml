let get_completions ~debug ~path ~pos ~current_file ~for_hover =
  let text_opt = Files.read_file current_file in
  match text_opt with
  | None | Some "" -> None
  | Some text -> (
    match
      CompletionFrontEnd.completion_with_parser ~debug ~path ~pos_cursor:pos
        ~current_file ~text
    with
    | None -> None
    | Some (completable, scope) -> (
      (* Only perform expensive ast operations if there are completables *)
      match Cmt.load_full_cmt_from_path ~path with
      | None -> None
      | Some full ->
        let env = SharedTypes.QueryEnv.from_file full.file in
        let completables =
          completable
          |> CompletionBackEnd.process_completable ~debug ~full ~pos ~scope ~env
               ~for_hover
        in
        Some (completables, full, scope)))
