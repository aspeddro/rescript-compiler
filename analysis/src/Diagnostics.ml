let document_syntax ~path =
  let get_diagnostics diagnostics =
    diagnostics
    |> List.map (fun diagnostic ->
           let _, startline, startcol =
             Location.get_pos_info (Res_diagnostics.get_start_pos diagnostic)
           in
           let _, endline, endcol =
             Location.get_pos_info (Res_diagnostics.get_end_pos diagnostic)
           in
           Protocol.stringify_diagnostic
             {
               range =
                 {
                   start = {line = startline - 1; character = startcol};
                   end_ = {line = endline - 1; character = endcol};
                 };
               message = Res_diagnostics.explain diagnostic;
               severity = 1;
             })
  in
  if FindFiles.is_implementation path then
    let parse_implementation =
      Res_driver.parsing_engine.parse_implementation ~for_printer:false
        ~filename:path
    in
    get_diagnostics parse_implementation.diagnostics
  else if FindFiles.is_interface path then
    let parse_interface =
      Res_driver.parsing_engine.parse_interface ~for_printer:false
        ~filename:path
    in
    get_diagnostics parse_interface.diagnostics
  else []
