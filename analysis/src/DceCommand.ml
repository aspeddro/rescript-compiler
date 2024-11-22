let command () =
  Reanalyze.RunConfig.dce ();
  Reanalyze.run_analysis ~cmt_root:None;
  let issues = !Reanalyze.Log_.Stats.issues in
  Printf.printf "issues:%d\n" (List.length issues)
