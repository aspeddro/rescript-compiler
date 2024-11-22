(* This is the return that's expected when resolving code actions *)
type result = Protocol.code_action list

let stringify_code_actions code_actions =
  Printf.sprintf {|%s|}
    (code_actions |> List.map Protocol.stringify_code_action |> Protocol.array)

let make ~title ~kind ~uri ~new_text ~range =
  let uri = uri |> Uri.from_path |> Uri.to_string in
  {
    Protocol.title;
    code_action_kind = kind;
    edit =
      {
        document_changes =
          [
            TextDocumentEdit
              {
                Protocol.text_document = {version = None; uri};
                edits = [{new_text; range}];
              };
          ];
      };
  }

let make_with_document_changes ~title ~kind ~document_changes =
  {Protocol.title; code_action_kind = kind; edit = {document_changes}}
