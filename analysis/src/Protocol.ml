type position = {line: int; character: int}
type range = {start: position; end_: position}
type markup_content = {kind: string; value: string}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#command *)
type command = {title: string; command: string}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeLens *)
type code_lens = {range: range; command: command option}

type inlay_hint = {
  position: position;
  label: string;
  kind: int;
  padding_left: bool;
  padding_right: bool;
}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#parameterInformation *)
type parameter_information = {label: int * int; documentation: markup_content}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#signatureInformation *)
type signature_information = {
  label: string;
  parameters: parameter_information list;
  documentation: markup_content option;
}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#signatureHelp *)
type signature_help = {
  signatures: signature_information list;
  active_signature: int option;
  active_parameter: int option;
}

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#insertTextFormat *)
type insert_text_format = Snippet

let insert_text_format_to_int f =
  match f with
  | Snippet -> 2

type completion_item = {
  label: string;
  kind: int;
  tags: int list;
  detail: string;
  sort_text: string option;
  filter_text: string option;
  insert_text_format: insert_text_format option;
  insert_text: string option;
  documentation: markup_content option;
  data: (string * string) list option;
}

type location = {uri: string; range: range}
type document_symbol_item = {
  name: string;
  kind: int;
  range: range;
  children: document_symbol_item list;
}
type rename_file = {old_uri: string; new_uri: string}
type text_edit = {range: range; new_text: string}

type diagnostic = {range: range; message: string; severity: int}

type optional_versioned_text_document_identifier = {
  version: int option;
  uri: string;
}

type text_document_edit = {
  text_document: optional_versioned_text_document_identifier;
  edits: text_edit list;
}

type create_file_options = {overwrite: bool option; ignore_if_exists: bool option}
type create_file = {uri: string; options: create_file_options option}

type document_change =
  | TextDocumentEdit of text_document_edit
  | CreateFile of create_file

type code_action_edit = {document_changes: document_change list}
type code_action_kind = RefactorRewrite

type code_action = {
  title: string;
  code_action_kind: code_action_kind;
  edit: code_action_edit;
}

let wrap_in_quotes s = "\"" ^ Json.escape s ^ "\""

let null = "null"
let array l = "[" ^ String.concat ", " l ^ "]"

let stringify_position p =
  Printf.sprintf {|{"line": %i, "character": %i}|} p.line p.character

let stringify_range r =
  Printf.sprintf {|{"start": %s, "end": %s}|}
    (stringify_position r.start)
    (stringify_position r.end_)

let stringify_markup_content (m : markup_content) =
  Printf.sprintf {|{"kind": %s, "value": %s}|} (wrap_in_quotes m.kind)
    (wrap_in_quotes m.value)

(** None values are not emitted in the output. *)
let stringify_object ?(start_on_newline = false) ?(indentation = 1) properties =
  let indentation_str = String.make (indentation * 2) ' ' in
  (if start_on_newline then "\n" ^ indentation_str else "")
  ^ {|{
|}
  ^ (properties
    |> List.filter_map (fun (key, value) ->
           match value with
           | None -> None
           | Some v ->
             Some (Printf.sprintf {|%s  "%s": %s|} indentation_str key v))
    |> String.concat ",\n")
  ^ "\n" ^ indentation_str ^ "}"

let opt_wrap_in_quotes s =
  match s with
  | None -> None
  | Some s -> Some (wrap_in_quotes s)

let stringify_completion_item c =
  stringify_object
    [
      ("label", Some (wrap_in_quotes c.label));
      ("kind", Some (string_of_int c.kind));
      ("tags", Some (c.tags |> List.map string_of_int |> array));
      ("detail", Some (wrap_in_quotes c.detail));
      ( "documentation",
        Some
          (match c.documentation with
          | None -> null
          | Some doc -> stringify_markup_content doc) );
      ("sortText", opt_wrap_in_quotes c.sort_text);
      ("filterText", opt_wrap_in_quotes c.filter_text);
      ("insertText", opt_wrap_in_quotes c.insert_text);
      ( "insertTextFormat",
        match c.insert_text_format with
        | None -> None
        | Some insert_text_format ->
          Some (Printf.sprintf "%i" (insert_text_format_to_int insert_text_format)) );
      ( "data",
        match c.data with
        | None -> None
        | Some fields ->
          Some
            (fields
            |> List.map (fun (key, value) -> (key, Some (wrap_in_quotes value)))
            |> stringify_object ~indentation:2) );
    ]

let stringify_hover value =
  Printf.sprintf {|{"contents": %s}|}
    (stringify_markup_content {kind = "markdown"; value})

let stringify_location (h : location) =
  Printf.sprintf {|{"uri": %s, "range": %s}|} (wrap_in_quotes h.uri)
    (stringify_range h.range)

let stringify_document_symbol_items items =
  let buf = Buffer.create 10 in
  let stringify_name name = Printf.sprintf "\"%s\"" (Json.escape name) in
  let stringify_kind kind = string_of_int kind in
  let emit_str = Buffer.add_string buf in
  let emit_sep () = emit_str ",\n" in
  let rec emit_item ~indent item =
    let open_brace = Printf.sprintf "%s{\n" indent in
    let close_brace = Printf.sprintf "\n%s}" indent in
    let indent = indent ^ "  " in
    let emit_field name s =
      emit_str (Printf.sprintf "%s\"%s\": %s" indent name s)
    in
    emit_str open_brace;
    emit_field "name" (stringify_name item.name);
    emit_sep ();
    emit_field "kind" (stringify_kind item.kind);
    emit_sep ();
    emit_field "range" (stringify_range item.range);
    emit_sep ();
    emit_field "selectionRange" (stringify_range item.range);
    if item.children <> [] then (
      emit_sep ();
      emit_field "children" "[\n";
      emit_body ~indent (List.rev item.children);
      emit_str "]");
    emit_str close_brace
  and emit_body ~indent items =
    match items with
    | [] -> ()
    | item :: rest ->
      emit_item ~indent item;
      if rest <> [] then emit_sep ();
      emit_body ~indent rest
  in
  let indent = "" in
  emit_str "[\n";
  emit_body ~indent (List.rev items);
  emit_str "\n]";
  Buffer.contents buf

let stringify_rename_file {old_uri; new_uri} =
  Printf.sprintf {|{
  "kind": "rename",
  "oldUri": %s,
  "newUri": %s
}|}
    (wrap_in_quotes old_uri) (wrap_in_quotes new_uri)

let stringify_text_edit (te : text_edit) =
  Printf.sprintf {|{
  "range": %s,
  "newText": %s
  }|}
    (stringify_range te.range) (wrap_in_quotes te.new_text)

let stringifyoptional_versioned_text_document_identifier td =
  Printf.sprintf {|{
  "version": %s,
  "uri": %s
  }|}
    (match td.version with
    | None -> null
    | Some v -> string_of_int v)
    (wrap_in_quotes td.uri)

let stringify_text_document_edit tde =
  Printf.sprintf {|{
  "textDocument": %s,
  "edits": %s
  }|}
    (stringifyoptional_versioned_text_document_identifier tde.text_document)
    (tde.edits |> List.map stringify_text_edit |> array)

let stringify_create_file cf =
  stringify_object
    [
      ("kind", Some (wrap_in_quotes "create"));
      ("uri", Some (wrap_in_quotes cf.uri));
      ( "options",
        match cf.options with
        | None -> None
        | Some options ->
          Some
            (stringify_object
               [
                 ( "overwrite",
                   match options.overwrite with
                   | None -> None
                   | Some ov -> Some (string_of_bool ov) );
                 ( "ignoreIfExists",
                   match options.ignore_if_exists with
                   | None -> None
                   | Some i -> Some (string_of_bool i) );
               ]) );
    ]

let stringify_document_change dc =
  match dc with
  | TextDocumentEdit tde -> stringify_text_document_edit tde
  | CreateFile cf -> stringify_create_file cf

let code_action_kind_to_string kind =
  match kind with
  | RefactorRewrite -> "refactor.rewrite"

let stringify_code_action_edit cae =
  Printf.sprintf {|{"documentChanges": %s}|}
    (cae.document_changes |> List.map stringify_document_change |> array)

let stringify_code_action ca =
  Printf.sprintf {|{"title": %s, "kind": %s, "edit": %s}|}
    (wrap_in_quotes ca.title)
    (wrap_in_quotes (code_action_kind_to_string ca.code_action_kind))
    (ca.edit |> stringify_code_action_edit)

let stringify_hint hint =
  Printf.sprintf
    {|{
    "position": %s,
    "label": %s,
    "kind": %i,
    "paddingLeft": %b,
    "paddingRight": %b
}|}
    (stringify_position hint.position)
    (wrap_in_quotes hint.label) hint.kind hint.padding_left hint.padding_right

let stringify_command (command : command) =
  Printf.sprintf {|{"title": %s, "command": %s}|}
    (wrap_in_quotes command.title)
    (wrap_in_quotes command.command)

let stringify_code_lens (code_lens : code_lens) =
  Printf.sprintf
    {|{
        "range": %s,
        "command": %s
    }|}
    (stringify_range code_lens.range)
    (match code_lens.command with
    | None -> ""
    | Some command -> stringify_command command)

let stringify_parameter_information (parameter_information : parameter_information)
    =
  Printf.sprintf {|{"label": %s, "documentation": %s}|}
    (let line, chr = parameter_information.label in
     "[" ^ string_of_int line ^ ", " ^ string_of_int chr ^ "]")
    (stringify_markup_content parameter_information.documentation)

let stringify_signature_information (signature_information : signature_information)
    =
  Printf.sprintf
    {|{
    "label": %s,
    "parameters": %s%s
  }|}
    (wrap_in_quotes signature_information.label)
    (signature_information.parameters
    |> List.map stringify_parameter_information
    |> array)
    (match signature_information.documentation with
    | None -> ""
    | Some docs ->
      Printf.sprintf ",\n    \"documentation\": %s"
        (stringify_markup_content docs))

let stringify_signature_help (signature_help : signature_help) =
  Printf.sprintf
    {|{
  "signatures": %s,
  "activeSignature": %s,
  "activeParameter": %s
}|}
    (signature_help.signatures |> List.map stringify_signature_information |> array)
    (match signature_help.active_signature with
    | None -> null
    | Some active_signature -> active_signature |> string_of_int)
    (match signature_help.active_parameter with
    | None -> null
    | Some active_parameter -> active_parameter |> string_of_int)

(* https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#diagnostic *)
let stringify_diagnostic d =
  Printf.sprintf
    {|{
  "range": %s,
  "message": %s,
  "severity": %d,
  "source": "ReScript"
}|}
    (stringify_range d.range) (wrap_in_quotes d.message) d.severity
