open Common

type language = Ml | Res

let pos_language (pos : Lexing.position) =
  if
    Filename.check_suffix pos.pos_fname ".res"
    || Filename.check_suffix pos.pos_fname ".resi"
  then Res
  else Ml

let dead_annotation = "dead"
let annotate_at_end ~pos =
  match pos_language pos with
  | Res -> false
  | Ml -> true

let get_pos_annotation decl =
  match annotate_at_end ~pos:decl.pos with
  | true -> decl.pos_end
  | false -> decl.pos_start

let rec lineToString_ {original; declarations} =
  match declarations with
  | [] -> original
  | ({decl_kind; path; pos} as decl) :: next_declarations ->
    let language = pos_language pos in
    let annotation_str =
      match language with
      | Res ->
        "@" ^ dead_annotation ^ "(\"" ^ (path |> Path.without_head) ^ "\") "
      | Ml ->
        " " ^ "["
        ^ (match decl_kind |> DeclKind.is_type with
          | true -> "@"
          | false -> "@@")
        ^ dead_annotation ^ " \""
        ^ (path |> Path.without_head)
        ^ "\"] "
    in
    let pos_annotation = decl |> get_pos_annotation in
    let col = pos_annotation.pos_cnum - pos_annotation.pos_bol in
    let original_len = String.length original in
    {
      original =
        (if String.length original >= col && col > 0 then
           let original1, original2 =
             try
               ( String.sub original 0 col,
                 String.sub original col (original_len - col) )
             with Invalid_argument _ -> (original, "")
           in
           if language = Res && decl_kind = VariantCase then
             if
               String.length original2 >= 2
               && (String.sub [@doesNotRaise]) original2 0 2 = "| "
             then
               original1 ^ "| " ^ annotation_str
               ^ (String.sub [@doesNotRaise]) original2 2
                   (String.length original2 - 2)
             else if
               String.length original2 >= 1
               && (String.sub [@doesNotRaise]) original2 0 1 = "|"
             then
               original1 ^ "|" ^ annotation_str
               ^ (String.sub [@doesNotRaise]) original2 1
                   (String.length original2 - 1)
             else original1 ^ "| " ^ annotation_str ^ original2
           else original1 ^ annotation_str ^ original2
         else
           match language = Ml with
           | true -> original ^ annotation_str
           | false -> annotation_str ^ original);
      declarations = next_declarations;
    }
    |> lineToString_

let line_to_string {original; declarations} =
  let declarations =
    declarations
    |> List.sort (fun decl1 decl2 ->
           (get_pos_annotation decl2).pos_cnum
           - (get_pos_annotation decl1).pos_cnum)
  in
  lineToString_ {original; declarations}

let current_file = ref ""
let current_file_lines = (ref [||] : line array ref)

let read_file file_name =
  let channel = open_in file_name in
  let lines = ref [] in
  let rec loop () =
    let line = {original = input_line channel; declarations = []} in
    lines := line :: !lines;
    loop ()
      [@@raises End_of_file]
  in
  try loop ()
  with End_of_file ->
    close_in_noerr channel;
    !lines |> List.rev |> Array.of_list

let write_file file_name lines =
  if file_name <> "" && !Cli.write then (
    let channel = open_out file_name in
    let last_line = Array.length lines in
    lines
    |> Array.iteri (fun n line ->
           output_string channel (line |> line_to_string);
           if n < last_line - 1 then output_char channel '\n');
    close_out_noerr channel)

let offset_of_pos_adjustment = function
  | FirstVariant | Nothing -> 0
  | OtherVariant -> 2

let get_line_annotation ~decl ~line =
  if !Cli.json then
    let pos_annotation = decl |> get_pos_annotation in
    let offset = decl.pos_adjustment |> offset_of_pos_adjustment in
    EmitJson.emit_annotate
      ~pos:
        ( pos_annotation.pos_lnum - 1,
          pos_annotation.pos_cnum - pos_annotation.pos_bol + offset )
      ~text:
        (if decl.pos_adjustment = FirstVariant then
           (* avoid syntax error *)
           "| @dead "
         else "@dead ")
      ~action:"Suppress dead code warning"
  else
    Format.asprintf "@.  <-- line %d@.  %s" decl.pos.pos_lnum
      (line |> line_to_string)

let cant_find_line () = if !Cli.json then "" else "\n  <-- Can't find line"

let line_annotation_to_string = function
  | None -> cant_find_line ()
  | Some (decl, line) -> get_line_annotation ~decl ~line

let add_line_annotation ~decl : line_annotation =
  let file_name = decl.pos.pos_fname in
  if Sys.file_exists file_name then (
    if file_name <> !current_file then (
      write_file !current_file !current_file_lines;
      current_file := file_name;
      current_file_lines := read_file file_name);
    let index_in_lines = (decl |> get_pos_annotation).pos_lnum - 1 in
    match !current_file_lines.(index_in_lines) with
    | line ->
      line.declarations <- decl :: line.declarations;
      Some (decl, line)
    | exception Invalid_argument _ -> None)
  else None

let write () = write_file !current_file !current_file_lines
