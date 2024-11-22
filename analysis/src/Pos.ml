type t = int * int

let of_lexing {Lexing.pos_lnum; pos_cnum; pos_bol} =
  (pos_lnum - 1, pos_cnum - pos_bol)

let to_string (loc, col) = Printf.sprintf "%d:%d" loc col

let offset_of_line text line =
  let ln = String.length text in
  let rec loop i lno =
    if i >= ln then None
    else
      match text.[i] with
      | '\n' -> if lno = line - 1 then Some (i + 1) else loop (i + 1) (lno + 1)
      | _ -> loop (i + 1) lno
  in
  match line with
  | 0 -> Some 0
  | _ -> loop 0 0

let position_to_offset text (line, character) =
  match offset_of_line text line with
  | None -> None
  | Some bol ->
    if bol + character <= String.length text then Some (bol + character)
    else None

let pos_before_cursor pos = (fst pos, max 0 (snd pos - 1))
