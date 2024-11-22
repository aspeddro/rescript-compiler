type t = Location.t

let start (loc : t) = Pos.of_lexing loc.loc_start
let end_ (loc : t) = Pos.of_lexing loc.loc_end
let range loc : Range.t = (start loc, end_ loc)

let to_string (loc : t) =
  (if loc.loc_ghost then "__ghost__" else "") ^ (loc |> range |> Range.to_string)

let has_pos ~pos loc = start loc <= pos && pos < end_ loc

(** Allows the character after the end to be included. Ie when the cursor is at the 
    end of the word, like `someIdentifier<cursor>`. Useful in some scenarios. *)
let has_pos_inclusive_end ~pos loc = start loc <= pos && pos <= end_ loc
