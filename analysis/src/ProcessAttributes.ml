open SharedTypes

(* TODO should I hang on to location? *)
let rec find_doc_attribute attributes =
  let open Parsetree in
  match attributes with
  | [] -> None
  | ( {Asttypes.txt = "ocaml.doc" | "ocaml.text" | "ns.doc" | "res.doc"},
      PStr
        [
          {
            pstr_desc =
              Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (doc, _))}, _);
          };
        ] )
    :: _ ->
    Some doc
  | _ :: rest -> find_doc_attribute rest

let rec find_deprecated_attribute attributes =
  let open Parsetree in
  match attributes with
  | [] -> None
  | ( {Asttypes.txt = "deprecated"},
      PStr
        [
          {
            pstr_desc =
              Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (msg, _))}, _);
          };
        ] )
    :: _ ->
    Some msg
  | ({Asttypes.txt = "deprecated"}, _) :: _ -> Some ""
  | _ :: rest -> find_deprecated_attribute rest

let new_declared ~item ~extent ~name ~stamp ~module_path is_exported attributes
    =
  {
    Declared.name;
    stamp;
    extent_loc = extent;
    is_exported;
    module_path;
    deprecated = find_deprecated_attribute attributes;
    docstring =
      (match find_doc_attribute attributes with
      | None -> []
      | Some d -> [d]);
    item;
  }
