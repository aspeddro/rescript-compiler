let raises_lib_table : (Name.t, Exceptions.t) Hashtbl.t =
  let table = Hashtbl.create 15 in
  let open Exn in
  let array =
    [
      ("get", [invalid_argument]);
      ("set", [invalid_argument]);
      ("make", [invalid_argument]);
      ("init", [invalid_argument]);
      ("make_matrix", [invalid_argument]);
      ("fill", [invalid_argument]);
      ("blit", [invalid_argument]);
      ("iter2", [invalid_argument]);
      ("map2", [invalid_argument]);
    ]
  in
  let belt_array =
    [("getExn", [assert_failure]); ("setExn", [assert_failure])]
  in
  let belt_list =
    [
      ("getExn", [not_found]); ("headExn", [not_found]); ("tailExn", [not_found]);
    ]
  in
  let belt_map = [("getExn", [not_found])] in
  let belt_mutable_map = belt_map in
  let belt_mutable_queue =
    [("peekExn", [not_found]); ("popExn", [not_found])]
  in
  let belt_mutable_set = [("getExn", [not_found])] in
  let belt_option = [("getExn", [not_found])] in
  let belt_result = [("getExn", [not_found])] in
  let belt_set = [("getExn", [not_found])] in
  let bs_json =
    (* bs-json *)
    [
      ("bool", [decode_error]);
      ("float", [decode_error]);
      ("int", [decode_error]);
      ("string", [decode_error]);
      ("char", [decode_error]);
      ("date", [decode_error]);
      ("nullable", [decode_error]);
      ("nullAs", [decode_error]);
      ("array", [decode_error]);
      ("list", [decode_error]);
      ("pair", [decode_error]);
      ("tuple2", [decode_error]);
      ("tuple3", [decode_error]);
      ("tuple4", [decode_error]);
      ("dict", [decode_error]);
      ("field", [decode_error]);
      ("at", [decode_error; invalid_argument]);
      ("oneOf", [decode_error]);
      ("either", [decode_error]);
    ]
  in
  let buffer =
    [
      ("sub", [invalid_argument]);
      ("blit", [invalid_argument]);
      ("nth", [invalid_argument]);
      ("add_substitute", [not_found]);
      ("add_channel", [end_of_file]);
      ("truncate", [invalid_argument]);
    ]
  in
  let bytes =
    [
      ("get", [invalid_argument]);
      ("set", [invalid_argument]);
      ("create", [invalid_argument]);
      ("make", [invalid_argument]);
      ("init", [invalid_argument]);
      ("sub", [invalid_argument]);
      ("sub_string", [invalid_argument]);
      ("extend", [invalid_argument]);
      ("fill", [invalid_argument]);
      ("blit", [invalid_argument]);
      ("blit_string", [invalid_argument]);
      (* ("concat", [invalidArgument]), if longer than {!Sys.max_string_length}
         ("cat", [invalidArgument]), if longer than {!Sys.max_string_length}
         ("escaped", [invalidArgument]), if longer than {!Sys.max_string_length} *)
      ("index", [not_found]);
      ("rindex", [not_found]);
      ("index_from", [invalid_argument; not_found]);
      ("index_from_opt", [invalid_argument]);
      ("rindex_from", [invalid_argument; not_found]);
      ("rindex_from_opt", [invalid_argument]);
      ("contains_from", [invalid_argument]);
      ("rcontains_from", [invalid_argument]);
    ]
  in
  let filename =
    [
      ("chop_extension", [invalid_argument]);
      ("temp_file", [sys_error]);
      ("open_temp_file", [sys_error]);
    ]
  in
  let hashtbl = [("find", [not_found])] in
  let list =
    [
      ("hd", [failure]);
      ("tl", [failure]);
      ("nth", [failure; invalid_argument]);
      ("nth_opt", [invalid_argument]);
      ("init", [invalid_argument]);
      ("iter2", [invalid_argument]);
      ("map2", [invalid_argument]);
      ("fold_left2", [invalid_argument]);
      ("fold_right2", [invalid_argument]);
      ("for_all2", [invalid_argument]);
      ("exists2", [invalid_argument]);
      ("find", [not_found]);
      ("assoc", [not_found]);
      ("combine", [invalid_argument]);
    ]
  in
  let string =
    [
      ("get", [invalid_argument]);
      ("set", [invalid_argument]);
      ("create", [invalid_argument]);
      ("make", [invalid_argument]);
      ("init", [invalid_argument]);
      ("sub", [invalid_argument]);
      ("fill", [invalid_argument]);
      (* ("concat", [invalidArgument]), if longer than {!Sys.max_string_length}
         ("escaped", [invalidArgument]), if longer than {!Sys.max_string_length} *)
      ("index", [not_found]);
      ("rindex", [not_found]);
      ("index_from", [invalid_argument; not_found]);
      ("index_from_opt", [invalid_argument]);
      ("rindex_from", [invalid_argument; not_found]);
      ("rindex_from_opt", [invalid_argument]);
      ("contains_from", [invalid_argument]);
      ("rcontains_from", [invalid_argument]);
    ]
  in
  let stdlib =
    [
      ("invalid_arg", [invalid_argument]);
      ("failwith", [failure]);
      ("/", [division_by_zero]);
      ("mod", [division_by_zero]);
      ("char_of_int", [invalid_argument]);
      ("bool_of_string", [invalid_argument]);
      ("int_of_string", [failure]);
      ("float_of_string", [failure]);
      ("read_int", [failure]);
      ("output", [invalid_argument]);
      ("close_out", [sys_error]);
      ("input_char", [end_of_file]);
      ("input_line", [end_of_file]);
      ("input", [invalid_argument]);
      ("really_input", [end_of_file; invalid_argument]);
      ("really_input_string", [end_of_file]);
      ("input_byte", [end_of_file]);
      ("input_binary_int", [end_of_file]);
      ("close_in", [sys_error]);
      ("exit", [exit]);
    ]
  in
  let str =
    [
      ("search_forward", [not_found]);
      ("search_backward", [not_found]);
      ("matched_group", [not_found]);
      ("group_beginning", [not_found; invalid_argument]);
      ("group_end", [not_found; invalid_argument]);
    ]
  in
  let yojson_basic = [("from_string", [yojson_json_error])] in
  let yojson_basic_util =
    [
      ("member", [yojson_type_error]);
      ("to_assoc", [yojson_type_error]);
      ("to_bool", [yojson_type_error]);
      ("to_bool_option", [yojson_type_error]);
      ("to_float", [yojson_type_error]);
      ("to_float_option", [yojson_type_error]);
      ("to_int", [yojson_type_error]);
      ("to_list", [yojson_type_error]);
      ("to_number", [yojson_type_error]);
      ("to_number_option", [yojson_type_error]);
      ("to_string", [yojson_type_error]);
      ("to_string_option", [yojson_type_error]);
    ]
  in
  [
    ("Array", array);
    ("Belt.Array", belt_array);
    ("Belt_Array", belt_array);
    ("Belt.List", belt_list);
    ("Belt_List", belt_list);
    ("Belt.Map", belt_map);
    ("Belt.Map.Int", belt_map);
    ("Belt.Map.String", belt_map);
    ("Belt_Map", belt_map);
    ("Belt_Map.Int", belt_map);
    ("Belt_Map.String", belt_map);
    ("Belt_MapInt", belt_map);
    ("Belt_MapString", belt_map);
    ("Belt.MutableMap", belt_mutable_map);
    ("Belt.MutableMap.Int", belt_mutable_map);
    ("Belt.MutableMap.String", belt_mutable_map);
    ("Belt_MutableMap", belt_mutable_map);
    ("Belt_MutableMap.Int", belt_mutable_map);
    ("Belt_MutableMap.String", belt_mutable_map);
    ("Belt_MutableMapInt", belt_mutable_map);
    ("Belt_MutableMapString", belt_mutable_map);
    ("Belt.MutableQueue", belt_mutable_queue);
    ("Belt_MutableQueue", belt_mutable_queue);
    ("Belt.Option", belt_option);
    ("Belt_Option", belt_option);
    ("Belt.Result", belt_result);
    ("Belt_Result", belt_result);
    ("Belt.Set", belt_set);
    ("Belt.Set.Int", belt_set);
    ("Belt.Set.String", belt_set);
    ("Belt_Set", belt_set);
    ("Belt_Set.Int", belt_set);
    ("Belt_Set.String", belt_set);
    ("Belt_SetInt", belt_set);
    ("Belt_SetString", belt_set);
    ("Belt.MutableSet", belt_mutable_set);
    ("Belt.MutableSet.Int", belt_mutable_set);
    ("Belt.MutableSet.String", belt_mutable_set);
    ("MutableSet", belt_mutable_set);
    ("MutableSet.Int", belt_mutable_set);
    ("MutableSet.String", belt_mutable_set);
    ("Belt_MutableSetInt", belt_mutable_set);
    ("Belt_MutableSetString", belt_mutable_set);
    ("Buffer", buffer);
    ("Bytes", bytes);
    ("Char", [("chr", [invalid_argument])]);
    ("Filename", filename);
    ("Hashtbl", hashtbl);
    ("Js.Json", [("parseExn", [js_exn_error])]);
    ("Json_decode", bs_json);
    ("Json.Decode", bs_json);
    ("List", list);
    ("Pervasives", stdlib);
    ("Stdlib", stdlib);
    ("Stdlib.Array", array);
    ("Stdlib.Buffer", buffer);
    ("Stdlib.Bytes", bytes);
    ("Stdlib.Filename", filename);
    ("Stdlib.Hashtbl", hashtbl);
    ("Stdlib.List", list);
    ("Stdlib.Str", str);
    ("Stdlib.String", string);
    ("Str", str);
    ("String", string);
    ("Yojson.Basic", yojson_basic);
    ("Yojson.Basic.Util", yojson_basic_util);
  ]
  |> List.iter (fun (name, group) ->
         group
         |> List.iter (fun (s, e) ->
                Hashtbl.add table
                  (name ^ "." ^ s |> Name.create)
                  (e |> Exceptions.from_list)));
  table

let find (path : Common.Path.t) =
  Hashtbl.find_opt raises_lib_table (path |> Common.Path.to_name)
