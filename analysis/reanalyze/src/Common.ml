let current_src = ref ""
let current_module = ref ""
let current_module_name = ref ("" |> Name.create)
let run_config = RunConfig.run_config

(* Location printer: `filename:line: ' *)
let pos_to_string (pos : Lexing.position) =
  let file = pos.Lexing.pos_fname in
  let line = pos.Lexing.pos_lnum in
  let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  (file |> Filename.basename)
  ^ ":" ^ string_of_int line ^ ":" ^ string_of_int col

module Cli = struct
  let debug = ref false
  let ci = ref false

  (** The command was a -cmt variant (e.g. -exception-cmt) *)
  let cmt_command = ref false

  let experimental = ref false
  let json = ref false
  let write = ref false

  (* names to be considered live values *)
  let live_names = ref ([] : string list)

  (* paths of files where all values are considered live *)

  let live_paths = ref ([] : string list)

  (* paths of files to exclude from analysis *)
  let exclude_paths = ref ([] : string list)
end

module StringSet = Set.Make (String)

module LocSet = Set.Make (struct
  include Location

  let compare = compare
end)

module FileSet = Set.Make (String)

module FileHash = struct
  include Hashtbl.Make (struct
    type t = string

    let hash (x : t) = Hashtbl.hash x
    let equal (x : t) y = x = y
  end)
end

module FileReferences = struct
  (* references across files *)
  let table = (FileHash.create 256 : FileSet.t FileHash.t)

  let find_set table key =
    try FileHash.find table key with Not_found -> FileSet.empty

  let add (loc_from : Location.t) (loc_to : Location.t) =
    let key = loc_from.loc_start.pos_fname in
    let set = find_set table key in
    FileHash.replace table key (FileSet.add loc_to.loc_start.pos_fname set)

  let add_file file_name =
    let set = find_set table file_name in
    FileHash.replace table file_name set

  let exists file_name = FileHash.mem table file_name

  let find file_name =
    match FileHash.find_opt table file_name with
    | Some set -> set
    | None -> FileSet.empty

  let iter f = FileHash.iter f table
end

module Path = struct
  type t = Name.t list

  let to_name (path : t) =
    path |> List.rev_map Name.to_string |> String.concat "." |> Name.create

  let to_string path = path |> to_name |> Name.to_string

  let without_head path =
    match
      path |> List.rev_map (fun n -> n |> Name.to_interface |> Name.to_string)
    with
    | _ :: tl -> tl |> String.concat "."
    | [] -> ""

  let on_ok_path ~when_contains_apply ~f path =
    match path |> Path.flatten with
    | `Ok (id, mods) -> f (Ident.name id :: mods |> String.concat ".")
    | `Contains_apply -> when_contains_apply

  let from_path_t path =
    match path |> Path.flatten with
    | `Ok (id, mods) -> Ident.name id :: mods |> List.rev_map Name.create
    | `Contains_apply -> []

  let module_to_implementation path =
    match path |> List.rev with
    | module_name :: rest ->
      (module_name |> Name.to_implementation) :: rest |> List.rev
    | [] -> path

  let module_to_interface path =
    match path |> List.rev with
    | module_name :: rest -> (module_name |> Name.to_interface) :: rest |> List.rev
    | [] -> path

  let to_module_name ~is_type path =
    match path with
    | _ :: tl when not is_type -> tl |> to_name
    | _ :: _ :: tl when is_type -> tl |> to_name
    | _ -> "" |> Name.create

  let type_to_interface path =
    match path with
    | type_name :: rest -> (type_name |> Name.to_interface) :: rest
    | [] -> path
end

module OptionalArgs = struct
  type t = {
    mutable count: int;
    mutable unused: StringSet.t;
    mutable always_used: StringSet.t;
  }

  let empty =
    {unused = StringSet.empty; always_used = StringSet.empty; count = 0}

  let from_list l =
    {unused = StringSet.of_list l; always_used = StringSet.empty; count = 0}

  let is_empty x = StringSet.is_empty x.unused

  let call ~arg_names ~arg_names_maybe x =
    let name_set = arg_names |> StringSet.of_list in
    let name_set_maybe = arg_names_maybe |> StringSet.of_list in
    let name_set_always = StringSet.diff name_set name_set_maybe in
    if x.count = 0 then x.always_used <- name_set_always
    else x.always_used <- StringSet.inter name_set_always x.always_used;
    arg_names
    |> List.iter (fun name -> x.unused <- StringSet.remove name x.unused);
    x.count <- x.count + 1

  let combine x y =
    let unused = StringSet.inter x.unused y.unused in
    x.unused <- unused;
    y.unused <- unused;
    let always_used = StringSet.inter x.always_used y.always_used in
    x.always_used <- always_used;
    y.always_used <- always_used

  let iter_unused f x = StringSet.iter f x.unused
  let iter_always_used f x = StringSet.iter (fun s -> f s x.count) x.always_used
end

module DeclKind = struct
  type t =
    | Exception
    | RecordLabel
    | VariantCase
    | Value of {
        is_toplevel: bool;
        mutable optional_args: OptionalArgs.t;
        side_effects: bool;
      }

  let is_type dk =
    match dk with
    | RecordLabel | VariantCase -> true
    | Exception | Value _ -> false

  let to_string dk =
    match dk with
    | Exception -> "Exception"
    | RecordLabel -> "RecordLabel"
    | VariantCase -> "VariantCase"
    | Value _ -> "Value"
end

type pos_adjustment = FirstVariant | OtherVariant | Nothing

type decl = {
  decl_kind: DeclKind.t;
  module_loc: Location.t;
  pos_adjustment: pos_adjustment;
  path: Path.t;
  pos: Lexing.position;
  pos_end: Lexing.position;
  pos_start: Lexing.position;
  mutable resolved_dead: bool option;
  mutable report: bool;
}

type line = {mutable declarations: decl list; original: string}

module ExnSet = Set.Make (Exn)

type missing_raise_info = {
  exn_name: string;
  exn_table: (Exn.t, LocSet.t) Hashtbl.t;
  loc_full: Location.t;
  missing_annotations: ExnSet.t;
  raise_set: ExnSet.t;
}

type severity = Warning | Error
type dead_optional = WarningUnusedArgument | WarningRedundantOptionalArgument

type termination =
  | ErrorHygiene
  | ErrorNotImplemented
  | ErrorTermination
  | TerminationAnalysisInternal

type dead_warning =
  | WarningDeadException
  | WarningDeadType
  | WarningDeadValue
  | WarningDeadValueWithSideEffects
  | IncorrectDeadAnnotation

type line_annotation = (decl * line) option

type description =
  | Circular of {message: string}
  | ExceptionAnalysis of {message: string}
  | ExceptionAnalysisMissing of missing_raise_info
  | DeadModule of {message: string}
  | DeadOptional of {dead_optional: dead_optional; message: string}
  | DeadWarning of {
      dead_warning: dead_warning;
      path: string;
      message: string;
      should_write_line_annotation: bool;
      line_annotation: line_annotation;
    }
  | Termination of {termination: termination; message: string}

type issue = {
  name: string;
  severity: severity;
  loc: Location.t;
  description: description;
}
