type t = {
  mutable bsb_project_root: string;
  mutable dce: bool;
  mutable exception_: bool;
  mutable project_root: string;
  mutable suppress: string list;
  mutable termination: bool;
  mutable transitive: bool;
  mutable unsuppress: string list;
}

let run_config =
  {
    bsb_project_root = "";
    dce = false;
    exception_ = false;
    project_root = "";
    suppress = [];
    termination = false;
    transitive = false;
    unsuppress = [];
  }

let all () =
  run_config.dce <- true;
  run_config.exception_ <- true;
  run_config.termination <- true

let dce () = run_config.dce <- true
let exception_ () = run_config.exception_ <- true
let termination () = run_config.termination <- true

let transitive b = run_config.transitive <- b
