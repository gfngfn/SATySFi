
open MyUtil
open ConfigError


type envelope_name = string

type envelope_dependency = {
  dependency_name    : envelope_name;
  dependency_used_as : string;
}

type envelope_spec = {
  envelope_name          : envelope_name;
  envelope_path          : string;
  envelope_dependencies  : envelope_dependency list;
}

type t = {
  envelopes             : envelope_spec list;
  explicit_dependencies : envelope_dependency list;
}


let write (_abspath_deps_config : abs_path) (_deps_config : t) : (unit, config_error) result =
  failwith "TODO: DepsConfig.write"
