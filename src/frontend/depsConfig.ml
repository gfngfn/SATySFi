
open MyUtil
open EnvelopeSystemBase
open ConfigError


type envelope = {
  envelope_name         : envelope_name;
  envelope_path         : relative_path;
  envelope_dependencies : envelope_dependency list;
  test_only_envelope    : bool;
}

type t = {
  envelopes : envelope list;
}


let load (_abspath_deps_config : abs_path) : (t, config_error) result =
  failwith "TODO: DepsConfig.load"
