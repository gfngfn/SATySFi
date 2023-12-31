
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
  envelopes           : envelope_spec list;
  direct_dependencies : envelope_dependency list;
}
