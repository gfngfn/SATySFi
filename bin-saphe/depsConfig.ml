
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


let envelope_dependency_encoder (dep : envelope_dependency) : Yaml.value =
  let { dependency_name; dependency_used_as } = dep in
  `O([
    ("name", `String(dependency_name));
    ("used_as", `String(dependency_used_as));
  ])


let envelope_spec_encoder (spec : envelope_spec) : Yaml.value =
  let { envelope_name; envelope_path; envelope_dependencies } = spec in
  `O([
    ("name", `String(envelope_name));
    ("path", `String(envelope_path));
    ("dependencies", `A(envelope_dependencies |> List.map envelope_dependency_encoder));
  ])

let deps_config_encoder (deps_config : t) : Yaml.value =
  let { envelopes; explicit_dependencies } = deps_config in
  `O([
    ("envelopes", `A(envelopes |> List.map envelope_spec_encoder));
    ("dependencies", `A(explicit_dependencies |> List.map envelope_dependency_encoder));
  ])


let write (abspath_deps_config : abs_path) (deps_config : t) : (unit, config_error) result =
  let yaml = deps_config_encoder deps_config in
  let data = encode_yaml yaml in
  write_file abspath_deps_config data
    |> Result.map_error (fun message ->
      CannotWriteDepsConfig{ message; path = abspath_deps_config }
    )
