
open SapheMain__ConfigError
open SapheMain__PackageSystemBase
open SapheTestUtil
module PackageReleaseConfig = SapheMain__PackageReleaseConfig


let input1 = {yaml|
saphe: "^0.0.1"
satysfi: "^0.1.0"
name: "stdlib"
version: "0.0.1"
source:
  tar_gzip:
    url: "B"
    checksum: "A"
authors:
  - "Takashi Suwa"
registries:
  - name: "default"
    git:
      url: "https://github.com/SATySFi/default-registry"
      branch: "temp-dev-saphe"
dependencies: []
test_dependencies:
  - used_as: "Testing"
    registered:
      registry: "default"
      name: "testing"
      requirement: "^0.0.1"
|yaml}


let expected1 =
  PackageReleaseConfig.{
    ecosystem_requirement = CompatibleWith(make_version "0.0.1");
    package_name          = "stdlib";
    implementation = ImplRecord{
      language_requirement = CompatibleWith(make_version "0.1.0");
      package_version      = make_version "0.0.1";
      source               = TarGzip{ url = "B"; checksum = "A" };
      dependencies         = [];
    };
  }


let parsed_release_config = Alcotest.of_pp PackageReleaseConfig.pp
let yaml_error = Alcotest.of_pp pp_yaml_error


let parse_test_1 () =
  let got = PackageReleaseConfig.parse input1 in
  let expected = Ok(expected1) in
  Alcotest.(check (result parsed_release_config yaml_error)) "parse" expected got


let test_cases =
  Alcotest.[
    test_case "parse 1" `Quick parse_test_1;
  ]
