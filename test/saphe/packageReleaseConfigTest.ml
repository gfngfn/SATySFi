
open SapheMain__ConfigError
open SapheMain__PackageSystemBase
open SapheMain__PackageReleaseConfigImpl
open SapheTestUtil
module PackageReleaseConfig = SapheMain__PackageReleaseConfig


let input1 = {yaml|
saphe: "^0.0.1"
satysfi: "^0.1.0"
name: "stdlib"
version: "0.0.1"
source:
  tar_gzip:
    url: "https://example.com/packages/stdlib.tar.gz"
    checksum: "c0bebeef4423abad1dea"
authors:
  - "Takashi Suwa"
dependencies: []
|yaml}


let expected1 =
  ParsedPackageReleaseConfig{
    ecosystem_requirement = CompatibleWith(make_version "0.0.1");
    language_requirement = CompatibleWith(make_version "0.1.0");
    package_name          = "stdlib";
    package_version       = make_version "0.0.1";
    source = TarGzip{
      url      = "https://example.com/packages/stdlib.tar.gz";
      checksum = "c0bebeef4423abad1dea";
    };
    registry_specs        = [];
    dependencies          = [];
  }


let input2 = {yaml|
saphe: "^0.0.1"
satysfi: "^0.1.0"
name: "foo"
version: "0.0.1"
source:
  tar_gzip:
    url: "https://example.com/packages/foo.tar.gz"
    checksum: "c0bebeef4423abad1dea"
authors:
- "Takashi Suwa"
registries:
- name: "an-external-registry"
  git:
    url: "https://example.com/an-external-registry"
    branch: "master"
dependencies:
- used_as: "Bar"
  registry: "an-external-registry"
  name: "bar"
  requirement: "^1.0.0"
- used_as: "Qux"
  name: "qux"
  requirement: "^1.2.1"
|yaml}


let expected2 =
  ParsedPackageReleaseConfig{
    ecosystem_requirement = CompatibleWith(make_version "0.0.1");
    language_requirement = CompatibleWith(make_version "0.1.0");
    package_name          = "foo";
    package_version       = make_version "0.0.1";
    source = TarGzip{
      url      = "https://example.com/packages/foo.tar.gz";
      checksum = "c0bebeef4423abad1dea";
    };
    registry_specs = [
      ("an-external-registry", GitRegistry{
        url    = "https://example.com/an-external-registry";
        branch = "master";
      });
    ];
    dependencies = [
      ParsedPackageDependencyInRegistry{
        used_as                    = "Bar";
        registry_local_name_option = Some("an-external-registry");
        package_name               = "bar";
        version_requirement        = CompatibleWith(make_version "1.0.0");
      };
      ParsedPackageDependencyInRegistry{
        used_as                    = "Qux";
        registry_local_name_option = None;
        package_name               = "qux";
        version_requirement        = CompatibleWith(make_version "1.2.1");
      };
    ];
  }


let parsed_release_config = Alcotest.of_pp pp_parsed_package_release_config
let yaml_error = Alcotest.of_pp pp_yaml_error


let parse_test_1 () =
  let got = PackageReleaseConfig.parse input1 in
  let expected = Ok(expected1) in
  Alcotest.(check (result parsed_release_config yaml_error)) "parse" expected got


let parse_test_2 () =
  let got = PackageReleaseConfig.parse input2 in
  let expected = Ok(expected2) in
  Alcotest.(check (result parsed_release_config yaml_error)) "parse" expected got


let test_cases =
  Alcotest.[
    test_case "parse 1 (basic)" `Quick parse_test_1;
    test_case "parse 2 (external registries)" `Quick parse_test_2;
  ]
