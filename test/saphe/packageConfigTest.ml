
open SapheTestUtil
open SapheMain__ConfigError
open SapheMain__PackageSystemBase
open SapheMain__PackageConfigImpl
module PackageConfig = SapheMain__PackageConfig


let input1 = {yaml|
saphe: "^0.1.0-alpha.1"
satysfi: "^0.1.0"
name: "stdlib"
authors:
  - "Takashi Suwa"
registries:
  - name: "default"
    git:
      url: "https://github.com/SATySFi/default-registry"
      branch: "temp-dev-saphe"
external_resources:
  - name: "dummy"
    zip:
      url: "https://example.com/foo.zip"
      checksum: "c0bebeefc0bebeefc0bebeefc0bebeef"
      extractions:
        - from: "bar.txt"
          to: "./resources/bar.txt"
        - from: "qux.dat"
          to: "./resources/qux.dat"
intermediate_directory: "./_build"
contents:
  library:
    main_module: "Stdlib"
    source_directories:
      - "./src"
    test_directories:
      - "./test"
dependencies: []
test_dependencies:
  - used_as: "Testing"
    registered:
      registry: "default"
      name: "testing"
      requirement: "^0.0.1"
|yaml}


let expected1 =
  ParsedPackageConfig{
    language_requirement = SemanticVersion.CompatibleWith(make_version "0.1.0");
    package_name = Some("stdlib");
    package_authors = ["Takashi Suwa"];
    external_resources = [
      ("dummy", ExternalZip{
        url = "https://example.com/foo.zip";
        checksum = "c0bebeefc0bebeefc0bebeefc0bebeef";
        extractions = [
          {
            extracted_from = "bar.txt";
            extracted_to = "./resources/bar.txt";
          };
          {
            extracted_from = "qux.dat";
            extracted_to = "./resources/qux.dat";
          };
        ];
      });
    ];
    intermediate_directory = Some("./_build");
    package_contents =
      ParsedLibrary{
        main_module_name = "Stdlib";
        source_directories = [ "./src" ];
        test_directories = [ "./test" ];
        markdown_conversion = None;
      };
    registry_specs = [
      ("default", GitRegistry{
        url = "https://github.com/SATySFi/default-registry";
        branch = "temp-dev-saphe";
      });
    ];
    source_dependencies = [];
    test_dependencies = [
      ParsedPackageDependency{
        used_as = "Testing";
        spec =
          ParsedRegisteredDependency{
            package_name = "testing";
            registry_local_name = "default";
            version_requirement = SemanticVersion.CompatibleWith(make_version "0.0.1");
          };
        };
    ];
  }


let parsed_package_config = Alcotest.of_pp pp_parsed_package_config
let yaml_error = Alcotest.of_pp pp_yaml_error


let parse_test_1 () =
  let got = PackageConfig.parse input1 in
  let expected = Ok(expected1) in
  Alcotest.(check (result parsed_package_config yaml_error)) "parse" expected got


let test_cases =
  Alcotest.[
    test_case "parse 1" `Quick parse_test_1;
  ]
