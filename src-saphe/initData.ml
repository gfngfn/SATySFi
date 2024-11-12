
let document_contents = Core.String.lstrip {string|
use package open StdJaReport

document (|
  title = {The Title of Your Document},
  author = {Your Name},
|) '<
  +chapter{First Chapter}<
    +p{
      Hello, world!
    }
  >
>
|string}


let markdown_contents = Core.String.lstrip {string|
<!-- MDJa -->
<!-- (|
  title = {The Title of Your Document},
  author = {Your Name},
|) -->
# First Section

Hello, world!
|string}


let document_package_config_contents = Core.String.lstrip (Printf.sprintf {string|
saphe: "^%s"
satysfi: "^%s"
authors:
  - "Your Name"
registries:
  - name: "default"
    git:
      url: "https://github.com/SATySFi/default-registry"
      branch: "temp-dev-saphe"
contents:
  document: {}
dependencies:
  - used_as: "StdJaReport"
    registered:
      registry: "default"
      name: "std-ja-report"
      requirement: "^0.0.1"
|string}
  (SemanticVersion.to_string Constant.current_ecosystem_version)
  (SemanticVersion.to_string Constant.satysfi_version_for_init))


let markdown_package_config_contents = Core.String.lstrip (Printf.sprintf {string|
saphe: "^%s"
satysfi: "^%s"
authors:
  - "Your Name"
registries:
  - name: "default"
    git:
      url: "https://github.com/SATySFi/default-registry"
      branch: "temp-dev-saphe"
contents:
  document: {}
dependencies:
  - used_as: "MDJa"
    registered:
      registry: "default"
      name: "md-ja"
      requirement: "^0.0.1"
|string}
  (SemanticVersion.to_string Constant.current_ecosystem_version)
  (SemanticVersion.to_string Constant.satysfi_version_for_init))


let library_package_config_contents = Core.String.lstrip (Printf.sprintf {string|
saphe: "^%s"
satysfi: "^%s"
name: "your-library"
authors:
  - "Your Name"
registries:
  - name: "default"
    git:
      url: "https://github.com/SATySFi/default-registry"
      branch: "temp-dev-saphe"
contents:
  library:
    main_module: "Calc"
    source_directories:
    - "./src"
    test_directories:
    - "./test"
dependencies:
  - used_as: "Stdlib"
    registered:
      registry: "default"
      name: "stdlib"
      requirement: "^0.0.1"
test_dependencies:
  - used_as: "Testing"
    registered:
      registry: "default"
      name: "testing"
      requirement: "^0.0.1"
|string}
  (SemanticVersion.to_string Constant.current_ecosystem_version)
  (SemanticVersion.to_string Constant.satysfi_version_for_init))


let library_source_contents = Core.String.lstrip {string|
module Calc :> sig
  val succ : int -> int
end = struct
  val succ n = n + 1
end
|string}


let library_test_contents = Core.String.lstrip {string|
use package open Testing
use Calc

module CalcTest = struct

  #[test]
  val succ-test () =
    assert-equal Equality.int 43 (Calc.succ 42)

end
|string}
