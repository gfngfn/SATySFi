
let () =
  let open Alcotest in
  run "SATySFi-Util Test" [
    ("DependencyGraph", DependencyGraphTest.test_cases);
    ("AbsPath", AbsPathTest.test_cases);
  ]
