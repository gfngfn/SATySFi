
(* Assumes that `Semver` works correctly: *)
let semver_from_parts major minor patch prerelease build =
  match Semver.from_parts major minor patch prerelease build with
  | Some(semver) -> semver
  | None         -> assert false


(* Do "non-pretty" pretty printing instead of using `Semver.pp`: *)
let pp_semver ppf (Semver.{ major; minor; patch; prerelease; build }) =
  let pp_sep ppf () = Format.fprintf ppf "; " in
  Format.fprintf ppf "{major = %d; minor = %d, patch = %d, prerelease = [%a], build = [%a]}"
    major minor patch
    (Format.pp_print_list ~pp_sep Format.pp_print_string) prerelease
    (Format.pp_print_list ~pp_sep Format.pp_print_string) build


let parse_test () =
  List.iter (fun (input, expected) ->
    let got = Option.map SemanticVersion.ForTest.get_content (SemanticVersion.parse input) in
    Alcotest.(check (option (of_pp pp_semver))) "parse" expected got
  ) [
    ("0.1.0", Some(semver_from_parts 0 1 0 [] []));
    ("-0.1.0", None);
    ("0.-1.0", None);
    ("0.1.0-", None);
    ("0.1.0+", None);
    ("0.1.0-alpha.1", Some(semver_from_parts 0 1 0 ["alpha"; "1"] []));
    ("0.1.0+20240325", None);
    ("0.1.0-alpha.1+20240325", None);
  ]


(* Assumes that `SemanticVersion.parse` works fine henceforth: *)
let make_version (s_version : string) : SemanticVersion.t =
  match SemanticVersion.parse s_version with
  | Some(semver) -> semver
  | None         -> assert false


let get_compatibility_unit_test () =
  List.iter (fun (input, expected) ->
    let got = SemanticVersion.get_compatibility_unit input in
    Alcotest.(check string) "get_compatibility_unit" expected got
  ) [
    (make_version "0.1.0", "0.1");
    (make_version "1.1.0", "1");
    (make_version "0.1.0-alpha.1", "0.1.0-alpha.1");
    (make_version "1.1.0-alpha.1", "1.1.0-alpha.1");
  ]


let test_cases =
  Alcotest.[
    test_case "parse" `Quick parse_test;
    test_case "get_compatibility_unit" `Quick get_compatibility_unit_test;
  ]
