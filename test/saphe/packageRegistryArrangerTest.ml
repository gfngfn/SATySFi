
open SapheMain__PackageSystemBase
module PackageRegistryArranger = SapheMain__PackageRegistryArranger
module CanonicalRegistryUrl = SapheMain__CanonicalRegistryUrl


let error = Alcotest.of_pp CanonicalRegistryUrl.pp_error


let solve_test () =
  let open ResultMonad in
  let make url = GitRegistry{ url; branch = "master" } in
  let a = make "https://url-a.com" in
  let b = make "https://url-b.com" in
  let c = make "https://url-c.com" in
  let d = make "https://url-d.com" in
  let e = make "https://url-e.com" in
  let got =
    PackageRegistryArranger.main
      ~err:(fun e -> e)
      (fun n (GitRegistry{ url; _ }) ->
        let (i, deps) =
          match url with
          | "https://url-a.com" -> (1, [e; b])
          | "https://url-b.com" -> (10, [a; e])
          | "https://url-c.com" -> (100, [a; c])
          | "https://url-d.com" -> (1000, [d])
          | "https://url-e.com" -> (10000, [d])
          | _                   -> assert false
        in
        return (deps, n + i)
      )
      [a]
      0
  in
  let expected = Ok(11011) in  (* Only `c` is ignored *)
  Alcotest.(check (result int error)) "check" expected got


let test_cases =
  Alcotest.[
    test_case "solve" `Quick solve_test;
  ]
