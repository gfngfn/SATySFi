
open Types


type error =
  | Unexpected of Range.t

type t = {
  for_test_only : bool;
}


let make (attrs : untyped_attribute list) : (t, error) result =
  let open ResultMonad in
  match attrs with
  | [] ->
      return { for_test_only = false }

  | [ (_, UTAttribute("test-only", None)) ] ->
      return { for_test_only = true }

  | (rng, _) :: _ ->
      err @@ Unexpected(rng)
