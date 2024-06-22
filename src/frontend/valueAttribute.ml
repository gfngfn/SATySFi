
open Types


type error =
  | Unexpected of Range.t

type t = {
  is_test : bool;
}


let make (attrs : untyped_attribute list) : (t, error) result =
  let open ResultMonad in
  match attrs with
  | [] ->
      return { is_test = false }

  | [ (_, UTAttribute("test", None)) ] ->
      return { is_test = true }

  | (rng, _) :: _ ->
      err @@ Unexpected(rng)
