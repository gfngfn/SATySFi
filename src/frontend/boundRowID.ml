
open SyntaxBase


include IdScheme.Make(struct
  type t = LabelSet.t

  let show n _ = "?#" ^ string_of_int n
end)


let fresh labset =
  generate labset


let pp ppf bid =
  Format.fprintf ppf "%s" (show bid)
