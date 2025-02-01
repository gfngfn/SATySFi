
open SyntaxBase


include IdScheme.Make(struct
  type t = LabelSet.t

  let show n _ = "?#" ^ string_of_int n
end)


let fresh labset =
  generate labset


let get_label_set brid =
  get_supplement brid


let pp ppf brid =
  Format.fprintf ppf "%s" (show brid)
