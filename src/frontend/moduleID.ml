
include IdScheme.Make(struct
  type t = string
  let show n m = Printf.sprintf "%d(%s)" n m
end)


let fresh modnm =
  generate modnm


let pp ppf mid =
  Format.fprintf ppf "%s" (show mid)
