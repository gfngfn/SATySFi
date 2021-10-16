
include IdScheme.Make(struct
  type t = string
  let show n m = Printf.sprintf "%d(%s)" n m
end)


let pp ppf mid =
  Format.fprintf ppf "%s" (show mid)
