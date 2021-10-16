
include IdScheme.Make(struct
  type t = unit

  let show n () = "#" ^ string_of_int n
end)


let pp ppf bid =
  Format.fprintf ppf "%s" (show bid)
