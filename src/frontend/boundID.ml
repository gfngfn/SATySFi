
include IdScheme.Make(struct
  type t = unit

  let show n () = "#" ^ string_of_int n
end)


let fresh () =
  generate ()


let pp ppf bid =
  Format.fprintf ppf "%s" (show bid)
