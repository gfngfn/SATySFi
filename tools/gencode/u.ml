
let default v = function
  | Some(x) -> x
  | None    -> v


let trim_re =
  Pcre.regexp ~flags:[`UTF8; `DOTALL] "\\A *\n(.*\n) *\\z"


let trim s =
  match Pcre.exec ~rex:trim_re s with
  | exception Not_found -> s
  | substrings -> Pcre.get_substring substrings 1


let opt_map f = function
  | Some(v) -> Some(f v)
  | None    -> None


let split_lines s =
  Core_kernel.String.split_lines s


let nullp = function
  | [] -> true
  | _  -> false


let const v = fun _ -> v


let puts fmt =
  Printf.printf (fmt ^^ "\n")


let ( @% ) fmt =
  Printf.sprintf fmt
