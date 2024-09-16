
let is_middle_char (char : char) : bool =
  Char.equal char '-' || Core.Char.is_alpha char || Core.Char.is_digit char


let is_uppercased_identifier (s : string) : bool =
  match Core.String.to_list s with
  | []         -> false
  | ch0 :: chs -> Core.Char.is_uppercase ch0 && List.for_all is_middle_char chs


let is_lowercased_identifier (s : string) : bool =
  match Core.String.to_list s with
  | []         -> false
  | ch0 :: chs -> Core.Char.is_lowercase ch0 && List.for_all is_middle_char chs


let cut_module_names (s : string) : string list * string =
  match List.rev (String.split_on_char '.' s) with
  | varnm :: modnms_rev -> (List.rev modnms_rev, varnm)
  | _                   -> assert false (* `String.split_on_char` always returns a non-empty list *)


let parse_long_command ~(prefix : string) (s : string) : (string list * string) option =
  let open OptionMonad in
  let* s_tail = Core.String.chop_prefix ~prefix s in
  let (modnms, varnm) = cut_module_names s_tail in
  if List.for_all is_uppercased_identifier modnms && is_lowercased_identifier varnm then
    return (modnms, varnm)
  else
    None


let parse_long_identifier (s : string) : (string list * string) option =
  let open OptionMonad in
  let (modnms, varnm) = cut_module_names s in
  if List.for_all is_uppercased_identifier modnms && is_lowercased_identifier varnm then
    return (modnms, varnm)
  else
    None
