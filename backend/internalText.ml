
module ChEnc = UCoreLib.CharEncoding


type t = UCoreLib.text

exception NotEncodableToUTF16BE of t


let of_utf_8 (str_utf8 : string) : t =
  let dcdr_utf8 = ChEnc.create_decoder ChEnc.utf8 in
  let (_, s) = ChEnc.decode dcdr_utf8 str_utf8 in
    s


let to_utf16be_hex (intext : t) =
  let hexify_byte = Printf.sprintf "%X" in
  let rec hexify_string (acc : string) (i : int) (len : int) (str : string) : string =
    if len <= i then acc else
      let b = Char.code (String.get str i) in
      let strhex = hexify_byte b in
        hexify_string (acc ^ strhex) (i + 1) len str
  in
  let encdr_utf16be = ChEnc.create_encoder ChEnc.utf16be in
    match ChEnc.encode encdr_utf16be intext with
    | `Error                   -> raise (NotEncodableToUTF16BE(intext))
    | `Success(_, str_utf16be) -> hexify_string "" 0 (String.length str_utf16be) str_utf16be

