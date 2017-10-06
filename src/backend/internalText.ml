
module ChEnc = UCoreLib.CharEncoding


type t = UCoreLib.text

exception NotEncodableToUTF16BE of t


let of_utf_8 (str_utf8 : string) : t =
  let dcdr_utf8 = ChEnc.create_decoder ChEnc.utf8 in
  let (_, s) = ChEnc.decode dcdr_utf8 str_utf8 in
    s


let to_utf16be_hex (intext : t) =
  let hexify_byte = Printf.sprintf "%02X" in
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


let to_uchar_list (intext : t) : Uchar.t list =
  let len = UCoreLib.Text.length intext in
  let rec aux acc i =
    if len <= i then List.rev acc else
      let uch_ucore = UCoreLib.Text.get intext i in
        aux (uch_ucore :: acc) (i + 1)
  in
  let lst = aux [] 0 in
    List.map (fun uch_ucore -> Uchar.of_int (UCoreLib.UChar.code uch_ucore)) lst


let to_utf8 (intext : t) : string = UCoreLib.Text.to_string intext
  

let of_uchar (uch : Uchar.t) : t =
  UCoreLib.Text.make 1 (UCoreLib.UChar.of_int (Uchar.to_int uch))
