
type element =
  | Data of string
  | Kern of int
  [@@deriving show]

type style =
  | Literal
  | Hex
  [@@deriving show]

type t = style * element list
  [@@deriving show]

let empty_literal_style = (Literal, [])

let empty_hex_style = (Hex, [])

let append_kern (sty, otxtmain) rawwid =
  (sty, Kern(rawwid) :: otxtmain)

let append_uchar (sty, otxtmain) uch =
  let data =
    match sty with
    | Literal -> InternalText.to_utf8 (InternalText.of_uchar uch)
    | Hex     -> InternalText.to_utf16be_hex (InternalText.of_uchar uch)
  in
      (* temporary; inefficient conversion *)
    (sty, Data(data) :: otxtmain)

let append_glyph_id (sty, otxtmain) gid =
  let data = FontFormat.hex_of_glyph_id gid in
    (sty, Data(data) :: otxtmain)

let to_TJ_argument (sty, otxtmain) =
  let pdfstr =
    match sty with
    | Literal -> (fun x -> Pdf.String(x))
    | Hex     -> (fun x -> Pdf.StringHex(x))
  in
  let lst =
    otxtmain |> List.rev |> List.map (function
      | Data(data)   -> pdfstr data
      | Kern(rawwid) -> Pdf.Integer(-rawwid)
    )
  in
    Pdf.Array(lst)
