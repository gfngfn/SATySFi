
type element =
  | Data of string
  | Kern of int
  [@@deriving show]

type style =
  | Literal
  | Hex
  [@@deriving show]

type t = style * element Alist.t

let pp fmt _ =
  Format.fprintf fmt "<output-text>"


let empty_hex_style = (Hex, Alist.empty)


let append_kern (sty, otxtmain) rawwid =
  (sty, Alist.extend otxtmain (Kern(rawwid)))

let append_glyph_synthesis (sty, otxtmain) (gid, markinfolst) =
  let data = FontFormat.hex_of_glyph_id gid in
    (sty, Alist.extend otxtmain (Data(data)))


let to_TJ_argument (sty, otxtmain) =
  let pdfstr =
    match sty with
    | Literal -> (fun x -> Pdf.String(x))
    | Hex     -> (fun x -> Pdf.StringHex(x))
  in
  let lst =
    otxtmain |> Alist.to_list |> List.map (function
      | Data(data)   -> pdfstr data
      | Kern(rawwid) -> Pdf.Integer(-rawwid)
    )
  in
    Pdf.Array(lst)
