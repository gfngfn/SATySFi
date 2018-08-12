
type element =
  | Data  of string
  | Kern  of int  (* -- negative if two characters are closer -- *)
  | Raise of (int * int) * int * string

type t = element Alist.t


let pp fmt _ =
  Format.fprintf fmt "<output-text>"


let empty_hex_style = Alist.empty


let append_kern otxt rawwid =
  Alist.extend otxt (Kern(rawwid))


let append_glyph_synthesis otxt (FontFormat.PerMille(w)) (gid, markinfolst) =
  let data = FontFormat.hex_of_glyph_id gid in
  let otxt = Alist.extend otxt (Data(data)) in
  match markinfolst with
  | [] ->
      otxt

  | _ :: _ ->
      let otxt = Alist.extend otxt (Kern(-w)) in
      let otxt =
        markinfolst |> List.fold_left (fun otxt (FontFormat.Mark(gidmark, FontFormat.PerMille(wmark), v)) ->
          let (FontFormat.PerMille(x), FontFormat.PerMille(y)) = v in
          let data = FontFormat.hex_of_glyph_id gidmark in
          Alist.extend otxt (Raise((x, y), wmark, data))
        ) otxt
      in
      let otxt = Alist.extend otxt (Kern(w)) in
      otxt


let backward rawwid =
  Pdf.Integer(rawwid)


let forward rawwid =
  backward (-rawwid)


let to_TJ_arguments otxt : (FontFormat.per_mille option * Pdf.pdfobject list) list =
  let (tjargacc, acc) =
    otxt |> Alist.to_list |> List.fold_left (fun (tjargacc, acc) elem ->
      match elem with
      | Data(data) ->
          (tjargacc, Alist.extend acc (Pdf.StringHex(data)))

      | Kern(rawwid) ->
          (tjargacc, Alist.extend acc (forward rawwid))

      | Raise((x, y), w, data) ->
          let tjarg =
            (Some(FontFormat.PerMille(y)), [forward x; Pdf.StringHex(data); backward (x + w)])
          in
          let tjargaccnew =
            match Alist.to_list acc with
            | []     -> Alist.extend tjargacc tjarg
            | _ :: _ -> Alist.extend (Alist.extend tjargacc (None, Alist.to_list acc)) tjarg
          in
          (tjargaccnew, Alist.empty)

    ) (Alist.empty, Alist.empty)
  in
  match Alist.to_list acc with
  | []     -> Alist.to_list tjargacc
  | _ :: _ -> Alist.to_list (Alist.extend tjargacc (None, Alist.to_list acc))
