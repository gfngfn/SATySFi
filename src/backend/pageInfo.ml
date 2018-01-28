
open HorzBox


let rec embed_page_info (pbinfo : page_break_info) (imhblst : intermediate_horz_box list) : evaled_horz_box list =
  let iter = embed_page_info pbinfo in
    imhblst |> List.map (function
      | ImHorz(evhb)                                    -> evhb
      | ImHorzRising(wid, hgt, dpt, lenrising, imhblst) -> (wid, EvHorzRising(hgt, dpt, lenrising, iter imhblst))
      | ImHorzFrame(wid, hgt, dpt, deco, imhblst)       -> (wid, EvHorzFrame(hgt, dpt, deco, iter imhblst))
      | ImHorzInlineTabular(wid, hgt, dpt, imtabular)   -> (wid, EvHorzInlineTabular(hgt, dpt, embed_page_info_to_tabular pbinfo imtabular))
      | ImHorzEmbeddedVert(wid, hgt, dpt, imvblst)      -> (wid, EvHorzEmbeddedVert(hgt, dpt, embed_page_info_vert pbinfo imvblst))
      | ImHorzHookPageBreak(hookf)                      -> (Length.zero, EvHorzHookPageBreak(pbinfo, hookf))
      | ImHorzInlineGraphics(wid, hgt, dpt, graphics)   -> (wid, EvHorzInlineGraphics(hgt, dpt, graphics))
    )

and embed_page_info_to_tabular (pbinfo : page_break_info) (imtabular : intermediate_row list) : evaled_row list =
  imtabular |> List.map (fun (widtotal, imcelllst) ->
    let evcelllst =
      imcelllst |> List.map (function
        | ImEmptyCell(len)            -> EvEmptyCell(len)
        | ImNormalCell(info, imhblst) -> EvNormalCell(info, embed_page_info pbinfo imhblst)
        | ImMultiCell(info, imhblst)  -> EvMultiCell(info, embed_page_info pbinfo imhblst)
      )
    in
      (widtotal, evcelllst)
  )

and embed_page_info_vert (pbinfo : page_break_info) (imvblst : intermediate_vert_box list) : evaled_vert_box list =
  imvblst |> List.map (function
    | ImVertLine(hgt, dpt, imhblst)         -> EvVertLine(hgt, dpt, embed_page_info pbinfo imhblst)
    | ImVertFixedEmpty(vskip)               -> EvVertFixedEmpty(vskip)
    | ImVertFrame(pads, deco, wid, imvblst) -> EvVertFrame(pads, pbinfo, deco, wid, embed_page_info_vert pbinfo imvblst)
  )
