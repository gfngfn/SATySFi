
open HorzBox


(* --
   'embed_page_info pbinfo imhblst'
   associates 'ImHorzHookPageBreak(...)' in 'imhblst' with 'pbinfo', and
   returns footnotes in 'imhblst'
   -- *)

let rec embed_page_info (pbinfo : page_break_info) (imhblst : intermediate_horz_box list) : evaled_horz_box list * (intermediate_vert_box list) list =
  let iter = embed_page_info pbinfo in
  let (evhbacc, footnoteacc) =
    imhblst |> List.fold_left (fun (evhbacc, footnoteacc) imhb ->
      let extH = Alist.extend evhbacc in
      let appendF = Alist.append footnoteacc in
      let ext evhb = (extH evhb, footnoteacc) in
      match imhb with
      | ImHorz(evhb) ->
          ext evhb

      | ImHorzRising(wid, hgt, dpt, lenrising, imhblst) ->
          let (evhblst, footnotelst) = iter imhblst in
          let evhb = (wid, EvHorzRising(hgt, dpt, lenrising, evhblst)) in
          (extH evhb, appendF footnotelst)

      | ImHorzFrame(wid, hgt, dpt, deco, imhblst) ->
          let (evhblst, footnotelst) = iter imhblst in
          let evhb = (wid, EvHorzFrame(hgt, dpt, deco, evhblst)) in
          (extH evhb, appendF footnotelst)

      | ImHorzInlineTabular(wid, hgt, dpt, imtabular, widlst, lenlst, rulesf) ->
          let (evrowlst, footnotelst) = embed_page_info_to_tabular pbinfo imtabular in
          ext (wid, EvHorzInlineTabular(hgt, dpt, evrowlst, widlst, lenlst, rulesf))

      | ImHorzEmbeddedVert(wid, hgt, dpt, imvblst) ->
          let (evvblst, footnotelst) = embed_page_info_vert pbinfo imvblst in
          let evhb = (wid, EvHorzEmbeddedVert(hgt, dpt, evvblst)) in
          (extH evhb, appendF footnotelst)

      | ImHorzHookPageBreak(hookf) ->
          ext (Length.zero, EvHorzHookPageBreak(pbinfo, hookf))

      | ImHorzInlineGraphics(wid, hgt, dpt, graphics) ->
          ext (wid, EvHorzInlineGraphics(hgt, dpt, graphics))

      | ImHorzFootnote(imvblst) ->
          (evhbacc, Alist.extend footnoteacc imvblst)

    ) (Alist.empty, Alist.empty)
  in
  (Alist.to_list evhbacc, Alist.to_list footnoteacc)


and embed_page_info_to_tabular (pbinfo : page_break_info) (imtabular : intermediate_row list) : evaled_row list * (intermediate_vert_box list) list =
  let (evrowacc, footnoteacc) =
    imtabular |> List.fold_left (fun (evrowacc, footnoteacc) (widtotal, imcelllst) ->
      let (evcellacc, footnoteacc) =
        imcelllst |> List.fold_left (fun (evcellacc, footnoteacc) imcell ->
          let extC = Alist.extend evcellacc in
          let appendF = Alist.append footnoteacc in
          match imcell with
          | ImEmptyCell(len) ->
              (extC (EvEmptyCell(len)), footnoteacc)

          | ImNormalCell(info, imhblst) ->
              let (evhblst, footnotelst) = embed_page_info pbinfo imhblst in
              let evcell = EvNormalCell(info, evhblst) in
              (extC evcell, appendF footnotelst)

          | ImMultiCell(info, imhblst) ->
              let (evhblst, footnotelst) = embed_page_info pbinfo imhblst in
              let evcell = EvMultiCell(info, evhblst) in
              (extC evcell, appendF footnotelst)

        ) (Alist.empty, footnoteacc)
      in
      (Alist.extend evrowacc (widtotal, Alist.to_list evcellacc), footnoteacc)
    ) (Alist.empty, Alist.empty)
  in
  (Alist.to_list evrowacc, Alist.to_list footnoteacc)


and embed_page_info_vert (pbinfo : page_break_info) (imvblst : intermediate_vert_box list) : evaled_vert_box list * (intermediate_vert_box list) list =
  let (evvbacc, footnoteacc) =
    imvblst |> List.fold_left (fun (evvbacc, footnoteacc) imvb ->
      let extV = Alist.extend evvbacc in
      let appendF = Alist.append footnoteacc in
      match imvb with
      | ImVertLine(hgt, dpt, imhblst) ->
          let (imvblst, footnotelst) = embed_page_info pbinfo imhblst in
          let evvb = EvVertLine(hgt, dpt, imvblst) in
          (extV evvb, appendF footnotelst)

      | ImVertFixedEmpty(vskip) ->
          (extV (EvVertFixedEmpty(vskip)), footnoteacc)

      | ImVertFrame(pads, deco, wid, imvblst) ->
          let (evvblst, footnotelst) = embed_page_info_vert pbinfo imvblst in
          let evvb = EvVertFrame(pads, pbinfo, deco, wid, evvblst) in
          (extV evvb, appendF footnotelst)

    ) (Alist.empty, Alist.empty)
  in
  (Alist.to_list evvbacc, Alist.to_list footnoteacc)
