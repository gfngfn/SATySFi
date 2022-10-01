
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

      | ImHorzRising{ width; height; depth; rising; contents = imhblst } ->
          let (evhblst, footnotelst) = iter imhblst in
          let evhb = (width, EvHorzRising{ height; depth; rising; contents = evhblst }) in
          (extH evhb, appendF footnotelst)

      | ImHorzFrame{ ratios; width; height; depth; decoration; contents = imhbs } ->
          let (evhbs, footnotelst) = iter imhbs in
          let evhb = (width, EvHorzFrame{ ratios; height; depth; decoration; contents = evhbs }) in
          (extH evhb, appendF footnotelst)

      | ImHorzInlineTabular{
          width;
          height;
          depth;
          rows = imtabular;
          column_widths;
          row_heights;
          rule_graphics;
        } ->
          let (evrowlst, _footnotelst) = embed_page_info_to_tabular pbinfo imtabular in
          ext (width, EvHorzInlineTabular{
            height;
            depth;
            rows = evrowlst;
            column_widths;
            row_heights;
            rule_graphics;
          })

      | ImHorzEmbeddedVert{ width; height; depth; contents = imvblst } ->
          let (evvblst, footnotelst) = embed_page_info_vert pbinfo imvblst in
          let evhb = (width, EvHorzEmbeddedVert{ height; depth; contents = evvblst }) in
          (extH evhb, appendF footnotelst)

      | ImHorzHookPageBreak(hookf) ->
          ext (Length.zero, EvHorzHookPageBreak(pbinfo, hookf))

      | ImHorzInlineGraphics{ width; height; depth; graphics } ->
          ext (width, EvHorzInlineGraphics{ height; depth; graphics })

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

          | ImNormalCell(ratios, info, imhblst) ->
              let (evhblst, footnotelst) = embed_page_info pbinfo imhblst in
              let evcell = EvNormalCell(ratios, info, evhblst) in
              (extC evcell, appendF footnotelst)

          | ImMultiCell(ratios, info, imhblst) ->
              let (evhblst, footnotelst) = embed_page_info pbinfo imhblst in
              let evcell = EvMultiCell(ratios, info, evhblst) in
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
      | ImVertLine(ratios, hgt, dpt, imhbs) ->
          let (imvbs, footnotelst) = embed_page_info pbinfo imhbs in
          let evvb = EvVertLine(ratios, hgt, dpt, imvbs) in
          (extV evvb, appendF footnotelst)

      | ImVertFixedEmpty(debug_margins, vskip) ->
          (extV (EvVertFixedEmpty(debug_margins, vskip)), footnoteacc)

      | ImVertFrame(pads, deco, wid, imvblst) ->
          let (evvblst, footnotelst) = embed_page_info_vert pbinfo imvblst in
          let evvb = EvVertFrame(pads, pbinfo, deco, wid, evvblst) in
          (extV evvb, appendF footnotelst)

      | ImVertHookPageBreak(hookf) ->
          let evvb = EvVertHookPageBreak(hookf) in
          (extV evvb, footnoteacc)

    ) (Alist.empty, Alist.empty)
  in
  (Alist.to_list evvbacc, Alist.to_list footnoteacc)
