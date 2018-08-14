
open MyUtil
open LengthInterface
open HorzBox
open LineBreakBox


type lb_either =
  | TextChunks  of CharBasis.break_opportunity * line_break_chunk list
  | LB          of lb_box
  | ScriptGuard of CharBasis.script * lb_box list


type lb_pure_either =
  | PTextChunks  of CharBasis.break_opportunity * line_break_chunk list
  | PLB          of lb_pure_box
  | PScriptGuard of CharBasis.script * lb_pure_box list


let ( ~. ) = float_of_int
let ( ~@ ) = int_of_float


let get_metrics (lphb : lb_pure_box) : metrics =
  match lphb with
  | LBAtom(metr, _)                           -> metr
  | LBRising(metr, _, _)                      -> metr
  | LBOuterFrame(metr, _, _)                  -> metr
  | LBFixedFrame(wid, hgt, dpt, _, _)         -> (natural wid, hgt, dpt)
  | LBEmbeddedVert(wid, hgt, dpt, _)          -> (natural wid, hgt, dpt)
  | LBFixedGraphics(wid, hgt, dpt, _)         -> (natural wid, hgt, dpt)
  | LBFixedTabular(wid, hgt, dpt, _, _, _, _) -> (natural wid, hgt, dpt)
  | LBFixedImage(wid, hgt, _)                 -> (natural wid, hgt, Length.zero)
  | LBHookPageBreak(_)                        -> (widinfo_zero, Length.zero, Length.zero)


let get_total_metrics (lphblst : lb_pure_box list) : metrics =
  lphblst @|> (widinfo_zero, Length.zero, Length.zero) @|> List.fold_left (fun (wiacc, hacc, dacc) lphb ->
    let (wi, h, d) = get_metrics lphb in
      (wiacc +%@ wi, max hacc h, min dacc d)
  )


let get_width_info (lphb : lb_pure_box) =
  let (widinfo, _, _) = get_metrics lphb in widinfo


let get_width_info_list lphblst =
  lphblst |> List.fold_left (fun wiacc lphb -> let widinfo = get_width_info lphb in wiacc +%@ widinfo) widinfo_zero


let empty_vert (widinfo : length_info) : metrics =
  (widinfo, Length.zero, Length.zero)


let append_vert_padding (metr : metrics) (pads : paddings) : metrics =
  let (widinfo, hgt, dpt) = metr in
    (widinfo, hgt +% pads.paddingT, dpt -% pads.paddingB)


let append_horz_padding (lhblst : lb_box list) (pads : paddings) =
  List.append
    (LBPure(LBAtom(empty_vert (natural pads.paddingL), EvHorzEmpty)) :: lhblst)
    (LBPure(LBAtom(empty_vert (natural pads.paddingR), EvHorzEmpty)) :: [])


let append_horz_padding_pure (lphblst : lb_pure_box list) (widinfo : length_info) (pads : paddings) =
  let lphblstnew =
    List.append
      (LBAtom(empty_vert (natural pads.paddingL), EvHorzEmpty) :: lphblst)
      (LBAtom(empty_vert (natural pads.paddingR), EvHorzEmpty) :: [])
  in
  let widinfonew =
    {
      natural     = widinfo.natural +% pads.paddingL +% pads.paddingR;
      shrinkable  = widinfo.shrinkable;
      stretchable = widinfo.stretchable;
    }
  in
    (lphblstnew, widinfonew)


let append_chunks (chunkacc : line_break_chunk Alist.t) (alwmid : CharBasis.break_opportunity) (chunklst : line_break_chunk list) =
  match Alist.chop_last chunkacc with
  | None ->
      Alist.append Alist.empty chunklst

  | Some(accsub, (ctx, chunkmain)) ->
      begin
        match chunkmain with
        | Space
        | UnbreakableSpace
            -> Alist.append chunkacc chunklst

        | IdeographicChunk(script, lbc, uch, _) ->
            let chunknew = (ctx, IdeographicChunk(script, lbc, uch, alwmid)) in
              Alist.append (Alist.extend accsub chunknew) chunklst

        | AlphabeticChunk(script, lbc1, lbc2, uchlst, _) ->
            let chunknew = (ctx, AlphabeticChunk(script, lbc1, lbc2, uchlst, alwmid)) in
              Alist.append (Alist.extend accsub chunknew) chunklst
      end


let convert_pure_box_for_line_breaking_scheme (type a) (listf : horz_box list -> lb_pure_box list) (puref : lb_pure_box -> a) (chunkf : CharBasis.break_opportunity * line_break_chunk list -> a) (alw : CharBasis.break_opportunity) (phb : pure_horz_box) : a =
  match phb with
  | PHCInnerString(ctx, uchlst) ->
      chunkf (ConvertText.to_chunks ctx uchlst alw)

  | PHCInnerMathGlyph(mathinfo, wid, hgt, dpt, otxt) ->
      puref (LBAtom((natural wid, hgt, dpt), EvHorzMathGlyph(mathinfo, hgt, dpt, otxt)))

  | PHGRising(lenrising, hblst) ->
      let lphblst = listf hblst in
      let (widinfo, hgt, dpt) = get_total_metrics lphblst in
      let hgtsub = Length.max Length.zero (hgt +% lenrising) in
      let dptsub = Length.min Length.zero (dpt +% lenrising) in
        puref (LBRising((widinfo, hgtsub, dptsub), lenrising, lphblst))

  | PHSFixedEmpty(wid) ->
      puref (LBAtom(empty_vert (natural wid), EvHorzEmpty))

  | PHSOuterEmpty(wid, widshrink, widstretch) ->
      puref (LBAtom(empty_vert { natural = wid; shrinkable = widshrink; stretchable = FiniteStretch(widstretch); }, EvHorzEmpty))

  | PHSOuterFil ->
      puref (LBAtom(empty_vert { natural = Length.zero; shrinkable = Length.zero; stretchable = Fils(1); }, EvHorzEmpty))

  | PHGOuterFrame(pads, deco, hblst) ->
      let lphblst = listf hblst in
      let (widinfo_sub, hgt, dpt) = get_total_metrics lphblst in
      let (lphblstnew, widinfo_total) = append_horz_padding_pure lphblst widinfo_sub pads in
        puref (LBOuterFrame((widinfo_total, hgt +% pads.paddingT, dpt -% pads.paddingB), deco, lphblstnew))

  | PHGInnerFrame(pads, deco, hblst) ->
      let lphblst = listf hblst in
      let (widinfo_sub, hgt, dpt) = get_total_metrics lphblst in
      let (lphblstnew, widinfo_total) = append_horz_padding_pure lphblst widinfo_sub pads in
        puref (LBFixedFrame(widinfo_total.natural, hgt +% pads.paddingT, dpt -% pads.paddingB, deco, lphblstnew))

  | PHGFixedFrame(pads, wid_req, deco, hblst) ->
      let lphblst = listf hblst in
      let (widinfo_sub, hgt, dpt) = get_total_metrics lphblst in
      let (lphblstnew, _) = append_horz_padding_pure lphblst widinfo_sub pads in
        puref (LBFixedFrame(wid_req, hgt +% pads.paddingT, dpt -% pads.paddingB, deco, lphblstnew))

  | PHGEmbeddedVert(wid, hgt, dpt, imvblst) ->
      puref (LBEmbeddedVert(wid, hgt, dpt, imvblst))

  | PHGFixedGraphics(wid, hgt, dpt, graphics) ->
      puref (LBFixedGraphics(wid, hgt, dpt, graphics))

  | PHGFixedTabular(wid, hgt, dpt, imtabular, widlst, lenlst, rulesf) ->
      puref (LBFixedTabular(wid, hgt, dpt, imtabular, widlst, lenlst, rulesf))

  | PHGFixedImage(wid, hgt, imgkey) ->
      puref (LBFixedImage(wid, hgt, imgkey))

  | PHGHookPageBreak(hookf) ->
      puref (LBHookPageBreak(hookf))


let convert_pure_box_for_line_breaking_pure listf (phb : pure_horz_box) : lb_pure_either =
  let puref p = PLB(p) in
  let chunkf (alwfirst, c) = PTextChunks(alwfirst, c) in
    convert_pure_box_for_line_breaking_scheme listf puref chunkf CharBasis.PreventBreak phb


let convert_pure_box_for_line_breaking listf alw (phb : pure_horz_box) : lb_either =
  let puref p = LB(LBPure(p)) in
  let chunkf (alwfirst, c) = TextChunks(alwfirst, c) in
    convert_pure_box_for_line_breaking_scheme listf puref chunkf alw phb


let can_break_before tail =
  match tail with
  | [] ->
      false

  | hb :: _ ->
      begin
        match hb with
        | HorzDiscretionary(_)
        | HorzEmbeddedVertBreakable(_)
            -> false

        | HorzScriptGuard(_)
        | HorzFrameBreakable(_)
            -> true

        | HorzPure(phb) ->
            begin
              match phb with
              | PHCInnerString(_)
              | PHGFixedFrame(_)
              | PHGInnerFrame(_)
              | PHGOuterFrame(_)
              | PHGEmbeddedVert(_)
              | PHGFixedGraphics(_)
              | PHGFixedTabular(_)
              | PHGFixedImage(_)
                  -> true

              | _ -> false
            end

      end


let rec convert_list_for_line_breaking (hblst : horz_box list) : lb_either list =
  let rec aux lbeacc hblst =
    match hblst with
    | [] ->
        Alist.to_list lbeacc

    | HorzDiscretionary(pnlty, hblst0, hblst1, hblst2) :: tail ->
        let lphblst0 = convert_list_for_line_breaking_pure hblst0 in
        let lphblst1 = convert_list_for_line_breaking_pure hblst1 in
        let lphblst2 = convert_list_for_line_breaking_pure hblst2 in
        let dscrid = DiscretionaryID.fresh () in
          aux (Alist.extend lbeacc (LB(LBDiscretionary(pnlty, dscrid, lphblst0, lphblst1, lphblst2)))) tail

    | HorzEmbeddedVertBreakable(wid, vblst) :: tail ->
        let dscrid = DiscretionaryID.fresh () in
          aux (Alist.extend lbeacc (LB(LBEmbeddedVertBreakable(dscrid, wid, vblst)))) tail

    | HorzPure(phb) :: tail ->
        let alw = if can_break_before tail then CharBasis.AllowBreak else CharBasis.PreventBreak in
        let lbe = convert_pure_box_for_line_breaking convert_list_for_line_breaking_pure alw phb in
          aux (Alist.extend lbeacc lbe) tail

    | HorzFrameBreakable(pads, wid1, wid2, decoS, decoH, decoM, decoT, hblst) :: tail ->
        let lbelst = convert_list_for_line_breaking hblst in
        let lhblst = normalize_chunks lbelst in
        let lhblstnew = append_horz_padding lhblst pads in
          aux (Alist.extend lbeacc (LB(LBFrameBreakable(pads, wid1, wid2, decoS, decoH, decoM, decoT, lhblstnew)))) tail

    | HorzScriptGuard(script, hblstG) :: tail ->
        let lbelstG = convert_list_for_line_breaking hblstG in
        let lhblstG = normalize_chunks lbelstG in
          aux (Alist.extend lbeacc (ScriptGuard(script, lhblstG))) tail
  in
    aux Alist.empty hblst


and convert_list_for_line_breaking_pure (hblst : horz_box list) : lb_pure_box list =
  let rec aux lbpeacc hblst =
    match hblst with
    | [] -> Alist.to_list lbpeacc

    | HorzDiscretionary(pnlty, hblst0, _, _) :: tail ->
        let lphblst0 = aux Alist.empty hblst0 in
          aux (Alist.append lbpeacc lphblst0) tail

    | HorzEmbeddedVertBreakable(wid, vblst) :: tail ->
        let imvblst = PageBreak.solidify vblst in
        let (hgt, dpt) = PageBreak.adjust_to_first_line imvblst in
          aux (Alist.extend lbpeacc (PLB(LBEmbeddedVert(wid, hgt, dpt, imvblst)))) tail

    | HorzPure(phb) :: tail ->
        let lbpe = convert_pure_box_for_line_breaking_pure convert_list_for_line_breaking_pure phb in
          aux (Alist.extend lbpeacc lbpe) tail

    | HorzFrameBreakable(pads, wid1, wid2, decoS, decoH, decoM, decoT, hblstsub) :: tail ->
        let lphblst = convert_list_for_line_breaking_pure hblstsub in
        let (widinfo_sub, hgt, dpt) = get_total_metrics lphblst in
        let (lphblstnew, widinfo_total) = append_horz_padding_pure lphblst widinfo_sub pads in
          aux (Alist.extend lbpeacc (PLB(LBOuterFrame((widinfo_total, hgt +% pads.paddingT, dpt -% pads.paddingB), decoS, lphblst)))) tail

    | HorzScriptGuard(script, hblstG) :: tail ->
        let lphblstG = convert_list_for_line_breaking_pure hblstG in
          aux (Alist.extend lbpeacc (PScriptGuard(script, lphblstG))) tail
  in
  let lbpelst = aux Alist.empty hblst in
    normalize_chunks_pure lbpelst


and normalize_chunks (lbeitherlst : lb_either list) : lb_box list =

  let rec aux lhbacc (optprev : (CharBasis.script * line_break_chunk Alist.t) option) lbeitherlst =
    match lbeitherlst with
    | [] ->
        begin
          match optprev with
          | None ->
              Alist.to_list lhbacc

          | Some((scriptB, chunkacc)) ->
              let scriptA = CharBasis.OtherScript in
              let lhblst = ConvertText.chunks_to_boxes convert_list_for_line_breaking_pure scriptB (Alist.to_list chunkacc) scriptA in
              Alist.to_list (Alist.append lhbacc lhblst)
        end

    | TextChunks(alwfirst, chunklst) :: lbeithertail ->
        begin
          match optprev with
          | None ->
              aux lhbacc (Some((CharBasis.OtherScript, Alist.of_list chunklst))) lbeithertail

          | Some((scriptB, chunkacc)) ->
              aux lhbacc (Some((scriptB, append_chunks chunkacc alwfirst chunklst))) lbeithertail
        end

    | ScriptGuard(scriptG, lhblstG) :: lbeithertail ->
        let optnext = Some(scriptG, Alist.empty) in
        begin
          match optprev with
          | None ->
              aux (Alist.append lhbacc lhblstG) optnext lbeithertail

          | Some((scriptB, chunkacc)) ->
              let scriptA = scriptG in
              let lhblstC = ConvertText.chunks_to_boxes convert_list_for_line_breaking_pure scriptB (Alist.to_list chunkacc) scriptA in
              aux (Alist.append (Alist.append lhbacc lhblstC) lhblstG) optnext lbeithertail
        end

    | LB(lhb) :: lbeithertail ->
        begin
          match optprev with
          | None ->
              aux (Alist.extend lhbacc lhb) None lbeithertail

          | Some((scriptB, chunkacc)) ->
              let scriptA = CharBasis.OtherScript in
              let lhblst = ConvertText.chunks_to_boxes convert_list_for_line_breaking_pure scriptB (Alist.to_list chunkacc) scriptA in
              aux (Alist.extend (Alist.append lhbacc lhblst) lhb) None lbeithertail
        end
  in
    aux Alist.empty None lbeitherlst


and normalize_chunks_pure (lbpelst : lb_pure_either list) : lb_pure_box list =
  let rec aux lphbacc chunkaccopt lbpelst =
    match lbpelst with
    | [] ->
        begin
          match chunkaccopt with
          | None ->
              Alist.to_list lphbacc

          | Some((scriptB, chunkacc)) ->
              let scriptA = CharBasis.OtherScript in
              let lphblst = ConvertText.chunks_to_boxes_pure scriptB (Alist.to_list chunkacc) scriptA in
              Alist.to_list (Alist.append lphbacc lphblst)
        end

    | PTextChunks(alwfirst, chunklst) :: lbpetail ->
        begin
          match chunkaccopt with
          | None ->
              aux lphbacc (Some((CharBasis.OtherScript, Alist.of_list chunklst))) lbpetail

          | Some((scriptB, chunkacc)) ->
              aux lphbacc (Some((scriptB, append_chunks chunkacc alwfirst chunklst))) lbpetail
        end

    | PLB(lphb) :: lbpetail ->
        begin
          match chunkaccopt with
          | None ->
              aux (Alist.extend lphbacc lphb) None lbpetail

          | Some((scriptB, chunkacc)) ->
              let scriptA = CharBasis.OtherScript in
              let lphblst = ConvertText.chunks_to_boxes_pure scriptB (Alist.to_list chunkacc) scriptA in
              aux (Alist.extend (Alist.append lphbacc lphblst) lphb) None lbpetail
        end

    | PScriptGuard(scriptG, lphblstG) :: lbpetail  ->
        begin
          match chunkaccopt with
          | None ->
              aux (Alist.append lphbacc lphblstG) None lbpetail

          | Some((scriptB, chunkacc)) ->
              let scriptA = scriptG in
              let lphblstC = ConvertText.chunks_to_boxes_pure scriptB (Alist.to_list chunkacc) scriptA in
              aux (Alist.append (Alist.append lphbacc lphblstC) lphblstG) None lbpetail
        end
  in
    aux Alist.empty None lbpelst


module WidthMap
: sig
    type t
    val empty : t
    val add_width_all : length_info -> t -> t
    val add : DiscretionaryID.t -> length_info -> t -> t
    val iter : (DiscretionaryID.t -> length_info -> bool ref -> unit) -> t -> unit
    val remove : DiscretionaryID.t -> t -> t
  end
= struct

    module DiscretionaryIDMap = Map.Make (DiscretionaryID)

    type t = (length_info * bool ref) DiscretionaryIDMap.t

    let empty =
      DiscretionaryIDMap.empty

    let add dscrid distinfo wmap =
      wmap |> DiscretionaryIDMap.add dscrid (distinfo, ref false)

    let iter f =
      DiscretionaryIDMap.iter (fun dscrid (distinfo, bref) -> f dscrid distinfo bref)

    let add_width_all (widinfo : length_info) (wmap : t) : t =
      wmap |> DiscretionaryIDMap.map (fun (distinfo, bref) -> (distinfo +%@ widinfo, bref))

    let remove =
      DiscretionaryIDMap.remove

  end


module LineBreakGraph = FlowGraph.Make
  (DiscretionaryID)
  (struct
    type t = pure_badness
    let show = string_of_int
    let add = ( + )
    let compare b1 b2 = b1 - b2
    let zero = 0
  end)


module RemovalSet = MutableSet.Make(DiscretionaryID)


let ratio_stretch_limit = 2.
let ratio_shrink_limit = -1.

let calculate_ratios (widrequired : length) (widinfo_total : length_info) : ratios * length =
  let widnatural = widinfo_total.natural in
  let widshrink  = widinfo_total.shrinkable in
  let stretch = widinfo_total.stretchable in
  let widdiff = widrequired -% widnatural in
    if widnatural <% widrequired then
    (* -- if the natural width is shorter than the required one; widdiff is positive -- *)
      match stretch with
      | Fils(nfil)  when nfil > 0 -> (PermissiblyLong(0.), widdiff *% (1. /. (~. nfil)))
      | Fils(_)                   -> assert false  (* -- number of fils cannot be nonpositive -- *)
      | FiniteStretch(widstretch) ->
          if Length.is_nearly_zero widstretch then
          (* -- if unable to stretch -- *)
            (TooShort, Length.zero)
          else
            let ratio_raw = widdiff /% widstretch in
            if ratio_raw >= ratio_stretch_limit then
              (TooShort, Length.zero)
            else
              (PermissiblyShort(ratio_raw), Length.zero)
    else
    (* -- if the natural width is longer than the required one; widdiff is nonpositive -- *)
      if Length.is_nearly_zero widshrink then
      (* -- if unable to shrink -- *)
        (TooLong, Length.zero)
      else
        let ratio_raw = widdiff /% widshrink in
        if ratio_raw <= ratio_shrink_limit then
          (TooLong, Length.zero)
        else
          (PermissiblyLong(ratio_raw), Length.zero)


let rec determine_widths (widreqopt : length option) (lphblst : lb_pure_box list) : intermediate_horz_box list * length * length =
  let (widinfo_total, hgt_total, dpt_total) = get_total_metrics lphblst in
  let (ratios, widperfil) =
    match widreqopt with
    | Some(widreq) -> calculate_ratios widreq widinfo_total
    | None         -> (PermissiblyShort(0.), Length.zero)
  in

  let get_intermediate_total_width imhblst =
    imhblst |> List.fold_left (fun wacc imhb ->
      match imhb with
      | ImHorz(w, _)                             -> wacc +% w
      | ImHorzRising(w, _, _, _, _)              -> wacc +% w
      | ImHorzFrame(w, _, _, _, _)               -> wacc +% w
      | ImHorzInlineTabular(w, _, _, _, _, _, _) -> wacc +% w
      | ImHorzInlineGraphics(w, _, _, _)         -> wacc +% w
      | ImHorzEmbeddedVert(w, _, _, _)           -> wacc +% w
      | ImHorzHookPageBreak(_)                   -> wacc
      | ImHorzFootnote(_)                        -> wacc
    ) Length.zero
  in

  let rec main_conversion ratios widperfil lphb : intermediate_horz_box =
    match lphb with
    | LBAtom((widinfo, _, _), evhb) ->
        begin
          match widinfo.stretchable with
          | Fils(nfil)  when nfil > 0 -> ImHorz(widinfo.natural +% widperfil, evhb)
          | Fils(_)                   -> assert false  (* -- number of fils cannot be nonpositive -- *)
          | FiniteStretch(widstretch) ->
              let widappend =
                match ratios with
                | TooLong                      -> Length.negate widinfo.shrinkable
                | PermissiblyLong(pure_ratio)  -> widinfo.shrinkable *% pure_ratio  (* -- pure_ratio is nonpositive -- *)
                | PermissiblyShort(pure_ratio) -> widstretch *% pure_ratio          (* -- pure_ratio is positive -- *)
                | TooShort                     -> widstretch
              in
                ImHorz(widinfo.natural +% widappend, evhb)
        end

    | LBRising((_, hgtsub, dptsub), lenrising, lphblstsub) ->
        let imhblst = lphblstsub |> List.map (main_conversion ratios widperfil) in
        let wid_total = get_intermediate_total_width imhblst in
          ImHorzRising(wid_total, hgtsub, dptsub, lenrising, imhblst)

    | LBOuterFrame((_, hgt_frame, dpt_frame), deco, lphblstsub) ->
        let imhblst = lphblstsub |> List.map (main_conversion ratios widperfil) in
        let wid_total = get_intermediate_total_width imhblst in
          ImHorzFrame(wid_total, hgt_frame, dpt_frame, deco, imhblst)

    | LBFixedFrame(wid_frame, hgt_frame, dpt_frame, deco, lphblstsub) ->
        let (imhblst, _, _) = determine_widths (Some(wid_frame)) lphblstsub in
          ImHorzFrame(wid_frame, hgt_frame, dpt_frame, deco, imhblst)

    | LBEmbeddedVert(wid, hgt, dpt, imvblst) ->
        ImHorzEmbeddedVert(wid, hgt, dpt, imvblst)

    | LBFixedGraphics(wid, hgt, dpt, graphics) ->
        ImHorzInlineGraphics(wid, hgt, dpt, graphics)

    | LBFixedTabular(wid, hgt, dpt, imtabular, widlst, lenlst, rulesf) ->
        ImHorzInlineTabular(wid, hgt, dpt, imtabular, widlst, lenlst, rulesf)

    | LBFixedImage(wid, hgt, imgkey) ->
        ImHorz(wid, EvHorzInlineImage(hgt, imgkey))

    | LBHookPageBreak(hookf) ->
        ImHorzHookPageBreak(hookf)
  in
      let imhblst = lphblst |> List.map (main_conversion ratios widperfil) in
(*
      (* begin : for debug *)
      let checksum = get_intermediate_total_width imhblst in
      let msg_stretch =
        match widinfo_total.stretchable with
        | FiniteStretch(widstretch) -> Length.show widstretch
        | Fils(i)                   -> "infinite * " ^ (string_of_int i)
      in
      let msg =
        match ratios with
        | TooShort                     -> "stretchable = " ^ msg_stretch ^ ", too_short"
        | PermissiblyShort(pure_ratio) -> "stretchable = " ^ msg_stretch ^ ", R = " ^ (string_of_float pure_ratio)
        | PermissiblyLong(pure_ratio)  -> "shrinkable = " ^ (Length.show widinfo_total.shrinkable) ^ ", R = " ^ (string_of_float pure_ratio)
        | TooLong                      -> "shrinkable = " ^ (Length.show widinfo_total.shrinkable) ^ ", too_long"
      in

      let () = PrintForDebug.linebreakE
        ("natural = " ^ (Length.show widinfo_total.natural) ^ ", " ^
         msg ^ ", " ^
         "checksum = " ^ (Length.show checksum)) in
      (* end : for debug *)
*)
        (imhblst, hgt_total, dpt_total)


type line_break_info = {
  is_breakable_top    : bool;
  is_breakable_bottom : bool;
  margin_top          : length;
  margin_bottom       : length;
  paragraph_width     : length;
  leading_required    : length;
  vskip_min           : length;
  min_first_ascender  : length;
  min_last_descender  : length;
}

type line_either =
  | PureLine    of lb_pure_box list
  | AlreadyVert of length * vert_box list


let break_into_lines (lbinfo : line_break_info) (path : DiscretionaryID.t list) (lhblst : lb_box list) : vert_box list =

  let calculate_vertical_skip (dptoptprev : length option) (hgt : length) : vert_box list =
    match dptoptprev with
    | Some(dptprev) ->
        let vskipraw = lbinfo.leading_required -% (Length.negate dptprev) -% hgt in
        let vskip =
          if vskipraw <% lbinfo.vskip_min then lbinfo.vskip_min else vskipraw
        in
          [VertFixedBreakable(vskip)]

    | None ->
        []
  in

  let append_framed_lines
        (lines_in_frame : line_either list)
        (acclines_before : line_either Alist.t)
        (accline_before : lb_pure_box Alist.t)
        (decoS : decoration) (decoH : decoration) (decoM : decoration) (decoT : decoration)
        (pads : paddings)
        : line_either Alist.t * lb_pure_box Alist.t =
    let rec aux
          (first : (lb_pure_box Alist.t) option)
          (last : (lb_pure_box Alist.t) option)
          (acclines : line_either Alist.t)
          (lines : line_either list)
        =
      match lines with
      | [] ->
          begin
            match last with
            | Some(accline) -> (acclines, accline)
            | None          -> (acclines, Alist.empty)
          end

      | PureLine(line) :: [] ->
          let metr_sub = get_total_metrics line in
          let metr_total = append_vert_padding metr_sub pads in
          begin
            match first with
            | Some(accline) ->  (* -- single line -- *)
                aux None (Some(Alist.extend accline (LBOuterFrame(metr_total, decoS, line)))) acclines []

            | None ->  (* -- last line -- *)
                aux None (Some(Alist.extend Alist.empty (LBOuterFrame(metr_total, decoT, line)))) acclines []
          end

      | PureLine(line) :: tail ->
          let metr_sub = get_total_metrics line in
          let metr_total = append_vert_padding metr_sub pads in
          begin
            match first with
            | Some(accline) ->  (* -- first line -- *)
                let acclinesnew =
                  Alist.extend acclines
                    (PureLine(Alist.to_list (Alist.extend accline (LBOuterFrame(metr_total, decoH, line)))))
                in
                  aux None None acclinesnew tail

            | None ->
                let acclinesnew =
                  Alist.extend acclines
                    (PureLine([LBOuterFrame(metr_total, decoM, line)]))
                in
                  aux None None acclinesnew tail
          end

      | AlreadyVert(wid, vblst) :: tail ->
          let imvblst = PageBreak.solidify vblst in
          let (hgt, dpt) = PageBreak.adjust_to_first_line imvblst in
          let metr_sub = (natural wid, hgt, dpt) in
          let metr_total = append_vert_padding metr_sub pads in
          let inner = [LBEmbeddedVert(wid, hgt, dpt, imvblst)] in
          begin
            match first with
            | Some(accline) ->  (* -- first line -- *)
                let acclinesnew =
                  Alist.extend acclines
                    (PureLine(Alist.to_list (Alist.extend accline (LBOuterFrame(metr_total, decoH, inner)))))
                in
                  aux None None acclinesnew tail

            | None ->
                let acclinesnew =
                  Alist.extend acclines
                    (PureLine([LBOuterFrame(metr_total, decoM, inner)]))
                in
                  aux None None acclinesnew tail
          end
    in
      aux (Some(accline_before)) None acclines_before lines_in_frame
  in

  (* --
     cut: cuts inline box list into lines based on the break point list `path`
     -- *)
  let rec cut (acclines : line_either Alist.t) (accline : lb_pure_box Alist.t) (lhblst : lb_box list) : line_either Alist.t =
    match lhblst with
    | LBDiscretionary(_, dscrid, lphblst0, lphblst1, lphblst2) :: tail ->
        if List.mem dscrid path then
          let acclinesub = Alist.append accline lphblst1 in
          let acclinefresh = Alist.of_list lphblst2 in
            cut (Alist.extend acclines (PureLine(Alist.to_list acclinesub))) acclinefresh tail
        else
          let acclinenew = Alist.append accline lphblst0 in
            cut acclines acclinenew tail

    | LBDiscretionaryList(_, lphblst0, dscrlst) :: tail ->
        begin
          match dscrlst |> List.find_opt (fun (dscrid, _, _) -> List.mem dscrid path) with
          | None ->
(*
              Format.printf "LineBreak> None\n";  (* for debug *)
*)
              let acclinenew = Alist.append accline lphblst0 in
                cut acclines acclinenew tail

          | Some((dscrid, lphblst1, lphblst2)) ->
(*
              Format.printf "LineBreak> Some(%s)\n" (DiscretionaryID.show dscrid);  (* for debug *)
*)
              let acclinesub = Alist.append accline lphblst1 in
              let acclinefresh = Alist.of_list lphblst2 in
                cut (Alist.extend acclines (PureLine(Alist.to_list acclinesub))) acclinefresh tail
        end

    | LBEmbeddedVertBreakable(dscrid, wid, vblst) :: tail ->
        if not (List.mem dscrid path) then
          assert false
        else
          let acclinesnew =
            Alist.extend (Alist.extend acclines (PureLine(Alist.to_list accline))) (AlreadyVert(wid, vblst))
          in
            cut acclinesnew Alist.empty tail

    | LBPure(lphb) :: tail ->
        cut acclines (Alist.extend accline lphb) tail

    | LBFrameBreakable(pads, wid1, wid2, decoS, decoH, decoM, decoT, lhblst) :: tail ->
        let acclines_in_frame = cut Alist.empty Alist.empty lhblst in
        let (acclinesnew, acclinenew) =
          append_framed_lines (Alist.to_list acclines_in_frame) acclines accline decoS decoH decoM decoT pads
        in
          cut acclinesnew acclinenew tail

    | [] ->
        Alist.extend acclines (PureLine(Alist.to_list accline))
  in


  (*  --
      arrange: inserts vertical spaces between lines and top/bottom margins
      -- *)
  let rec arrange (prevopt : (length option * length) option) (accvlines : vert_box Alist.t) (lines : line_either list) =
    match lines with
    | PureLine(line) :: tail ->
        let (evhblst, hgt, dpt) = determine_widths (Some(lbinfo.paragraph_width)) line in
        begin
          match prevopt with
          | None ->
            (* -- first line -- *)
              let margin_top =
                lbinfo.margin_top +% (Length.max Length.zero (lbinfo.min_first_ascender -% hgt))
              in
              arrange (Some(Some(dpt), margin_top)) (Alist.extend Alist.empty (VertLine(hgt, dpt, evhblst))) tail

          | Some(dptoptprev, margin_top) ->
              let vblstskip = calculate_vertical_skip dptoptprev hgt in
                arrange (Some(Some(dpt), margin_top)) (Alist.append accvlines (List.append vblstskip [VertLine(hgt, dpt, evhblst)])) tail
        end

    | AlreadyVert(_, vblst) :: tail ->
        begin
          match prevopt with
          | None ->
            (* -- first line -- *)
              let opt = Some(None, lbinfo.margin_top) in
                arrange opt (Alist.append accvlines vblst) tail

          | Some(_, margin_top) ->
              let opt = Some(None, margin_top) in
                arrange opt (Alist.append accvlines vblst) tail
        end

    | [] ->
        let vbtop =
          match prevopt with
          | Some(_, margin_top) -> VertTopMargin(lbinfo.is_breakable_top, margin_top)
          | None                -> VertTopMargin(lbinfo.is_breakable_top, lbinfo.margin_top)
        in
        let vbbottom =
          let margin_bottom =
            match prevopt with
            | Some(Some(dpt), _)   -> lbinfo.margin_bottom +% (Length.max Length.zero (lbinfo.min_last_descender -% (Length.negate dpt)))
            | Some(None, _) | None -> lbinfo.margin_bottom
          in
            VertBottomMargin(lbinfo.is_breakable_bottom, margin_bottom)
        in
          vbtop :: (Alist.to_list (Alist.extend accvlines vbbottom))
  in

  let acclines = cut Alist.empty Alist.empty lhblst in
    arrange None Alist.empty (Alist.to_list acclines)


let natural (hblst : horz_box list) : intermediate_horz_box list * length * length =
  let lphblst = convert_list_for_line_breaking_pure hblst in
    determine_widths None lphblst


let fit (hblst : horz_box list) (widreq : length) : intermediate_horz_box list * length * length =
  let lphblst = convert_list_for_line_breaking_pure hblst in
    determine_widths (Some(widreq)) lphblst


let main (is_breakable_top : bool) (is_breakable_bottom : bool) (margin_top : length) (margin_bottom : length) (ctx : context_main) (hblst : horz_box list) : vert_box list =

  let paragraph_width = ctx.paragraph_width in
  let leading_required = ctx.leading in
  let vskip_min = ctx.min_gap_of_lines in

  let calculate_badness pure_ratio =
    (abs (int_of_float (pure_ratio ** 3.))) * 10000
  in

  let badness_for_too_long = 100000 in

  let grph = LineBreakGraph.create () in

  let htomit : RemovalSet.t = RemovalSet.create 32 in

  let found_candidate = ref false in

  (* --
     '(b, wmapsub) = update_graph wmap did wi p ()' adds a new break candidate point 'did' to the graph 'grph' as a vertex, and returns 'b' (whether 'did' is reachable from other candidate(s)) and the updated width map 'wmapsub'.
     'wi' is a width_info from the current position, and 'p' is the penalty for breaking at 'did'.
  -- *)
  let update_graph (wmap : WidthMap.t) (dscridto : DiscretionaryID.t) (widinfobreak : length_info) (pnltybreak : pure_badness) () : bool * WidthMap.t =
    begin
      LineBreakGraph.add_vertex grph dscridto;
      found_candidate := false;
      RemovalSet.clear htomit;
      wmap |> WidthMap.iter (fun dscridfrom widinfofrom is_already_too_long ->
        let (ratios, _) = calculate_ratios paragraph_width (widinfofrom +%@ widinfobreak) in
          match ratios with
          | PermissiblyLong(pure_ratio)
          | PermissiblyShort(pure_ratio)
            ->
              let badness = calculate_badness pure_ratio in
              begin
                found_candidate := true;
                LineBreakGraph.add_edge grph dscridfrom dscridto (badness + pnltybreak);
              end

          | TooShort -> ()

          | TooLong ->
              if !is_already_too_long then
              (* -- if this is the second time of experiencing 'TooLong' about 'dscridfrom' -- *)
                begin
                  RemovalSet.add htomit dscridfrom;
                end
              else
                begin
                  is_already_too_long := true;
                  found_candidate := true;
                  LineBreakGraph.add_edge grph dscridfrom dscridto badness_for_too_long;
                end

      );
      (!found_candidate, RemovalSet.fold (fun dscrid wm -> wm |> WidthMap.remove dscrid) htomit wmap)
    end
  in

  let rec aux (iterdepth : int) (wmap : WidthMap.t) (lhblst : lb_box list) =
    match lhblst with
    | LBDiscretionary(pnlty, dscrid, lphblst0, lphblst1, lphblst2) :: tail ->
        let widinfo0 = get_width_info_list lphblst0 in
        let widinfo1 = get_width_info_list lphblst1 in
        let widinfo2 = get_width_info_list lphblst2 in
        let (found, wmapsub) = update_graph wmap dscrid widinfo1 pnlty () in
        let wmapnew =
          if found then
            wmapsub |> WidthMap.add_width_all widinfo0 |> WidthMap.add dscrid widinfo2
          else
            wmapsub |> WidthMap.add_width_all widinfo0
        in
          aux iterdepth wmapnew tail

    | LBDiscretionaryList(pnlty, lphblst0, dscrlst) :: tail ->
        let widinfo0 = get_width_info_list lphblst0 in
        let (wmapsub, foundpairacc) =
          dscrlst |> List.fold_left (fun (wmap, foundpairacc) (dscrid, lphblst1, lphblst2) ->
            let widinfo1 = get_width_info_list lphblst1 in
            let widinfo2 = get_width_info_list lphblst2 in
            let (found, wmapsub) = update_graph wmap dscrid widinfo1 pnlty () in
            if found then
              (wmapsub, Alist.extend foundpairacc (dscrid, widinfo2))
            else
              (wmapsub, foundpairacc)
          ) (wmap, Alist.empty)
        in
        let wmapall = wmapsub |> WidthMap.add_width_all widinfo0 in
        let wmapnew =
          foundpairacc |> Alist.to_list |> List.fold_left (fun wmap (dscrid, widinfo2) ->
            wmap |> WidthMap.add dscrid widinfo2
          ) wmapall
        in
          aux iterdepth wmapnew tail

    | LBEmbeddedVertBreakable(dscrid, _, _) :: tail ->
        let (_, _) = update_graph wmap dscrid widinfo_zero 0 () in
        let wmapnew = WidthMap.empty |> WidthMap.add dscrid widinfo_zero in
          aux iterdepth wmapnew tail

    | LBPure(lphb) :: tail ->
        let widinfo = get_width_info lphb in
        let wmapnew = wmap |> WidthMap.add_width_all widinfo in
          aux iterdepth wmapnew tail

    | LBFrameBreakable(pads, wid1, wid2, _, _, _, _, lhblstsub) :: tail ->
        let wmapsub = aux (iterdepth + 1) wmap lhblstsub in
          aux iterdepth wmapsub tail

    | [] ->
        if iterdepth = 0 then
          let dscrid = DiscretionaryID.final in
          let (_, wmapfinal) = update_graph wmap dscrid widinfo_zero 0 () in
            wmapfinal
        else
          wmap
  in

  let wmapinit = WidthMap.empty |> WidthMap.add DiscretionaryID.beginning widinfo_zero in
  begin
    DiscretionaryID.initialize ();
    LineBreakGraph.add_vertex grph DiscretionaryID.beginning;
    let lbelst = convert_list_for_line_breaking hblst in
    let lhblst = normalize_chunks lbelst in
    let _ (* wmapfinal *) = aux 0 wmapinit lhblst in
    let pathopt = LineBreakGraph.shortest_path grph DiscretionaryID.beginning DiscretionaryID.final in
      match pathopt with
      | None ->
        (* -- when no set of discretionary points is suitable for line breaking -- *)
          Format.printf "LineBreak> UNREACHABLE\n";  (* for debug *)
          let (imhblst, hgt, dpt) = natural hblst in
            [
              VertTopMargin(is_breakable_top, margin_top);
              VertLine(hgt, dpt, imhblst);
              VertBottomMargin(is_breakable_bottom, margin_bottom);
            ]

      | Some(path) ->
          break_into_lines {
            is_breakable_top;
            is_breakable_bottom;
            margin_top;
            margin_bottom;
            paragraph_width;
            leading_required;
            vskip_min;
            min_first_ascender = ctx.min_first_line_ascender;
            min_last_descender = ctx.min_last_line_descender;
          } path lhblst
  end


let get_metrics_of_horz_box (hblst : horz_box list) : length_info * length * length =
  let lphblst = convert_list_for_line_breaking_pure hblst in
    get_total_metrics lphblst


let get_natural_metrics (hblst : horz_box list) : length * length * length =
  let (widinfo, hgt, dpt) = get_metrics_of_horz_box hblst in
    (widinfo.natural, hgt, dpt)
