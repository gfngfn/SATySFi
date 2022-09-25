
open MyUtil
open LengthInterface
open HorzBox
open LineBreakBox


type lb_either =
  | TextChunks  of CharBasis.break_opportunity * line_break_chunk list
  | LB          of lb_box
  | ScriptGuard of CharBasis.script * CharBasis.script * lb_box list


type lb_pure_either =
  | PTextChunks  of CharBasis.break_opportunity * line_break_chunk list
  | PLB          of lb_pure_box
  | PScriptGuard of CharBasis.script * CharBasis.script * lb_pure_box list

type internal_ratios =
  | LBTooLong     of length
  | LBPermissible of float
  | LBTooShort    of length


let ( ~. ) = float_of_int
let ( ~@ ) = int_of_float


let get_metrics (lphb : lb_pure_box) : metrics =
  match lphb with
  | LBAtom{ metrics }                         -> metrics
  | LBRising{ metrics }                       -> metrics
  | LBOuterFrame{ metrics }                   -> metrics
  | LBFixedFrame{ width; height; depth }      -> (natural width, height, depth)
  | LBEmbeddedVert{ width; height; depth }    -> (natural width, height, depth)
  | LBFixedGraphics{ width; height; depth }   -> (natural width, height, depth)
  | LBFixedTabular{ width; height; depth }    -> (natural width, height, depth)
  | LBFixedImage{ width; height}              -> (natural width, height, Length.zero)

  | LBOuterFilGraphics{ height; depth } ->
      let widinfo =
        {
          natural     = Length.zero;
          shrinkable  = Length.zero;
          stretchable = Fils(1);
        }
      in
      (widinfo, height, depth)

  | LBHookPageBreak(_)
  | LBFootnote(_) ->
      (widinfo_zero, Length.zero, Length.zero)


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
  let metrics1 = empty_vert (natural pads.paddingL) in
  let metrics2 = empty_vert (natural pads.paddingR) in
  List.append
    (LBPure(LBAtom{ metrics = metrics1; main = EvHorzEmpty }) :: lhblst)
    (LBPure(LBAtom{ metrics = metrics2; main = EvHorzEmpty }) :: [])


let append_horz_padding_pure (lphblst : lb_pure_box list) (widinfo : length_info) (pads : paddings) =
  let metrics1 = empty_vert (natural pads.paddingL) in
  let metrics2 = empty_vert (natural pads.paddingR) in
  let lphblstnew =
    List.append
      (LBAtom{ metrics = metrics1; main = EvHorzEmpty } :: lphblst)
      (LBAtom{ metrics = metrics2; main = EvHorzEmpty } :: [])
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


let convert_pure_box_for_line_breaking_scheme (type a) (listf : horz_box list -> lb_pure_box list) (puref : lb_pure_box -> a) (chunkf : CharBasis.break_opportunity * line_break_chunk list -> a) (alwlast : CharBasis.break_opportunity) (phb : pure_horz_box) : a =
  match phb with
  | PHCInnerString{ context = ctx; chars = uchs } ->
      chunkf (ConvertText.to_chunks ctx uchs alwlast)

  | PHCInnerMathGlyph{
      info   = mathinfo;
      width  = wid;
      height = hgt;
      depth  = dpt;
      output = otxt;
    } ->
      let metrics = (natural wid, hgt, dpt) in
      puref (LBAtom{ metrics; main = EvHorzMathGlyph(mathinfo, hgt, dpt, otxt) })

  | PHGRising{ rising = len_rising; inner = hbs0 } ->
      let lphbs0 = listf hbs0 in
      let (widinfo0, hgt0, dpt0) = get_total_metrics lphbs0 in
      let hgt = Length.max Length.zero (hgt0 +% len_rising) in
      let dpt = Length.min Length.zero (dpt0 +% len_rising) in
      puref (LBRising{ metrics = (widinfo0, hgt, dpt); rising = len_rising; contents = lphbs0 })

  | PHSFixedEmpty{ width = wid } ->
      let metrics = empty_vert (natural wid) in
      puref (LBAtom{ metrics; main = EvHorzEmpty })

  | PHSOuterEmpty{ natural; shrinkable; stretchable = wid_stretch } ->
      let metrics = empty_vert { natural; shrinkable; stretchable = FiniteStretch(wid_stretch); } in
      puref (LBAtom{ metrics; main = EvHorzEmpty })

  | PHSOuterFil ->
      let metrics = empty_vert { natural = Length.zero; shrinkable = Length.zero; stretchable = Fils(1); } in
      puref (LBAtom{ metrics; main = EvHorzEmpty })

  | PHGOuterFrame{ paddings = pads; decoration; inner = hbs } ->
      let lphbs = listf hbs in
      let (widinfo_sub, hgt, dpt) = get_total_metrics lphbs in
      let (_lphbs_new, widinfo_total) = append_horz_padding_pure lphbs widinfo_sub pads in
      let metrics = (widinfo_total, hgt +% pads.paddingT, dpt -% pads.paddingB) in
      puref (LBOuterFrame{ metrics; decoration; contents = lphbs })
        (* TODO: doubtful; maybe should use `lphbs_new` for `contents` *)

  | PHGInnerFrame{ paddings = pads; decoration = deco; inner = hbs } ->
      let lphbs = listf hbs in
      let (widinfo_sub, hgt, dpt) = get_total_metrics lphbs in
      let (lphbs_new, widinfo_total) = append_horz_padding_pure lphbs widinfo_sub pads in
      puref (LBFixedFrame{
        width      = widinfo_total.natural;
        height     = hgt +% pads.paddingT;
        depth      = dpt -% pads.paddingB;
        decoration = deco;
        contents   = lphbs_new;
      })

  | PHGFixedFrame{ required_width = wid_req; paddings = pads; decoration = deco; inner = hbs } ->
      let lphbs = listf hbs in
      let (widinfo_sub, hgt, dpt) = get_total_metrics lphbs in
      let (lphbs_new, _) = append_horz_padding_pure lphbs widinfo_sub pads in
      puref (LBFixedFrame{
        width      = wid_req;
        height     = hgt +% pads.paddingT;
        depth      = dpt -% pads.paddingB;
        decoration = deco;
        contents   = lphbs_new;
      })

  | PHGEmbeddedVert{ width; height; depth; contents } ->
      puref (LBEmbeddedVert{ width; height; depth; contents })

  | PHGFixedGraphics{ width; height; depth; graphics } ->
      puref (LBFixedGraphics{ width; height; depth; graphics })

  | PHGOuterFilGraphics{ height; depth; graphics } ->
      puref (LBOuterFilGraphics{ height; depth; graphics })

  | PHGFixedTabular{ width; height; depth; rows; column_widths; lengths; rule_graphics } ->
      puref (LBFixedTabular{ width; height; depth; rows; column_widths; lengths; rule_graphics })

  | PHGFixedImage{ width; height; key } ->
      puref (LBFixedImage{ width; height; key })

  | PHGHookPageBreak(hookf) ->
      puref (LBHookPageBreak(hookf))

  | PHGFootnote(imvblst) ->
      puref (LBFootnote(imvblst))


let convert_pure_box_for_line_breaking_pure listf (phb : pure_horz_box) : lb_pure_either =
  let puref p = PLB(p) in
  let chunkf (alwfirst, c) = PTextChunks(alwfirst, c) in
    convert_pure_box_for_line_breaking_scheme listf puref chunkf CharBasis.PreventBreak phb


let convert_pure_box_for_line_breaking listf alwlast (phb : pure_horz_box) : lb_either =
  let puref p = LB(LBPure(p)) in
  let chunkf (alwfirst, c) = TextChunks(alwfirst, c) in
  convert_pure_box_for_line_breaking_scheme listf puref chunkf alwlast phb


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
        | HorzOmitSkipAfter
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


let is_whitespace_character (uch : Uchar.t) : bool =
  match LineBreakDataMap.find uch with
  | SP | INBR -> true
  | _         -> false
     (* --
        needs re-consideration:
        it may be better to use the criterion of whether
        the general category of the given character is `Zs` or not.
        -- *)


let rec omit_space_uchars (uchlst : Uchar.t list) : Uchar.t list =
  match uchlst with
  | []             -> []
  | uch :: uchtail -> if is_whitespace_character uch then omit_space_uchars uchtail else uchlst


let rec omit_skips (hblst : horz_box list) : horz_box list =
  match hblst with
  | HorzPure(phb) :: tail ->
      begin
        match phb with
        | PHSOuterEmpty(_)
        | PHSOuterFil
        | PHSFixedEmpty(_) ->
            omit_skips tail

        | PHCInnerString{ context = ctxmain; chars = uchlst } ->
            begin
              match omit_space_uchars uchlst with
              | []     -> omit_skips tail
              | uchlst -> HorzPure(PHCInnerString{ context = ctxmain; chars = uchlst }) :: tail
            end

        | _ ->
            hblst
      end

  | _ ->
      hblst


let rec convert_list_for_line_breaking (hblst : horz_box list) : lb_either list =
  (* Use explicit recursion (instead of `List.fold_left` etc.) for handling `HorzOmitSkipAfter` *)
  let rec aux lbeacc hblst =
    match hblst with
    | [] ->
        Alist.to_list lbeacc

    | HorzDiscretionary{ penalty; no_break = hbs0; pre = hbs1; post = hbs2 } :: tail ->
        let lphbs0 = convert_list_for_line_breaking_pure hbs0 in
        let lphbs1 = convert_list_for_line_breaking_pure hbs1 in
        let lphbs2 = convert_list_for_line_breaking_pure hbs2 in
        let dscrid = DiscretionaryID.fresh () in
        aux (Alist.extend lbeacc (LB(LBDiscretionary(penalty, dscrid, lphbs0, lphbs1, lphbs2)))) tail

    | HorzEmbeddedVertBreakable{ width = wid; contents = vbs } :: tail ->
        let dscrid = DiscretionaryID.fresh () in
        aux (Alist.extend lbeacc (LB(LBEmbeddedVertBreakable(dscrid, wid, vbs)))) tail

    | HorzPure(phb) :: tail ->
        let alwlast = if can_break_before tail then CharBasis.AllowBreak else CharBasis.PreventBreak in
        let lbe = convert_pure_box_for_line_breaking convert_list_for_line_breaking_pure alwlast phb in
        aux (Alist.extend lbeacc lbe) tail

    | HorzFrameBreakable{
        paddings              = pads;
        decoration_standalone = decoS;
        decoration_head       = decoH;
        decoration_middle     = decoM;
        decoration_tail       = decoT;
        contents              = hbs;
      } :: tail ->
        let lbes = convert_list_for_line_breaking hbs in
        let lhbs = normalize_chunks lbes in
        let lhbs_new = append_horz_padding lhbs pads in
        aux (Alist.extend lbeacc (LB(LBFrameBreakable(pads, decoS, decoH, decoM, decoT, lhbs_new)))) tail

    | HorzScriptGuard(scriptL, scriptR, hblstG) :: tail ->
        let lbelstG = convert_list_for_line_breaking hblstG in
        let lhblstG = normalize_chunks lbelstG in
        aux (Alist.extend lbeacc (ScriptGuard(scriptR, scriptL, lhblstG))) tail

    | HorzOmitSkipAfter :: tail ->
        let tail = omit_skips tail in
        aux lbeacc tail
  in
  aux Alist.empty hblst


and convert_list_for_line_breaking_pure (hblst : horz_box list) : lb_pure_box list =
  let rec aux lbpeacc hblst =
    match hblst with
    | [] -> Alist.to_list lbpeacc

    | HorzDiscretionary{ no_break = hbs0 } :: tail ->
        let lphbs0 = aux Alist.empty hbs0 in
        aux (Alist.append lbpeacc lphbs0) tail

    | HorzEmbeddedVertBreakable{ width; contents = vbs } :: tail ->
        let imvbs = PageBreak.solidify vbs in
        let (height, depth) = PageBreak.adjust_to_first_line imvbs in
        aux (Alist.extend lbpeacc (PLB(LBEmbeddedVert{ width; height; depth; contents = imvbs }))) tail

    | HorzPure(phb) :: tail ->
        let lbpe = convert_pure_box_for_line_breaking_pure convert_list_for_line_breaking_pure phb in
        aux (Alist.extend lbpeacc lbpe) tail

    | HorzFrameBreakable{
        paddings              = pads;
        decoration_standalone = decoS;
        decoration_head       = decoH;
        decoration_middle     = decoM;
        decoration_tail       = decoT;
        contents              = hbs0;
      } :: tail ->
        let lphbs0 = convert_list_for_line_breaking_pure hbs0 in
        let (widinfo0, hgt0, dpt0) = get_total_metrics lphbs0 in
        let (lphbs, widinfo_total) = append_horz_padding_pure lphbs0 widinfo0 pads in
        let metrics = (widinfo_total, hgt0 +% pads.paddingT, dpt0 -% pads.paddingB) in
        aux (Alist.extend lbpeacc (PLB(LBOuterFrame{ metrics; decoration = decoS; contents = lphbs }))) tail

    | HorzScriptGuard(scriptL, scriptR, hblstG) :: tail ->
        let lphblstG = convert_list_for_line_breaking_pure hblstG in
        aux (Alist.extend lbpeacc (PScriptGuard(scriptL, scriptR, lphblstG))) tail

    | HorzOmitSkipAfter :: tail ->
        let tail = omit_skips tail in
        aux lbpeacc tail
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

    | ScriptGuard(scriptL, scriptR, lhblstG) :: lbeithertail ->
        let optnext = Some(scriptR, Alist.empty) in
        begin
          match optprev with
          | None ->
              aux (Alist.append lhbacc lhblstG) optnext lbeithertail

          | Some((scriptB, chunkacc)) ->
              let scriptA = scriptL in
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

    | PScriptGuard(scriptL, scriptR, lphblstG) :: lbpetail  ->
        begin
          match chunkaccopt with
          | None ->
              aux (Alist.append lphbacc lphblstG) None lbpetail

          | Some((scriptB, chunkacc)) ->
              let scriptA = scriptL in
              let lphblstC = ConvertText.chunks_to_boxes_pure scriptB (Alist.to_list chunkacc) scriptA in
              aux (Alist.append (Alist.append lphbacc lphblstC) lphblstG) None lbpetail
                (* DOUBTFUL; the current implementation does not use `scriptR` at all. *)
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

let calculate_ratios (wid_required : length) (widinfo_total : length_info) : internal_ratios * length =
  let wid_natural = widinfo_total.natural in
  let widshrink  = widinfo_total.shrinkable in
  let stretch = widinfo_total.stretchable in
  let widdiff = wid_required -% wid_natural in
    if wid_natural <=% wid_required then
    (* If the natural width is shorter than or equal to the required one; `widdiff` is positive *)
      match stretch with
      | Fils(nfil) ->
          if nfil > 0 then
            (LBPermissible(0.), widdiff *% (1. /. (~. nfil)))
          else
            assert false
              (* The number of fils cannot be nonpositive *)

      | FiniteStretch(widstretch) ->
          if Length.is_nearly_zero widstretch then
          (* If unable to stretch *)
            if Length.is_nearly_zero widdiff then
              (LBPermissible(0.), Length.zero)
            else
              (LBTooShort(wid_required), Length.zero)
          else
            let ratio_raw = widdiff /% widstretch in
            if ratio_raw >= ratio_stretch_limit then
              (LBTooShort(wid_required), Length.zero)
            else
              (LBPermissible(ratio_raw), Length.zero)
    else
    (* If the natural width is longer than the required one; `widdiff` is nonpositive *)
      if Length.is_nearly_zero widshrink then
      (* If unable to shrink *)
        (LBTooLong(wid_required), Length.zero)
      else
        let ratio_raw = widdiff /% widshrink in
        if ratio_raw <= ratio_shrink_limit then
          (LBTooLong(wid_required), Length.zero)
        else
          (LBPermissible(ratio_raw), Length.zero)


let rec determine_widths (widreqopt : length option) (lphblst : lb_pure_box list) : intermediate_horz_box list * ratios * length * length =
  let (widinfo_total, hgt_total, dpt_total) = get_total_metrics lphblst in
  let (lbratios, widperfil) =
    match widreqopt with
    | Some(widreq) -> calculate_ratios widreq widinfo_total
    | None         -> (LBPermissible(0.), Length.zero)
  in

  let get_intermediate_total_width imhblst =
    imhblst |> List.fold_left (fun wacc imhb ->
      match imhb with
      | ImHorz(w, _)                             -> wacc +% w
      | ImHorzRising(w, _, _, _, _)              -> wacc +% w
      | ImHorzFrame(_, w, _, _, _, _)            -> wacc +% w
      | ImHorzInlineTabular(w, _, _, _, _, _, _) -> wacc +% w
      | ImHorzInlineGraphics(w, _, _, _)         -> wacc +% w
      | ImHorzEmbeddedVert(w, _, _, _)           -> wacc +% w
      | ImHorzHookPageBreak(_)                   -> wacc
      | ImHorzFootnote(_)                        -> wacc
    ) Length.zero
  in

  let rec main_conversion lbratios widperfil lphb : intermediate_horz_box =
    match lphb with
    | LBAtom{ metrics = (widinfo, _, _); main = evhb } ->
        begin
          match widinfo.stretchable with
          | Fils(nfil) ->
              if nfil > 0 then
                ImHorz(widinfo.natural +% widperfil, evhb)
              else
                assert false
                  (* The number of fils cannot be nonpositive *)

          | FiniteStretch(widstretch) ->
              let widappend =
                match lbratios with
                | LBTooLong(_) ->
                    Length.negate widinfo.shrinkable

                | LBPermissible(pure_ratio) ->
                    if pure_ratio <= 0. then
                    (* If `pure_ratio` is nonpositive *)
                      widinfo.shrinkable *% pure_ratio
                    else
                    (* If `pure_ratio` is positive *)
                      widstretch *% pure_ratio

                | LBTooShort(_) ->
                    widstretch
              in
              ImHorz(widinfo.natural +% widappend, evhb)
        end

    | LBRising{ metrics = (_, hgt0, dpt0); rising = len_rising; contents = lphbs0 } ->
        let imhbs = lphbs0 |> List.map (main_conversion lbratios widperfil) in
        let wid_total = get_intermediate_total_width imhbs in
        ImHorzRising(wid_total, hgt0, dpt0, len_rising, imhbs)

    | LBOuterFrame{ metrics = (_, hgt_frame, dpt_frame); decoration = deco; contents = lphbs } ->
        let imhbs = lphbs |> List.map (main_conversion lbratios widperfil) in
        let wid_total = get_intermediate_total_width imhbs in
        ImHorzFrame(Permissible(0.), wid_total, hgt_frame, dpt_frame, deco, imhbs)

    | LBFixedFrame{
        width      = wid_frame;
        height     = hgt_frame;
        depth      = dpt_frame;
        decoration = deco;
        contents   = lphbs;
      } ->
        let (imhbs, ratios, _, _) = determine_widths (Some(wid_frame)) lphbs in
        ImHorzFrame(ratios, wid_frame, hgt_frame, dpt_frame, deco, imhbs)

    | LBEmbeddedVert{ width = wid; height = hgt; depth = dpt; contents = imvbs } ->
        ImHorzEmbeddedVert(wid, hgt, dpt, imvbs)

    | LBFixedGraphics{ width = wid; height = hgt; depth = dpt; graphics } ->
        ImHorzInlineGraphics(wid, hgt, dpt, ImGraphicsFixed(graphics))

    | LBOuterFilGraphics{ height = hgt; depth = dpt; graphics } ->
        ImHorzInlineGraphics(widperfil, hgt, dpt, ImGraphicsVariable(graphics))

    | LBFixedTabular{
        width         = wid;
        height        = hgt;
        depth         = dpt;
        rows          = imtabular;
        column_widths = widlst;
        lengths       = lenlst;
        rule_graphics = rulesf;
      } ->
        ImHorzInlineTabular(wid, hgt, dpt, imtabular, widlst, lenlst, rulesf)

    | LBFixedImage{ width = wid; height = hgt; key = imgkey } ->
        ImHorz(wid, EvHorzInlineImage(hgt, imgkey))

    | LBHookPageBreak(hookf) ->
        ImHorzHookPageBreak(hookf)

    | LBFootnote(imvblst) ->
        ImHorzFootnote(imvblst)
  in
  let imhblst = lphblst |> List.map (main_conversion lbratios widperfil) in
  let ratios =
    match lbratios with
    | LBTooLong(wid_required) ->
        let wid_actual = get_intermediate_total_width imhblst in
        TooLong{ required = wid_required; actual = wid_actual }

    | LBTooShort(wid_required) ->
        let wid_actual = get_intermediate_total_width imhblst in
        TooShort{ required = wid_required; actual = wid_actual }

    | LBPermissible(r) ->
        Permissible(r)
  in
  (imhblst, ratios, hgt_total, dpt_total)


type line_break_info = {
  breakability_top    : breakability;
  breakability_bottom : breakability;
  paragraph_margin_top    : length;
  paragraph_margin_bottom : length;
  paragraph_width     : length;
  leading_required    : length;
  vskip_min           : length;
  min_first_ascender  : length;
  min_last_descender  : length;
}

type line_either =
  | PureLine    of lb_pure_box list
  | AlreadyVert of length * vert_box list

type paragraph_accumulator = {
  previous_depth        : length;
  saved_margin_top      : (breakability * length) option;
  accumulated_paragraph : paragraph_element Alist.t;
}

type arrangement_state =
  | BuildingVertList
  | BuildingParagraph of paragraph_accumulator

type arrangement_accumulator = {
  state       : arrangement_state;
  accumulated : vert_box Alist.t;
}

let break_into_lines (lbinfo : line_break_info) (path : DiscretionaryID.t list) (lhblst : lb_box list) : vert_box list =

  let calculate_vertical_skip (dptprev : length) (hgt : length) : length =
    let vskipraw = lbinfo.leading_required -% (Length.negate dptprev) -% hgt in
    if vskipraw <% lbinfo.vskip_min then lbinfo.vskip_min else vskipraw
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
            | Some(accline) -> (* Single line *)
                let lb = LBOuterFrame{ metrics = metr_total; decoration = decoS; contents = line } in
                aux None (Some(Alist.extend accline lb)) acclines []

            | None -> (* Last line *)
                let lb = LBOuterFrame{ metrics = metr_total; decoration = decoT; contents = line } in
                aux None (Some(Alist.extend Alist.empty lb)) acclines []
          end

      | PureLine(line) :: tail ->
          let metr_sub = get_total_metrics line in
          let metr_total = append_vert_padding metr_sub pads in
          begin
            match first with
            | Some(accline) -> (* First line *)
                let lb = LBOuterFrame{ metrics = metr_total; decoration = decoH; contents = line } in
                let acclines =
                  Alist.extend acclines (PureLine(Alist.to_list (Alist.extend accline lb)))
                in
                aux None None acclines tail

            | None ->
                let lb = LBOuterFrame{ metrics = metr_total; decoration = decoM; contents = line } in
                let acclines = Alist.extend acclines (PureLine([ lb ])) in
                aux None None acclines tail
          end

      | AlreadyVert(width, vbs) :: tail ->
          let imvbs = PageBreak.solidify vbs in
          let (height, depth) = PageBreak.adjust_to_first_line imvbs in
          let metr_sub = (natural width, height, depth) in
          let metr_total = append_vert_padding metr_sub pads in
          let inner = [ LBEmbeddedVert{ width; height; depth; contents = imvbs } ] in
          begin
            match first with
            | Some(accline) -> (* First line *)
                let lb = LBOuterFrame{ metrics = metr_total; decoration = decoH; contents = inner } in
                let acclines =
                  Alist.extend acclines (PureLine(Alist.to_list (Alist.extend accline lb)))
                in
                aux None None acclines tail

            | None ->
                let lb = LBOuterFrame{ metrics = metr_total; decoration = decoM; contents = inner } in
                let acclines = Alist.extend acclines (PureLine([ lb ])) in
                aux None None acclines tail
          end
    in
      aux (Some(accline_before)) None acclines_before lines_in_frame
  in

  (* --
     cut: cuts inline box list into lines based on the break point list `path`
     * `acclines`: accumulated lines for building a paragraph
     * `accline`: accumulated inline contents for building a single line
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
          let le = AlreadyVert(wid, vblst) in
          let acclinesnew =
            match Alist.to_list accline with
            | []     -> Alist.extend acclines le
            | _ :: _ -> Alist.extend (Alist.extend acclines (PureLine(Alist.to_list accline))) le
          in
            cut acclinesnew Alist.empty tail

    | LBPure(lphb) :: tail ->
        cut acclines (Alist.extend accline lphb) tail

    | LBFrameBreakable(pads, decoS, decoH, decoM, decoT, lhblst) :: tail ->
        let acclines_in_frame = cut Alist.empty Alist.empty lhblst in
        let (acclinesnew, acclinenew) =
          append_framed_lines (Alist.to_list acclines_in_frame) acclines accline decoS decoH decoM decoT pads
        in
          cut acclinesnew acclinenew tail

    | [] ->
        begin
          match Alist.to_list accline with
          | []   -> acclines
          | line -> Alist.extend acclines (PureLine(line))
        end
  in


  (*  --
      arrange: inserts vertical spaces between lines and top/bottom margins
      -- *)
  let rec arrange (acc : arrangement_accumulator) (lines : line_either list) : vert_box list =
    match lines with
    | PureLine(line) :: tail ->
        let wid_required = lbinfo.paragraph_width in
        let (imhbs, ratios, hgt, dpt) = determine_widths (Some(wid_required)) line in
        begin
          match acc.state with
          | BuildingVertList ->
              begin
                match Alist.to_list_rev acc.accumulated with
                | [] ->
                  (* If `line` is the first line in the given paragraph *)
                    let margin_top =
                      lbinfo.paragraph_margin_top +% (Length.max Length.zero (lbinfo.min_first_ascender -% hgt))
                    in
                    let acc =
                      {
                        state = BuildingParagraph{
                          saved_margin_top      = Some(lbinfo.breakability_top, margin_top);
                          previous_depth        = dpt;
                          accumulated_paragraph = Alist.extend Alist.empty (VertParagLine(Reachable(ratios), hgt, dpt, imhbs));
                        };
                        accumulated = Alist.empty;
                      }
                    in
                    arrange acc tail

                | _ :: _ ->
                  (* If `line` is the first line after the embedded vertical boxes *)
                    let accnew =
                      {
                        state = BuildingParagraph{
                          saved_margin_top      = None;
                          previous_depth        = dpt;
                          accumulated_paragraph = Alist.extend Alist.empty (VertParagLine(Reachable(ratios), hgt, dpt, imhbs));
                        };
                        accumulated = acc.accumulated;
                      }
                    in
                    arrange accnew tail
              end

          | BuildingParagraph(peacc) ->
              let len = calculate_vertical_skip peacc.previous_depth hgt in
              let parnew =
                Alist.append peacc.accumulated_paragraph [
                  VertParagSkip(len);
                  VertParagLine(Reachable(ratios), hgt, dpt, imhbs)
                ];
              in
              let accnew =
                {
                  state = BuildingParagraph{
                    saved_margin_top      = peacc.saved_margin_top;
                    previous_depth        = dpt;
                    accumulated_paragraph = parnew;
                  };
                  accumulated = acc.accumulated;
                }
              in
              arrange accnew tail
        end

    | AlreadyVert(_, vblst) :: tail ->
        begin
          match acc.state with
          | BuildingVertList ->
              let accnew =
                {
                  state = BuildingVertList;
                  accumulated = Alist.append acc.accumulated vblst;
                }
              in
              arrange accnew tail

          | BuildingParagraph(peacc) ->
              let vbpar =
                let parelems = Alist.to_list peacc.accumulated_paragraph in
                let margins =
                  {
                    margin_top    = peacc.saved_margin_top;
                    margin_bottom = None;
                  }
                in
                VertParagraph(margins, parelems)
              in
              let accnew =
                {
                  state = BuildingVertList;
                  accumulated = Alist.append (Alist.extend acc.accumulated vbpar) vblst;
                }
              in
              arrange accnew tail
        end

    | [] ->
        begin
          match acc.state with
          | BuildingVertList ->
              Alist.to_list acc.accumulated

          | BuildingParagraph(peacc) ->
              let vbpar =
                let parelems = Alist.to_list peacc.accumulated_paragraph in
                let margins =
                  {
                    margin_top = peacc.saved_margin_top;
                    margin_bottom = Some((lbinfo.breakability_bottom, lbinfo.paragraph_margin_bottom));
                  }
                in
                VertParagraph(margins, parelems)
              in
              Alist.to_list (Alist.extend acc.accumulated vbpar)
        end
  in

  let acclines = cut Alist.empty Alist.empty lhblst in
  arrange { state = BuildingVertList; accumulated = Alist.empty; } (Alist.to_list acclines)


let natural (hbs : horz_box list) : intermediate_horz_box list * length * length =
  let lphbs = convert_list_for_line_breaking_pure hbs in
  let (imhbs, _, hgt, dpt) = determine_widths None lphbs in
  (imhbs, hgt, dpt)


let fit (hblst : horz_box list) (widreq : length) : intermediate_horz_box list * ratios * length * length =
  let lphblst = convert_list_for_line_breaking_pure hblst in
  determine_widths (Some(widreq)) lphblst


type lb_state =
  | NormalState
  | ImmediateAfterEmbeddedVert of DiscretionaryID.t


let main ((breakability_top, paragraph_margin_top) : breakability * length) ((breakability_bottom, paragraph_margin_bottom) : breakability * length) (ctx : context_main) (hblst : horz_box list) : vert_box list =

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
        let (lbratios, _) = calculate_ratios paragraph_width (widinfofrom +%@ widinfobreak) in
          match lbratios with
          | LBPermissible(pure_ratio) ->
              let badness = calculate_badness pure_ratio in
              found_candidate := true;
              LineBreakGraph.add_edge grph dscridfrom dscridto (badness + pnltybreak)

          | LBTooShort(_) ->
              ()

          | LBTooLong(_) ->
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

  let unify_discretionary_points dscrid1 dscrid2 =
    LineBreakGraph.add_vertex grph dscrid2;
    LineBreakGraph.add_edge grph dscrid1 dscrid2 0
  in

  let rec aux (state : lb_state) (iterdepth : int) (wmap : WidthMap.t) (lhblst : lb_box list) : WidthMap.t =
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
        aux NormalState iterdepth wmapnew tail

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
        aux NormalState iterdepth wmapnew tail

    | LBEmbeddedVertBreakable(dscrid, _, _) :: tail ->
        begin
          match state with
          | ImmediateAfterEmbeddedVert(dscrid_last) ->
              unify_discretionary_points dscrid_last dscrid

          | NormalState ->
              let (_, _) = update_graph wmap dscrid widinfo_zero 0 () in
              ()
        end;
        let wmapnew = WidthMap.empty |> WidthMap.add dscrid widinfo_zero in
        aux (ImmediateAfterEmbeddedVert(dscrid)) iterdepth wmapnew tail

    | LBPure(lphb) :: tail ->
        let widinfo = get_width_info lphb in
        let wmapnew = wmap |> WidthMap.add_width_all widinfo in
        aux NormalState iterdepth wmapnew tail

    | LBFrameBreakable(pads, _, _, _, _, lhblstsub) :: tail ->
        let wmapsub = aux NormalState (iterdepth + 1) wmap lhblstsub in
        aux NormalState iterdepth wmapsub tail

    | [] ->
        if iterdepth = 0 then
          match state with
          | ImmediateAfterEmbeddedVert(dscrid_last) ->
              unify_discretionary_points dscrid_last DiscretionaryID.final;
              wmap

          | NormalState ->
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
    let _ = aux NormalState 0 wmapinit lhblst in
    let pathopt = LineBreakGraph.shortest_path grph DiscretionaryID.beginning DiscretionaryID.final in
      match pathopt with
      | None ->
        (* -- when no set of discretionary points is suitable for line breaking -- *)
          Format.printf "LineBreak> UNREACHABLE\n";  (* for debug *)
          let (imhbs, hgt, dpt) = natural hblst in
          let margins =
            {
              margin_top    = Some((breakability_top, paragraph_margin_top));
              margin_bottom = Some((breakability_bottom, paragraph_margin_bottom));
            }
          in
          [ VertParagraph(margins, [ VertParagLine(Unreachable, hgt, dpt, imhbs) ]); ]

      | Some(path) ->
          break_into_lines {
            breakability_top;
            breakability_bottom;
            paragraph_margin_top;
            paragraph_margin_bottom;
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


let get_leftmost_script (hblst : horz_box list) : CharBasis.script option =
  let rec aux hblst =
    match hblst with
    | [] ->
        None

    | HorzScriptGuard(scriptL, _, _) :: _ ->
        Some(scriptL)

    | HorzDiscretionary{ no_break = hbs0 } :: tail ->
        begin
          match hbs0 with
          | [] -> aux tail
          | _  -> aux hbs0
        end

    | HorzPure(phb) :: tail ->
        begin
          match phb with
          | PHCInnerString{ context = ctxmain; chars = uchlst } ->
              let aux_chunks chunks =
                match chunks with
                | []                                            -> aux tail
                | (_, AlphabeticChunk(script, _, _, _, _)) :: _ -> Some(script)
                | (_, IdeographicChunk(script, _, _, _)) :: _   -> Some(script)
                | _ :: _                                        -> None
              in
              let (_, chunks) = ConvertText.to_chunks ctxmain uchlst PreventBreak in
              aux_chunks chunks

          | PHCInnerMathGlyph(_) ->
              Some(CharBasis.Latin)

          | PHGRising{ inner = hbs0 } ->
              begin
                match hbs0 with
                | [] -> aux tail
                | _  -> aux hbs0
              end

          | PHGHookPageBreak(_)
          | PHGFootnote(_) ->
              aux tail

          | _ ->
              None
        end

    | _ ->
        None
  in
  aux hblst


let get_rightmost_script (hblst : horz_box list) : CharBasis.script option =
  let rec aux hbrev =
    match hbrev with
    | [] ->
        None

    | HorzScriptGuard(_, scriptR, _) :: _ ->
        Some(scriptR)

    | HorzDiscretionary{ no_break = hbs0 } :: revtail ->
        begin
          match hbs0 with
          | [] -> aux revtail
          | _  -> aux (List.rev hbs0)
        end

    | HorzPure(phb) :: revtail ->
        begin
          match phb with
          | PHCInnerString{ context = ctxmain; chars = uchlst } ->
              let aux_chunks chunkrev =
                match chunkrev with
                | []                                            -> aux revtail
                | (_, AlphabeticChunk(script, _, _, _, _)) :: _ -> Some(script)
                | (_, IdeographicChunk(script, _, _, _)) :: _   -> Some(script)
                | _                                             -> None
              in
              let (_, chunks) = ConvertText.to_chunks ctxmain uchlst PreventBreak in
              aux_chunks (List.rev chunks)

          | PHCInnerMathGlyph(_) ->
              Some(CharBasis.Latin)

          | PHGRising{ inner = hbs0 } ->
              begin
                match hbs0 with
                | [] -> aux revtail
                | _  -> aux (List.rev hbs0)
              end

          | PHGHookPageBreak(_)
          | PHGFootnote(_) ->
              aux revtail

          | _ ->
              None
        end

    | _ ->
        None
  in
  aux (List.rev hblst)
