
open HorzBox

let ( ~. ) = float_of_int
let ( ~@ ) = int_of_float


let widinfo_zero =
  {
    natural     = Length.zero;
    shrinkable  = Length.zero;
    stretchable = FiniteStretch(Length.zero);
  }

let ( +%@ ) wi1 wi2 =
  {
    natural     = wi1.natural +% wi2.natural;
    shrinkable  = wi1.shrinkable +% wi2.shrinkable;
    stretchable = add_stretchable wi1.stretchable wi2.stretchable;
  }

type metrics = length_info * length * length

type lb_pure_box =
  | Atom        of metrics * evaled_horz_box_main
  | OuterFrame  of metrics * decoration * lb_pure_box list
  | FixedFrame  of length * length * length * decoration * lb_pure_box list
  | EmbeddedVert of length * length * length * evaled_vert_box list

type lb_box =
  | LBPure           of lb_pure_box
  | LBDiscretionary  of pure_badness * DiscretionaryID.t * lb_pure_box option * lb_pure_box option * lb_pure_box option
  | LBFrameBreakable of paddings * length * length * decoration * decoration * decoration * decoration * lb_box list


let natural wid =
  {
    natural     = wid;
    shrinkable  = Length.zero;
    stretchable = FiniteStretch(Length.zero);
  }


let get_metrics (lphb : lb_pure_box) : metrics =
  match lphb with
  | Atom(metr, _)                   -> metr
  | OuterFrame(metr, _, _)          -> metr
  | FixedFrame(wid, hgt, dpt, _, _) -> (natural wid, hgt, dpt)
  | EmbeddedVert(wid, hgt, dpt, _)  -> (natural wid, hgt, dpt)


let get_total_metrics (lphblst : lb_pure_box list) : metrics =
  lphblst @|> (widinfo_zero, Length.zero, Length.zero) @|> List.fold_left (fun (wiacc, hacc, dacc) lphb ->
    let (wi, h, d) = get_metrics lphb in
      (wiacc +%@ wi, max hacc h, min dacc d)
  )


let get_width_info (lphb : lb_pure_box) =
  let (widinfo, _, _) = get_metrics lphb in widinfo


let get_width_info_opt = function
  | None      -> widinfo_zero
  | Some(lhb) -> get_width_info lhb


let empty_vert (widinfo : length_info) : metrics =
  (widinfo, Length.zero, Length.zero)


let append_vert_padding (metr : metrics) (pads : paddings) : metrics =
  let (widinfo, hgt, dpt) = metr in
    (widinfo, hgt +% pads.paddingT, dpt -% pads.paddingB)


let append_horz_padding (lhblst : lb_box list) (pads : paddings) =
  List.append
    (LBPure(Atom(empty_vert (natural pads.paddingL), EvHorzEmpty)) :: lhblst)
    (LBPure(Atom(empty_vert (natural pads.paddingR), EvHorzEmpty)) :: [])


let append_horz_padding_pure (lphblst : lb_pure_box list) (widinfo : length_info) (pads : paddings) =
  let lphblstnew =
    List.append
      (Atom(empty_vert (natural pads.paddingL), EvHorzEmpty) :: lphblst)
      (Atom(empty_vert (natural pads.paddingR), EvHorzEmpty) :: [])
  in
  let widinfonew =
    {
      natural     = widinfo.natural +% pads.paddingL +% pads.paddingR;
      shrinkable  = widinfo.shrinkable;
      stretchable = widinfo.stretchable;
    }
  in
    (lphblstnew, widinfonew)


let rec convert_list_for_line_breaking (hblst : horz_box list) : lb_box list =
  let rec aux acc hblst =
    match hblst with
    | [] -> List.rev acc

    | HorzDiscretionary(pnlty, phbopt0, phbopt1, phbopt2) :: tail ->
        let lphbopt0 = convert_pure_box_for_line_breaking_opt phbopt0 in
        let lphbopt1 = convert_pure_box_for_line_breaking_opt phbopt1 in
        let lphbopt2 = convert_pure_box_for_line_breaking_opt phbopt2 in
        let dscrid = DiscretionaryID.fresh () in
          aux (LBDiscretionary(pnlty, dscrid, lphbopt0, lphbopt1, lphbopt2) :: acc) tail

    | HorzPure(phb) :: tail ->
        let lphb = convert_pure_box_for_line_breaking phb in
          aux (LBPure(lphb) :: acc) tail

    | HorzFrameBreakable(pads, wid1, wid2, decoS, decoH, decoM, decoT, hblst) :: tail ->
        let lhblst = convert_list_for_line_breaking hblst in
        let lhblstnew = append_horz_padding lhblst pads in
          aux (LBFrameBreakable(pads, wid1, wid2, decoS, decoH, decoM, decoT, lhblstnew) :: acc) tail

  in
    aux [] hblst


and convert_list_for_line_breaking_pure (hblst : horz_box list) : lb_pure_box list =
  let rec aux acc hblst =
    match hblst with
    | [] -> List.rev acc

    | HorzDiscretionary(pnlty, phbopt0, phbopt1, phbopt2) :: tail ->
        let lphbopt0 = convert_pure_box_for_line_breaking_opt phbopt0 in
        begin
          match lphbopt0 with
          | None        -> aux acc tail
          | Some(lphb0) -> aux (lphb0 :: acc) tail
        end

    | HorzPure(phb) :: tail ->
        let lphb = convert_pure_box_for_line_breaking phb in
          aux (lphb :: acc) tail

    | HorzFrameBreakable(pads, wid1, wid2, decoS, decoH, decoM, decoT, hblstsub) :: tail ->
        let lphblst = convert_list_for_line_breaking_pure hblstsub in
        let (widinfo_sub, hgt, dpt) = get_total_metrics lphblst in
        let (lphblstnew, widinfo_total) = append_horz_padding_pure lphblst widinfo_sub pads in
          aux (OuterFrame((widinfo_total, hgt +% pads.paddingT, dpt -% pads.paddingB), decoS, lphblst) :: acc) tail
  in
    aux [] hblst

  
and convert_pure_box_for_line_breaking (phb : pure_horz_box) : lb_pure_box =
  match phb with
  | PHFixedString(info, uchlst) ->
      let (otxt, wid, hgt, dpt) = FontInfo.get_metrics_of_word info.font_abbrev info.font_size uchlst in
        Atom((natural wid, hgt, dpt), EvHorzString(info, otxt))

  | PHFixedEmpty(wid) ->
      Atom(empty_vert (natural wid), EvHorzEmpty)

  | PHOuterEmpty(wid, widshrink, widstretch) ->
      Atom(empty_vert { natural = wid; shrinkable = widshrink; stretchable = FiniteStretch(widstretch); }, EvHorzEmpty)

  | PHOuterFil ->
      Atom(empty_vert { natural = Length.zero; shrinkable = Length.zero; stretchable = Fils(1); }, EvHorzEmpty)

  | PHOuterFrame(pads, deco, hblst) ->
      let lphblst = convert_list_for_line_breaking_pure hblst in
      let (widinfo_sub, hgt, dpt) = get_total_metrics lphblst in
      let (lphblstnew, widinfo_total) = append_horz_padding_pure lphblst widinfo_sub pads in
        OuterFrame((widinfo_total, hgt +% pads.paddingT, dpt -% pads.paddingB), deco, lphblstnew)

  | PHInnerFrame(pads, deco, hblst) ->
      let lphblst = convert_list_for_line_breaking_pure hblst in
      let (widinfo_sub, hgt, dpt) = get_total_metrics lphblst in
      let (lphblstnew, widinfo_total) = append_horz_padding_pure lphblst widinfo_sub pads in
        FixedFrame(widinfo_total.natural, hgt +% pads.paddingT, dpt -% pads.paddingB, deco, lphblstnew)

  | PHFixedFrame(pads, wid_req, deco, hblst) ->
      let lphblst = convert_list_for_line_breaking_pure hblst in
      let (widinfo_sub, hgt, dpt) = get_total_metrics lphblst in
      let (lphblstnew, _) = append_horz_padding_pure lphblst widinfo_sub pads in
        FixedFrame(wid_req, hgt +% pads.paddingT, dpt -% pads.paddingB, deco, lphblstnew)

  | PHEmbeddedVert(wid, hgt, dpt, evvblst) ->
      EmbeddedVert(wid, hgt, dpt, evvblst)


and convert_pure_box_for_line_breaking_opt (phbopt : pure_horz_box option) =
  match phbopt with
  | None      -> None
  | Some(phb) -> Some(convert_pure_box_for_line_breaking phb)


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

    let empty = DiscretionaryIDMap.empty

    let add dscrid widinfo wmap = wmap |> DiscretionaryIDMap.add dscrid (widinfo, ref false)

    let iter f = DiscretionaryIDMap.iter (fun dscrid (widinfo, bref) -> f dscrid widinfo bref)

    let add_width_all (widinfo : length_info) (wmap : t) : t =
      wmap |> DiscretionaryIDMap.map (fun (distinfo, bref) -> (distinfo +%@ widinfo, bref))

    let remove = DiscretionaryIDMap.remove

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


let rec determine_widths (wid_req : length) (lphblst : lb_pure_box list) : evaled_horz_box list * length * length =
  let (widinfo_total, hgt_total, dpt_total) = get_total_metrics lphblst in
  let (ratios, widperfil) = calculate_ratios wid_req widinfo_total in

  let rec main_conversion ratios widperfil lphb =
    match lphb with
    | Atom((widinfo, _, _), evhb) ->
        begin
          match widinfo.stretchable with
          | Fils(nfil)  when nfil > 0 -> EvHorz(widinfo.natural +% widperfil, evhb)
          | Fils(_)                   -> assert false  (* -- number of fils cannot be nonpositive -- *)
          | FiniteStretch(widstretch) ->
              let widappend =
                match ratios with
                | TooLong                      -> Length.negate widinfo.shrinkable
                | PermissiblyLong(pure_ratio)  -> widinfo.shrinkable *% pure_ratio  (* -- pure_ratio is nonpositive -- *)
                | PermissiblyShort(pure_ratio) -> widstretch *% pure_ratio          (* -- pure_ratio is positive -- *)
                | TooShort                     -> widstretch
              in
                EvHorz(widinfo.natural +% widappend, evhb)
        end

    | OuterFrame((_, hgt_frame, dpt_frame), deco, lphblstsub) ->
        let evhblst = lphblstsub |> List.map (main_conversion ratios widperfil) in
        let wid_total = evhblst @|> Length.zero @|> List.fold_left (fun acc (EvHorz(w, _)) -> acc +% w) in
          EvHorz(wid_total, EvHorzFrame(hgt_frame, dpt_frame, deco, evhblst))

    | FixedFrame(wid_frame, hgt_frame, dpt_frame, deco, lphblstsub) ->
        let (evhblst, _, _) = determine_widths wid_frame lphblstsub in
          EvHorz(wid_frame, EvHorzFrame(hgt_frame, dpt_frame, deco, evhblst))

    | EmbeddedVert(wid, hgt, dpt, evvblst) ->
        EvHorz(wid, EvHorzEmbeddedVert(hgt, dpt, evvblst))
  in
      let evhblst = lphblst |> List.map (main_conversion ratios widperfil) in

      (* begin : for debug *)
      let checksum =
        evhblst |> List.map (function
          | EvHorz(wid, _) -> wid
        ) |> List.fold_left ( +% ) Length.zero
      in
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

        (evhblst, hgt_total, dpt_total)


(* -- distance from the top of the paragraph and its first baseline -- *)
let first_leading = Length.of_pdf_point 10.  (* temporary; should be variable *)


let break_into_lines (margin_top : length) (margin_bottom : length) (paragraph_width : length) (leading_required : length) (path : DiscretionaryID.t list) (lhblst : lb_box list) : intermediate_vert_box list =

  let calculate_vertical_skip (dptprev : length) (hgt : length) : length =
    let leadingsub = leading_required -% (Length.negate dptprev) -% hgt in
      if leadingsub <% Length.zero then Length.zero else leadingsub
  in

  let append_framed_lines
        (lines_in_frame : (lb_pure_box list) list)
        (acclines_before : (lb_pure_box list) list)
        (accline_before : lb_pure_box list)
        (decoS : decoration) (decoH : decoration) (decoM : decoration) (decoT : decoration)
        (pads : paddings)
        : (lb_pure_box list) list * lb_pure_box list =
    let rec aux
          (first : (lb_pure_box list) option)
          (last : (lb_pure_box list) option)
          (acclines : (lb_pure_box list) list)
          (lines : (lb_pure_box list) list)
        =
      match lines with
      | [] ->
          begin
            match last with
            | Some(accline) -> (acclines, accline)
            | None          -> (acclines, [])
          end

      | line :: [] ->  (* -- last line -- *)
          let metr_sub = get_total_metrics line in
          let metr_total = append_vert_padding metr_sub pads in
          begin
            match first with
            | Some(accline) ->
                aux None (Some(OuterFrame(metr_total, decoS, line) :: accline)) acclines []
            | None ->
                aux None (Some(OuterFrame(metr_total, decoT, line) :: [])) acclines []
          end

      | line :: tail ->
          let metr_sub = get_total_metrics line in
          let metr_total = append_vert_padding metr_sub pads in
          begin
            match first with
            | Some(accline) ->
                aux None None ((List.rev (OuterFrame(metr_total, decoH, line) :: accline)) :: acclines) tail
            | None ->
                aux None None ((OuterFrame(metr_total, decoM, line) :: []) :: acclines) tail
          end
    in
      aux (Some(accline_before)) None acclines_before lines_in_frame
  in

  let rec cut (acclines : (lb_pure_box list) list) (accline : lb_pure_box list) (lhblst : lb_box list) : (lb_pure_box list) list =
    match lhblst with
    | LBDiscretionary(_, dscrid, lphbopt0, lphbopt1, lphbopt2) :: tail ->
        if List.mem dscrid path then
          let acclinesub   = match lphbopt1 with None -> accline | Some(lphb1) -> lphb1 :: accline in
          let acclinefresh = match lphbopt2 with None -> []      | Some(lphb2) -> lphb2 :: [] in
            cut ((List.rev acclinesub) :: acclines) acclinefresh tail
        else
          let acclinenew   = match lphbopt0 with None -> accline | Some(lhb0) -> lhb0 :: accline in
            cut acclines acclinenew tail

    | LBPure(lphb) :: tail ->
        cut acclines (lphb :: accline) tail

    | LBFrameBreakable(pads, wid1, wid2, decoS, decoH, decoM, decoT, lhblst) :: tail ->
        let acclines_in_frame = cut [] [] lhblst in
        let (acclinesnew, acclinenew) = append_framed_lines (List.rev acclines_in_frame) acclines accline decoS decoH decoM decoT pads in
          cut acclinesnew acclinenew tail

    | [] ->
        (List.rev accline) :: acclines
  in

  let rec arrange (dptprevopt : length option) (accvlines : intermediate_vert_box list) (lines : (lb_pure_box list) list) =
    match lines with
    | line :: tail ->
        let (evhblst, hgt, dpt) = determine_widths paragraph_width line in
        begin
          match dptprevopt with
          | None ->
              arrange (Some(dpt)) (ImVertLine(hgt, dpt, evhblst) :: []) tail

          | Some(dptprev) ->
              let vskip = calculate_vertical_skip dptprev hgt in
                arrange (Some(dpt)) (ImVertLine(hgt, dpt, evhblst) :: ImVertFixedBreakable(vskip) :: accvlines) tail 
        end

    | [] -> ImVertTopMargin(true, margin_top) :: (List.rev (ImVertBottomMargin(true, margin_bottom) :: accvlines))
  in

  let acclines = cut [] [] lhblst in
    arrange None [] (List.rev acclines)


let main (margin_top : length) (margin_bottom : length) (paragraph_width : length) (leading_required : length) (hblst : horz_box list) : intermediate_vert_box list =

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
          | ( PermissiblyLong(pure_ratio) | PermissiblyShort(pure_ratio) ) ->
              let badness = calculate_badness pure_ratio in
              begin
                found_candidate := true;
                LineBreakGraph.add_edge grph dscridfrom dscridto (badness + pnltybreak);
              end

          | TooShort -> ()

          | TooLong ->
              if !is_already_too_long then
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
    | LBDiscretionary(pnlty, dscrid, lphbopt0, lphbopt1, lphbopt2) :: tail ->
        let widinfo0 = get_width_info_opt lphbopt0 in
        let widinfo1 = get_width_info_opt lphbopt1 in
        let widinfo2 = get_width_info_opt lphbopt2 in
        let (found, wmapsub) = update_graph wmap dscrid widinfo1 pnlty () in
        let wmapnew =
          if found then
            wmapsub |> WidthMap.add_width_all widinfo0 |> WidthMap.add dscrid widinfo2
          else
            wmapsub |> WidthMap.add_width_all widinfo0
        in
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
    let lhblst = convert_list_for_line_breaking hblst in
    let _ (* wmapfinal *) = aux 0 wmapinit lhblst in
    let pathopt = LineBreakGraph.shortest_path grph DiscretionaryID.beginning DiscretionaryID.final in
      match pathopt with
      | None       -> (* -- when no set of discretionary points is suitable for line breaking -- *)
          [ImVertLine(Length.zero, Length.zero, [])] (* temporary *)
      | Some(path) ->
          break_into_lines margin_top margin_bottom paragraph_width leading_required path lhblst
  end
