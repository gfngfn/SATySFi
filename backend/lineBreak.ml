
let print_for_debug msg =
  print_endline msg

open HorzBox

let ( ~. ) = float_of_int
let ( ~@ ) = int_of_float


let widinfo_zero =
  {
    natural= SkipLength.zero;
    shrinkable= SkipLength.zero;
    stretchable= SkipLength.zero;
    fils= 0;
  }

let ( +%@ ) wi1 wi2 =
  {
    natural= wi1.natural +% wi2.natural;
    shrinkable= wi1.shrinkable +% wi2.shrinkable;
    stretchable= wi1.stretchable +% wi2.stretchable;
    fils= wi1.fils + wi2.fils;
  }


type lb_horz_box =
  | LBHorzFixedBoxAtom  of skip_info * skip_height * skip_depth * evaled_horz_fixed_atom
  | LBHorzOuterBoxAtom  of skip_info * skip_height * skip_depth * horz_outer_atom
  | LBHorzDiscretionary of pure_badness * DiscretionaryID.t * lb_horz_box option * lb_horz_box option * lb_horz_box option
  | LBHorzOuterBoxBlock of skip_info * skip_height * skip_depth * unit * lb_horz_box list


let metrics_of_horz_fixed_atom (hfa : horz_fixed_atom) : evaled_horz_fixed_atom * skip_info * skip_height * skip_depth =
  match hfa with
  | FixedString(((fntabrv, size, _) as info), word) ->
      let (tjstr, wid, hgt, dpt) = FontInfo.get_metrics_of_word fntabrv size word in
        (EvFixedString(info, tjstr), { natural= wid; shrinkable= SkipLength.zero; stretchable= SkipLength.zero; fils= 0; }, hgt, dpt)

  | FixedEmpty(wid) ->
        (EvFixedEmpty(wid), { natural= wid; shrinkable= SkipLength.zero; stretchable= SkipLength.zero; fils= 0; }, SkipLength.zero, SkipLength.zero)

let metrics_of_horz_outer_atom (hoa : horz_outer_atom) : skip_info * skip_height * skip_depth =
  match hoa with
  | OuterEmpty(wid, widshrink, widstretch) ->
      ({ natural= wid; shrinkable= widshrink; stretchable= widstretch; fils= 0; }, SkipLength.zero, SkipLength.zero)

  | OuterFil ->
      ({ natural= SkipLength.zero; shrinkable= SkipLength.zero; stretchable= SkipLength.zero; fils= 1; }, SkipLength.zero, SkipLength.zero)


let rec convert_box_for_line_breaking = function
  | HorzDiscretionary(_, _, _, _) -> assert false
  | HorzFixedBoxAtom(hfa)         -> let (evhfa, widinfo, hgt, dpt) = metrics_of_horz_fixed_atom hfa in LBHorzFixedBoxAtom(widinfo, hgt, dpt, evhfa)
  | HorzOuterBoxAtom(hoa)         -> let (widinfo, hgt, dpt) = metrics_of_horz_outer_atom hoa in LBHorzOuterBoxAtom(widinfo, hgt, dpt, hoa)
  | HorzOuterBoxBlock((), hlst)   ->
      let lbhlst = hlst |> List.map convert_box_for_line_breaking in
      let (widinfo, hgt, dpt) =
        lbhlst @|> (widinfo_zero, SkipLength.zero, SkipLength.zero) @|> List.fold_left (fun (wiacc, hacc, dacc) lbh ->
          match lbh with
          | LBHorzOuterBoxAtom(widinfo, hgt, dpt, _)     -> (widinfo +%@ wiacc, SkipLength.max hgt hacc, SkipLength.min dpt dacc)
          | LBHorzFixedBoxAtom(widinfo, hgt, dpt, _)     -> (widinfo +%@ wiacc, SkipLength.max hgt hacc, SkipLength.min dpt dacc)
          | LBHorzDiscretionary(_, _, _, _, _)           -> assert false
          | LBHorzOuterBoxBlock(widinfo, hgt, dpt, _, _) -> (widinfo +%@ wiacc, SkipLength.max hgt hacc, SkipLength.min dpt dacc)
        )
      in
        LBHorzOuterBoxBlock(widinfo, hgt, dpt, (), lbhlst)


let convert_box_for_line_breaking_opt (hbopt : horz_box option) =
  match hbopt with
  | None     -> None
  | Some(hb) -> Some(convert_box_for_line_breaking hb)


let get_width_info = function
  | LBHorzDiscretionary(_, _, _, _, _)       -> assert false
  | LBHorzFixedBoxAtom(widinfo, _, _, _)     -> widinfo
  | LBHorzOuterBoxAtom(widinfo, _, _, _)     -> widinfo
  | LBHorzOuterBoxBlock(widinfo, _, _, _, _) -> widinfo


let get_width_info_opt = function
  | None      -> widinfo_zero
  | Some(lhb) -> get_width_info lhb


module WidthMap
: sig
    type t
    val empty : t
    val add_width_all : skip_info -> t -> t
    val add : DiscretionaryID.t -> skip_info -> t -> t
    val iter : (DiscretionaryID.t -> skip_info -> bool ref -> unit) -> t -> unit
    val remove : DiscretionaryID.t -> t -> t
  end
= struct

    module DiscretionaryIDMap = Map.Make (DiscretionaryID)

    type t = (skip_info * bool ref) DiscretionaryIDMap.t

    let empty = DiscretionaryIDMap.empty

    let add dscrid widinfo wmap = wmap |> DiscretionaryIDMap.add dscrid (widinfo, ref false)

    let iter f = DiscretionaryIDMap.iter (fun dscrid (widinfo, bref) -> f dscrid widinfo bref)

    let add_width_all (widinfo : skip_info) (wmap : t) : t =
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


module RemovalSet = MutableSet.Make
  (DiscretionaryID)


let paragraph_width = SkipLength.of_pdf_point 450.0 (* temporary; should be variable *)

let calculate_ratios (widrequired : skip_width) (widinfo_total : skip_info) : bool * float * skip_width =
  let widnatural = widinfo_total.natural in
  let widstretch = widinfo_total.stretchable in
  let widshrink  = widinfo_total.shrinkable in
  let nfil       = widinfo_total.fils in
  let widdiff = widrequired -% widnatural in
  let is_short = (widnatural <% widrequired) in
  let (ratio, widperfil) =
    if is_short then
      if nfil > 0 then  (* -- when the line contains fils -- *)
        (0., widdiff *% (1. /. (~. nfil)))
      else if nfil = 0 then
        if SkipLength.is_nearly_zero widstretch then (+.infinity, SkipLength.zero) else
          (widdiff /% widstretch, SkipLength.zero)
      else
        assert false
    else
      if SkipLength.is_nearly_zero widshrink then (-.infinity, SkipLength.zero) else (widdiff /% widshrink, SkipLength.zero)
  in
    (is_short, ratio, widperfil)


let determine_widths (widrequired : skip_width) (lhblst : lb_horz_box list) : evaled_horz_box list * skip_height * skip_depth * badness =
  let (widinfo_total, hgt_total, dpt_total) =
    lhblst |> List.map (function
      | LBHorzFixedBoxAtom(widinfo, hgt, dpt, _)     -> (widinfo, hgt, dpt)
      | LBHorzOuterBoxAtom(widinfo, hgt, dpt, _)     -> (widinfo, hgt, dpt)
      | LBHorzDiscretionary(_, _, _, _, _)           -> assert false
      | LBHorzOuterBoxBlock(widinfo, hgt, dpt, _, _) -> (widinfo, hgt, dpt)
    ) |> List.fold_left (fun (wi, h, d) (wiacc, hacc, dacc) -> (wi +%@ wiacc, SkipLength.max h hacc, SkipLength.min d dacc)) (widinfo_zero, SkipLength.zero, SkipLength.zero)
  in
  let (is_short, ratio, widperfil) = calculate_ratios widrequired widinfo_total in
  let main_conversion = function
    | LBHorzDiscretionary(_, _, _, _, _)       -> assert false
    | LBHorzFixedBoxAtom(widinfo, _, _, evhfa) -> (EvHorzFixedBoxAtom(widinfo.natural, evhfa), 0)
    | LBHorzOuterBoxAtom(widinfo, _, _, hoa)   ->
        let nfil = widinfo.fils in
          if nfil > 0 then
            (EvHorzOuterBoxAtom(widinfo.natural +% widperfil, hoa), 0)
          else if nfil = 0 then
            let widdiff =
              if is_short then widinfo.stretchable *% ratio
                          else widinfo.shrinkable *% ratio
            in
              (EvHorzOuterBoxAtom(widinfo.natural +% widdiff, hoa), abs (~@ (ratio *. 100.0)))
          else
            assert false  (* -- nfil cannot be negative -- *)
  in
      let pairlst =
        lhblst |> List.map main_conversion
      in
      let evhblst = pairlst |> List.map (fun (evhb, _) -> evhb) in
      let totalpb = pairlst |> List.fold_left (fun acc (_, pb) -> pb + acc) 0 in
      let badns =
        if is_short then
          if totalpb >= 10000 then TooShort else Badness(totalpb)
        else
          if totalpb >= 10000 then TooLong(totalpb) else Badness(totalpb)
      in
      (* begin : for debug *)
      let checksum =
        evhblst |> List.map (function
        | EvHorzFixedBoxAtom(wid, _) -> wid
        | EvHorzOuterBoxAtom(wid, _) -> wid
        ) |> List.fold_left ( +% ) SkipLength.zero
      in
      let () = print_for_debug ("natural = " ^ (SkipLength.show widinfo_total.natural) ^ ", " ^
                                (if is_short then
                                  "stretchable = " ^ (SkipLength.show widinfo_total.stretchable)
                                 else
                                  "shrinkable = " ^ (SkipLength.show widinfo_total.shrinkable)) ^ ", " ^
                                "nfil = " ^ (string_of_int widinfo_total.fils) ^ ", " ^
                                "ratio = " ^ (string_of_float ratio) ^ ", " ^
                                "checksum = " ^ (SkipLength.show checksum)) in
      (* end : for debug *)
        (evhblst, hgt_total, dpt_total, badns)


(* -- distance from the top of the paragraph and its first baseline -- *)
let first_leading = SkipLength.of_pdf_point 10.  (* temporary; should be variable *)


let break_into_lines (leading_required : SkipLength.t) (path : DiscretionaryID.t list) (lhblst : lb_horz_box list) : intermediate_vert_box list =

  let calculate_vertical_skip (dptprev : skip_depth) (hgt : skip_height) : SkipLength.t =
    let leadingsub = leading_required -% dptprev -% hgt in
      if leadingsub <% SkipLength.zero then SkipLength.zero else leadingsub
  in

  let rec aux (dptprev : skip_depth) (acclines : intermediate_vert_box list) (accline : lb_horz_box list) (lhblst : lb_horz_box list) =
    match lhblst with
    | LBHorzDiscretionary(_, dscrid, lhbopt0, lhbopt1, lhbopt2) :: tail ->
        if List.mem dscrid path then
          let line         = match lhbopt1 with None -> accline | Some(lhb1) -> lhb1 :: accline in
          let acclinefresh = match lhbopt2 with None -> []      | Some(lhb2) -> lhb2 :: [] in
          let (evhblst, hgt, dpt, _) = determine_widths paragraph_width (List.rev line) in
          let vskip = calculate_vertical_skip dptprev hgt in
            aux dpt (ImVertLine(hgt, dpt, evhblst) :: ImVertFixedBreakable(vskip) :: acclines) acclinefresh tail
        else
          let acclinenew   = match lhbopt0 with None -> accline | Some(lhb0) -> (lhb0 :: accline) in
            aux dptprev acclines acclinenew tail

    | hb :: tail ->
        aux dptprev acclines (hb :: accline) tail

    | [] ->
        let (evhblst, hgt, dpt, _) = determine_widths paragraph_width (List.rev accline) in
        let vskip = calculate_vertical_skip dptprev hgt in
          List.rev (ImVertLine(hgt, dpt, evhblst) :: ImVertFixedBreakable(vskip) :: acclines)
  in
    aux (leading_required -% first_leading) [] [] lhblst


let main (leading_required : SkipLength.t) (hblst : horz_box list) : intermediate_vert_box list =

  let get_badness_for_line_breaking (widrequired : skip_width) (widinfo_total : skip_info) : badness =
    let criterion_short = 10. in
    let criterion_long = -.1. in
    let (is_short, ratio, _) = calculate_ratios widrequired widinfo_total in
    let pb = abs (~@ (ratio *. 10000. /. (if is_short then criterion_short else criterion_short))) in
      if      ratio > criterion_short then TooShort
      else if ratio < criterion_long  then TooLong(pb)
      else Badness(pb)
  in

  let grph = LineBreakGraph.create () in

  let htomit : RemovalSet.t = RemovalSet.create 32 in

  let found_candidate = ref false in

  let update_graph (wmap : WidthMap.t) (dscridto : DiscretionaryID.t) (widinfobreak : skip_info) (pnltybreak : pure_badness) () : bool * WidthMap.t =
    begin
      LineBreakGraph.add_vertex grph dscridto ;
      found_candidate := false ;
      RemovalSet.clear htomit ;
      wmap |> WidthMap.iter (fun dscridfrom widinfofrom is_already_too_long ->
        let badns = get_badness_for_line_breaking paragraph_width (widinfofrom +%@ widinfobreak) in
          match badns with
          | Badness(pb) ->
              begin
                found_candidate := true ;
                LineBreakGraph.add_edge grph dscridfrom dscridto (pb + pnltybreak) ;
              end

          | TooShort    -> ()

          | TooLong(pb) ->
              if !is_already_too_long then
                begin
                  RemovalSet.add htomit dscridfrom ;
                end
              else
                begin
                  is_already_too_long := true ;
                  found_candidate := true ;
                  LineBreakGraph.add_edge grph dscridfrom dscridto pb ;
                end
                  
      ) ;
      (!found_candidate, RemovalSet.fold (fun dscrid wm -> wm |> WidthMap.remove dscrid) htomit wmap)
    end
  in

  let convert_for_line_breaking (hblst : horz_box list) : lb_horz_box list =
    let rec aux acc hblst =
      match hblst with
      | [] -> List.rev acc

      | HorzDiscretionary(pnlty, hbopt0, hbopt1, hbopt2) :: tail ->
          let lhbopt0 = convert_box_for_line_breaking_opt hbopt0 in
          let lhbopt1 = convert_box_for_line_breaking_opt hbopt1 in
          let lhbopt2 = convert_box_for_line_breaking_opt hbopt2 in
          let dscrid = DiscretionaryID.fresh () in
            aux (LBHorzDiscretionary(pnlty, dscrid, lhbopt0, lhbopt1, lhbopt2) :: acc) tail

      | hb :: tail ->
          let lhb = convert_box_for_line_breaking hb in
            aux (lhb :: acc) tail
    in
      aux [] hblst
  in

  let rec aux (wmap : WidthMap.t) (lhblst : lb_horz_box list) =
    match lhblst with
    | LBHorzDiscretionary(pnlty, dscrid, lhbopt0, lhbopt1, lhbopt2) :: tail ->
        let widinfo0 = get_width_info_opt lhbopt0 in
        let widinfo1 = get_width_info_opt lhbopt1 in
        let widinfo2 = get_width_info_opt lhbopt2 in
        let (found, wmapsub) = update_graph wmap dscrid widinfo1 pnlty () in
        let wmapnew =
          if found then
            wmapsub |> WidthMap.add_width_all widinfo0 |> WidthMap.add dscrid widinfo2
          else
            wmapsub |> WidthMap.add_width_all widinfo0
        in
          aux wmapnew tail

    | hb :: tail ->
        let widinfo = get_width_info hb in
        let wmapnew = wmap |> WidthMap.add_width_all widinfo in
          aux wmapnew tail

    | [] ->
        let dscrid = DiscretionaryID.final in
        let (_, wmapfinal) = update_graph wmap dscrid widinfo_zero 0 () in
          wmapfinal
  in
  let wmapinit = WidthMap.empty |> WidthMap.add DiscretionaryID.beginning widinfo_zero in
  begin
    DiscretionaryID.initialize () ;
    LineBreakGraph.add_vertex grph DiscretionaryID.beginning ;
    let lhblst = convert_for_line_breaking hblst in
    let _ (* wmapfinal *) = aux wmapinit lhblst in
    let pathopt = LineBreakGraph.shortest_path grph DiscretionaryID.beginning DiscretionaryID.final in
      match pathopt with
      | None       -> (* -- when no discretionary point is suitable for line breaking -- *)
          [ImVertLine(SkipLength.zero, SkipLength.zero, [])] (* temporary *)
      | Some(path) ->
          break_into_lines leading_required path lhblst
  end
