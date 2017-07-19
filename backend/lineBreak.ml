
let print_for_debug msg =
  print_endline msg

open HorzBox

let ( ~. ) = float_of_int
let ( ~@ ) = int_of_float


let widinfo_zero =
  {
    naturalWidth= SkipLength.zero;
    shrinkableWidth= SkipLength.zero;
    stretchableWidth= SkipLength.zero;
    numberOfFils= 0;
  }

let ( +%@ ) wi1 wi2 =
  {
    naturalWidth= wi1.naturalWidth +% wi2.naturalWidth;
    shrinkableWidth= wi1.shrinkableWidth +% wi2.shrinkableWidth;
    stretchableWidth= wi1.stretchableWidth +% wi2.stretchableWidth;
    numberOfFils= wi1.numberOfFils + wi2.numberOfFils;
  }


module DiscretionaryID
: sig
    type t
    val initialize : unit -> unit
    val fresh : unit -> t
    val equal : t -> t -> bool
    val beginning : t
    val final : t
    val show : t -> string
  end
= struct

    type t = Beginning | Middle of int | Final

    let current_id = ref 0

    let beginning = Beginning

    let final = Final

    let initialize () =
      begin current_id := 0 ; end

    let fresh () =
      begin
        incr current_id ;
          Middle(!current_id)
      end

    let equal did1 did2 =
      match (did1, did2) with
      | (Beginning, Beginning)   -> true
      | (Middle(i1), Middle(i2)) -> i1 = i2
      | (Final, Final)           -> true
      | _                        -> false

    let show did =
      match did with
      | Beginning -> "<beginning>"
      | Final     -> "<final>"
      | Middle(i) -> "<" ^ (string_of_int i) ^ ">"

  end


type lb_horz_box =
  | LBHorzFixedBoxAtom  of skip_width_info * horz_fixed_atom
  | LBHorzOuterBoxAtom  of skip_width_info * horz_outer_atom
  | LBHorzDiscretionary of DiscretionaryID.t * lb_horz_box option * lb_horz_box option * lb_horz_box option


let size_of_horz_fixed_atom (hfa : horz_fixed_atom) : skip_width_info =
  match hfa with
  | FixedString((fntabrv, size), word) ->
      let wid = FontInfo.get_width_of_word fntabrv size word in
        { naturalWidth= wid; shrinkableWidth= SkipLength.zero; stretchableWidth= SkipLength.zero; numberOfFils= 0; }
          (* temporary; should get height and depth *)


let size_of_horz_outer_atom (hoa : horz_outer_atom) : skip_width_info =
  match hoa with
  | OuterEmpty(wid, widshrink, widstretch) ->
      { naturalWidth= wid; shrinkableWidth= widshrink; stretchableWidth= widstretch; numberOfFils= 0; }

  | OuterFil ->
      { naturalWidth= SkipLength.zero; shrinkableWidth= SkipLength.zero; stretchableWidth= SkipLength.zero; numberOfFils= 1; }


let convert_box_for_line_breaking = function
  | HorzDiscretionary(_, _, _) -> assert false
  | HorzFixedBoxAtom(hfa)      -> let widinfo = size_of_horz_fixed_atom hfa in LBHorzFixedBoxAtom(widinfo, hfa)
  | HorzOuterBoxAtom(hoa)      -> let widinfo = size_of_horz_outer_atom hoa in LBHorzOuterBoxAtom(widinfo, hoa)


let convert_box_for_line_breaking_opt (hbopt : horz_box option) =
  match hbopt with
  | None     -> None
  | Some(hb) -> Some(convert_box_for_line_breaking hb)


let get_width_info = function
  | LBHorzDiscretionary(_, _, _, _) -> assert false
  | LBHorzFixedBoxAtom(widinfo, _)      -> widinfo
  | LBHorzOuterBoxAtom(widinfo, _)      -> widinfo


let get_width_info_opt = function
  | None      -> widinfo_zero
  | Some(lhb) -> get_width_info lhb


module WidthMap
: sig
    type t
    val empty : t
    val add_width_all : skip_width_info -> t -> t
    val add : DiscretionaryID.t -> skip_width_info -> t -> t
    val iter : (DiscretionaryID.t -> skip_width_info -> unit) -> t -> unit
    val remove : DiscretionaryID.t -> t -> t
  end
= struct

    module DiscretionaryIDMap = Map.Make(
      struct
        type t = DiscretionaryID.t
        let compare = compare
      end)

    type t = skip_width_info DiscretionaryIDMap.t

    let empty = DiscretionaryIDMap.empty

    let add = DiscretionaryIDMap.add

    let iter = DiscretionaryIDMap.iter

    let add_width_all (widinfo : skip_width_info) (wmap : t) : t =
      wmap |> DiscretionaryIDMap.map (fun distinfo -> distinfo +%@ widinfo)

    let remove = DiscretionaryIDMap.remove

  end


module LineBreakGraph = FlowGraph.Make(
  struct
    type t = DiscretionaryID.t
    let equal = DiscretionaryID.equal
    let hash = Hashtbl.hash
    let show = DiscretionaryID.show (* for debug *)
  end)
  (struct
    type t = pure_badness
    let show = string_of_int
    let add = ( + )
    let compare b1 b2 = b1 - b2
    let zero = 0
  end)


let paragraph_width = SkipLength.of_pdf_point 400.0 (* temporary; should be variable *)

let calculate_ratios (widrequired : skip_width) (widinfo_total : skip_width_info) : bool * float * skip_width =
    let widnatural = widinfo_total.naturalWidth in
    let widdiff = widrequired -% widnatural in
    let widstretch = widinfo_total.stretchableWidth in
    let widshrink = widinfo_total.shrinkableWidth in
    let nfil = widinfo_total.numberOfFils in
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


let determine_widths (widrequired : skip_width) (lhblst : lb_horz_box list) : evaled_horz_box list * badness =
  let widinfo_total =
    lhblst |> List.map (function
      | LBHorzFixedBoxAtom(widinfo, _)      -> widinfo
      | LBHorzOuterBoxAtom(widinfo, _)      -> widinfo
      | LBHorzDiscretionary(_, _, _, _) -> assert false
    ) |> List.fold_left (+%@) widinfo_zero
  in
  let (is_short, ratio, widperfil) = calculate_ratios widrequired widinfo_total in
      let pairlst =
        lhblst |> List.map (function
          | LBHorzDiscretionary(_, _, _, _)  -> assert false
          | LBHorzFixedBoxAtom(widinfo, hfa) -> (EvHorzFixedBoxAtom(widinfo.naturalWidth, hfa), 0)
          | LBHorzOuterBoxAtom(widinfo, hoa) ->
              let nfil = widinfo.numberOfFils in
                if nfil > 0 then
                  (EvHorzOuterBoxAtom(widinfo.naturalWidth +% widperfil, hoa), 0)
                else if nfil = 0 then
                  let widdiff =
                    if is_short then widinfo.stretchableWidth *% ratio
                                else widinfo.shrinkableWidth *% ratio
                  in
                    (EvHorzOuterBoxAtom(widinfo.naturalWidth +% widdiff, hoa), abs (~@ (ratio *. 100.0)))
                else
                  assert false
        )
      in
      let evhblst = pairlst |> List.map (fun (evhb, _) -> evhb) in
      let totalpb = pairlst |> List.fold_left (fun acc (_, pb) -> pb + acc) 0 in
      let badns =
        if is_short then
          if totalpb >= 10000 then TooShort else Badness(totalpb)
        else
          if totalpb >= 10000 then TooLong else Badness(totalpb)
      in
      (* begin : for debug *)
      let checksum =
        evhblst |> List.map (function
        | EvHorzFixedBoxAtom(wid, _) -> wid
        | EvHorzOuterBoxAtom(wid, _) -> wid
        ) |> List.fold_left ( +% ) SkipLength.zero
      in
      let () = print_for_debug ("natural = " ^ (SkipLength.show widinfo_total.naturalWidth) ^ ", " ^
                                (if is_short then
                                  "stretchable = " ^ (SkipLength.show widinfo_total.stretchableWidth)
                                 else
                                  "shrinkable = " ^ (SkipLength.show widinfo_total.shrinkableWidth)) ^ ", " ^
                                "nfil = " ^ (string_of_int widinfo_total.numberOfFils) ^ ", " ^
                                "ratio = " ^ (string_of_float ratio) ^ ", " ^
                                "checksum = " ^ (SkipLength.show checksum)) in
      (* end : for debug *)
        (evhblst, badns)


let break_into_lines (path : DiscretionaryID.t list) (lhblst : lb_horz_box list) : evaled_vert_box list =
  let rec aux (acclines : evaled_vert_box list) (accline : lb_horz_box list) (lhblst : lb_horz_box list) =
    match lhblst with
    | LBHorzDiscretionary(dscrid, lhbopt0, lhbopt1, lhbopt2) :: tail ->
        if List.mem dscrid path then
          let line         = match lhbopt1 with None -> accline | Some(lhb1) -> lhb1 :: accline in
          let acclinefresh = match lhbopt2 with None -> []      | Some(lhb2) -> lhb2 :: [] in
          let (evhblst, _) = determine_widths paragraph_width (List.rev line) in
            aux (EvVertLine(evhblst) :: acclines) acclinefresh tail
        else
          let acclinenew   = match lhbopt0 with None -> accline | Some(lhb0) -> (lhb0 :: accline) in
            aux acclines acclinenew tail

    | hb :: tail ->
        aux acclines (hb :: accline) tail

    | [] ->
        let (evhblst, _) = determine_widths paragraph_width (List.rev accline) in
          List.rev (EvVertLine(evhblst) :: acclines)
  in
    aux [] [] lhblst


let break_horz_box_list (hblst : horz_box list) : evaled_vert_box list =

  let get_badness_for_line_breaking (widrequired : skip_width) (widinfo_total : skip_width_info) : badness =
    let criterion = 2. in
    let (is_short, ratio, _) = calculate_ratios widrequired widinfo_total in
      if      ratio > criterion    then TooShort
      else if ratio < -. criterion then TooLong
      else                              Badness(~@ (ratio *. 10000. /. criterion))
(*
    let widdiff = required_width -% widinfo.naturalWidth in
    let is_short = (widdiff <% SkipLength.zero) in
    let widcriterion = SkipLength.of_pdf_point (-80.0) in
      match () with
(*      | _ when diff > 5000  -> TooShort *)
      | _ when widdiff <% widcriterion -> TooLong
      | _                              -> Badness(abs (~@ ((widdiff /% widcriterion) *. 10000.)))
    (* temporary; should be like `determine_widths` *)
*)
  in

  let grph = LineBreakGraph.create () in

  let htomit : (DiscretionaryID.t, unit) Hashtbl.t = Hashtbl.create 32 in

  let found_candidate = ref false in

  let update_graph (wmap : WidthMap.t) (dscrid : DiscretionaryID.t) (widinfobreak : skip_width_info) : bool * WidthMap.t =

      begin
        LineBreakGraph.add_vertex grph dscrid ;
        found_candidate := false ;
        Hashtbl.clear htomit ;
        wmap |> WidthMap.iter (fun dscridX widinfoX ->
          let badns = get_badness_for_line_breaking paragraph_width (widinfoX +%@ widinfobreak) in
            match badns with
            | TooShort    -> ()
            | TooLong     -> begin Hashtbl.add htomit dscridX () ; end
            | Badness(pb) ->
                begin
                  found_candidate := true ;
                  LineBreakGraph.add_edge grph dscridX dscrid pb ;
                end
        ) ;
        (!found_candidate, Hashtbl.fold (fun dscrid () wm -> wm |> WidthMap.remove dscrid) htomit wmap)
      end
  in

  let convert_for_line_breaking (hblst : horz_box list) : lb_horz_box list =
    let rec aux acc hblst =
      match hblst with
      | [] -> List.rev acc

      | HorzDiscretionary(hbopt0, hbopt1, hbopt2) :: tail ->
          let lhbopt0 = convert_box_for_line_breaking_opt hbopt0 in
          let lhbopt1 = convert_box_for_line_breaking_opt hbopt1 in
          let lhbopt2 = convert_box_for_line_breaking_opt hbopt2 in
          let dscrid = DiscretionaryID.fresh () in
            aux (LBHorzDiscretionary(dscrid, lhbopt0, lhbopt1, lhbopt2) :: acc) tail

      | hb :: tail ->
          let lhb = convert_box_for_line_breaking hb in
            aux (lhb :: acc) tail
    in
      aux [] hblst
  in

  let rec aux (wmap : WidthMap.t) (lhblst : lb_horz_box list) =
    match lhblst with
    | LBHorzDiscretionary(dscrid, lhbopt0, lhbopt1, lhbopt2) :: tail ->
        let widinfo0 = get_width_info_opt lhbopt0 in
        let widinfo1 = get_width_info_opt lhbopt1 in
        let widinfo2 = get_width_info_opt lhbopt2 in
        let (found, wmapsub) = update_graph wmap dscrid widinfo1 in
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
        let (_, wmapfinal) = update_graph wmap dscrid widinfo_zero in
          wmapfinal
  in
  let wmapinit = WidthMap.empty |> WidthMap.add DiscretionaryID.beginning widinfo_zero in
  begin
    DiscretionaryID.initialize () ;
    LineBreakGraph.add_vertex grph DiscretionaryID.beginning ;
    let lhblst = convert_for_line_breaking hblst in
    let wmapfinal = aux wmapinit lhblst in
    let pathopt = LineBreakGraph.shortest_path grph DiscretionaryID.beginning DiscretionaryID.final in
      match pathopt with
      | None       -> (* -- when no discretionary point is suitable for line breaking -- *)
          [EvVertLine([])] (* temporary *)
      | Some(path) ->
          break_into_lines path lhblst
  end


(* for test *)
let print_evaled_vert_box (EvVertLine(evhblst)) =
  begin
    Format.printf "@[(vert@ " ;
    evhblst |> List.iter (function
      | EvHorzFixedBoxAtom(wid, FixedString(_, str)) -> Format.printf "@[(fixed@ \"%s\"@ :@ %s)@]@ " str (SkipLength.show wid)
      | EvHorzOuterBoxAtom(wid, _)                   -> Format.printf "@[(outer@ :@ %s)@]@ " (SkipLength.show wid)
    ) ;
    Format.printf ")@]@ " ;
  end


let () =
  let ( ~% ) = SkipLength.of_pdf_point in
  begin
    FontInfo.initialize () ;
    let font0 = ("TimesIt", ~% 16.) in
    let font1 = ("Hlv", ~% 16.) in
    let word s = HorzFixedBoxAtom(FixedString(font0, s)) in
    let word1 s = HorzFixedBoxAtom(FixedString(font1, s)) in
    let space = HorzDiscretionary(Some(HorzOuterBoxAtom(OuterEmpty(~% 8., ~% 1., ~% 4.))), None, None) in
    let fill = HorzOuterBoxAtom(OuterFil) in
    let soft_hyphen = HorzDiscretionary(None, Some(HorzFixedBoxAtom(FixedString(font0, "-"))), None) in
    let evvblst =
      break_horz_box_list [
        word "discre"; soft_hyphen; word "tionary"; space; word "hyphen"; space;
(*        word1 "discre"; soft_hyphen; word1 "tionary"; space; word1 "hyphen"; space; *)
        word1 "5000"; space; word1 "cho-yen"; space; word1 "hoshii!"; space;
        word "discre"; soft_hyphen; word "tionary"; space; word "hyphen"; space;
        word "The"; space; word "quick"; space; word "brown"; space; word "fox"; space;
        word "jumps"; space; word "over"; space; word "the"; space; word "lazy"; space; word "dog.";
        space;
        word "My"; space; word "quiz"; space; word "above"; space; word "the"; space; word "kiwi"; space; word "juice"; space;
        word "needs"; space; word "price"; soft_hyphen ; word "less"; space; word "fixing."; fill;
      ]
    in
    begin
      Format.printf "--------@\n" ;
      List.iter print_evaled_vert_box evvblst ;
      Format.printf "@\n--------@\n" ;
      HandlePdf.write_vert_lines evvblst ;
    end
  end
