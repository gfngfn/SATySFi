open HorzBox

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
  | LBHorzFixedBoxAtom  of skip_width * horz_fixed_atom
  | LBHorzOuterBoxAtom  of skip_width * horz_outer_atom
  | LBHorzDiscretionary of DiscretionaryID.t * lb_horz_box option * lb_horz_box option * lb_horz_box option


let size_of_horz_fixed_atom (hfa : horz_fixed_atom) =
  let uchar_list_of_string str =
    let rec aux acc i =
      if i < 0 then List.rev acc else
        aux ((Uchar.of_char (String.get str i)) :: acc) (i - 1)
    in
      aux [] ((String.length str) - 1)
  in
    match hfa with
    | FixedString((fntabrv, size), str) ->
        let uchlst = uchar_list_of_string str in
        let wid = FontInfo.get_width_of_word fntabrv uchlst in
          wid (* temporary; should get height and depth *)


let size_of_horz_outer_atom (hoa : horz_outer_atom) =
  match hoa with
  | OuterEmpty(wid, _, _) -> wid


let get_natural_width (hb : horz_box) =
  match hb with
  | HorzDiscretionary(_, _, _) -> assert false
  | HorzFixedBoxAtom(hfa)      -> let wid = size_of_horz_fixed_atom hfa in (wid, LBHorzFixedBoxAtom(wid, hfa))
  | HorzOuterBoxAtom(hoa)      -> let wid = size_of_horz_outer_atom hoa in (wid, LBHorzOuterBoxAtom(wid, hoa))


let get_natural_width_opt (hbopt : horz_box option) =
  match hbopt with
  | None     -> (0, None)
  | Some(hb) -> let (wid, lhb) = get_natural_width hb in (wid, Some(lhb))


module WidthMap
: sig
    type t
    val empty : t
    val add_width_all : skip_width -> t -> t
    val add : DiscretionaryID.t -> skip_width -> t -> t
    val iter : (DiscretionaryID.t -> skip_width -> unit) -> t -> unit
    val remove : DiscretionaryID.t -> t -> t
  end
= struct

    module DiscretionaryIDMap = Map.Make(
      struct
        type t = DiscretionaryID.t
        let compare = compare
      end)

    type t = skip_width DiscretionaryIDMap.t

    let empty = DiscretionaryIDMap.empty

    let add = DiscretionaryIDMap.add

    let iter = DiscretionaryIDMap.iter

    let add_width_all (wid : skip_width) (wmap : t) : t =
      wmap |> DiscretionaryIDMap.map (fun dist -> dist + wid)

    let remove = DiscretionaryIDMap.remove

  end


module LineBreakGraph = FlowGraph.Make(
  struct
    type t = DiscretionaryID.t
    type weight = pure_badness
    let equal = DiscretionaryID.equal
    let hash = Hashtbl.hash
    let show = DiscretionaryID.show (* for debug *)
    let add = ( + )
    let compare w1 w2 = w2 - w1
    let zero = 0
  end)


let paragraph_width = 10000 (* temporary; should be variable *)

let determine_widths (required_width : skip_width) (lhblst : lb_horz_box list) : evaled_horz_box list =
  let natural_width =
    lhblst |> List.map (function
      | LBHorzFixedBoxAtom(wid, _)      -> wid
      | LBHorzOuterBoxAtom(wid, _)      -> wid
      | LBHorzDiscretionary(_, _, _, _) -> assert false
    ) |> List.fold_left (+) 0
  in
    let is_short = (natural_width <= required_width) in
      let stretchable_width =
        lhblst |> List.map (function
          | LBHorzFixedBoxAtom(_, _)                                    -> 0
          | LBHorzOuterBoxAtom(_, OuterEmpty(_, widshrink, widstretch)) -> if is_short then widstretch else widshrink
          | LBHorzDiscretionary(_, _, _, _)                             -> assert false
        ) |> List.fold_left (+) 0
      in
      let ( ~. ) = float_of_int in
      let ratio = (~. stretchable_width) /. (~. (required_width - natural_width)) in
        lhblst |> List.map (function
          | LBHorzFixedBoxAtom(wid, hfa)                                    -> EvHorzFixedBoxAtom(wid, hfa)
          | LBHorzOuterBoxAtom(wid, (OuterEmpty(_, _, stretchable) as hoa)) -> EvHorzOuterBoxAtom(wid + (int_of_float ((~. stretchable) *. ratio)), hoa)
          | LBHorzDiscretionary(_, _, _, _)                                 -> assert false
        )
  


let break_into_lines (path : DiscretionaryID.t list) (lhblst : lb_horz_box list) : evaled_vert_box list =
  let rec aux (acclines : evaled_vert_box list) (accline : lb_horz_box list) (lhblst : lb_horz_box list) =
    match lhblst with
    | LBHorzDiscretionary(dscrid, lhbopt0, lhbopt1, lhbopt2) :: tail ->
        if List.mem dscrid path then
          let line =
            match lhbopt1 with None -> accline | Some(lhb1) -> lhb1 :: accline
          in
          let acclinefresh =
            match lhbopt2 with None -> [] | Some(lhb2) -> lhb2 :: []
          in
          let evhblst = determine_widths paragraph_width (List.rev line) in
            aux (EvVertLine(evhblst) :: acclines) acclinefresh tail
        else
          let acclinenew =
            match lhbopt0 with None -> accline | Some(lhb0) -> (lhb0 :: accline)
          in
            aux acclines acclinenew tail

    | hb :: tail ->
        aux acclines (hb :: accline) tail

    | [] ->
        let evhblst = determine_widths paragraph_width (List.rev accline) in
          List.rev (EvVertLine(evhblst) :: acclines)
  in
    aux [] [] lhblst


let break_horz_box_list (hblst : horz_box list) : evaled_vert_box list =

  let get_badness_for_linebreaking (required_width : skip_width) (natural_width : skip_width) : badness =
    let diff = required_width - natural_width in
      match () with
(*      | _ when diff > 100000 -> TooShort *)
      | _ when diff < -1000 -> TooLong
      | _                   -> Badness(abs diff)
    (* temporary; should be like `determine_widths` *)
  in

  let grph = LineBreakGraph.create () in

  let found_candidate = ref false in

  let htomit : (DiscretionaryID.t, unit) Hashtbl.t = Hashtbl.create 32 in

  let update_graph (wmap : WidthMap.t) (dscrid : DiscretionaryID.t) (widbreak : skip_width) : WidthMap.t =
          begin
            LineBreakGraph.add_vertex grph dscrid ;
            found_candidate := false ;
            Hashtbl.clear htomit ;
            wmap |> WidthMap.iter (fun dscridX widX ->
              let badns = get_badness_for_linebreaking paragraph_width (widX + widbreak) in
                match badns with
                | TooShort    -> ()
                | TooLong     ->
                    begin
                      Hashtbl.add htomit dscridX () ;
                    end
                | Badness(pb) ->
                    begin
                      found_candidate := true ;
                      LineBreakGraph.add_edge grph dscridX dscrid pb ;
                    end
            ) ;
            Hashtbl.fold (fun dscrid () wm -> wm |> WidthMap.remove dscrid) htomit wmap
          end
  in

  let rec aux (wmap : WidthMap.t) (acc : lb_horz_box list) (hblst : horz_box list) =
    match hblst with
    | HorzDiscretionary(hbopt0, hbopt1, hbopt2) :: tail -> 
        let (wid0, lhbopt0) = get_natural_width_opt hbopt0 in
        let (wid1, lhbopt1) = get_natural_width_opt hbopt1 in
        let (wid2, lhbopt2) = get_natural_width_opt hbopt2 in
        let dscrid = DiscretionaryID.fresh () in
        let wmapsub = update_graph wmap dscrid wid1 in
        let wmapnew =
          if !found_candidate then
            wmapsub |> WidthMap.add_width_all wid0 |> WidthMap.add dscrid wid2
          else
            wmapsub |> WidthMap.add_width_all wid0
        in
          aux wmapnew (LBHorzDiscretionary(dscrid, lhbopt0, lhbopt1, lhbopt2) :: acc) tail

    | hb :: tail ->
        let (wid, lhb) = get_natural_width hb in
        let wmapnew = wmap |> WidthMap.add_width_all wid in
          aux wmapnew (lhb :: acc) tail

    | [] ->
        let dscrid = DiscretionaryID.final in
        let wmapfinal = update_graph wmap dscrid 0 in
          (List.rev acc, wmapfinal)
  in
  let wmapinit = WidthMap.empty |> WidthMap.add DiscretionaryID.beginning 0 in
  begin
    DiscretionaryID.initialize () ;
    LineBreakGraph.add_vertex grph DiscretionaryID.beginning ;
    let (lhblst, wmapfinal) = aux wmapinit [] hblst in
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
      | EvHorzFixedBoxAtom(wid, FixedString(_, str)) -> Format.printf "@[(fixed@ %s@ :@ %d)@]@ " str wid
      | EvHorzOuterBoxAtom(wid, _)                   -> Format.printf "@[(outer@ :@ %d)@]" wid
    ) ;
    Format.printf ")@]" ;
  end


let () =
  begin
    FontInfo.initialize () ;
    let hlv = ("Hlv", 32) in
    let word s = HorzFixedBoxAtom(FixedString(hlv, s)) in
    let space = HorzDiscretionary(Some(HorzOuterBoxAtom(OuterEmpty(1000, 500, 500))), None, None) in
    let evvblst =
      break_horz_box_list [
        word "The"; space; word "quick"; space; word "brown"; space; word "fox"; space;
        word "jumps"; space; word "over"; space; word "the"; space; word "lazy"; space; word "dog"
      ]
    in
    begin
      Format.printf "--------@\n" ;
      List.iter print_evaled_vert_box evvblst ;
      Format.printf "@\n--------@\n" ;
    end
  end
