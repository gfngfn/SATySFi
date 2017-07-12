
let print_for_debug msg =
  print_endline msg


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


let convert_box_for_line_breaking = function
  | HorzDiscretionary(_, _, _) -> assert false
  | HorzFixedBoxAtom(hfa)      -> let wid = size_of_horz_fixed_atom hfa in LBHorzFixedBoxAtom(wid, hfa)
  | HorzOuterBoxAtom(hoa)      -> let wid = size_of_horz_outer_atom hoa in LBHorzOuterBoxAtom(wid, hoa)


let convert_box_for_line_breaking_opt (hbopt : horz_box option) =
  match hbopt with
  | None     -> None
  | Some(hb) -> Some(convert_box_for_line_breaking hb)


let get_natural_width = function
  | LBHorzDiscretionary(_, _, _, _) -> assert false
  | LBHorzFixedBoxAtom(wid, _)      -> wid
  | LBHorzOuterBoxAtom(wid, _)      -> wid


let get_natural_width_opt = function
  | None      -> 0
  | Some(lhb) -> get_natural_width lhb


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
    let show_weight = string_of_int
    let add = ( + )
    let compare w1 w2 = w1 - w2
    let zero = 0
  end)


let paragraph_width = 50000 (* temporary; should be variable *)

let determine_widths (required_width : skip_width) (lhblst : lb_horz_box list) : evaled_horz_box list * badness =
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
          | LBHorzOuterBoxAtom(_, OuterEmpty(_, widshrink, widstretch)) -> if is_short then widstretch else widshrink  (* -- both are positive -- *)
          | LBHorzDiscretionary(_, _, _, _)                             -> assert false
        ) |> List.fold_left (+) 0
      in
      let ( ~. ) = float_of_int in
      let ( ~@ ) = int_of_float in
      let ratio =
        if stretchable_width = 0 then  (* -- when no box is stretchable/shrinkable -- *)
          0.0
        else
          (~. (required_width - natural_width)) /. (~. stretchable_width)
      in
      let pairlst =
        lhblst |> List.map (function
          | LBHorzDiscretionary(_, _, _, _)                                        -> assert false
          | LBHorzFixedBoxAtom(wid, hfa)                                           -> (EvHorzFixedBoxAtom(wid, hfa), 0)
          | LBHorzOuterBoxAtom(wid, (OuterEmpty(_, widshrink, widstretch) as hoa)) ->
              let widdiff =
                if is_short then
                  ~@ ((~. widstretch) *. ratio)
                else
                  ~@ ((~. widshrink) *. ratio)
              in
                (EvHorzOuterBoxAtom(wid + widdiff, hoa), abs (~@ (ratio *. 100.0)))
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
        ) |> List.fold_left (+) 0
      in
      let () = print_for_debug ("natural = " ^ (string_of_int natural_width) ^ ", " ^
                                "stretchable = " ^ (string_of_int stretchable_width) ^ ", " ^
                                "ratio = " ^ (string_of_float ratio) ^ ", " ^
                                "checksum = " ^ (string_of_int checksum)) in
      (* end : for debug *)
        (evhblst, badns)
  


let break_into_lines (path : DiscretionaryID.t list) (lhblst : lb_horz_box list) : evaled_vert_box list =
  let rec aux (acclines : evaled_vert_box list) (accline : lb_horz_box list) (lhblst : lb_horz_box list) =
    match lhblst with
    | LBHorzDiscretionary(dscrid, lhbopt0, lhbopt1, lhbopt2) :: tail ->
        if List.mem dscrid path then
          let line         = match lhbopt1 with None -> accline | Some(lhb1) -> lhb1 :: accline in
          let acclinefresh = match lhbopt2 with None -> [] | Some(lhb2) -> lhb2 :: [] in
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

  let get_badness_for_line_breaking (required_width : skip_width) (natural_width : skip_width) : badness =
    let diff = required_width - natural_width in
      match () with
(*      | _ when diff > 5000  -> TooShort *)
      | _ when diff < -5000 -> TooLong
      | _                   -> Badness(abs diff)
    (* temporary; should be like `determine_widths` *)
  in

  let grph = LineBreakGraph.create () in

  let htomit : (DiscretionaryID.t, unit) Hashtbl.t = Hashtbl.create 32 in

  let update_graph (wmap : WidthMap.t) (dscrid : DiscretionaryID.t) (widbreak : skip_width) : bool * WidthMap.t =
    let found_candidate = ref false in
      begin
        LineBreakGraph.add_vertex grph dscrid ;
        found_candidate := false ;
        Hashtbl.clear htomit ;
        wmap |> WidthMap.iter (fun dscridX widX ->
          let badns = get_badness_for_line_breaking paragraph_width (widX + widbreak) in
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
        let wid0 = get_natural_width_opt lhbopt0 in
        let wid1 = get_natural_width_opt lhbopt1 in
        let wid2 = get_natural_width_opt lhbopt2 in
        let (found, wmapsub) = update_graph wmap dscrid wid1 in
        let wmapnew =
          if found then
            wmapsub |> WidthMap.add_width_all wid0 |> WidthMap.add dscrid wid2
          else
            wmapsub |> WidthMap.add_width_all wid0
        in
          aux wmapnew tail

    | hb :: tail ->
        let wid = get_natural_width hb in
        let wmapnew = wmap |> WidthMap.add_width_all wid in
          aux wmapnew tail

    | [] ->
        let dscrid = DiscretionaryID.final in
        let (_, wmapfinal) = update_graph wmap dscrid 0 in
          wmapfinal
  in
  let wmapinit = WidthMap.empty |> WidthMap.add DiscretionaryID.beginning 0 in
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
      | EvHorzFixedBoxAtom(wid, FixedString(_, str)) -> Format.printf "@[(fixed@ \"%s\"@ :@ %d)@]@ " str wid
      | EvHorzOuterBoxAtom(wid, _)                   -> Format.printf "@[(outer@ :@ %d)@]@ " wid
    ) ;
    Format.printf ")@]@ " ;
  end


let () =
  begin
    FontInfo.initialize () ;
    let hlv = ("Hlv", 32) in
    let word s = HorzFixedBoxAtom(FixedString(hlv, s)) in
    let space = HorzDiscretionary(Some(HorzOuterBoxAtom(OuterEmpty(1000, 100, 500))), None, None) in
    let soft_hyphen = HorzDiscretionary(None, Some(HorzFixedBoxAtom(FixedString(hlv, "-"))), None) in
    let evvblst =
      break_horz_box_list [
        word "hyphen"; space;
        word "discre"; soft_hyphen; word "tionary"; space; word "hyphen"; space;
        word "discre"; soft_hyphen; word "tionary"; space;
        word "The"; space; word "quick"; space; word "brown"; space; word "fox"; space;
        word "jumps"; space; word "over"; space; word "the"; space; word "lazy"; space; word "dog.";
        space;
        word "My"; space; word "quiz"; space; word "above"; space; word "the"; space; word "kiwi"; space; word "juice"; space;
        word "needs"; space; word "priceless"; space; word "fixing.";
      ]
    in
    begin
      Format.printf "--------@\n" ;
      List.iter print_evaled_vert_box evvblst ;
      Format.printf "@\n--------@\n" ;
    end
  end
