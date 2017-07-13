
let print_for_debug msg =
  print_endline msg

let ( ~. ) = float_of_int
let ( ~@ ) = int_of_float


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


let size_of_horz_fixed_atom (hfa : horz_fixed_atom) : SkipLength.t * SkipLength.t * SkipLength.t =
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
        let wid = FontInfo.get_width_of_word fntabrv size uchlst in
          (wid, SkipLength.zero, SkipLength.zero) (* temporary; should get height and depth *)


let size_of_horz_outer_atom (hoa : horz_outer_atom) : SkipLength.t * SkipLength.t * SkipLength.t =
  match hoa with
  | OuterEmpty(wid, widshrink, widstretch) -> (wid, widshrink, widstretch)


let convert_box_for_line_breaking = function
  | HorzDiscretionary(_, _, _) -> assert false
  | HorzFixedBoxAtom(hfa)      -> let (wid, _, _) = size_of_horz_fixed_atom hfa in LBHorzFixedBoxAtom(wid, hfa)
  | HorzOuterBoxAtom(hoa)      -> let (wid, _, _) = size_of_horz_outer_atom hoa in LBHorzOuterBoxAtom(wid, hoa)


let convert_box_for_line_breaking_opt (hbopt : horz_box option) =
  match hbopt with
  | None     -> None
  | Some(hb) -> Some(convert_box_for_line_breaking hb)


let get_natural_width = function
  | LBHorzDiscretionary(_, _, _, _) -> assert false
  | LBHorzFixedBoxAtom(wid, _)      -> wid
  | LBHorzOuterBoxAtom(wid, _)      -> wid


let get_natural_width_opt = function
  | None      -> SkipLength.zero
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
      wmap |> DiscretionaryIDMap.map (fun dist -> dist +% wid)

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


let paragraph_width = SkipLength.of_pdf_point 800.0 (* temporary; should be variable *)

let determine_widths (required_width : skip_width) (lhblst : lb_horz_box list) : evaled_horz_box list * badness =
  let natural_width =
    lhblst |> List.map (function
      | LBHorzFixedBoxAtom(wid, _)      -> wid
      | LBHorzOuterBoxAtom(wid, _)      -> wid
      | LBHorzDiscretionary(_, _, _, _) -> assert false
    ) |> List.fold_left (+%) SkipLength.zero
  in
    let is_short = (natural_width <= required_width) in
      let stretchable_width =
        lhblst |> List.map (function
          | LBHorzFixedBoxAtom(_, _)                                    -> SkipLength.zero
          | LBHorzOuterBoxAtom(_, OuterEmpty(_, widshrink, widstretch)) -> if is_short then widstretch else widshrink  (* -- both are positive -- *)
          | LBHorzDiscretionary(_, _, _, _)                             -> assert false
        ) |> List.fold_left ( +% ) SkipLength.zero
      in
      let ratio =
        if SkipLength.is_nearly_zero stretchable_width then  (* -- when no box is stretchable/shrinkable -- *)
          0.0
        else
          (required_width -% natural_width) /% stretchable_width
      in
      let pairlst =
        lhblst |> List.map (function
          | LBHorzDiscretionary(_, _, _, _)                                        -> assert false
          | LBHorzFixedBoxAtom(wid, hfa)                                           -> (EvHorzFixedBoxAtom(wid, hfa), 0)
          | LBHorzOuterBoxAtom(wid, (OuterEmpty(_, widshrink, widstretch) as hoa)) ->
              let widdiff =
                if is_short then
                  widstretch *% ratio
                else
                  widshrink *% ratio
              in
                (EvHorzOuterBoxAtom(wid +% widdiff, hoa), abs (~@ (ratio *. 100.0)))
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
      let () = print_for_debug ("natural = " ^ (SkipLength.show natural_width) ^ ", " ^
                                "stretchable = " ^ (SkipLength.show stretchable_width) ^ ", " ^
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

  let get_badness_for_line_breaking (required_width : skip_width) (natural_width : skip_width) : badness =
    let widdiff = required_width -% natural_width in
    let widcriterion = SkipLength.of_pdf_point (-80.0) in
      match () with
(*      | _ when diff > 5000  -> TooShort *)
      | _ when widdiff <% widcriterion -> TooLong
      | _                              -> Badness(abs (~@ ((widdiff /% widcriterion) *. 10000.)))
    (* temporary; should be like `determine_widths` *)
  in

  let grph = LineBreakGraph.create () in

  let htomit : (DiscretionaryID.t, unit) Hashtbl.t = Hashtbl.create 32 in

  let found_candidate = ref false in

  let update_graph (wmap : WidthMap.t) (dscrid : DiscretionaryID.t) (widbreak : skip_width) : bool * WidthMap.t =

      begin
        LineBreakGraph.add_vertex grph dscrid ;
        found_candidate := false ;
        Hashtbl.clear htomit ;
        wmap |> WidthMap.iter (fun dscridX widX ->
          let badns = get_badness_for_line_breaking paragraph_width (widX +% widbreak) in
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
        let (_, wmapfinal) = update_graph wmap dscrid SkipLength.zero in
          wmapfinal
  in
  let wmapinit = WidthMap.empty |> WidthMap.add DiscretionaryID.beginning SkipLength.zero in
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
    let hlv = ("Hlv", ~% 32.) in
    let word s = HorzFixedBoxAtom(FixedString(hlv, s)) in
    let space = HorzDiscretionary(Some(HorzOuterBoxAtom(OuterEmpty(~% 32., ~% 3.2, ~% 16.))), None, None) in
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
