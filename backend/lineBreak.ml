open HorzBox

module DiscretionaryID
: sig
    type t
    val initialize : unit -> unit
    val fresh : unit -> t
    val equal : t -> t -> bool
    val beginning : t
    val final : t
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

  end


type horz_box_for_line_break =
  | LBHorzFixedBoxAtom  of skip_width * horz_fixed_atom
  | LBHorzOuterBoxAtom  of skip_width * horz_outer_atom
  | LBHorzDiscretionary of DiscretionaryID.t * horz_box_for_line_break * horz_box_for_line_break * horz_box_for_line_break


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
  | OuterEmpty(wid, _) -> wid


let get_natural_width (hb : horz_box) =
  match hb with
  | HorzDiscretionary(_, _, _) -> assert false
  | HorzFixedBoxAtom(hfa)      -> let wid = size_of_horz_fixed_atom hfa in (wid, LBHorzFixedBoxAtom(wid, hfa))
  | HorzOuterBoxAtom(hoa)      -> let wid = size_of_horz_outer_atom hoa in (wid, LBHorzOuterBoxAtom(wid, hoa))


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
    let add = ( + )
    let compare w1 w2 = w2 - w1
    let zero = 0
  end)


let break_horz_box_list (hblst : horz_box list) =

  let get_badness_for_linebreaking (_ : skip_width) : badness = Badness(100) (* temporary *) in

  let grph = LineBreakGraph.create () in

  let found_candidate = ref false in

  let htomit : (DiscretionaryID.t, unit) Hashtbl.t = Hashtbl.create 32 in

  let update_graph (wmap : WidthMap.t) (dscrid : DiscretionaryID.t) (widbreak : skip_width) : WidthMap.t =
          begin
            LineBreakGraph.add_vertex grph dscrid ;
            found_candidate := false ;
            Hashtbl.clear htomit ;
            wmap |> WidthMap.iter (fun dscridX widX ->
              let badns = get_badness_for_linebreaking (widX + widbreak) in
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

  let rec aux (wmap : WidthMap.t) (acc : horz_box_for_line_break list) (hblst : horz_box list) =
    match hblst with
    | HorzDiscretionary(hb0, hb1, hb2) :: tail -> 
        let (wid0, lhb0) = get_natural_width hb0 in
        let (wid1, lhb1) = get_natural_width hb1 in
        let (wid2, lhb2) = get_natural_width hb2 in
        let dscrid = DiscretionaryID.fresh () in
        let wmapsub = update_graph wmap dscrid wid1 in
        let wmapnew =
          if !found_candidate then
            wmapsub |> WidthMap.add_width_all wid0 |> WidthMap.add dscrid wid2
          else
            wmapsub |> WidthMap.add_width_all wid0
        in
          aux wmapnew (LBHorzDiscretionary(dscrid, lhb0, lhb1, lhb2) :: acc) tail

    | hb :: tail ->
        let (wid, lhb) = get_natural_width hb in
        let wmapnew = wmap |> WidthMap.add_width_all wid in
          aux wmapnew (lhb :: acc) tail

    | [] ->
        let dscrid = DiscretionaryID.final in
        let wmapfinal = update_graph wmap dscrid 0 in
          (List.rev acc, wmapfinal)
  in
  let indent_width = 1000 in (* temporary; should specify indent width (or make it variable) later *)
  let wmapinit = WidthMap.empty |> WidthMap.add DiscretionaryID.beginning indent_width in
  begin
    DiscretionaryID.initialize () ;
    LineBreakGraph.add_vertex grph DiscretionaryID.beginning ;
    let (lhblst, wmapfinal) = aux wmapinit [] hblst in
    let pathopt = LineBreakGraph.shortest_path grph DiscretionaryID.beginning DiscretionaryID.final in
      lhblst (* temporary *)
    (* TODO: will implement here how to break lines according to `wmap` *)
  end


(* for test *)
let () =
  begin
    FontInfo.initialize () ;
    let hlv = ("Hlv", 32) in
    let word s = HorzFixedBoxAtom(FixedString(hlv, s)) in
    let space = HorzDiscretionary(HorzOuterBoxAtom(OuterEmpty(1000, (fun x -> Badness(abs (1000 - x))))),
                                  HorzOuterBoxAtom(OuterEmpty(0, (fun x -> TooLong))),
                                  HorzOuterBoxAtom(OuterEmpty(0, (fun x -> TooLong)))) in
    let _ =
      break_horz_box_list [
        word "The";
        space;
        word "quick";
        space;
        word "brown";
        space;
        word "fox";
      ]
    in ()
  end
