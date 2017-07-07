
type skip_length = float
type skip_width  = skip_length
type skip_height = skip_length
type skip_depth  = skip_length

type badness = int

type font_info = string * int

type horz_fixed_atom =
  | FixedString of font_info * string

type horz_outer_atom =
  | OuterEmpty of skip_width * (skip_width -> float)


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


type horz_box =
  | HorzFixedBoxAtom  of horz_fixed_atom
  | HorzOuterBoxAtom  of horz_outer_atom
  | HorzDiscretionary of DiscretionaryID.t * horz_box * horz_box * horz_box


let size_of_horz_fixed_atom (hfa : horz_fixed_atom) : (skip_width * skip_height * skip_depth) =
  match hfa with
  | FixedString(fnt, s) -> (1.0, 1.0, 0.5) (* temporary *)


let get_natural_width (hb : horz_box) =
  match hb with
  | HorzDiscretionary(_, _, _, _) -> assert false
  | HorzFixedBoxAtom(hfa)         -> let (w, _, _) = size_of_horz_fixed_atom hfa in w
  | HorzOuterBoxAtom(hoa)         -> 1.0 (* temporary *) (* natural_size_of_horz_outer_atom hoa *)




module WidthMap
: sig
    type t
    val empty : t
    val add_width_all : skip_width -> t -> t
    val add : DiscretionaryID.t -> skip_width -> t -> t
    val iter : (DiscretionaryID.t -> skip_width -> unit) -> t -> unit
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
      wmap |> DiscretionaryIDMap.map (fun dist -> dist +. wid)

  end


module LineBreakGraph = FlowGraph.Make(
  struct
    type t = DiscretionaryID.t
    type weight = badness
    let equal = DiscretionaryID.equal
    let hash = Hashtbl.hash
  end)


let break_horz_box_list (hblst : horz_box list) =

  let gr = LineBreakGraph.create () in

  let found_candidate = ref false in

  let breakable _ = true (* temporary *) in

  let get_badness_for_linebreaking _ = 100 (* temporary *) in

  let rec aux (wmap : WidthMap.t) (hblst : horz_box list) =
    match hblst with
    | HorzDiscretionary(dscrid, hb0, hb1, hb2) :: tail -> 
        let wid0 = get_natural_width hb0 in
        let wid1 = get_natural_width hb1 in
        let wid2 = get_natural_width hb2 in
        let () =
          begin
            LineBreakGraph.add_vertex gr dscrid ;
            found_candidate := false ;
            wmap |> WidthMap.iter (fun dscridX widX ->
              let badns = get_badness_for_linebreaking (widX +. wid1) in
                if breakable badns then
                begin
                  found_candidate := true ;
                  LineBreakGraph.add_edge gr dscridX dscrid badns ;
                end
            ) ;
          end
        in
        let wmapnew =
          if !found_candidate then
            wmap |> WidthMap.add_width_all wid0 |> WidthMap.add dscrid wid2
          else
            wmap |> WidthMap.add_width_all wid0
        in
          aux wmapnew tail

    | hb :: tail ->
        let wid = get_natural_width hb in
        let wmapnew = wmap |> WidthMap.add_width_all wid in
          aux wmapnew tail
  in
  let wmapinit = WidthMap.empty |> WidthMap.add DiscretionaryID.beginning 0.0 in (* temporary; 0.0 *)
  begin
    LineBreakGraph.add_vertex gr DiscretionaryID.beginning (* 0 : beginning of the paragraph *) ;
    aux wmapinit hblst
  end

