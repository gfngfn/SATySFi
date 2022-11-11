
open LengthInterface
open HorzBox


type rest_row = ((int * length) option) list

type rest_column = ((int * length) option) list


let determine_row_metrics (restprev : rest_row) (row : row) : rest_row * length * length =
  let rec aux restacc hgtmax dptmin rest row =
    match (rest, row) with
    | ([], []) ->
        (restacc, hgtmax, dptmin)

    | ([], _ :: _)
    | (_ :: _, [])
      -> assert false

    | (None :: rtail, cell :: ctail) ->
        begin
          match cell with
          | NormalCell(pads, hblst) ->
              let (_, hgt, dpt) = LineBreak.get_natural_metrics hblst in
              let hgtmaxnew = Length.max (hgt +% pads.paddingT) hgtmax in
              let dptminnew = Length.min (dpt -% pads.paddingB) dptmin in
              aux (Alist.extend restacc None) hgtmaxnew dptminnew rtail ctail

          | EmptyCell ->
              aux (Alist.extend restacc None) hgtmax dptmin rtail ctail

          | MultiCell(numrow, _numcol, pads, hblst) ->
              let (_, hgt, dpt) = LineBreak.get_natural_metrics hblst in
              let len = (hgt +% pads.paddingT) +% ((Length.negate dpt) +% pads.paddingB) in
                (* needs reconsideration *)
              let restelem =
                if numrow < 1 then
                  assert false
                else if numrow = 1 then
                  None
                else
                  Some(numrow, len)
              in
              aux (Alist.extend restacc restelem) hgtmax dptmin rtail ctail
        end

    | ((Some((numrow, _len)) as rsome) :: rtail, cell :: ctail) ->
        begin
          match cell with
          | NormalCell(_)
          | MultiCell(_, _, _, _)
            -> assert false  (* temporary; maybe should just warn users *)

          | EmptyCell ->
              let (hgtmaxnew, dptminnew) =
                if numrow < 1 then
                  assert false
                else if numrow = 1 then
                  (hgtmax, dptmin)  (* temporary; should consider 'len' *)
                else
                  (hgtmax, dptmin)
              in
              aux (Alist.extend restacc rsome) hgtmaxnew dptminnew rtail ctail
        end
  in
  let (restacc, hgtmax, dptmin) = aux Alist.empty Length.zero Length.zero restprev row in
  let rest =
    restacc |> Alist.to_list |> List.map (function
      | None                -> None
      | Some((1, _))        -> None
      | Some((numrow, len)) -> Some((numrow - 1, len -% hgtmax -% (Length.negate dptmin)))
    )
  in
    (rest, hgtmax, dptmin)


let determine_column_width (restprev : rest_column) (col : column) : rest_column * length =
(*
  Format.printf "Tabular> L(restprev) = %d, L(col) = %d\n" (List.length restprev) (List.length col);
*)
  let rec aux restacc widmax rest col =
    match (rest, col) with
    | ([], []) ->
        (restacc, widmax)

    | ([], _ :: _)
    | (_ :: _, [])
      ->
        assert false

    | (None :: rtail, cell :: ctail) ->
        begin
          match cell with
          | NormalCell(pads, hblst) ->
              let (wid, _, _) = LineBreak.get_natural_metrics hblst in
              let widmaxnew = Length.max (pads.paddingL +% wid +% pads.paddingR) widmax in
                aux (Alist.extend restacc None) widmaxnew rtail ctail

          | EmptyCell ->
              aux (Alist.extend restacc None) widmax rtail ctail

          | MultiCell(_numrow, numcol, pads, hblst) ->
              let (widraw, _, _) = LineBreak.get_natural_metrics hblst in
              let wid = pads.paddingL +% widraw +% pads.paddingR in
              let widmaxnew =
                if numcol < 1 then
                  assert false
                else if numcol = 1 then
                  Length.max widmax wid
                else
                  widmax
              in
              aux (Alist.extend restacc (Some((numcol, wid)))) widmaxnew rtail ctail
                (* temporary; does not take 'numcol' into consideration *)
        end

    | ((Some((numcol, widrest)) as rsome) :: rtail, cell :: ctail) ->
        begin
          match cell with
          | NormalCell(_)
          | MultiCell(_)
            -> assert false  (* temporary; maybe should just warn users *)

          | EmptyCell ->
              let widmaxnew =
                if numcol < 1 then
                  assert false
                else if numcol = 1 then
                  Length.max widrest widmax
                else
                  widmax
              in
              aux (Alist.extend restacc rsome) widmaxnew rtail ctail
        end
  in
  let (restacc, widmax) = aux Alist.empty Length.zero restprev col in
  let rest =
    restacc |> Alist.to_list |> List.map (function
      | None                -> None
      | Some((1, _))        -> None
      | Some((numcol, wid)) -> Some((numcol - 1, wid -% widmax))
    )
  in
    (rest, widmax)


(* -- chop the leftmost column from a (row-first) tabular -- *)
let chop_column (tabular : row list) : (column * row list) option =
  let sepoptlst =
    tabular |> List.map (fun row ->
      match row with
      | []              -> None
      | cell :: rowtail -> Some((cell, rowtail))
    )
  in
  let is_final =
    sepoptlst |> List.fold_left (fun b sepopt ->
      match sepopt with
      | None    -> b
      | Some(_) -> false
    ) true
  in
  if is_final then
  (* -- if the given tabular has no cell -- *)
    None
  else
    let seplst =
      sepoptlst |> List.map (function
        | None      -> (EmptyCell, [])
        | Some(sep) -> sep
      )
    in
    let col = seplst |> List.map (fun (cell, _) -> cell) in
    let tabularsub = seplst |> List.map (fun (_, tail) -> tail) in
      Some(col, tabularsub)


let transpose_tabular (tabular : row list) : int * column list =
  let rec aux nrows colacc tabular =
    match chop_column tabular with
    | None                    -> (nrows, Alist.to_list colacc)
    | Some((col, tabularsub)) -> aux (max nrows (List.length col)) (Alist.extend colacc col) tabularsub
  in
    aux 0 Alist.empty tabular


let normalize_tabular (tabular : row list) : int * row list =
  let ncols =
    tabular |> List.fold_left (fun nrowsmax row -> max nrowsmax (List.length row)) 0
  in
  let htabular =
    tabular |> List.fold_left (fun acc row ->
      let empties = List.init (ncols - (List.length row)) (fun _ -> EmptyCell) in
        Alist.extend acc (List.append row empties)
    ) Alist.empty |> Alist.to_list
  in
    (ncols, htabular)


let access arr index =
  try arr.(index) with
  | Invalid_argument(_) -> assert false


let multi_cell_width widarr indexC nc =
(*
  Format.printf "Tabular> L(widarr) = %d\n" (Array.length widarr);
*)
  let rec aux len i =
(*
    Format.printf "Tabular> access C %d\n" i;
*)
    let lennew = len +% widarr.(i) in
      if i >= indexC + nc - 1 then
        lennew
      else
        aux lennew (i + 1)
  in
  try
    aux Length.zero indexC
  with
  | Invalid_argument(_) -> assert false


let multi_cell_vertical vmetrarr indexR nr =
(*
  Format.printf "Tabular> L(vmetrarr) = %d\n" (Array.length vmetrarr);
*)
  let rec aux len i =
(*
    Format.printf "Tabular> access R %d\n" i;
*)
    let (hgt, dpt) = vmetrarr.(i) in
    let lennew = len +% hgt +% (Length.negate dpt) in
      if i >= indexR + nr - 1 then
        lennew
      else
        aux lennew (i + 1)
  in
  try
    aux Length.zero indexR
  with
  | Invalid_argument(_) -> assert false


let solidify_tabular (vmetrlst : (length * length) list) (widlst : length list) (htabular : row list) : intermediate_row list =
  let vmetrarr = Array.of_list vmetrlst in
  let widarr = Array.of_list widlst in
  htabular |> Core.List.foldi ~f:(fun indexR evrowacc row ->
    let (hgtnmlcell, dptnmlcell) = access vmetrarr indexR in
    let evrow =
      row |> Core.List.foldi ~f:(fun indexC evcellacc cell ->
        let evcell =
          match cell with
          | EmptyCell ->
              let wid = access widarr indexC in
                ImEmptyCell(wid)

          | NormalCell(pads, hblst) ->
              let wid = access widarr indexC in
              let hblstwithpads =
                List.concat [
                  [ HorzPure(PHSFixedEmpty{ width = pads.paddingL }) ];
                  hblst;
                  [ HorzPure(PHSFixedEmpty{ width = pads.paddingR }) ];
                ]
              in
              let (imhbs, ratios, _hgt, _dpt) = LineBreak.fit hblstwithpads wid in
                ImNormalCell(ratios, (wid, hgtnmlcell, dptnmlcell), imhbs)
                (* temporary; should return information about vertical psitioning *)

          | MultiCell(nr, nc, pads, hblst) ->
              let widsingle = access widarr indexC in
(*
              Format.printf "Tabular> indexC = %d, nc = %d\n" indexC nc;
*)
              let widmulti = multi_cell_width widarr indexC nc in
              let hblstwithpads =
                List.concat [
                  [ HorzPure(PHSFixedEmpty{ width = pads.paddingL }) ];
                  hblst;
                  [ HorzPure(PHSFixedEmpty{ width = pads.paddingR }) ];
                ]
              in
              let (imhbs, ratios, hgt, dpt) = LineBreak.fit hblstwithpads widmulti in
              let (hgtcell, dptcell) =
                if nr < 1 then
                  assert false
                else if nr = 1 then
                  access vmetrarr indexR
                else
                  let vlencell = multi_cell_vertical vmetrarr indexR nr in
                  let vlencontent = hgt +% (Length.negate dpt) in
                  let lenspace = (vlencell -% vlencontent) *% 0.5 in
                    (hgt +% lenspace, dpt -% lenspace)
              in
                ImMultiCell(ratios, (nr, nc, widsingle, widmulti, hgtcell, dptcell), imhbs)
        in
          Alist.extend evcellacc evcell
      ) ~init:Alist.empty |> Alist.to_list
    in
    let vlen = hgtnmlcell +% (Length.negate dptnmlcell) in
      Alist.extend evrowacc (vlen, evrow)
  ) ~init:Alist.empty |> Alist.to_list


let main (tabular : row list) : intermediate_row list * length list * length list * length * length * length =

  let (ncols, htabular) = normalize_tabular tabular in
  let (nrows, vtabular) = transpose_tabular tabular in
(*
  Format.printf "nrows = %d, ncols = %d\n" nrows ncols;
*)
  let (_, vmetracc) =
    htabular |> List.fold_left (fun (restprev, vmetracc) row ->
(*
      Format.printf "Tabular> L(row) = ncols = %d\n" (List.length row);
*)
      let (rest, hgt, dpt) = determine_row_metrics restprev row in
        (rest, Alist.extend vmetracc (hgt, dpt))
    ) (List.init ncols (fun _ -> None), Alist.empty)
  in
  let vmetrlst = Alist.to_list vmetracc in

  let (_, widacc) =
    vtabular |> List.fold_left (fun (restprev, widacc) col ->
(*
      Format.printf "Tabular> L(col) = nrows = %d\n" (List.length col);
*)
      let (rest, wid) = determine_column_width restprev col in
      (rest, Alist.extend widacc wid)
    ) (List.init nrows (fun _ -> None), Alist.empty)
  in
  let widlst = Alist.to_list widacc in

  let widtotal = List.fold_left (+%) Length.zero widlst in
  let hgttotal = List.fold_left (fun len (hgt, dpt) -> len +% hgt +% (Length.negate dpt)) Length.zero vmetrlst in
  let dpttotal = Length.zero in
  let imtabular = solidify_tabular vmetrlst widlst htabular in
  let lenlst = vmetrlst |> List.map (fun (hgt, dpt) -> hgt +% (Length.negate dpt)) in
    (imtabular, widlst, lenlst, widtotal, hgttotal, dpttotal)
