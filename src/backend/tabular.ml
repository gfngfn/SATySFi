
open HorzBox
open Util


type rest_row = ((int * length) option) list

type rest_column = ((int * length) option) list


let determine_row_metrics (natmetrf : horz_box list -> length * length * length) (restprev : rest_row) (row : row) : rest_row * length * length =
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
          | NormalCell(hblst) ->
              let (_, hgt, dpt) = natmetrf hblst in
              let hgtmaxnew = Length.max hgt hgtmax in
              let dptminnew = Length.min dpt dptmin in
              aux (None :: restacc) hgtmaxnew dptminnew rtail ctail

          | EmptyCell ->
              aux (None :: restacc) hgtmax dptmin rtail ctail

          | MultiCell(numrow, numcol, hblst) ->
              let (_, hgt, dpt) = natmetrf hblst in
              let len = hgt +% (Length.negate dpt) in
                (* needs reconsideration *)
              aux (Some(numcol, len) :: restacc) hgtmax dptmin rtail ctail
        end

    | ((Some((numcol, len)) as rsome) :: rtail, cell :: ctail) ->
        begin
          match cell with
          | NormalCell(_)
          | MultiCell(_, _, _)
            -> assert false  (* temporary; maybe should just warn users *)
              
          | EmptyCell ->
              let (hgtmaxnew, dptminnew) =
                if numcol < 1 then
                  assert false
                else if numcol = 1 then
                  (hgtmax, dptmin)  (* temporary; should consider 'len' *)
                else
                  (hgtmax, dptmin)
              in
              aux (rsome :: restacc) hgtmaxnew dptminnew rtail ctail
        end
  in
  let (restacc, hgtmax, dptmin) = aux [] Length.zero Length.zero restprev row in
  let rest =
    restacc |> List.map (function
      | None                -> None
      | Some((numcol, len)) -> Some((numcol - 1, len -% hgtmax -% (Length.negate dptmin)))
    ) |> List.rev
  in
    (rest, hgtmax, dptmin)


let determine_column_width (natmetrf : horz_box list -> length * length * length) (restprev : rest_column) (col : column) : rest_column * length =
  let rec aux restacc widmax rest col =
    match (rest, col) with
    | ([], []) ->
        (restacc, widmax)

    | ([], _ :: _)
    | (_ :: _, [])
      -> assert false

    | (None :: rtail, cell :: ctail) ->
        begin
          match cell with
          | NormalCell(hblst) ->
              let (wid, _, _) = natmetrf hblst in
              let widmaxnew = Length.max wid widmax in
                aux (None :: restacc) widmaxnew rtail ctail

          | EmptyCell ->
              aux (None :: restacc) widmax rtail ctail

          | MultiCell(rownum, colnum, hblst) ->
              let (wid, _, _) = natmetrf hblst in
              aux (Some((rownum, wid)) :: restacc) widmax rtail ctail
                (* temporary; does not take 'colnum' into consideration *)
        end

    | ((Some((rownumrest, widrest)) as rsome) :: rtail, cell :: ctail) ->
        begin
          match cell with
          | NormalCell(_)
          | MultiCell(_)
            -> assert false  (* temporary; maybe should just warn users *)

          | EmptyCell ->
              let widmaxnew =
                if rownumrest < 1 then
                  assert false
                else if rownumrest = 1 then
                  Length.max widrest widmax
                else
                  widmax
              in
              aux (rsome :: restacc) widmaxnew rtail ctail
        end
  in
  let (restacc, widmax) = aux [] Length.zero restprev col in
  let rest =
    restacc |> List.map (function
      | None                -> None
      | Some((rownum, wid)) -> Some((rownum - 1, wid -% widmax))
    ) |> List.rev
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
  let rec aux ncols colacc tabular =
    match chop_column tabular with
    | None                    -> (ncols, List.rev colacc)
    | Some((col, tabularsub)) -> aux (ncols + 1) (col :: colacc) tabularsub
  in
    aux 0 [] tabular


let normalize_tabular (tabular : row list) : int * row list =
  let nrows =
    tabular |> List.fold_left (fun nrowsmax row -> max nrowsmax (List.length row)) 0
  in
  let htabular =
    tabular |> List.fold_left (fun acc row ->
      let empties = list_make (nrows - (List.length row)) EmptyCell in
        (List.append row empties) :: acc
    ) [] |> List.rev
  in
    (nrows, htabular)


let multi_cell_width widarr indexC nc =
  let rec aux len i =
    if i > nc then len else
      aux (len +% widarr.(i)) (i + 1)
  in
    aux Length.zero indexC


let multi_cell_vertical vmetrarr indexR nr =
  let rec aux len i =
    if i > nr then len else
      let (hgt, dpt) = vmetrarr.(i) in
        aux (len +% hgt +% (Length.negate dpt)) (i + 1)
  in
    aux Length.zero indexR


let solidify_tabular (fitf : horz_box list -> length -> evaled_horz_box list * length * length) (vmetrlst : (length * length) list) (widlst : length list) (htabular : row list) : evaled_row list =
  let vmetrarr = Array.of_list vmetrlst in
  let widarr = Array.of_list widlst in
  htabular |> list_fold_left_index (fun indexR evrowacc row ->
    let (hgtnmlcell, dptnmlcell) = vmetrarr.(indexR) in
    let evrow =
      row |> list_fold_left_index (fun indexC evcellacc cell ->
        let evcell =
          match cell with
          | EmptyCell ->
              let wid = widarr.(indexC) in
                EvEmptyCell(wid)

          | NormalCell(hblst) ->
              let wid = widarr.(indexC) in
              let (evhblst, hgt, dpt) = fitf hblst wid in
                EvNormalCell(hgtnmlcell, dptnmlcell, evhblst)
                (* temporary; should return information about vertical psitioning *)

          | MultiCell(nr, nc, hblst) ->
              let widsingle = widarr.(indexC) in
              let widmulti = multi_cell_width widarr indexC nc in
              let vlencell = multi_cell_vertical vmetrarr indexC nc in
              let (evhblst, hgt, dpt) = fitf hblst widmulti in
              let vlencontent = hgt +% (Length.negate dpt) in
              let lenspace = (vlencell -% vlencontent) *% 0.5 in
              let hgtcell = hgt +% lenspace in
              let dptcell = dpt -% lenspace in
                EvMultiCell(nr, nc, widsingle, hgtcell, dptcell, evhblst)
        in
        evcell :: evcellacc
      ) [] |> List.rev
    in
    let vlen = hgtnmlcell +% (Length.negate dptnmlcell) in
      (vlen, evrow) :: evrowacc
  ) []


let main (fitf : horz_box list -> length -> evaled_horz_box list * length * length) (natmetrf : horz_box list -> length * length * length) (tabular : row list) =
  let (nrows, htabular) = normalize_tabular tabular in
  let (ncols, vtabular) = transpose_tabular tabular in
  let (_, vmetrlst) =
    htabular |> List.fold_left (fun (restprev, acc) row ->
      let (rest, hgt, dpt) = determine_row_metrics natmetrf restprev row in
        (rest, (hgt, dpt) :: acc)
    ) (list_make nrows None, [])
  in
  let (_, widlst) =
    vtabular |> List.fold_left (fun (restprev, acc) col ->
      let (rest, wid) = determine_column_width natmetrf restprev col in
      (rest, wid :: acc)
    ) (Util.list_make ncols None, [])
  in
  let widtotal = List.fold_left (+%) Length.zero widlst in
  let hgttotal = List.fold_left (fun len (hgt, dpt) -> len +% hgt +% (Length.negate dpt)) Length.zero vmetrlst in
  let dpttotal = Length.zero in
  let evtabular = solidify_tabular fitf vmetrlst widlst htabular in
    (evtabular, widtotal, hgttotal, dpttotal)
