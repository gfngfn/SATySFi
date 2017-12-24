
open HorzBox


type cell =
  | NormalCell of horz_box list
  | EmptyCell
  | MultiCell  of int * int * horz_box list

type rest_column = ((int * length) option) list

type column = cell list

(*
let calculate_width (tabular : (cell list) list) =
  tabular |> List.fold_left (fun acc row ->
    row |> List.fold_left (fun acc cell ->
      match cell with
      | NormalCell(hblst) -> ()
      | EmptyCell         -> ()
    ) ()
  ) ()
*)

let determine_column_width (restprev : rest_column) (col : column) : rest_column * length =
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
              let (wid, _, _) = LineBreak.get_natural_metrics hblst in
              let widmaxnew = Length.max wid widmax in
                aux (None :: restacc) widmaxnew rtail ctail

          | EmptyCell ->
              aux (None :: restacc) widmax rtail ctail

          | MultiCell(rownum, colnum, hblst) ->
              let (wid, _, _) = LineBreak.get_natural_metrics hblst in
              aux (Some((rownum, wid)) :: restacc) widmax rtail ctail
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


let solidify_tabular (vtabular : column list) =
  let ncols = List.length vtabular in
  let make n c =
    let rec aux acc n =
      if n <= 0 then List.rev acc else
        aux (c :: acc) (n - 1)
    in
    aux [] n
  in
  vtabular |> List.fold_left (fun (restprev, acc) col ->
    let (rest, wid) = determine_column_width restprev col in
    (rest, wid :: acc)
  ) (make ncols None, [])
