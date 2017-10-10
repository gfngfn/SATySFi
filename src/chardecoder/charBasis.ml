
type code_point = int

type code_point_kind =
  | CodePoint      of code_point
  | CodePointRange of code_point * code_point


let add_to_map cp scr umap =
  match UCoreLib.UChar.of_int cp with
  | Some(uch_ucore) -> umap |> UCoreLib.UMap.add uch_ucore scr
  | None            -> umap  (* needs reconsideration; maybe should cause an error *)


let add_range_to_map cp1 cp2 scr umap =
  match (UCoreLib.UChar.of_int cp1, UCoreLib.UChar.of_int cp2) with
  | (Some(uch_ucore1), Some(uch_ucore2)) -> umap |> UCoreLib.UMap.add_range uch_ucore1 uch_ucore2 scr
  | _                                    -> umap  (* needs reconsideration; maybe should canse an error *)


let map_of_list (type a) (readf : string -> a) (lst : (code_point_kind * string) list) =
  lst |> List.fold_left (fun accmap elem ->
    match elem with
    | (CodePoint(cp), data)            -> accmap |> add_to_map cp (readf data)
    | (CodePointRange(cp1, cp2), data) -> accmap |> add_range_to_map cp1 cp2 (readf data)
  ) (UCoreLib.UMap.empty ~eq:(=))

