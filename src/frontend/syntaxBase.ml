
let pp_set fold _pp_elem ppf set =
  Format.fprintf ppf "@[<hv1>{";
  fold (fun k is_first ->
    begin
      if is_first then
        Format.fprintf ppf "%s" k
      else
        Format.fprintf ppf ",@ %s" k
    end;
    false
  ) set true |> ignore;
  Format.fprintf ppf "@]"


let pp_map fold pp_k pp_v ppf map =
  Format.fprintf ppf "@[<hv1>{";
  fold (fun k v is_first ->
    begin
      if is_first then
        Format.fprintf ppf "%a -> %a" pp_k k pp_v v
      else
        Format.fprintf ppf ",@ %a -> %a" pp_k k pp_v v
    end;
    false
  ) map true |> ignore;
  Format.fprintf ppf "@]"


module LabelSet = struct
  include Set.Make(String)


  let pp_elem ppf label =
    Format.fprintf ppf "\"%s\"" label


  let pp ppf labset =
    pp_set fold pp_elem ppf labset

end


module LabelMap = struct
  include Map.Make(String)


  let pp_k ppf label =
    Format.fprintf ppf "\"%s\"" label


  let pp pp_v ppf labmap =
    pp_map fold pp_k pp_v ppf labmap

end
