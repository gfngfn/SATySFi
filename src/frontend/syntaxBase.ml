
module LabelSet = struct
  include Set.Make(String)


  let pp ppf labset =
    Format.fprintf ppf "@[<hv1>{";
    fold (fun k is_first ->
      begin
        if is_first then
          Format.fprintf ppf "%s" k
        else
          Format.fprintf ppf ",@ %s" k
      end;
      false
    ) labset true |> ignore;
    Format.fprintf ppf "@]";

end


module LabelMap = struct
  include Map.Make(String)


  let pp pp_v ppf labmap =
    Format.fprintf ppf "@[<hv1>{";
    fold (fun k v is_first ->
      begin
        if is_first then
          Format.fprintf ppf "%s -> %a" k pp_v v
        else
          Format.fprintf ppf ",@ %s -> %a" k pp_v v
      end;
      false
    ) labmap true |> ignore;
    Format.fprintf ppf "@]"

end
