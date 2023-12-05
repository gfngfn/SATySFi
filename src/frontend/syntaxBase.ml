
let pp_set fold pp_elem ppf set =
  Format.fprintf ppf "@[<hv1>{";
  fold (fun k is_first ->
    begin
      if is_first then
        Format.fprintf ppf "%a" pp_elem k
      else
        Format.fprintf ppf ",@ %a" pp_elem k
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


  (* The monadic enrichment of `LabelMap.fold`. *)
  let foldM (f : key -> 'v -> 'a -> ('a, 'e) result) (labmap : 'v t) (acc : 'a) : ('a, 'e) result =
    let open ResultMonad in
    fold (fun label v res ->
      res >>= fun acc ->
      f label v acc
    ) labmap (return acc)


  (* The monadic enrichment of `LabelMap.merge`. *)
  let mergeM (f : key -> 'v1 option -> 'v2 option -> ('w option, 'e) result) (labmap1 : 'v1 t) (labmap2 : 'v2 t) : ('w t, 'e) result =
    let labmap_res : (('w, 'e) result) t =
      merge (fun label v1_opt v2_opt ->
        match f label v1_opt v2_opt with
        | Ok(None)    -> None
        | Ok(Some(w)) -> Some(Ok(w))
        | Error(e)    -> Some(Error(e))
      ) labmap1 labmap2
    in
    foldM (fun label res labmap ->
      let open ResultMonad in
      res >>= fun w ->
      return (labmap |> add label w)
    ) labmap_res empty


  let pp_k ppf label =
    Format.fprintf ppf "\"%s\"" label


  let pp pp_v ppf labmap =
    pp_map fold pp_k pp_v ppf labmap

end
