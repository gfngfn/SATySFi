
module AssocMap = Map.Make(String)

type key = AssocMap.key

type 'a t = 'a AssocMap.t


let empty = AssocMap.empty


let rec add amap key value =
  amap |> AssocMap.add key value


let find_opt amap key =
  amap |> AssocMap.find_opt key


let to_list amap =
  AssocMap.fold (fun k v acc ->
    (k, v) :: acc
  ) amap [] |> List.rev


let of_list lst =
  List.fold_left (fun imap (k, v) -> imap |> AssocMap.add k v) empty lst


let map_value f amap =
  AssocMap.map f amap


let iter_value f amap =
  AssocMap.iter (fun _ v -> f v) amap

let iter f amap =
  AssocMap.iter (fun k v -> f k v) amap

let fold_value f init amap =
  AssocMap.fold (fun _ v acc -> f acc v) amap init


let to_value_list amap =
  AssocMap.fold (fun _ v acc ->
    v :: acc
  ) amap [] |> List.rev


let fold f init amap =
  AssocMap.fold (fun k v acc -> f acc k v) amap init


let rec mem key amap =
  AssocMap.mem key amap


let domain_included amap1 amap2 =
  AssocMap.fold (fun k _ b -> b && (mem k amap2)) amap1 true


let domain_same amap1 amap2 =
  (domain_included amap1 amap2) && (domain_included amap2 amap1)


let intersection amap1 amap2 =
  AssocMap.fold (fun k v acc ->
    match AssocMap.find_opt k amap2 with
    | None -> acc
    | Some(w) -> (v, w) :: acc
  ) amap1 [] |> List.rev


let combine_value amap1 amap2 =
  AssocMap.fold (fun k v acc ->
    match AssocMap.find_opt k amap2 with
    | None -> acc
    | Some(w) -> (v, w) :: acc
  ) amap1 [] |> List.rev


let union amap1 amap2 =
  AssocMap.union (fun k v1 v2 -> Some(v2)) amap1 amap2

let cardinal amap =
  AssocMap.cardinal amap

