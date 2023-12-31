
open SyntaxBase
open Types


type t = {
  current_max   : int;
  free_ids      : string FreeIDMap.t;
  free_row_ids  : (string * LabelSet.t) FreeRowIDMap.t;
  bound_ids     : string BoundIDMap.t;
  bound_row_ids : (string * LabelSet.t) BoundRowIDMap.t;
}


let empty =
  {
    current_max   = 0;
    free_ids      = FreeIDMap.empty;
    free_row_ids  = FreeRowIDMap.empty;
    bound_ids     = BoundIDMap.empty;
    bound_row_ids = BoundRowIDMap.empty;
  }


let make_value (prefix : string) (i : int) =
  let rec aux chs i =
    let q = i / 26 in
    let r = i mod 26 in
    let ch = Char.chr (Char.code 'a' + r) in
    if q <= 0 then
      ch :: chs
    else
      aux (ch :: chs) r
  in
  let chs = aux [] i in
  prefix ^ (Core.String.of_char_list chs)


let add_free_id fid dispmap =
  let fids = dispmap.free_ids in
  match fids |> FreeIDMap.find_opt fid with
  | Some(s) ->
      (dispmap, s)

  | None ->
      let i = dispmap.current_max in
      let s = make_value "'" i in
      let dispmap =
        { dispmap with
          current_max = i + 1;
          free_ids    = fids |> FreeIDMap.add fid s;
        }
      in
      (dispmap, s)


let add_free_row_id frid labset dispmap =
  let frids = dispmap.free_row_ids in
  match frids |> FreeRowIDMap.find_opt frid with
  | Some((s, _)) ->
      (dispmap, s)

  | None ->
    let i = dispmap.current_max in
    let s = make_value "?'" i in
    let dispmap =
      { dispmap with
        current_max  = i + 1;
        free_row_ids = dispmap.free_row_ids |> FreeRowIDMap.add frid (s, labset);
      }
    in
    (dispmap, s)


let add_bound_id bid dispmap =
  let bids = dispmap.bound_ids in
  if bids |> BoundIDMap.mem bid then
    dispmap
  else
    let i = dispmap.current_max in
    let s = make_value "#" i in
    { dispmap with
      current_max = i + 1;
      bound_ids   = bids |> BoundIDMap.add bid s;
    }


let add_bound_row_id brid labset dispmap =
  let brids = dispmap.bound_row_ids in
  if brids |> BoundRowIDMap.mem brid then
    dispmap
  else
    let i = dispmap.current_max in
    let s = make_value "?#" i in
    { dispmap with
      current_max   = i + 1;
      bound_row_ids = brids |> BoundRowIDMap.add brid (s, labset);
    }


let find_free_id fid dispmap =
  match dispmap.free_ids |> FreeIDMap.find_opt fid with
  | Some(s) -> s
  | None    -> Format.asprintf "!!%a!!" FreeID.pp fid


let find_free_row_id frid dispmap =
  match dispmap.free_row_ids |> FreeRowIDMap.find_opt frid with
  | Some((s, _)) -> s
  | None         -> Format.asprintf "!!%a!!" FreeRowID.pp frid


let find_bound_id bid dispmap =
  match dispmap.bound_ids |> BoundIDMap.find_opt bid with
  | Some(s) -> s
  | None    -> Format.asprintf "!!%a!!" BoundID.pp bid


let find_bound_row_id brid dispmap =
  match dispmap.bound_row_ids |> BoundRowIDMap.find_opt brid with
  | Some((s, _)) -> s
  | None         -> Format.asprintf "!!%a!!" BoundRowID.pp brid


let make_free_id_hash_set dispmap =
  let fidht = FreeIDHashTable.create 32 in
  dispmap.free_ids |> FreeIDMap.iter (fun fid _ ->
    FreeIDHashTable.add fidht fid ()
  );
  fidht


let make_free_row_id_hash_set dispmap =
  let fridht = FreeRowIDHashTable.create 32 in
  dispmap.free_row_ids |> FreeRowIDMap.iter (fun frid (_, labset) ->
    FreeRowIDHashTable.add fridht frid labset
  );
  fridht


let make_bound_id_hash_set dispmap =
  let bidht = BoundIDHashTable.create 32 in
  dispmap.bound_ids |> BoundIDMap.iter (fun bid _ ->
    BoundIDHashTable.add bidht bid ()
  );
  bidht


let make_bound_row_id_hash_set dispmap =
  let bridht = BoundRowIDHashTable.create 32 in
  dispmap.bound_row_ids |> BoundRowIDMap.iter (fun brid (_, labset) ->
    BoundRowIDHashTable.add bridht brid labset
  );
  bridht


let fold_free_id f acc dispmap =
  FreeIDMap.fold f dispmap.free_ids acc


let fold_free_row_id f acc dispmap =
  FreeRowIDMap.fold f dispmap.free_row_ids acc


let fold_bound_id f acc dispmap =
  BoundIDMap.fold f dispmap.bound_ids acc


let fold_bound_row_id f acc dispmap =
  BoundRowIDMap.fold f dispmap.bound_row_ids acc
