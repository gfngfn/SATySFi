
type json = Yojson.SafePos.json
module YS = Yojson.SafePos


exception MultipleDesignation of Range.t * string
exception MissingRequiredKey  of Range.t * string
exception SyntaxError         of string * string

module YojsonMap = Map.Make(String)

type assoc = Yojson.position * json YojsonMap.t


let make_range (pos : Yojson.position) =
  let open Yojson in
  let fname = BatOption.default "(none)" pos.file_name in
  Range.make_large fname pos.start_line pos.start_column pos.end_line pos.end_column


let make_assoc ((pos, _) as json : json) : assoc =
  let keyvals = json |> YS.Util.to_assoc in
  let assoc =
    keyvals |> List.fold_left (fun accmap (k, v) ->
      if accmap |> YojsonMap.mem k then
        let rng = make_range pos in
        raise (MultipleDesignation(rng, k))
      else
        accmap |> YojsonMap.add k v
    ) YojsonMap.empty
  in
  (pos, assoc)


let find (key : string) ((pos, assoc) : assoc) : json =
  match assoc |> YojsonMap.find_opt key with
  | None ->
      let rng = make_range pos in
      raise (MissingRequiredKey(rng, key))

  | Some(json) ->
      json
