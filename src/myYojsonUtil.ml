
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


let err_variant json branches =
  let s =
    branches |> List.map (function
    | (label, None)    -> "<" ^ label ^ ">"
    | (label, Some(_)) -> "<" ^ label ^ ": _ >"
    ) |> String.concat ", "
  in
  raise (YS.Util.Type_error("Expects " ^ s, json))


let decode_variant (type a) (branches : (string * (json -> a) option) list) (json : json) : a option =
  match json with
  | (_, `Variant(label, jsonopt)) ->
      begin
        match List.assoc_opt label branches with
        | None ->
            err_variant json branches

        | Some(fopt) ->
            begin
              match (fopt, jsonopt) with
              | (Some(f), Some(arg)) -> Some(f arg)
              | (None, None)         -> None
              | _                    -> err_variant json branches
            end
      end

  | _ ->
      err_variant json branches
