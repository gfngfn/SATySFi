
exception MultipleDesignation of string * string

module YojsonMap = Map.Make(String)

type assoc = Yojson.Safe.json YojsonMap.t


let make_assoc (exnmult : string -> exn) (assoc : (string * Yojson.Safe.json) list) : assoc =
  assoc |> List.fold_left (fun accmap (k, v) ->
    if accmap |> YojsonMap.mem k then
      raise (exnmult k)
    else
      accmap |> YojsonMap.add k v
  ) YojsonMap.empty


let find (exn : exn) (key : string) (asc : assoc) : Yojson.Safe.json =
  match asc |> YojsonMap.find_opt key with
  | None       -> raise exn
  | Some(json) -> json
