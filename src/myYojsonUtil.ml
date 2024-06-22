
type json = Yojson.SafePos.json
module YS = Yojson.SafePos


exception MultipleDesignation of Range.t * string
exception MissingRequiredKey  of Range.t * string
exception SyntaxError         of string * string

module YojsonMap = Map.Make(String)

type assoc = Yojson.position * json YojsonMap.t


let syntax_error srcpath msg =
  raise (SyntaxError(srcpath, msg))


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


let find_opt (key : string) ((_pos, assoc) : assoc) : json option =
  assoc |> YojsonMap.find_opt key


let find (key : string) ((pos, assoc) : assoc) : json =
  match assoc |> YojsonMap.find_opt key with
  | None ->
      let rng = make_range pos in
      raise (MissingRequiredKey(rng, key))

  | Some(json) ->
      json


let fold (type a) (f : string -> json -> a -> a) (init : a) ((_, assoc) : assoc) : a =
  YojsonMap.fold f assoc init


type 'a variant_arg =
  | NoArg of (unit -> 'a)
  | Arg   of (json -> 'a)


let err_variant json branches =
  let s =
    branches |> List.map (function
    | (label, NoArg(_)) -> "<" ^ label ^ ">"
    | (label, Arg(_))   -> "<" ^ label ^ ": _ >"
    ) |> String.concat ", "
  in
  raise (YS.Util.Type_error("Expects one of the following form(s): " ^ s, json))


let decode_variant (type a) (branches : (string * a variant_arg) list) (json : json) : a =
  match json with
  | (_, `Variant(label, jsonopt)) ->
      begin
        match List.assoc_opt label branches with
        | None ->
            err_variant json branches

        | Some(fopt) ->
            begin
              match (fopt, jsonopt) with
              | (Arg(f), Some(arg)) -> f arg
              | (NoArg(hook), None) -> hook ()
              | _                   -> err_variant json branches
            end
      end

  | _ ->
      err_variant json branches
