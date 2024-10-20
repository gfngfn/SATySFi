
open MyUtil
open ConfigError
open ConfigUtil


module CrossRefHashTable = Hashtbl.Make
  (struct
    include String
    let hash = Hashtbl.hash
  end)

module CrossRefKeySet = Set.Make(String)


let unresolved_crossrefs = ref CrossRefKeySet.empty
let changed = ref false
let main_hash_table = CrossRefHashTable.create 32


let entry_decoder : (string * string) ConfigDecoder.t =
  let open ConfigDecoder in
  get "key" string >>= fun key ->
  get "value" string >>= fun data ->
  succeed (key, data)


let entry_encoder ((key, data) : string * string) : Yaml.value =
  `O([
    ("key", `String(key));
    ("value", `String(data));
  ])


let dump_file_decoder : ((string * string) list) ConfigDecoder.t =
  let open ConfigDecoder in
  get "cross_references" (list entry_decoder)


let dump_file_encoder (keyvals : (string * string) list) : Yaml.value =
  `O([ ("cross_references", `A(keyvals |> List.map entry_encoder)) ])


let write_dump_file (abspath_dump : abs_path) : (unit, config_error) result =
  let keyvals =
    CrossRefHashTable.fold (fun key data acc ->
      Alist.extend acc (key, data)
    ) main_hash_table Alist.empty |> Alist.to_list
  in
  let yaml = dump_file_encoder keyvals in
  let data = encode_yaml yaml in
  AbsPathIo.write_file abspath_dump data
    |> Result.map_error (fun _ -> CannotWriteDumpFile(abspath_dump))


let initialize (abspath_dump : abs_path) : (bool, config_error) result =
  let open ResultMonad in
  CrossRefHashTable.clear main_hash_table;
  let res = AbsPathIo.read_file abspath_dump in
  match res with
  | Error(_) ->
      return false

  | Ok(s) ->
      let* keyvals =
        ConfigDecoder.run dump_file_decoder s
          |> Result.map_error (fun e -> DumpFileError(abspath_dump, e))
      in
      keyvals |> List.iter (fun (key, data) ->
        CrossRefHashTable.replace main_hash_table key data
      );
      return true


let reset () =
  unresolved_crossrefs := CrossRefKeySet.empty;
  changed := false


type answer =
  | NeedsAnotherTrial
  | CanTerminate of string list


let judge_termination () : answer =
  if !changed then
    NeedsAnotherTrial
  else
    CanTerminate (CrossRefKeySet.elements !unresolved_crossrefs)


let register (key : string) (value : string) =
  match CrossRefHashTable.find_opt main_hash_table key with
  | None ->
      changed := true;
      CrossRefHashTable.replace main_hash_table key value

  | Some(value_old) ->
      if String.equal value value_old then
        ()
      else
        begin
          changed := true;
          CrossRefHashTable.replace main_hash_table key value
        end


let probe (key : string) =
  CrossRefHashTable.find_opt main_hash_table key


let get (key : string) =
  let value_opt = CrossRefHashTable.find_opt main_hash_table key in
  begin
    match value_opt with
    | Some(_) -> ()
    | None    -> unresolved_crossrefs := CrossRefKeySet.add key !unresolved_crossrefs
  end;
  value_opt
