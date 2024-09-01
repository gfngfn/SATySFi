
open MyUtil

module YS = Yojson.Safe
module MYU = MyYojsonUtil


module CrossRefHashTable = Hashtbl.Make
  (struct
    include String
    let hash = Hashtbl.hash
  end)

module CrossRefKeySet = Set.Make(String)


let unresolved_crossrefs = ref CrossRefKeySet.empty
let changed = ref false
let main_hash_table = CrossRefHashTable.create 32


let read_assoc (assoc : (string * YS.json) list) : unit =
  assoc |> List.iter (fun (key, vjson) ->
    let value = vjson |> YS.Util.to_string in
    CrossRefHashTable.add main_hash_table key value
  )


let load_dump_file (abspath : abs_path) : unit =
  try
    let json = YS.from_file (get_abs_path_string abspath) in
      (* -- may raise 'Sys_error', or 'Yojson.Json_error' -- *)
    let assoc = json |> YS.Util.to_assoc in
    read_assoc assoc
  with
  | Yojson.Json_error(msg) -> MYU.syntax_error (get_abs_path_string abspath) msg


let write_dump_file (abspath : abs_path) : unit =
  let assoc =
    Alist.empty @|> main_hash_table @|> CrossRefHashTable.fold (fun key value acc ->
      Alist.extend acc (key, `String(value))
    ) |> Alist.to_list
  in
  let json = `Assoc(assoc) in
  YS.to_file (get_abs_path_string abspath) json


let initialize (abspath_dump : abs_path) : bool =
  begin
    CrossRefHashTable.clear main_hash_table;
    let dump_file_exists = Sys.file_exists (get_abs_path_string abspath_dump) in
    begin
      if dump_file_exists then
        load_dump_file abspath_dump
      else
        ()
    end;
    dump_file_exists
  end


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
      CrossRefHashTable.add main_hash_table key value

  | Some(value_old) ->
      if String.equal value value_old then
        ()
      else
        begin
          changed := true;
          CrossRefHashTable.add main_hash_table key value
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
