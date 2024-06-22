
open MyUtil

module YS = Yojson.Safe
module MYU = MyYojsonUtil


module CrossRefHashTable = Hashtbl.Make
  (struct
    type t = string
    let equal = String.equal
    let hash = Hashtbl.hash
  end)


let unresolved_crossrefs = ref []
let changed = ref false

let count = ref 0

let count_max = ref 4
  (* temporary *)

let main_hash_table = CrossRefHashTable.create 32
  (* temporary; initial size *)


let read_assoc (assoc : (string * YS.json) list) : unit =
  assoc |> List.iter (fun (key, vjson) ->
    let value = vjson |> YS.Util.to_string in
    CrossRefHashTable.add main_hash_table key value
  )


let read_dump_file (abspath : abs_path) : unit =
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
    count := 1;
    CrossRefHashTable.clear main_hash_table;
    let dump_file_exists = Sys.file_exists (get_abs_path_string abspath_dump) in
    begin
      if dump_file_exists then
        read_dump_file abspath_dump
      else
        ()
    end;
    dump_file_exists
  end


type answer =
  | NeedsAnotherTrial
  | CanTerminate of string list
  | CountMax


let needs_another_trial (abspath : abs_path) : answer =
  if !changed then
    if !count >= !count_max then
      begin
        write_dump_file abspath;
        CountMax
      end
    else
      begin
        unresolved_crossrefs := [];
        changed := false;
        incr count;
        NeedsAnotherTrial
      end
  else
    begin
      write_dump_file abspath;
      CanTerminate (List.sort_uniq String.compare !unresolved_crossrefs)
    end


let register (key : string) (value : string) =
  match CrossRefHashTable.find_opt main_hash_table key with
  | None ->
      changed := true;
      CrossRefHashTable.add main_hash_table key value

  | Some(value_old) ->
      if String.equal value value_old then () else begin
        changed := true;
        CrossRefHashTable.add main_hash_table key value
      end


let probe (key : string) =
  CrossRefHashTable.find_opt main_hash_table key


let get (key : string) =
  let valueopt = CrossRefHashTable.find_opt main_hash_table key in
  begin
    match valueopt with
    | Some(_) -> ()
    | None    -> unresolved_crossrefs := key :: !unresolved_crossrefs
  end;
  valueopt
