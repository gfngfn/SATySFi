
type file_path = string

exception InvalidYOJSON                of file_path * string
exception DumpFileOtherThanAssoc       of file_path
exception DumpFileValueOtherThanString of file_path * string * string


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


let read_assoc (srcpath : file_path) (assoc : (string * Yojson.Safe.json) list) : unit =
  assoc |> List.iter (fun (key, vjson) ->
    match vjson with
    | `String(value) -> CrossRefHashTable.add main_hash_table key value
    | json_other     -> raise (DumpFileValueOtherThanString(srcpath, key, Yojson.Safe.to_string json_other))
  )


let read_dump_file (srcpath : file_path) : unit =
  try
    let json = Yojson.Safe.from_file srcpath in  (* -- may raise 'Sys_error', or 'Yojson.Json_error' -- *)
      match json with
      | `Assoc(assoc) -> read_assoc srcpath assoc
      | json_other    -> raise (DumpFileOtherThanAssoc(srcpath))
  with
  | Yojson.Json_error(msg) -> raise (InvalidYOJSON(srcpath, msg))


let write_dump_file (outpath : file_path) : unit =
  let open MyUtil in
  let assoc =
    Alist.empty @|> main_hash_table @|> CrossRefHashTable.fold (fun key value acc ->
      Alist.extend acc (key, `String(value))
    ) |> Alist.to_list
  in
  let json = `Assoc(assoc) in
  Yojson.Safe.to_file outpath json


let initialize (srcpath : file_path) : bool =
  begin
    count := 1;
    CrossRefHashTable.clear main_hash_table;
    let dump_file_exists = Sys.file_exists srcpath in
    begin
      if dump_file_exists then
        read_dump_file srcpath
      else
        ()
    end;
    dump_file_exists
  end


type answer =
  | NeedsAnotherTrial
  | CanTerminate of string list
  | CountMax


let needs_another_trial (outpath : file_path) : answer =
  if !changed then
    if !count >= !count_max then
      begin
        write_dump_file outpath;
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
      write_dump_file outpath;
      CanTerminate (List.sort_uniq String.compare !unresolved_crossrefs)
    end


let register (key : string) (value : string) =
  match CrossRefHashTable.find_opt main_hash_table key with
  | None ->
      begin
        changed := true;
        CrossRefHashTable.add main_hash_table key value
      end


  | Some(value_old) ->
      if String.equal value value_old then () else
        begin
          changed := true;
          CrossRefHashTable.add main_hash_table key value;
        end


let get (key : string) =
  match CrossRefHashTable.find_opt main_hash_table key with
  | Some(value) -> Some(value)
  | None ->
      unresolved_crossrefs := key :: !unresolved_crossrefs;
      None
