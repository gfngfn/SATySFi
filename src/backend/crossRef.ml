
module CrossRefHashTable = Hashtbl.Make
  (struct
    type t = string
    let equal = String.equal
    let hash = Hashtbl.hash
  end)


let changed = ref false

let count = ref 0

let count_max = ref 4
  (* temporary *)

let main_hash_table = CrossRefHashTable.create 32
  (* temporary; initial size *)


let initialize () =
  begin
    count := 1;
    CrossRefHashTable.clear main_hash_table;
  end


type answer =
  | NeedsAnotherTrial
  | CanTerminate
  | CountMax


let needs_another_trial () =
  if !changed then
    if !count >= !count_max then
      CountMax
    else
    begin
      changed := false;
      incr count;
      NeedsAnotherTrial
    end
  else
    CanTerminate


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
  CrossRefHashTable.find_opt main_hash_table key
