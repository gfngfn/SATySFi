open Types


type t = (Tyvarid.t * kind_struct) list


let empty : t = []


let rec add (kdenv : t) (tvid : Tyvarid.t) (kdstr : kind_struct) =
  match kdenv with
  | []                                               -> (tvid, kdstr) :: []
  | (alpha, _) :: tail  when Tyvarid.same alpha tvid -> (tvid, kdstr) :: tail
  | (alpha, kd) :: tail                              -> (alpha, kd) :: (add tail tvid kdstr)


let rec find (kdenv : t) (tvid : Tyvarid.t) =
  match kdenv with
  | []                                                   -> raise Not_found
  | (alpha, kdstr) :: tail  when Tyvarid.same alpha tvid -> kdstr
  | _ :: tail                                            -> find tail tvid


let replace_type_variable_in_kind_struct (kdstr : kind_struct) (tvid : Tyvarid.t) (tystr : type_struct) =
  match kdstr with
  | UniversalKind   -> UniversalKind
  | RecordKind(asc) -> RecordKind(Assoc.map_value (fun ty -> replace_type_variable ty tvid tystr) asc)


let rec replace_type_variable_in_kindenv (kdenv : t) (tvid : Tyvarid.t) (tystr : type_struct) =
  let iter = (fun lst -> replace_type_variable_in_kindenv lst tvid tystr) in
    match kdenv with
    | []                       -> []
    | (alpha, kdstr) :: tail   -> (alpha, replace_type_variable_in_kind_struct kdstr tvid tystr) :: (iter tail)


(* for test *)
let to_string (kdenv : t) =
  List.fold_left (fun str (tvid, kdstr) -> str ^ (Tyvarid.show_direct tvid) ^ " :: " ^ "_" (* (Display.string_of_kind_struct_basic kdstr) *) ^ ", ") "" kdenv

