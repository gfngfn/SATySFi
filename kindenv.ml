open Types


type t = (Tyvarid.t * kind_struct) list


let empty : t = []


let rec add (kenv : t) (tvid : Tyvarid.t) (kdstr : kind_struct) =
  match kenv with
  | []                                    -> (tvid, kdstr) :: []
  | (alpha, _) :: tail  when alpha = tvid -> (tvid, kdstr) :: tail
  | (alpha, kd) :: tail                   -> (alpha, kd) :: (add kenv tvid kdstr)


let replace_type_variable_in_kind_struct (kdstr : kind_struct) (tvid : Tyvarid.t) (tystr : type_struct) =
  match kdstr with
  | UniversalKind   -> UniversalKind
  | RecordKind(acc) -> RecordKind(Assoc.map_value replace_type_variable acc)


let rec replace_type_variable_in_kindenv (kenv : t) (tvid : Tyvarid.t) (tystr : type_struct) =
  let iter = (fun lst -> replace_type_variable_in_kindenv lst tvid tystr) in
    match kenv with
    | []                       -> []
    | (alpha, kdstr) :: tail   -> (alpha, replace_type_variable_in_kind_struct kdstr tvid tystr) :: (iter tail)


