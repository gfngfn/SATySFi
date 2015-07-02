open Types

type t = (type_variable_id * type_struct) list

let empty = []

(* t -> type_variable_id -> type_struct -> t *)
let add theta key value = (key, value) :: theta
(*
  match asclst with
  | [] -> [(key, value)]
  | (k, v) :: tail ->
      if k = key then (key, value) :: tail else (k, v) :: (add tail key value)
*)

let rec overwrite_type_struct tystr key value =
  match tystr with
  | TypeVariable(k)    -> if k = key then value else TypeVariable(k)
  | FuncType(dom, cod) -> FuncType(overwrite_type_struct dom key value, overwrite_type_struct cod key value)
  | ListType(cont)     -> ListType(overwrite_type_struct cont key value)
  | other              -> other

let rec overwrite theta key value =
  match theta with
  | []             -> theta
  | (k, v) :: tail ->
      ( if k = key then (key, value) else (k, (overwrite_type_struct v key value)) ) :: (overwrite tail key value)

let rec compose theta2 theta1 =
  match theta2 with
  | []              -> theta1
  | (k, v) :: tail2 -> compose tail2 (overwrite theta1 k v)

let rec apply_to_type_variable theta tyvar =
  match theta with
  | []             -> tyvar
  | (k, v) :: tail -> if k = tyvar then v else apply_to_type_variable tail tyvar

