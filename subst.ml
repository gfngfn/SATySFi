open Types
open Typeenv

type t = (type_variable_id * type_struct) list

let empty = []

(* t -> type_variable_id -> type_struct -> t *)
let add theta key value = (key, value) :: theta

let rec find theta key =
  match theta with
  | []             -> raise Not_found
  | (k, v) :: tail -> if k = key then v else find tail key

let rec overwrite_type_struct tystr key value =
  match tystr with
  | TypeVariable(k)    -> if k = key then value else TypeVariable(k)
  | FuncType(dom, cod) -> FuncType(overwrite_type_struct dom key value, overwrite_type_struct cod key value)
  | ListType(cont)     -> ListType(overwrite_type_struct cont key value)
  | other              -> other

let rec overwrite theta key value =
  match theta with
  | []             -> add theta key value
  | (k, v) :: tail ->
      if k = key then (key, value) :: tail else (k, (overwrite_type_struct v key value)) :: (overwrite tail key value)

let rec compose theta2 theta1 =
  match theta2 with
  | []              -> theta1
  | (k, v) :: tail2 -> compose tail2 (overwrite theta1 k v)

(* Subst.t -> type_struct -> type_struct *)
let rec apply_to_type_struct theta tystr =
  match tystr with
  | FuncType(tydom, tycod) -> FuncType(apply_to_type_struct theta tydom, apply_to_type_struct theta tycod)
  | ListType(tycont) -> ListType(apply_to_type_struct theta tycont)
  | TypeVariable(tv) -> ( try find theta tv with Not_found -> TypeVariable(tv) )
  | other            -> other

(* Subst.t -> type_environment -> type_environment *)
let rec apply_to_type_environment theta tyenv =
  match tyenv with
  | []                     -> tyenv
  | (varnm, tystr) :: tail ->
      (varnm, apply_to_type_struct theta tystr) :: (apply_to_type_environment theta tail)

let rec apply_to_term theta ast = ast (* need writing *)

let rec emerge_in tvid tystr =
  match tystr with
  | TypeVariable(tvidx) -> tvidx == tvid
  | FuncType(dom, cod)  -> (emerge_in tvid dom) || (emerge_in tvid cod)
  | ListType(cont)      -> emerge_in tvid cont
  | _                   -> false

let rec unify tystr1 tystr2 =
  match (tystr1, tystr2) with
  | (IntType, IntType)       -> empty
  | (StringType, StringType) -> empty
  | (BoolType, BoolType)     -> empty
  | (UnitType, UnitType)     -> empty
  | (TypeEnvironmentType(_), TypeEnvironmentType(_)) -> empty
  | (FuncType(dom1, cod1), FuncType(dom2, cod2)) ->
      compose (unify dom1 dom2) (unify cod1 cod2)
  | (ListType(cont1), ListType(cont2)) -> unify cont1 cont2
  | (TypeVariable(tvid), tystr) ->
      if emerge_in tvid tystr then
        raise (TypeCheckError("error in unify 1"))
      else
        [(tvid, tystr)]
  | (tystr, TypeVariable(tvid)) -> unify (TypeVariable(tvid)) tystr
  | _ -> raise (TypeCheckError("error in unify 2"))