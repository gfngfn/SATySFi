open Types

type type_variable_id = int
type type_environment = (var_name * type_struct) list
and type_struct =
  | TypeEnvironmentType of type_environment
  | UnitType
  | IntType
  | StringType
  | BoolType
  | FuncType of type_struct * type_struct
  | ListType of type_struct
  | ForallType of type_variable_id * type_struct
  | TypeVariable of type_variable_id


let rec found_in_list tvid lst =
	match lst with
	| []       -> false
	| hd :: tl -> if hd = tvid then true else found_in_list tvid tl

let rec found_in_type_struct tvid tystr =
	match tystr with
	| TypeVariable(tvidx)    -> tvidx = tvid
	| FuncType(tydom, tycod) -> (found_in_type_struct tvid tydom) || (found_in_type_struct tvid tycod)
	| ListType(tycont)       -> found_in_type_struct tvid tycont
	| _                      -> false

let rec found_in_type_environment tvid tyenv =
	match tyenv with
	| []                     -> false
	| (tyvar, tystr) :: tail ->
	    if found_in_type_struct tvid tystr then
	      true
	    else
	      found_in_type_environment tvid tail

(* type_struct -> type_environment -> (type_variable_id list) -> (type_variable_id list) *)
let rec listup_unbound_id tystr tyenv lst =
	match tystr with
	| TypeVariable(tvid)     ->
	    if found_in_type_environment tvid tyenv then lst
	    else if found_in_list tvid lst then lst
	    else tvid :: lst
	| FuncType(tydom, tycod) -> (listup_unbound_id tydom tyenv lst) @ (listup_unbound_id tycod tyenv lst)
	| ListType(tycont)       -> listup_unbound_id tycont tyenv lst
	| _                      -> lst

(* type_variable_id list -> type_struct -> type_struct *)
let rec add_forall_struct lst tystr =
  match lst with
  | []           -> tystr
  | tvid :: tail -> ForallType(tvid, add_forall_struct tail tystr)

(* type_struct -> type_environment -> type_struct *)
let make_forall_type tystr tyenv = add_forall_struct (listup_unbound_id tystr tyenv []) tystr

let empty = []

let add tyenv varnm tystr = (varnm, tystr) :: tyenv

let rec find tyenv varnm =
  match tyenv with
  | []               -> raise Not_found
  | (vn, ts) :: tail -> if vn = varnm then ts else find tail varnm
