open Types

exception TypeCheckError of string

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


(* for test *)
let rec string_of_type_struct tystr =
  match tystr with
  | StringType -> "string"
  | IntType    -> "int"
  | BoolType   -> "bool"
  | UnitType   -> "unit"
  | TypeEnvironmentType(_) -> "env"
  | FuncType(tydom, tycod) -> "(" ^ (string_of_type_struct tydom) ^ " -> " ^ (string_of_type_struct tycod) ^ ")"
  | ListType(tycont) -> "(" ^ (string_of_type_struct tycont) ^ " list)"
  | TypeVariable(tvid) -> "'" ^ (string_of_int tvid)
  | ForallType(tvid, tycont) -> "(forall '" ^ (string_of_int tvid) ^ ". " ^ (string_of_type_struct tycont) ^ ")"
let rec string_of_type_environment tyenv =
  match tyenv with
  | []               -> ""
  | (vn, ts) :: tail -> "  " ^ vn ^ ": " ^ (string_of_type_struct ts) ^ "\n" ^ (string_of_type_environment tail)


let rec found_in_list tvid lst =
  match lst with
  | []       -> false
  | hd :: tl -> if hd == tvid then true else found_in_list tvid tl

let rec found_in_type_struct tvid tystr =
  match tystr with
  | TypeVariable(tvidx)    -> tvidx == tvid
  | FuncType(tydom, tycod) -> (found_in_type_struct tvid tydom) || (found_in_type_struct tvid tycod)
  | ListType(tycont)       -> found_in_type_struct tvid tycont
  | _                      -> false

let rec found_in_type_environment tvid tyenv =
  match tyenv with
  | []                 -> ( (* print_string "%found_in_type_environment: false\n" ; *) false )
  | (_, tystr) :: tail ->
      if found_in_type_struct tvid tystr then
      ( (* print_string ("%found_in_type_environment: " ^ (string_of_type_struct tystr) ^ "\n") ; *)
        true )
      else
        found_in_type_environment tvid tail


let unbound_id_list : type_variable_id list ref = ref []

(* type_struct -> type_environment -> (type_variable_id list) -> unit *)
let rec listup_unbound_id tystr tyenv =
  match tystr with
  | TypeVariable(tvid)     ->
    ( (* print_string ("%listup_unbound_id: '" ^ (string_of_int tvid) ^ "\n") ; *)
      if found_in_type_environment tvid tyenv then ()
      else if found_in_list tvid !unbound_id_list then ()
      else unbound_id_list := tvid :: !unbound_id_list
    )
  | FuncType(tydom, tycod) -> ( listup_unbound_id tydom tyenv ; listup_unbound_id tycod tyenv )
  | ListType(tycont)       -> listup_unbound_id tycont tyenv
  | _                      -> ()

(* type_variable_id list -> type_struct -> type_struct *)
let rec add_forall_struct lst tystr =
  (* print_string "%add_forall_struct\n" ; *)
  match lst with
  | []           -> tystr
  | tvid :: tail -> ForallType(tvid, add_forall_struct tail tystr)

(* type_struct -> type_environment -> type_struct *)
let make_forall_type tystr tyenv =
(*	print_string ("%make_forall_type\n" ^ (string_of_type_environment tyenv) ^ "\n") ; *)
	unbound_id_list := [] ; listup_unbound_id tystr tyenv ;
	add_forall_struct (!unbound_id_list) tystr


let empty = []

let rec add tyenv varnm tystr =
  match tyenv with
  | []               -> [(varnm, tystr)]
  | (vn, ts) :: tail -> if vn == varnm then (varnm, tystr) :: tail else (vn, ts) :: (add tail varnm tystr)

let rec find tyenv varnm =
  match tyenv with
  | []               -> raise Not_found
  | (vn, ts) :: tail -> if vn = varnm then ts else find tail varnm
