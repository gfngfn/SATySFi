open Types

exception TypeCheckError of string

type type_variable_id = int
type type_environment = (var_name * type_struct) list
and type_struct =
  | TypeEnvironmentType of code_range * type_environment
  | UnitType     of code_range
  | IntType      of code_range
  | StringType   of code_range
  | BoolType     of code_range
  | FuncType     of code_range * type_struct * type_struct
  | ListType     of code_range * type_struct
  | RefType      of code_range * type_struct
  | ForallType   of type_variable_id * type_struct
  | TypeVariable of code_range * type_variable_id

(* !!!! ---- global variable ---- !!!! *)
let global_hash_env : environment = Hashtbl.create 32


let rec string_of_ast ast =
  match ast with
  | LambdaAbstract(x, m)         -> "(" ^ x ^ " -> " ^ (string_of_ast m) ^ ")"
  | FuncWithEnvironment(x, m, _) -> "(" ^ x ^ " *-> " ^ (string_of_ast m) ^ ")"
  | ContentOf(v)           -> v
  | Apply(m, n)            -> "(" ^ (string_of_ast m) ^ " " ^ (string_of_ast n) ^ ")"
  | Concat(s, t)           -> (string_of_ast s) ^ (string_of_ast t)
  | StringEmpty            -> ""
  | StringConstant(sc)     -> "{" ^ sc ^ "}"
  | NumericConstant(nc)    -> string_of_int nc
  | BooleanConstant(bc)    -> string_of_bool bc
  | IfThenElse(b, t, f)    ->
      "(if " ^ (string_of_ast b) ^ " then " ^ (string_of_ast t) ^ " else " ^ (string_of_ast f) ^ ")"
  | IfClassIsValid(t, f)   -> "(if-class-is-valid " ^ (string_of_ast t) ^ " else " ^ (string_of_ast f) ^ ")"
  | Reference(a)           -> "!" ^ (string_of_ast a)
  | ReferenceFinal(a)     -> "!!" ^ (string_of_ast a)
  | Overwrite(vn, n)       -> "(" ^ vn ^ " <- " ^ (string_of_ast n) ^ ")"
  | MutableValue(mv)       -> "(mutable " ^ (string_of_ast mv) ^ ")"
  | UnitConstant           -> "()"
  | LetMutableIn(vn, d, f) -> "(let-mutable " ^ vn ^ " <- " ^ (string_of_ast d) ^ " in " ^ (string_of_ast f) ^ ")"
  | _ -> "..."


let print_for_debug msg = (* print_string msg ; *) ()

(* untyped_abstract_tree -> code_range *)
let get_range utast =
  let (rng, _) = utast in rng

let get_range_from_type tystr =
  match tystr with
  | IntType(rng)         -> rng
  | StringType(rng)      -> rng
  | BoolType(rng)        -> rng
  | UnitType(rng)        -> rng
  | TypeVariable(rng, _) -> rng
  | FuncType(rng, _, _)  -> rng
  | ListType(rng, _)     -> rng
  | RefType(rng, _)      -> rng
  | TypeEnvironmentType(rng, _) -> rng
  | ForallType(_, _)     -> (-64, 0, 0, 0)

let describe_position (sttln, sttpos, endln, endpos) =
  if sttln == endln then
    "line " ^ (string_of_int sttln) ^ ", characters " ^ (string_of_int sttpos)
      ^ "-" ^ (string_of_int endpos)
  else
    "line " ^ (string_of_int sttln) ^ ", character " ^ (string_of_int sttpos)
      ^ " to line " ^ (string_of_int endln) ^ ", character " ^ (string_of_int endpos)

let error_reporting rng errmsg =
  let (sttln, sttpos, endln, endpos) = rng in
    if sttln == endln then
      "at line " ^ (string_of_int sttln) ^ ", characters "
        ^ (string_of_int sttpos) ^ "-" ^ (string_of_int endpos) ^ ":\n"
        ^ "    " ^ errmsg
    else
      "at line " ^ (string_of_int sttln) ^ ", character " ^ (string_of_int sttpos)
        ^ " to line " ^ (string_of_int endln) ^ ", character " ^ (string_of_int endpos) ^ ":\n"
        ^ "    " ^ errmsg

let rec variable_name_of_int n =
  ( if n >= 26 then
      variable_name_of_int ((n - n mod 26) / 26 - 1)
    else
      ""
  ) ^ (String.make 1 (Char.chr ((Char.code 'a') + n mod 26)))

let meta_max    : type_variable_id ref = ref 0
let unbound_max : type_variable_id ref = ref 0
let unbound_type_valiable_name_list : (type_variable_id * string ) list ref = ref []

let new_meta_type_variable_name () =
  let res = variable_name_of_int (!meta_max) in
    meta_max := !meta_max + 1 ; res

let rec find_meta_type_variable lst tvid =
  match lst with
  | []             -> raise Not_found
  | (k, v) :: tail -> if k == tvid then v else find_meta_type_variable tail tvid

let new_unbound_type_variable_name tvid =
  let res = variable_name_of_int (!unbound_max) in
    unbound_max := !unbound_max + 1 ;
    unbound_type_valiable_name_list := (tvid, res) :: (!unbound_type_valiable_name_list) ;
    res

let find_unbound_type_variable tvid =
  find_meta_type_variable (!unbound_type_valiable_name_list) tvid

let rec string_of_type_struct tystr =
  meta_max := 0 ;
  unbound_max := 0 ;
  unbound_type_valiable_name_list := [] ;
  string_of_type_struct_sub tystr []

(* type_struct -> (type_variable_id * string) list -> string *)
and string_of_type_struct_sub tystr lst =
  match tystr with
  | StringType(_) -> "string"
  | IntType(_)    -> "int"
  | BoolType(_)   -> "bool"
  | UnitType(_)   -> "unit"
  | TypeEnvironmentType(_, _) -> "env"

  | TypeVariable(_, tvid)     ->
      ( try "'" ^ (find_meta_type_variable lst tvid) with
        | Not_found ->
            "'" ^
              ( try find_unbound_type_variable tvid with
                | Not_found -> new_unbound_type_variable_name tvid
              )
      )

  | FuncType(_, tydom, tycod) ->
      let strdom = string_of_type_struct_sub tydom lst in
      let strcod = string_of_type_struct_sub tycod lst in
      ( match tydom with
        | FuncType(_, _, _) -> "(" ^ strdom ^ ")"
        | _                 -> strdom
      ) ^ " -> " ^ strcod

  | ListType(_, tycont)       ->
      let strcont = string_of_type_struct_sub tycont lst in
      ( match tycont with
        | FuncType(_, _, _) -> "(" ^ strcont ^ ")"
        | _                 -> strcont
      ) ^ " list"

  | RefType(_, tycont)        ->
      let strcont = string_of_type_struct_sub tycont lst in
      ( match tycont with
        | FuncType(_, _, _) -> "(" ^ strcont ^ ")"
        | _                 -> strcont
      ) ^ " ref"

  | ForallType(tvid, tycont)  ->
      let meta = new_meta_type_variable_name () in
        (string_of_type_struct_sub tycont ((tvid, meta) :: lst))


let rec string_of_type_environment tyenv =
    " #===============================\n"
  ^ (string_of_type_environment_sub tyenv)
  ^ " #===============================\n"
and string_of_type_environment_sub tyenv =
  match tyenv with
  | []               -> ""
  | (vn, ts) :: tail -> " #  " ^ vn ^ " : " ^ (string_of_type_struct ts) ^ "\n" ^ (string_of_type_environment_sub tail)


let rec found_in_list tvid lst =
  match lst with
  | []       -> false
  | hd :: tl -> if hd == tvid then true else found_in_list tvid tl

let rec found_in_type_struct tvid tystr =
  match tystr with
  | TypeVariable(_, tvidx)    -> tvidx == tvid
  | FuncType(_, tydom, tycod) -> (found_in_type_struct tvid tydom) || (found_in_type_struct tvid tycod)
  | ListType(_, tycont)       -> found_in_type_struct tvid tycont
  | RefType(_, tycont)        -> found_in_type_struct tvid tycont
  | _                         -> false

let rec found_in_type_environment tvid tyenv =
  match tyenv with
  | []                 -> false
  | (_, tystr) :: tail ->
      if found_in_type_struct tvid tystr then
        true
      else
        found_in_type_environment tvid tail


let unbound_id_list : type_variable_id list ref = ref []

(* type_struct -> type_environment -> (type_variable_id list) -> unit *)
let rec listup_unbound_id tystr tyenv =
  match tystr with
  | TypeVariable(_, tvid)     ->
      if found_in_type_environment tvid tyenv then ()
      else if found_in_list tvid !unbound_id_list then ()
      else unbound_id_list := tvid :: !unbound_id_list
  | FuncType(_, tydom, tycod) -> ( listup_unbound_id tydom tyenv ; listup_unbound_id tycod tyenv )
  | ListType(_, tycont)       -> listup_unbound_id tycont tyenv
  | RefType(_, tycont)        -> listup_unbound_id tycont tyenv
  | _                         -> ()

(* type_variable_id list -> type_struct -> type_struct *)
let rec add_forall_struct lst tystr =
  match lst with
  | []           -> tystr
  | tvid :: tail -> ForallType(tvid, add_forall_struct tail tystr)

(* type_struct -> type_environment -> type_struct *)
let make_forall_type tystr tyenv =
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
