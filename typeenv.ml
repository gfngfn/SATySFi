open Types

type t = (var_name * type_struct) list

(* t *)
let empty = []


(* t -> (var_name * type_struct) list *)
let to_list tyenv = tyenv


(* (var_name * type_struct) list -> t *)
let from_list lst = lst


(* t -> var_name -> type_struct -> t *)
let rec add tyenv varnm tystr =
  match tyenv with
  | []                               -> [(varnm, tystr)]
  | (vn, ts) :: tail when vn = varnm -> (varnm, tystr) :: tail
  | (vn, ts) :: tail                 -> (vn, ts) :: (add tail varnm tystr)


(* t -> var_name -> type_struct *)
let rec find tyenv varnm =
  match tyenv with
  | []                               -> raise Not_found
  | (vn, ts) :: tail when vn = varnm -> ts
  | (vn, ts) :: tail                 -> find tail varnm


(* type_struct -> code_range *)
let get_range_from_type tystr =
  match tystr with
  | IntType(rng)              -> rng
  | StringType(rng)           -> rng
  | BoolType(rng)             -> rng
  | UnitType(rng)             -> rng
  | FuncType(rng, _, _)       -> rng
  | ListType(rng, _)          -> rng
  | RefType(rng, _)           -> rng
  | ProductType(rng, _)       -> rng
  | TypeVariable(rng, _)      -> rng
  | TypeSynonym(rng, _, _, _) -> rng
  | VariantType(rng, _, _)    -> rng
  | ForallType(_, _)          -> (-31, 0, 0, 0)
  | TypeArgument(rng, _)      -> rng


(* type_struct -> code_range -> type_struct *)
let overwrite_range_of_type tystr rng =
  match tystr with
  | IntType(_)                              -> IntType(rng)
  | StringType(_)                           -> StringType(rng)
  | BoolType(_)                             -> BoolType(rng)
  | UnitType(_)                             -> UnitType(rng)
  | FuncType(_, tydom, tycod)               -> FuncType(rng, tydom, tycod)
  | ListType(_, tycont)                     -> ListType(rng, tycont)
  | RefType(_, tycont)                      -> RefType(rng, tycont)
  | ProductType(_, tylist)                  -> ProductType(rng, tylist)
  | TypeVariable(_, tvid)                   -> TypeVariable(rng, tvid)
  | TypeSynonym(_, tyarglist, tynm, tycont) -> TypeSynonym(rng, tyarglist, tynm, tycont)
  | VariantType(_, tyarglist, varntnm)      -> VariantType(rng, tyarglist, varntnm)
  | ForallType(tvid, tycont)                -> ForallType(tvid, tycont)
  | TypeArgument(_, tyarg)                  -> TypeArgument(rng, tyarg)


(* type_struct -> type_struct *)
let rec erase_range_of_type tystr =
  let dummy = (-2048, 0, 0, 0) in
  let f = erase_range_of_type in
    match tystr with
    | IntType(_)                              -> IntType(dummy)
    | StringType(_)                           -> StringType(dummy)
    | BoolType(_)                             -> BoolType(dummy)
    | UnitType(_)                             -> UnitType(dummy)
    | FuncType(_, tydom, tycod)               -> FuncType(dummy, f tydom, f tycod)
    | ListType(_, tycont)                     -> ListType(dummy, f tycont)
    | RefType(_, tycont)                      -> RefType(dummy, f tycont)
    | ProductType(_, tylist)                  -> ProductType(dummy, List.map f tylist)
    | TypeVariable(_, tvid)                   -> TypeVariable(dummy, tvid)
    | TypeSynonym(_, tyarglist, tynm, tycont) -> TypeSynonym(dummy, List.map f tyarglist, tynm, f tycont)
    | VariantType(_, tyarglist, varntnm)      -> VariantType(dummy, List.map f tyarglist, varntnm)
    | ForallType(tvid, tycont)                -> ForallType(tvid, f tycont)
    | TypeArgument(_, tyargnm)                -> TypeArgument(dummy, tyargnm)


(* type_variable_id -> type_struct -> bool *)
let rec find_in_type_struct tvid tystr =
  match tystr with
  | FuncType(_, tydom, tycod)         -> (find_in_type_struct tvid tydom) || (find_in_type_struct tvid tycod)
  | ListType(_, tycont)               -> find_in_type_struct tvid tycont
  | RefType(_, tycont)                -> find_in_type_struct tvid tycont
  | ProductType(_, tylist)            -> find_in_type_struct_list tvid tylist
  | TypeVariable(_, tvidx)            -> tvidx = tvid
  | VariantType(_, tylist, _)         -> find_in_type_struct_list tvid tylist
  | TypeSynonym(_, tylist, _, tycont) -> (find_in_type_struct_list tvid tylist) || (find_in_type_struct tvid tycont)
  | _                                 -> false

and find_in_type_struct_list tvid tystr =
  match tystr with
  | []         -> false
  | ty :: tail -> if find_in_type_struct tvid ty then true else find_in_type_struct_list tvid tail


(* type_variable_id -> t -> bool *)
let rec find_in_type_environment tvid tyenv =
  match tyenv with
  | []                 -> false
  | (_, tystr) :: tail ->
      if find_in_type_struct tvid tystr then true else find_in_type_environment tvid tail


let unbound_id_list : type_variable_id list ref = ref []

(* type_struct -> t -> (type_variable_id list) -> unit *)
let rec listup_unbound_id tystr tyenv =
  match tystr with
  | TypeVariable(_, tvid)     ->
      if find_in_type_environment tvid tyenv then ()
      else if List.mem tvid !unbound_id_list then ()
      else unbound_id_list := tvid :: !unbound_id_list
  | FuncType(_, tydom, tycod) -> begin listup_unbound_id tydom tyenv ; listup_unbound_id tycod tyenv end
  | ListType(_, tycont)       -> listup_unbound_id tycont tyenv
  | RefType(_, tycont)        -> listup_unbound_id tycont tyenv
  | ProductType(_, tylist)    -> let _ = List.map (fun ty -> listup_unbound_id ty tyenv) in ()
  | _                         -> ()


(* type_variable_id list -> type_struct -> type_struct *)
let rec add_forall_struct lst tystr =
  match lst with
  | []           -> tystr
  | tvid :: tail -> ForallType(tvid, add_forall_struct tail tystr)


(* type_struct -> t -> type_struct *)
let make_forall_type tystr tyenv =
  begin
    unbound_id_list := [] ;
    listup_unbound_id tystr tyenv ;
    add_forall_struct (!unbound_id_list) tystr
  end


(* t -> string -> string *)
let rec string_of_type_environment tyenv msg =
    "    #==== " ^ msg ^ " " ^ (String.make (58 - (String.length msg)) '=') ^ "\n"
  ^ (string_of_type_environment_sub tyenv)
  ^ "    #================================================================\n"

and string_of_type_environment_sub tyenv =
  match tyenv with
  | []               -> ""
  | (vn, ts) :: tail ->
      let (a, _, _, _) = get_range_from_type ts in (* dirty code *)
        if -38 <= a && a <= -1 then
          string_of_type_environment_sub tail
        else
          "    #  "
            ^ ( let len = String.length vn in if len >= 16 then vn else vn ^ (String.make (16 - len) ' ') )
            ^ " : " ^ ((* string_of_type_struct ts *) "type") ^ "\n" (* remains to be implemented *)
            ^ (string_of_type_environment_sub tail)


(* t -> string *)
let rec string_of_control_sequence_type tyenv =
    "    #================================================================\n"
  ^ (string_of_control_sequence_type_sub tyenv)
  ^ "    #================================================================\n"

and string_of_control_sequence_type_sub tyenv =
  match tyenv with
  | []               -> ""
  | (vn, ts) :: tail ->
      begin match String.sub vn 0 1 with
      | "\\" ->
          "    #  "
            ^ ( let len = String.length vn in if len >= 16 then vn else vn ^ (String.make (16 - len) ' ') )
            ^ " : " ^ ((* string_of_type_struct ts *) "type") ^ "\n" (* remains to be implemented *)
      | _    -> ""
      end ^ (string_of_control_sequence_type_sub tail)



let tvidmax        : type_variable_id ref = ref 0

let initialize () = ( tvidmax := 0 )

let new_type_variable_id () =
  let res = !tvidmax in ( tvidmax := !tvidmax + 1 ; res )

(* 'a -> ('a * 'b) list -> 'b *)
let rec find_id_in_list elm lst =
  match lst with
  | []                                    -> raise Not_found
  | (tvid, tystr) :: tail when tvid = elm -> tystr
  | _ :: tail                             -> find_id_in_list elm tail


(* type_struct -> type_struct * (type_struct list) *)
let rec make_bounded_free tystr = eliminate_forall tystr []

and eliminate_forall tystr lst =
  match tystr with
  | ForallType(tvid, tycont) ->
      let ntvstr = TypeVariable((-2, 0, 0, 0), new_type_variable_id ()) in
        eliminate_forall tycont ((tvid, ntvstr) :: lst)

  | other ->
      let tyfree    = replace_id lst other in
      let tyarglist = List.map (fun (tvid, ntvstr) -> ntvstr) lst in
        (tyfree, tyarglist)


and replace_id lst tystr =
  let f = replace_id lst in
    match tystr with
    | TypeVariable(rng, tvid)           ->
        begin
          try find_id_in_list tvid lst with
          | Not_found -> TypeVariable(rng, tvid)
        end
    | ListType(rng, tycont)             -> ListType(rng, f tycont)
    | RefType(rng, tycont)              -> RefType(rng, f tycont)
    | ProductType(rng, tylist)          -> ProductType(rng, List.map f tylist)
    | FuncType(rng, tydom, tycod)       -> FuncType(rng, f tydom, f tycod)
    | VariantType(rng, tylist, varntnm) -> VariantType(rng, List.map f tylist, varntnm)
    | other                             -> other
