open Types

type t = (var_name * type_struct) list


let empty = []


let to_list tyenv = tyenv


let from_list lst = lst


let rec add tyenv (varnm : var_name) (tystr : type_struct) =
  match tyenv with
  | []                                -> (varnm, tystr) :: []
  | (vn, ts) :: tail  when vn = varnm -> (varnm, tystr) :: tail
  | (vn, ts) :: tail                  -> (vn, ts) :: (add tail varnm tystr)


let rec find (tyenv : t) (varnm : var_name) =
  match tyenv with
  | []                               -> raise Not_found
  | (vn, ts) :: tail when vn = varnm -> ts
  | (vn, ts) :: tail                 -> find tail varnm


let overwrite_range_of_type (tystr : type_struct) (rng : Range.t) =
  let (_, tymain) = tystr in (rng, tymain)


let rec erase_range_of_type (tystr : type_struct) =
  let (_, tymain) = tystr in (Range.dummy "erased", tymain)


let rec find_in_type_struct (tvid : Tyvarid.t) (tystr : type_struct) =
  let (_, tymain) = tystr in
    match tymain with
    | TypeVariable(tvidx)            -> Tyvarid.same tvidx tvid
    | FuncType(tydom, tycod)         -> (find_in_type_struct tvid tydom) || (find_in_type_struct tvid tycod)
    | ListType(tycont)               -> find_in_type_struct tvid tycont
    | RefType(tycont)                -> find_in_type_struct tvid tycont
    | ProductType(tylist)            -> find_in_type_struct_list tvid tylist
    | VariantType(tylist, _)         -> find_in_type_struct_list tvid tylist
    | TypeSynonym(tylist, _, tycont) -> (find_in_type_struct_list tvid tylist) || (find_in_type_struct tvid tycont)
    | _                              -> false

and find_in_type_struct_list (tvid : Tyvarid.t) (tystrlst : type_struct list) =
  match tystrlst with
  | []         -> false
  | ty :: tail -> if find_in_type_struct tvid ty then true else find_in_type_struct_list tvid tail


let rec find_in_type_environment (tvid : Tyvarid.t) (tyenv : t) =
  match tyenv with
  | []                 -> false
  | (_, tystr) :: tail ->
      if find_in_type_struct tvid tystr then true else find_in_type_environment tvid tail


let unbound_id_list : Tyvarid.t list ref = ref []


let rec listup_unbound_id (tystr : type_struct) (tyenv : t) =
  let iter = (fun ty -> listup_unbound_id ty tyenv) in
  let (_, tymain) = tystr in
    match tymain with
    | TypeVariable(tvid)     ->
        if find_in_type_environment tvid tyenv then ()
        else if List.mem tvid !unbound_id_list then ()
        else unbound_id_list := tvid :: !unbound_id_list
    | FuncType(tydom, tycod)         -> begin iter tydom ; iter tycod end
    | ListType(tycont)               -> iter tycont
    | RefType(tycont)                -> iter tycont
    | ProductType(tylist)            -> let _ = List.map iter tylist in ()
    | VariantType(tylist, _)         -> let _ = List.map iter tylist in ()
    | TypeSynonym(tylist, _, tycont) -> let _ = List.map iter tylist in () (* doubtful implementation *)
    | _                              -> ()


let rec add_forall_struct (lst : Tyvarid.t list) (tystr : type_struct) =
  match lst with
  | []           -> tystr
  | tvid :: tail ->
      if Tyvarid.is_quantifiable tvid then
        (Range.dummy "add_forall_struct", ForallType(tvid, add_forall_struct tail tystr))
      else
        add_forall_struct tail tystr


let make_forall_type (tystr : type_struct) (tyenv : t) =
  begin
    unbound_id_list := [] ;
    listup_unbound_id tystr tyenv ;
    add_forall_struct (!unbound_id_list) tystr
  end


let string_of_type_environment (tyenv : t) (msg : string) =
  let rec iter (tyenv : t) =
    match tyenv with
    | []               -> ""
    | (vn, ts) :: tail ->
            "    #  "
              ^ ( let len = String.length vn in if len >= 16 then vn else vn ^ (String.make (16 - len) ' ') )
              ^ " : " ^ ((* string_of_type_struct ts *) "type") ^ "\n" (* remains to be implemented *)
              ^ (iter tail)
  in
      "    #==== " ^ msg ^ " " ^ (String.make (58 - (String.length msg)) '=') ^ "\n"
    ^ (iter tyenv)
    ^ "    #================================================================\n"


let string_of_control_sequence_type (tyenv : t) =
  let rec iter (tyenv : t) =
    match tyenv with
    | []               -> ""
    | (vn, ts) :: tail ->
        ( match String.sub vn 0 1 with
          | "\\" ->
              "    #  "
                ^ ( let len = String.length vn in if len >= 16 then vn else vn ^ (String.make (16 - len) ' ') )
                ^ " : " ^ ((* string_of_type_struct ts *) "type") ^ "\n" (* remains to be implemented *)
          | _    -> ""
        ) ^ (iter tail)
  in
      "    #================================================================\n"
    ^ (iter tyenv)
    ^ "    #================================================================\n"



let rec find_id_in_list (elm : Tyvarid.t) (lst : (Tyvarid.t * type_struct) list) =
  match lst with
  | []                                               -> raise Not_found
  | (tvid, tystr) :: tail when Tyvarid.same tvid elm -> tystr
  | _ :: tail                                        -> find_id_in_list elm tail


let rec make_bounded_free qtfbl (tystr : type_struct) = eliminate_forall qtfbl tystr []

and eliminate_forall qtfbl (tystr : type_struct) (lst : (Tyvarid.t * type_struct) list) =
  let (rng, tymain) = tystr in
  match tymain with
  | ForallType(tvid, tycont) ->
      let ntvstr = (Range.dummy "eliminate_forall", TypeVariable(Tyvarid.fresh qtfbl)) in
        eliminate_forall qtfbl tycont ((tvid, ntvstr) :: lst)

  | _ ->
      let tyfree    = replace_id lst tystr in
      let tyqtf     = make_unquantifiable_if_needed qtfbl tyfree in
      let tyarglist = List.map (fun (tvid, ntvstr) -> ntvstr) lst in
        (tyqtf, tyarglist)

and make_unquantifiable_if_needed qtfbl tystr =
  let iter = make_unquantifiable_if_needed qtfbl in
  let (rng, tymain) = tystr in
  let tymainnew =
    match tymain with
    | TypeVariable(tvid)                   ->
        begin
          match qtfbl with
          | Tyvarid.Quantifiable   -> TypeVariable(tvid)
          | Tyvarid.Unquantifiable -> TypeVariable(Tyvarid.set_quantifiability Tyvarid.Unquantifiable tvid)
        end
    | ListType(tycont)                     -> ListType(iter tycont)
    | RefType(tycont)                      -> RefType(iter tycont)
    | ProductType(tylist)                  -> ProductType(List.map iter tylist)
    | FuncType(tydom, tycod)               -> FuncType(iter tydom, iter tycod)
    | VariantType(tylist, varntnm)         -> VariantType(List.map iter tylist, varntnm)
    | TypeSynonym(tylist, tysynnm, tycont) -> TypeSynonym(List.map iter tylist, tysynnm, iter tycont)
    | ForallType(tvid, tycont)             -> ForallType(tvid, iter tycont)
    | other                                -> other
  in
    (rng, tymainnew)


and replace_id (lst : (Tyvarid.t * type_struct) list) (tystr : type_struct) =
  let iter = replace_id lst in
  let (rng, tymain) = tystr in
    match tymain with
    | TypeVariable(tvid)                   ->
        begin
          try find_id_in_list tvid lst with
          | Not_found -> (rng, TypeVariable(tvid))
        end
    | ListType(tycont)                     -> (rng, ListType(iter tycont))
    | RefType(tycont)                      -> (rng, RefType(iter tycont))
    | ProductType(tylist)                  -> (rng, ProductType(List.map iter tylist))
    | FuncType(tydom, tycod)               -> (rng, FuncType(iter tydom, iter tycod))
    | VariantType(tylist, varntnm)         -> (rng, VariantType(List.map iter tylist, varntnm))
    | TypeSynonym(tylist, tysynnm, tycont) -> (rng, TypeSynonym(List.map iter tylist, tysynnm, iter tycont))
    | ForallType(tvid, tycont)             ->
        begin
          try let _ = find_id_in_list tvid lst in (rng, ForallType(tvid, tycont)) with
          | Not_found -> (rng, ForallType(tvid, iter tycont))
        end
    | other                                -> (rng, other)
