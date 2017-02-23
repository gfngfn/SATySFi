open Types

type t = (var_name * type_struct) list


let empty = []


let to_list tyenv = tyenv


let from_list lst = lst


let map f tyenv = List.map f tyenv


let rec add (tyenv : t) (varnm : var_name) (tystr : type_struct) =
  match tyenv with
  | []                                -> (varnm, tystr) :: []
  | (vn, ts) :: tail  when vn = varnm -> (varnm, tystr) :: tail
  | (vn, ts) :: tail                  -> (vn, ts) :: (add tail varnm tystr)


let rec find (tyenv : t) (varnm : var_name) =
  match tyenv with
  | []                               -> raise Not_found
  | (vn, ts) :: tail when vn = varnm -> ts
  | (vn, ts) :: tail                 -> find tail varnm


let rec find_in_type_struct (tvid : Tyvarid.t) (tystr : type_struct) =
  let iter      = find_in_type_struct tvid in
  let iter_list = find_in_type_struct_list tvid in
  let (_, tymain) = tystr in
    match tymain with
    | TypeVariable(tvidx)            -> Tyvarid.same tvidx tvid
    | FuncType(tydom, tycod)         -> (iter tydom) || (iter tycod)
    | ListType(tycont)               -> iter tycont
    | RefType(tycont)                -> iter tycont
    | ProductType(tylist)            -> iter_list tylist
    | VariantType(tylist, _)         -> iter_list tylist
    | TypeSynonym(tylist, _, tycont) -> (iter_list tylist) || (iter tycont)
    | _                              -> false


and find_in_type_struct_list (tvid : Tyvarid.t) (tystrlst : type_struct list) =
  List.fold_left (fun b tystr -> b || find_in_type_struct tvid tystr) false tystrlst


let rec find_in_type_environment (tvid : Tyvarid.t) (tyenv : t) =
  List.fold_left (fun b (_, tystr) -> b || find_in_type_struct tvid tystr) false tyenv


let quantifiable_unbound_id_list : Tyvarid.t list ref = ref []


let rec listup_quantifiable_unbound_id (tystr : type_struct) (tyenv : t) : unit =
  let iter = (fun ty -> listup_quantifiable_unbound_id ty tyenv) in
  let (_, tymain) = tystr in
    match tymain with
    | TypeVariable(tvid)             ->
        if Tyvarid.is_quantifiable tvid then
          if find_in_type_environment tvid tyenv then () else
            if List.mem tvid !quantifiable_unbound_id_list then () else
              quantifiable_unbound_id_list := tvid :: !quantifiable_unbound_id_list
        else
          ()
    | TypeSynonym(tylist, _, tycont) -> List.iter iter tylist (* doubtful implementation *)
    | FuncType(tydom, tycod)         -> begin iter tydom ; iter tycod end
    | ProductType(tylist)            -> List.iter iter tylist
    | RecordType(asc)                -> List.iter iter (List.map (fun (fldnm, tystr) -> tystr) (Assoc.to_list asc))
    | VariantType(tylist, _)         -> List.iter iter tylist
    | ListType(tycont)               -> iter tycont
    | RefType(tycont)                -> iter tycont
    | ( IntType | BoolType | UnitType | StringType ) -> ()
    | ( ForallType(_, _, _) | TypeArgument(_) )      -> failwith "listup_quantifiable_unbound_id"


let listup_quantifiable_unbound_id_in_kind_environment (kdenv : Kindenv.t) (tyenv : t) =
  let aux kdstr =
    match kdstr with
    | UniversalKind   -> ()
    | RecordKind(asc) -> List.iter (fun ty -> listup_quantifiable_unbound_id ty tyenv) (List.map (fun (fldnm, tystr) -> tystr) (Assoc.to_list asc))
  in
    List.iter aux (Kindenv.to_kind_struct_list kdenv)


let rec add_forall_struct (kdenv : Kindenv.t) (lst : Tyvarid.t list) (tystr : type_struct) =
  match lst with
  | []           -> tystr
  | tvid :: tail ->
     let kdstr = try Kindenv.find kdenv tvid with Not_found -> failwith ("add_forall_struct '" ^ (Tyvarid.show_direct tvid) ^ "'") in
       (Range.dummy "add_forall_struct", ForallType(tvid, kdstr, add_forall_struct kdenv tail tystr))


let make_forall_type (tystr : type_struct) (tyenv_before : t) (kdenv : Kindenv.t) =
  begin
    quantifiable_unbound_id_list := [] ;
    listup_quantifiable_unbound_id tystr tyenv_before ;
    listup_quantifiable_unbound_id_in_kind_environment kdenv tyenv_before ;
    add_forall_struct kdenv (!quantifiable_unbound_id_list) tystr
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


let rec replace_id (lst : (Tyvarid.t * type_struct) list) (tystr : type_struct) =
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
    | ForallType(tvid, kdstr, tycont)      ->
        begin
          try let _ = find_id_in_list tvid lst in (rng, ForallType(tvid, kdstr, tycont)) with
          | Not_found -> (rng, ForallType(tvid, kdstr, iter tycont))
        end
    | other                                -> (rng, other)


let rec make_unquantifiable_if_needed qtfbl tystr =
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
    | ForallType(tvid, kdstr, tycont)      -> ForallType(tvid, kdstr, iter tycont)
    | RecordType(asc)                      -> RecordType(Assoc.map_value (make_unquantifiable_if_needed qtfbl) asc)
    | other                                -> other
  in
    (rng, tymainnew)


let make_bounded_free qtfbl (kdenv : Kindenv.t) (tystr : type_struct) =
  let rec eliminate_forall qtfbl (kdenv : Kindenv.t) (tystr : type_struct) (lst : (Tyvarid.t * Tyvarid.t * type_struct) list) =
    let (rng, tymain) = tystr in
    match tymain with
    | ForallType(oldtvid, kdstr, tycont) ->
        let newtvid = Tyvarid.fresh qtfbl in
        let beta = (Range.dummy "eliminate_forall", TypeVariable(newtvid)) in
          eliminate_forall qtfbl (Kindenv.add kdenv newtvid kdstr) tycont ((oldtvid, newtvid, beta) :: lst)

    | _ ->
        let tyfree    = replace_id (List.map (fun (o, n, b) -> (o, b)) lst) tystr in
        let kdenvfree = List.fold_left (fun oldkdenv (oldtvid, newtvid, beta) -> Kindenv.replace_type_variable_in_kindenv oldkdenv oldtvid beta) kdenv lst in
        let tyqtf     = make_unquantifiable_if_needed qtfbl tyfree in
        let tyarglist = List.map (fun (o, n, b) -> b) lst in
          (tyqtf, tyarglist, kdenvfree)
  in
    eliminate_forall qtfbl kdenv tystr []
