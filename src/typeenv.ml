open Types

type t = (var_name * poly_type) list


let empty = []


let to_list tyenv = tyenv


let from_list lst = lst


let map f tyenv = List.map f tyenv


let rec add (tyenv : t) (varnm : var_name) (pty : poly_type) =
  match tyenv with
  | []                                -> (varnm, pty) :: []
  | (vn, pt) :: tail  when vn = varnm -> (varnm, pty) :: tail
  | (vn, pt) :: tail                  -> (vn, pt) :: (add tail varnm pty)


let rec find (tyenv : t) (varnm : var_name) =
  match tyenv with
  | []                               -> raise Not_found
  | (vn, ts) :: tail when vn = varnm -> ts
  | (vn, ts) :: tail                 -> find tail varnm


let rec find_in_mono_type (tvid : Tyvarid.t) (ty : mono_type) =
  let iter      = find_in_mono_type tvid in
  let iter_list = find_in_mono_type_list tvid in
(*  let iter_poly = find_in_poly_type tvid in *)
  let (_, tymain) = ty in
    match tymain with
    | TypeVariable(tvref) ->
        begin
          match !tvref with
          | Link(tyl)   -> iter tyl
          | Free(tvidx) -> Tyvarid.eq tvidx tvid
          | Bound(_)    -> false
        end
    | FuncType(tydom, tycod)         -> (iter tydom) || (iter tycod)
    | ListType(tycont)               -> iter tycont
    | RefType(tycont)                -> iter tycont
    | ProductType(tylist)            -> iter_list tylist
    | VariantType(tylist, _)         -> iter_list tylist
(*    | TypeSynonym(tylist, _, pty)    -> (iter_list tylist) || (iter_poly pty) *)
    | _                              -> false


and find_in_poly_type (tvid : Tyvarid.t) ((Poly(ty)) : poly_type) =
  find_in_mono_type tvid ty


and find_in_mono_type_list (tvid : Tyvarid.t) (tystrlst : mono_type list) =
  List.fold_left (fun b tystr -> b || find_in_mono_type tvid tystr) false tystrlst


let rec find_in_type_environment (tvid : Tyvarid.t) (tyenv : t) =
  List.fold_left (fun b (_, pty) -> b || find_in_poly_type tvid pty) false tyenv


(* ---- following are all for debugging --- *)

let string_of_type_environment (tyenv : t) (msg : string) =
  let rec iter (tyenv : t) =
    match tyenv with
    | []               -> ""
    | (vn, ts) :: tail ->
            "    #  "
              ^ ( let len = String.length vn in if len >= 16 then vn else vn ^ (String.make (16 - len) ' ') )
       ^ " : " ^ ((* string_of_mono_type ts *) "type") ^ "\n"
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
                ^ " : " ^ ((* string_of_mono_type ts *) "type") ^ "\n" (* remains to be implemented *)
          | _    -> ""
        ) ^ (iter tail)
  in
      "    #================================================================\n"
    ^ (iter tyenv)
    ^ "    #================================================================\n"



let rec find_id_in_list (elm : Tyvarid.t) (lst : (Tyvarid.t * mono_type) list) =
  match lst with
  | []                                              -> raise Not_found
  | (tvid, tystr) :: tail  when Tyvarid.eq tvid elm -> tystr
  | _ :: tail                                       -> find_id_in_list elm tail


let rec make_unquantifiable ((_, tymain) : mono_type) =
  let iter = make_unquantifiable in
    match tymain with
    | TypeVariable(tvref) ->
        begin
          match !tvref with
          | Link(tyl)  -> iter tyl
          | Free(tvid) -> ( tvref := Free(Tyvarid.set_quantifiability Unquantifiable tvid) )
          | Bound(bid) -> assert false
        end
    | ListType(tycont)                     -> iter tycont
    | RefType(tycont)                      -> iter tycont
    | ProductType(tylist)                  -> List.iter iter tylist
    | FuncType(tydom, tycod)               -> begin iter tydom ; iter tycod end
    | VariantType(tylist, varntnm)         -> List.iter iter tylist
(*    | TypeSynonym(tylist, tysynnm, pty)    -> begin List.iter iter tylist ; () (* temporary *) end *)
    | RecordType(asc)                      -> Assoc.iter_value iter asc
    | other                                -> ()
