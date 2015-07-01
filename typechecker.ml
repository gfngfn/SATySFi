open Types

exception TypeCheckError of string

let print_process msg =
(*
	print_string (msg ^ "\n") ;
*)
  ()

let rec string_of_type_struct tystr =
  match tystr with
  | TypeEnvironmentType(_) -> "env"
  | UnitType -> "unit"
  | IntType -> "int"
  | StringType -> "string"
  | BoolType -> "bool"
  | FuncType(tyf, tyl) -> "(" ^ (string_of_type_struct tyf) ^ " -> " ^ (string_of_type_struct tyl) ^ ")"
  | ListType(ty) -> "(" ^ (string_of_type_struct ty) ^ " list)"
  | TypeVariable(tvid, varnm) -> "'" ^ (string_of_int tvid) ^ "[" ^ varnm ^ "]"

let find_real_type theta tvid =
  ( print_process ("  *seeking '" ^ (string_of_int tvid)) ;
    Hashtbl.find theta tvid )

let tvidmax : type_variable_id ref = ref 0
let new_type_variable varnm =
  let res = TypeVariable(!tvidmax, varnm) in
  ( print_process ("  *make '" ^ (string_of_int !tvidmax) ^ " for " ^ varnm) ;
    tvidmax := !tvidmax + 1 ;
    res
  )
let rec equivalent tya tyb =
  match (tya, tyb) with
  | (UnitType, UnitType)     -> true
  | (IntType, IntType)       -> true
  | (StringType, StringType) -> true
  | (BoolType, BoolType)     -> true
  | (FuncType(tyadom, tyacod), FuncType(tybdom, tybcod))
      -> (equivalent tyadom tybdom) && (equivalent tyacod tybcod)
  | (ListType(tycnta), ListType(tycntb)) -> (equivalent tycnta tycntb)
  | (TypeVariable(tvida, _), TypeVariable(tvidb, _)) -> (tvida == tvidb)
  | _ -> false

(* type_environment -> untyped_abstract_tree -> (abstract_tree * type_struct * Subst.t) *)
let rec typecheck tyenv utast =
	let (rng, utastmain) = utast in
    match utastmain with
    | UTStringEmpty         -> (StringEmpty, StringType, Subst.empty)
    | UTNumericConstant(nc) -> (NumericConstant(nc), IntType, Subst.empty)
    | UTStringConstant(sc)  -> (StringConstant(sc), StringType, Subst.empty)
    | UTBooleanConstant(bc) -> (BooleanConstant(bc), BoolType, Subst.empty)
    | UTContentOf(nv) ->
        ( try
            let ty = Hashtbl.find tyenv nv in
            ( print_process ("  " ^ nv ^ ": <" ^ string_of_type_struct ty ^ ">") ;
              ty )
          with
          | Not_found -> raise (TypeCheckError("undefined variable '" ^ nv ^ "'"))
        )
    | UTConcat(utast1, utast2) ->
        let (e1, ty1, theta1) = typecheck tyenv utast1 in
        let (e2, ty2, theta2) = typecheck tyenv utast2 in
        let theta3 = Subst.unify ty1 StringType in
        let theta4 = Subst.unify ty2 StringType in
          let term_result = Concat()

    | UTApply(astf, astl) ->
        let (e1, ty1, theta1) = typecheck tyenv utast1 in
        let (e2, ty2, theta2) = typecheck tyenv utast2 in
        let beta = new_type_variable () in
        let theta3 = Subst.unify (FuncType(ty2, beta)) ty1 in
          let term_result = Apply(
          	                  Subst.apply_to_term (Subst.compose theta3 theta1) e1,
          	                  Subst.apply_to_term theta3 e2
                            )
          let type_result = Subst.apply_to_type_variable theta3 beta
          let theta_result = Subst.compose theta3 (Subst.compose theta2 theta1) in
            (term_result, type_result, theta_result)

    | UTBreakAndIndent -> (BreakAndIndent, StringType, Subst.empty)

    | UTLetIn(mutletcons, astl) ->
        let tyenv_new = Hashtbl.copy tyenv in
        ( add_mutual_variables tyenv_new mutletcons ;
          typecheck_mutual_contents tyeq tyenv_new mutletcons ;
            let tyl = typecheck tyenv_new astl in
              tyl
        )
    | UTIfThenElse(astb, asttru, astfls) ->
        let tyb = typecheck tyenv astb in
        let tytru = typecheck tyenv asttru in
        let tyfls = typecheck tyenv astfls in
        ( ( if equivalent BoolType tyb then () else Stacklist.push tyeq (BoolType, tyb) ) ;
          ( if equivalent tytru tyfls then () else Stacklist.push tyeq (tytru, tyfls) ) ;
          tytru
        )
    | UTIfClassIsValid(asttru, astfls) ->
        let tyenv_class = Hashtbl.copy tyenv in
        ( Hashtbl.add tyenv_class "class" StringType ;
          let tytru = typecheck tyenv_class asttru in
          let tyfls = typecheck tyenv astfls in
          ( ( if equivalent tytru tyfls then () else Stacklist.push tyeq (tytru, tyfls) ) ;
            tytru
          )
        )
    | UTIfIDIsValid(asttru, astfls) ->
        let tyenv_id = Hashtbl.copy tyenv in
        ( Hashtbl.add tyenv_id "id" StringType ;
          let tytru = typecheck tyenv_id asttru in
          let tyfls = typecheck tyenv astfls in
          ( ( if equivalent tytru tyfls then () else Stacklist.push tyeq (tytru, tyfls) ) ;
            tytru
          )
        )
    | UTLambdaAbstract(varnm, astdef) ->
      ( print_process ("#LambdaAbstract " ^ varnm ^ ". ...") ;
        let tyvar = new_type_variable varnm in
        let tyenv_new = Hashtbl.copy tyenv in
        ( Hashtbl.add tyenv_new varnm tyvar ;
          let tydef = typecheck tyenv_new astdef in
            FuncType(tyvar, tydef)
        )
      )
            (* AYASHII! *)
    | UTApplyClassAndID(_, _, astf) ->
        typecheck tyenv astf
  
    | UTListCons(asthd, asttl) ->
        let tyhd = typecheck tyenv asthd in
        let tytl = typecheck tyenv asttl in
        ( ( if equivalent (ListType(tyhd)) tytl then () else Stacklist.push tyeq (ListType(tyhd), tytl) ) ;
          ListType(tyhd)
        )
    | UTEndOfList -> let ntyvar = new_type_variable "*empty" in ListType(ntyvar)
  
    | UTLetMutableIn(varnm, astdflt, astaft) ->
        let tydflt = typecheck tyenv astdflt in
        let tyenv_new = Hashtbl.copy tyenv in
        ( Hashtbl.add tyenv varnm tydflt ;
          let tyaft = typecheck tyenv_new astaft in tyaft
        )
  
    | UTSequential(astf, astl) ->
        let tyf = typecheck tyenv astf in
        let tyl = typecheck tyenv astl in
        ( ( if equivalent UnitType tyf then () else Stacklist.push tyeq (UnitType, tyf)) ;
          tyl
        )
    | UTOverwrite(varnm, astnew) ->
        let _ = typecheck tyenv astnew in UnitType
  
    | UTFinishHeaderFile -> TypeEnvironmentType(tyenv)
  
    | UTNoContent -> StringType
  
    | _ -> raise (TypeCheckError("this cannot happen / remains to be implemented"))


and add_mutual_variables tyenv mutletcons =
  match mutletcons with
  | UTEndOfMutualLet -> ()
  | UTMutualLetCons(nv, _, tailcons) ->
      let ntv = new_type_variable nv in
      ( Hashtbl.add tyenv nv ntv ;
        add_mutual_variables tyenv tailcons )

and typecheck_mutual_contents tyenv mutletcons =
  match mutletcons with
  | UTEndOfMutualLet -> ()
  | UTMutualLetCons(nv, astcont, tailcons) ->
      let tycont = typecheck tyenv astcont in
      let ntv = Hashtbl.find tyenv nv in
      ( Stacklist.push tyeq (ntv, tycont) ;
        typecheck_mutual_contents tyenv tailcons )

(* type_variable_id -> type_struct -> bool *)
let rec emerge_in tyid tystr =
  match tystr with
  | TypeVariable(tyidsub, _) -> tyid == tyidsub
  | FuncType(tydom, tycod) -> (emerge_in tyid tydom) || (emerge_in tyid tycod)
  | _ -> false

(* ((type_variable_id, type_struct) Hashtbl.t) -> type_struct -> type_struct *)
let rec subst_type_by_theta theta tystr =
  match tystr with
  | TypeVariable(tvid, varnm) -> ( try find_real_type theta tvid with Not_found -> TypeVariable(tvid, varnm) )
  | FuncType(tydom, tycod) -> FuncType(subst_type_by_theta theta tydom, subst_type_by_theta theta tycod)
  | tys -> tys

let rec subst_list theta tyeqlst =
  match tyeqlst with
  | [] -> []
  | (tya, tyb) :: tail -> ((subst_type_by_theta theta tya), (subst_type_by_theta theta tyb)) :: (subst_list theta tail)

let rec string_of_tyeqlst tyeqlst =
  match tyeqlst with
  | [] -> ""
  | (tya, tyb) :: tail ->
      "<" ^ (string_of_type_struct tya) ^ "> = <" ^ (string_of_type_struct tyb) ^ ">\n"
        ^ (string_of_tyeqlst tail)

(* (type_struct * type_struct) -> ((type_variable_id, type_struct) Hashtbl.t) -> unit *)
let rec solve tyeqlst theta =
  (* uncommentout below if you would like to see recognized type equations *)

  print_process (string_of_tyeqlst tyeqlst) ;

  match tyeqlst with
  | [] -> ()
  | (tya, tyb) :: tail ->
      if equivalent tya tyb then
        solve tail theta
      else
      ( match (tya, tyb) with
        | (ListType(tycnta), ListType(tycntb)) ->
            solve ((tycnta, tycntb) :: tail) theta

        | (FuncType(tyadom, tyacod), FuncType(tybdom, tybcod)) ->
            solve ((tyadom, tybdom) :: (tyacod, tybcod) :: tail) theta

        | (TypeVariable(tvid, varnm), tystr) ->
            ( if emerge_in tvid tystr then
                raise (TypeCheckError("error 1: " ^ varnm ^ " is expected of type " ^ (string_of_type_struct tystr)))
              else
              ( print_process ("  $subst '" ^ (string_of_int tvid) ^ " := " ^ (string_of_type_struct tystr)) ;
                Hashtbl.add theta tvid tystr ;
                solve (subst_list theta tail) theta )
            )
        | (_, TypeVariable(_, _)) ->
            solve ((tyb, tya) :: tail) theta
              (*  this pattern matching must be after (TypeVariable(tvid), tystr)
                  in order to avoid endless loop
                  (TypeVariable(_), TypeVariable(_)) causes *)

        | (_, _) -> raise (TypeCheckError("inconsistent: "
                      ^ (string_of_type_struct tya) ^ " and " ^ (string_of_type_struct tyb)))
      )


(* type_equation -> ((type_variable_id, type_struct) Hashtbl.t) -> unit *)
let unify_type_variables tyeq theta =
  let tyeqlst = Stacklist.to_list !tyeq in solve tyeqlst theta

(* ((type_variable_id, type_struct) Hashtbl.t) -> type_struct -> type_struct *)
let rec unify theta ty =
  match ty with
  | FuncType(tydom, tycod) -> FuncType(unify theta tydom, unify theta tycod)
  | TypeVariable(tvid, varnm) -> ( try find_real_type theta tvid with Not_found -> TypeVariable(tvid, varnm) )
  | tystr -> tystr

(* Types.abstract_tree -> (string * type_environment) *)
let main tyenv ast =
  let tyeq : type_equation = ref Stacklist.empty in
  let theta : (type_variable_id, type_struct) Hashtbl.t = Hashtbl.create 128 in
    let type_before_unified = typecheck tyenv ast in
    ( unify_type_variables tyeq theta ;
      let type_after_unfied = subst_type_by_theta theta type_before_unified in
      let strty = string_of_type_struct type_after_unfied in
        match type_after_unfied with
        | TypeEnvironmentType(newtyenv) -> (strty, newtyenv)
        | _ -> (strty, tyenv)
    )

let initialize () = ( tvidmax := 0 )
