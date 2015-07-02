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
  | UnitType   -> "unit"
  | IntType    -> "int"
  | StringType -> "string"
  | BoolType   -> "bool"
  | FuncType(tyf, tyl) -> "(" ^ (string_of_type_struct tyf) ^ " -> " ^ (string_of_type_struct tyl) ^ ")"
  | ListType(ty)       -> "(" ^ (string_of_type_struct ty) ^ " list)"
  | TypeVariable(tvid) -> "'" ^ (string_of_int tvid) ^ ""
  | ForallType(tvid, tycont) -> "('" ^ (string_of_int tvid) ^ ". " ^ (string_of_type_struct tycont) ^ ")"


let tvidmax : type_variable_id ref = ref 0

let new_type_variable () =
  let res = TypeVariable(!tvidmax) in
  ( print_process ("  *make '" ^ (string_of_int !tvidmax)) ;
    tvidmax := !tvidmax + 1 ;
    res
  )

(* type_environment -> untyped_abstract_tree -> (abstract_tree * type_struct * Subst.t) *)
let rec typecheck tyenv utast =
  let (rng, utastmain) = utast in
    match utastmain with
    | UTStringEmpty         -> (StringEmpty, StringType, Subst.empty)
    | UTBreakAndIndent      -> (BreakAndIndent, StringType, Subst.empty)
    | UTNumericConstant(nc) -> (NumericConstant(nc), IntType, Subst.empty)
    | UTStringConstant(sc)  -> (StringConstant(sc), StringType, Subst.empty)
    | UTBooleanConstant(bc) -> (BooleanConstant(bc), BoolType, Subst.empty)
    | UTFinishHeaderFile    -> (FinishHeaderFile, TypeEnvironmentType(tyenv), Subst.empty)
    | UTNoContent           -> (NoContent, StringType, Subst.empty)
    | UTContentOf(nv) ->
        ( try
            let ty = Typeenv.find tyenv nv in
            ( print_process ("  " ^ nv ^ ": <" ^ string_of_type_struct ty ^ ">") ;
              ty )
          with
          | Not_found -> raise (TypeCheckError("undefined variable '" ^ nv ^ "'"))
        )
(*
    | UTConcat(utast1, utast2) ->
        let (e1, ty1, theta1) = typecheck tyenv utast1 in
        let (e2, ty2, theta2) = typecheck tyenv utast2 in
        let theta3 = Subst.unify ty1 StringType in
        let theta4 = Subst.unify ty2 StringType in
          let term_result = 
*)
    | UTApply(astf, astl) ->
        let (e1, ty1, theta1) = typecheck tyenv utast1 in
        let (e2, ty2, theta2) = typecheck tyenv utast2 in
        let beta = new_type_variable () in
        let theta3 = Subst.unify (FuncType(ty2, beta)) ty1 in
          let term_result = Apply(
                              Subst.apply_to_term (Subst.compose theta3 theta1) e1,
                              Subst.apply_to_term theta3 e2
                            ) in
          let type_result = Subst.apply_to_type_variable theta3 beta in
          let theta_result = Subst.compose theta3 (Subst.compose theta2 theta1) in
            (term_result, type_result, theta_result)

    | UTLambdaAbstract(varnm, utast1) ->
        let beta = new_type_variable () in
        let tyenv_new = Typeenv.copy tyenv in
        ( Typeenv.add tyenv_new varnm beta ;
          let (e1, ty1, theta1) = typecheck tyenv_new utast1 in
            let term_result = LambdaAbstract(varnm, e1) in
            let type_result = FuncType(Subst.apply_to_type_variable theta beta, ty1) in
            let theta_result = theta1 in
              (term_result, type_struct, theta_result)
        )

    | UTLetIn(utmutletcons, utast2) ->
        let tyenv_for_rec = add_mutual_variables tyenv utmutletcons in
        let (tyenv_new, mutletcons, theta1) = typecheck_mutual_contents tyenv_for_rec utmutletcons in
        let (e2, ty2, theta2) = typecheck tyenv_new utastlat in
          (LetIn(mutletcons, e2), tylat, Subst.compose theta2 theta1)
(*
    | UTIfThenElse(astb, asttru, astfls) ->
        let tyb = typecheck tyenv astb in
        let tytru = typecheck tyenv asttru in
        let tyfls = typecheck tyenv astfls in
        ( ( if equivalent BoolType tyb then () else Stacklist.push tyeq (BoolType, tyb) ) ;
          ( if equivalent tytru tyfls then () else Stacklist.push tyeq (tytru, tyfls) ) ;
          tytru
        )
    | UTIfClassIsValid(asttru, astfls) ->
        let tyenv_class = Typeenv.add tyenv "class" StringType in
          let tytru = typecheck tyenv_class asttru in
          let tyfls = typecheck tyenv astfls in
          ( ( if equivalent tytru tyfls then () else Stacklist.push tyeq (tytru, tyfls) ) ;
            tytru
          )
        )
    | UTIfIDIsValid(asttru, astfls) ->
        let tyenv_id = Typeenv.add tyenv_id "id" StringType in
          let tytru = typecheck tyenv_id asttru in
          let tyfls = typecheck tyenv astfls in
          ( ( if equivalent tytru tyfls then () else Stacklist.push tyeq (tytru, tyfls) ) ;
            tytru
          )
        )
    | UTApplyClassAndID(_, _, astf) -> typecheck tyenv astf
  
    | UTListCons(asthd, asttl) ->
        let tyhd = typecheck tyenv asthd in
        let tytl = typecheck tyenv asttl in
        ( ( if equivalent (ListType(tyhd)) tytl then () else Stacklist.push tyeq (ListType(tyhd), tytl) ) ;
          ListType(tyhd)
        )
    | UTEndOfList -> let ntyvar = new_type_variable "*empty" in ListType(ntyvar)
  
    | UTLetMutableIn(varnm, astdflt, astaft) ->
        let tydflt = typecheck tyenv astdflt in
        let tyenv_new = Typeenv.copy tyenv in
        ( Typeenv.add tyenv varnm tydflt ;
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
*)  
    | _ -> raise (TypeCheckError("this cannot happen / remains to be implemented"))

(* Typeenv.t -> untyped_mutual_let_cons -> Typeenv.t *)
and add_mutual_variables tyenv mutletcons =
  match mutletcons with
  | UTEndOfMutualLet -> tyenv
  | UTMutualLetCons(nv, _, tailcons) ->
      let ntv = new_type_variable () in
        add_mutual_variables (Typeenv.add tyenv nv ntv) tailcons

(* Typeenv.t -> untyped_mutual_let_cons -> (Typeenv.t * mutual_let_cons * Subst.t) *)
and typecheck_mutual_contents tyenv mutletcons =
  match mutletcons with
  | UTEndOfMutualLet -> (tyenv, EndOfMutualLet, Subst.empty)
  | UTMutualLetCons(nv, astcont, tailcons) ->
      let (e1, ty1, theta1) = typecheck tyenv astcont in
      let ntv = Typeenv.find tyenv nv in (* for self recursion *)
      let theta1_new = Subst.add theta1 ntv ty1 in
      let tyenv_new_pre = Subst.apply_to_type_environment theta1_new tyenv in
      let forallty = make_forall_type ty1 tyenv_new_pre in
      let tyenv_new = Typeenv.add tyenv_new_pre nv forallty in
      let (mutletcons_tail, tyenv_tail, theta_tail) = typecheck_mutual_contents tyenv_new tailcons in
        (MutualLetCons(nv, astcont, mutletcons_tail), tyenv_tail, Subst.compose theta_tail theta1_new)


(*
(* type_variable_id -> type_struct -> bool *)
let rec emerge_in tyid tystr =
  match tystr with
  | TypeVariable(tyidsub, _) -> tyid == tyidsub
  | FuncType(tydom, tycod) -> (emerge_in tyid tydom) || (emerge_in tyid tycod)
  | _ -> false

(* ((type_variable_id, type_struct) Typeenv.t) -> type_struct -> type_struct *)
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

(* (type_struct * type_struct) -> ((type_variable_id, type_struct) Typeenv.t) -> unit *)
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
                Typeenv.add theta tvid tystr ;
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


(* type_equation -> ((type_variable_id, type_struct) Typeenv.t) -> unit *)
let unify_type_variables tyeq theta =
  let tyeqlst = Stacklist.to_list !tyeq in solve tyeqlst theta

(* ((type_variable_id, type_struct) Typeenv.t) -> type_struct -> type_struct *)
let rec unify theta ty =
  match ty with
  | FuncType(tydom, tycod) -> FuncType(unify theta tydom, unify theta tycod)
  | TypeVariable(tvid, varnm) -> ( try find_real_type theta tvid with Not_found -> TypeVariable(tvid, varnm) )
  | tystr -> tystr
*)
(* Types.abstract_tree -> (string * type_environment) *)
let main tyenv ast =
  let theta : Subst.t = Subst.empty in
    let type_before_unified = typecheck tyenv ast in
    ( unify_type_variables tyeq theta ;
      let type_after_unfied = subst_type_by_theta theta type_before_unified in
      let strty = string_of_type_struct type_after_unfied in
        match type_after_unfied with
        | TypeEnvironmentType(newtyenv) -> (strty, newtyenv)
        | _ -> (strty, tyenv)
    )

let initialize () = ( tvidmax := 0 )
