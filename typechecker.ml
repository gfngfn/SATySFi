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

(* (type_struct * type_struct) -> type_environment -> Types.abstract_tree -> type_struct *)
let rec typecheck tyeq tyenv astch =
  match astch with
  | StringEmpty -> StringType
  | NumericConstant(_) -> IntType
  | StringConstant(_) -> StringType
  | BooleanConstant(_) -> BoolType
  | LiteralArea(_) -> StringType

  | ContentOf(nv) ->
      ( try
          let ty = Hashtbl.find tyenv nv in
          ( 
              print_process ("  " ^ nv ^ ": <" ^ string_of_type_struct ty ^ ">") ;
            
            ty )
        with
        | Not_found -> raise (TypeCheckError("undefined variable '" ^ nv ^ "'"))
      )
  | ConcatOperation(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent StringType tyf then () else Stacklist.push tyeq (StringType, tyf) ) ;
        ( if equivalent StringType tyl then () else Stacklist.push tyeq (StringType, tyl) ) ;
        StringType
      )
  | Concat(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent StringType tyf then () else Stacklist.push tyeq (StringType, tyf) ) ;
        ( if equivalent StringType tyl then () else Stacklist.push tyeq (StringType, tyl) ) ;
        StringType
      )
  | NumericApply(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( match tyf with
        | FuncType(tydom, tycod) ->
            ( ( if equivalent tydom tyl then () else Stacklist.push tyeq (tydom, tyl) ) ;
              tycod
            )
        | _ -> let ntycod = new_type_variable "*cod" in
            ( Stacklist.push tyeq (tyf, FuncType(tyl, ntycod)) ;
              ntycod
            )
      )
  | BreakAndIndent -> StringType

  | DeeperIndent(astf) ->
      let tyf = typecheck tyeq tyenv astf in
      ( ( if equivalent StringType tyf then () else Stacklist.push tyeq (StringType, tyf) ) ;
        StringType
      )
  | Times(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (IntType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (IntType, tyl) ) ;
        IntType
      )
  | Divides(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (IntType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (IntType, tyl) ) ;
        IntType
      )
  | Mod(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (IntType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (IntType, tyl) ) ;
        IntType
      )
  | Plus(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (IntType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (IntType, tyl) ) ;
        IntType
      )
  | Minus(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (IntType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (IntType, tyl) ) ;
        IntType
      )
  | GreaterThan(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (IntType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (IntType, tyl) ) ;
        BoolType
      )
  | LessThan(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (IntType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (IntType, tyl) ) ;
        BoolType
      )
  | EqualTo(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      (
        ( match (tyf, tyl) with
          | (FuncType(_, _), _) -> raise (TypeCheckError("cannot compare functions using '=='"))
          | (_, FuncType(_, _)) -> raise (TypeCheckError("cannot compare functions using '=='"))
          | (UnitType, _) -> raise (TypeCheckError("cannot compare units using '=='"))
          | (_, UnitType) -> raise (TypeCheckError("cannot compare units using '=='"))
          | _ -> ()
        ) ;
        ( if equivalent tyf tyl then () else Stacklist.push tyeq (tyf, tyl) ) ;
        BoolType
      )
  | LogicalAnd(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent BoolType tyf then () else Stacklist.push tyeq (BoolType, tyf) ) ;
        ( if equivalent BoolType tyl then () else Stacklist.push tyeq (BoolType, tyl) ) ;
        BoolType
      )
  | LogicalOr(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent BoolType tyf then () else Stacklist.push tyeq (BoolType, tyf) ) ;
        ( if equivalent BoolType tyl then () else Stacklist.push tyeq (BoolType, tyl) ) ;
        BoolType
      )
  | LogicalNot(astf) ->
      let tyf = typecheck tyeq tyenv astf in
      ( ( if equivalent BoolType tyf then () else Stacklist.push tyeq (BoolType, tyf) ) ;
        BoolType
      )
  | LetIn(mutletcons, astl) ->
    ( print_process "#LetIn" ;
      let tyenv_new = Hashtbl.copy tyenv in
      ( add_mutual_variables tyenv_new mutletcons ;
        typecheck_mutual_contents tyeq tyenv_new mutletcons ;
          let tyl = typecheck tyeq tyenv_new astl in
            tyl
      )
    )
  | IfThenElse(astb, asttru, astfls) ->
      let tyb = typecheck tyeq tyenv astb in
      let tytru = typecheck tyeq tyenv asttru in
      let tyfls = typecheck tyeq tyenv astfls in
      ( ( if equivalent BoolType tyb then () else Stacklist.push tyeq (BoolType, tyb) ) ;
        ( if equivalent tytru tyfls then () else Stacklist.push tyeq (tytru, tyfls) ) ;
        tytru
      )
  | IfClassIsValid(asttru, astfls) ->
      let tyenv_class = Hashtbl.copy tyenv in
      ( Hashtbl.add tyenv_class "class" StringType ;
        let tytru = typecheck tyeq tyenv_class asttru in
        let tyfls = typecheck tyeq tyenv astfls in
        ( ( if equivalent tytru tyfls then () else Stacklist.push tyeq (tytru, tyfls) ) ;
          tytru
        )
      )
  | IfIDIsValid(asttru, astfls) ->
      let tyenv_id = Hashtbl.copy tyenv in
      ( Hashtbl.add tyenv_id "id" StringType ;
        let tytru = typecheck tyeq tyenv_id asttru in
        let tyfls = typecheck tyeq tyenv astfls in
        ( ( if equivalent tytru tyfls then () else Stacklist.push tyeq (tytru, tyfls) ) ;
          tytru
        )
      )

  | LambdaAbstract(varnm, astdef) ->
    ( print_process ("#LambdaAbstract " ^ varnm ^ ". ...") ;
      let tyvar = new_type_variable varnm in
      let tyenv_new = Hashtbl.copy tyenv in
      ( Hashtbl.add tyenv_new varnm tyvar ;
        let tydef = typecheck tyeq tyenv_new astdef in
          FuncType(tyvar, tydef)
      )
    )
          (* AYASHII! *)
  | ApplyClassAndID(_, _, astf) ->
      typecheck tyeq tyenv astf

  | ListCons(asthd, asttl) ->
      let tyhd = typecheck tyeq tyenv asthd in
      let tytl = typecheck tyeq tyenv asttl in
      ( ( if equivalent (ListType(tyhd)) tytl then () else Stacklist.push tyeq (ListType(tyhd), tytl) ) ;
        ListType(tyhd)
      )
  | EndOfList -> let ntyvar = new_type_variable "*empty" in ListType(ntyvar)

  | LetMutableIn(varnm, astdflt, astaft) ->
      let tydflt = typecheck tyeq tyenv astdflt in
      let tyenv_new = Hashtbl.copy tyenv in
      ( Hashtbl.add tyenv varnm tydflt ;
        let tyaft = typecheck tyeq tyenv_new astaft in tyaft
      )

  | Sequential(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent UnitType tyf then () else Stacklist.push tyeq (UnitType, tyf)) ;
        tyl
      )
  | Overwrite(varnm, astnew) ->
      let _ = typecheck tyeq tyenv astnew in UnitType

  | FinishHeaderFile -> TypeEnvironmentType(tyenv)

  | NoContent -> StringType

  | _ -> raise (TypeCheckError("this cannot happen / remains to be implemented"))


and add_mutual_variables tyenv mutletcons =
  match mutletcons with
  | EndOfMutualLet -> ()
  | MutualLetCons(nv, _, tailcons) ->
      let ntv = new_type_variable nv in
      ( Hashtbl.add tyenv nv ntv ;
        add_mutual_variables tyenv tailcons )

and typecheck_mutual_contents tyeq tyenv mutletcons =
  match mutletcons with
  | EndOfMutualLet -> ()
  | MutualLetCons(nv, astcont, tailcons) ->
      let tycont = typecheck tyeq tyenv astcont in
      let ntv = Hashtbl.find tyenv nv in
      ( Stacklist.push tyeq (ntv, tycont) ;
        typecheck_mutual_contents tyeq tyenv tailcons )

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
    let type_before_unified = typecheck tyeq tyenv ast in
    ( unify_type_variables tyeq theta ;
      let type_after_unfied = subst_type_by_theta theta type_before_unified in
      let strty = string_of_type_struct type_after_unfied in
        match type_after_unfied with
        | TypeEnvironmentType(newtyenv) -> (strty, newtyenv)
        | _ -> (strty, tyenv)
    )

let initialize () = ( tvidmax := 0 )
