open Types

exception TypeCheckError of string

type type_variable_id = int
type type_struct =
  | IntType
  | StringType
  | BoolType
  | FuncType of type_struct * type_struct
  | TypeVariable of type_variable_id

type type_environment = (var_name, type_struct) Hashtbl.t
type type_equation = ((type_struct * type_struct) Stacklist.t) ref

let tvidmax : type_variable_id ref = ref 0
let new_type_variable () = 
  let res = TypeVariable(!tvidmax) in ( tvidmax := !tvidmax + 1 ; res )

let rec equivalent tya tyb =
  match (tya, tyb) with
  | (IntType, IntType)       -> true
  | (StringType, StringType) -> true
  | (BoolType, BoolType)     -> true
  | (FuncType(tyadom, tyacod), FuncType(tybdom, tybcod))
      -> (equivalent tyadom tybdom) && (equivalent tyacod tybcod)
  | _ -> false

(* type_environment -> Types.abstract_tree -> type_struct *)
let rec typecheck tyeq tyenv abstr =
  match abstr with
  | NumericEmpty -> IntType
  | StringEmpty -> StringType
  | NumericConstant(_) -> IntType
  | StringConstant(_) -> StringType

  | NumericContentOf(nv) -> 
    ( try Hashtbl.find tyenv nv with
      | Not_found -> raise (TypeCheckError("undefined variable '" ^ nv ^ "'"))
    )

  | StringContentOf(sv) ->
    ( try Hashtbl.find tyenv sv with
      | Not_found -> raise (TypeCheckError("undefined variable '" ^ sv ^ "'"))
    )

  | Concat(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
        if equivalent StringType tyf then
          if equivalent StringType tyl then StringType
          else raise (TypeCheckError("right operand of '^' is not of type string"))
        else raise (TypeCheckError("left openrand of '^' is not of type string"))

  | NumericApply(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( match tyf with
        | FuncType(tydom, tycod) ->
            ( ( if equivalent tydom tyl then () else Stacklist.push tyeq (tydom, tyl) ) ;
              tycod
            )
        | _ -> let ntycod = new_type_variable () in
            ( Stacklist.push tyeq (FuncType(tyl, ntycod), tyf) ;
              ntycod
            )
      )
(*
  | StringApply(cs, clsnm, idnm, argcons) ->
      let tycs = typecheck tyeq tyenv_ctrlseq cs in
        match tycs with
        | FuncType(tydom, tycod) ->
*)

  | BreakAndIndent -> StringType
  | DeeperIndent(astf) ->
      let tyf = typecheck tyeq tyenv astf in
      ( ( if equivalent StringType tyf then () else Stacklist.push tyeq (StringType, tyf) ) ;
        StringType
      )

  | Times(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (BoolType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (BoolType, tyl) ) ;
        IntType
      )

  | Divides(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (BoolType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (BoolType, tyl) ) ;
        IntType
      )

  | Mod(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (BoolType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (BoolType, tyl) ) ;
        IntType
      )

  | Plus(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (BoolType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (BoolType, tyl) ) ;
        IntType
      )

  | Minus(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (BoolType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (BoolType, tyl) ) ;
        IntType
      )

  | GreaterThan(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (BoolType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (BoolType, tyl) ) ;
        BoolType
      )

  | LessThan(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent IntType tyf then () else Stacklist.push tyeq (BoolType, tyf) ) ;
        ( if equivalent IntType tyl then () else Stacklist.push tyeq (BoolType, tyl) ) ;
        BoolType
      )

  | EqualTo(astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent tyf tyl then () else Stacklist.push tyeq (tyf, tyl) ) ;
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

  | LetNumIn(nv, astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyenv_new = Hashtbl.copy tyenv in
      ( Hashtbl.add tyenv_new nv tyf ;
        let tyl = typecheck tyeq tyenv_new astl in
        ( Hashtbl.clear tyenv_new ; tyl )
      )

  | LetStrIn(sv, astf, astl) ->
      let tyf = typecheck tyeq tyenv astf in
      let tyenv_new = Hashtbl.copy tyenv in
      ( Hashtbl.add tyenv_new sv tyf ;
        let tyl = typecheck tyeq tyenv_new astl in
        ( Hashtbl.clear tyenv_new ;
          ( if (equivalent StringType tyf) then () else Stacklist.push tyeq (StringType, tyf) ) ;
          tyl
        )
      )

  | IfThenElse(astb, astf, astl) ->
      let tyb = typecheck tyeq tyenv astb in
      let tyf = typecheck tyeq tyenv astf in
      let tyl = typecheck tyeq tyenv astl in
      ( ( if equivalent BoolType tyb then () else Stacklist.push tyeq (BoolType, tyb) ) ;
        ( if equivalent tyf tyl then () else Stacklist.push tyeq (tyf, tyl) ) ;
        BoolType
      )

  | LambdaAbstract(argvarcons, astf) ->
      assign_lambda_abstract_type tyeq tyenv argvarcons astf

  | _ -> raise (TypeCheckError("remains to be implemented"))

(* type_equation -> argument_variable_cons -> abstract_tree -> type_struct *)
and assign_lambda_abstract_type tyeq tyenv argvarcons astf =
  match argvarcons with
  | EndOfArgumentVariable -> typecheck tyeq tyenv astf
  | ArgumentVariableCons(av, avcsub) ->
      let ntv = new_type_variable () in 
      ( Hashtbl.add tyenv av ntv ;
        FuncType(ntv, assign_lambda_abstract_type tyeq tyenv avcsub astf)
      )

(* type_variable_id -> type_struct -> bool *)
let rec emerge_in tyid tystr =
  match tystr with
  | TypeVariable(tyidsub) -> tyid == tyidsub
  | FuncType(tydom, tycod) -> (emerge_in tyid tydom) || (emerge_in tyid tycod)
  | _ -> false

(* (type_struct * type_struct) -> ((type_variable_id, type_struct) Hashtbl.t) -> unit *)
let rec unify_type_variables_sub tyeqlst theta =
  match tyeqlst with
  | [] -> ()
  | (FuncType(tyadom, tyacod), FuncType(tybdom, tybcod)) :: tail ->
      unify_type_variables_sub ((tyadom, tybdom) :: (tyacod, tybcod) :: tail) theta

  | (TypeVariable(tvidx), TypeVariable(tvidy)) :: tail ->
      ( if tvidx == tvidy then
          ()
        else
        ( try 
            let tystrofx = Hashtbl.find theta tvidx in
            ( try
                let tystrofy = Hashtbl.find theta tvidy in
                  (* if both tvidx and tvidy are found *)
                  unify_type_variables_sub ((tystrofx, tystrofy) :: tail) theta
              with
              | Not_found -> (* if tvidx is found but tvidy is not *)
                  Hashtbl.add theta tvidy tystrofx
            )
          with
          | Not_found ->
            ( try
                let tystrofy = Hashtbl.find theta tvidy in
                  (* if tvidx is not found but tvidy is *)
                  Hashtbl.add theta tvidx tystrofy
              with
              | Not_found -> (* if neither tvidx nor tvidy is found *)
                let ntv = new_type_variable () in
                ( Hashtbl.add theta tvidy ntv ;
                  Hashtbl.add theta tvidx ntv
                )
            )
        )
      )
  | (tystr, TypeVariable(tvid)) :: tail ->
      ( if emerge_in tvid tystr then
          raise (TypeCheckError("error 1"))
        else
        ( Hashtbl.add theta tvid IntType ; unify_type_variables_sub tail theta )
      )
  | (TypeVariable(tvid), tystr) :: tail ->
      ( if emerge_in tvid tystr then
          raise (TypeCheckError("error 2"))
        else
        ( Hashtbl.add theta tvid IntType ; unify_type_variables_sub tail theta )
      )
  | _ -> raise (TypeCheckError("error 3"))

(* type_equation -> ((type_variable_id, type_struct) Hashtbl.t) -> unit *)
let unify_type_variables tyeq theta =
  let tyeqlst = Stacklist.to_list !tyeq in unify_type_variables_sub tyeqlst theta

let rec apply_unifying theta ty =
  match ty with
  | FuncType(tydom, tycod) -> FuncType(apply_unifying theta tydom, apply_unifying theta tycod)
  | TypeVariable(tvid) -> ( try Hashtbl.find theta tvid with Not_found -> TypeVariable(tvid) )
  | tystr -> tystr

let main abstr =
  let tyeq : type_equation = ref Stacklist.empty in
  let tyenv : type_environment = Hashtbl.create 128 in
  let theta : (type_variable_id, type_struct) Hashtbl.t = Hashtbl.create 128 in
  ( tvidmax := 0 ;
    let type_before_unifying = typecheck tyeq tyenv abstr in
      unify_type_variables tyeq theta ;
      apply_unifying theta type_before_unifying
  )
