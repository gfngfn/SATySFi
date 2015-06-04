open Types

exception TypeCheckError of string

type type_struct =
  | IntType
  | StringType
  | BoolType
  | FuncType of type_struct * type_struct

let rec equivalent tya tyb =
  match tya with
  | IntType -> ( match tyb with IntType -> true | _ -> false )
  | StringType -> ( match tyb with StringType -> true | _ -> false )
  | BoolType -> ( match tyb with BoolType -> true | _ -> false )
  | FuncType(tyadom, tyacod) ->
      ( match tyb with
        | FuncType(tybdom, tybcod) -> (equivalent tyadom tybdom) && (equivalent tyacod tybcod)
        | _ -> false
      )

let rec typecheck abstr =
  match abstr with
  | NumericEmpty -> IntType
  | StringEmpty -> StringType
  | NumericConstant(_) -> IntType
  | StringConstant(_) -> StringType
  | NumericContentOf(_) -> IntType
  | StringContentOf(_) -> StringType
  | Concat(astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
        if equivalent StringType tyf then
          if equivalent StringType tyl then StringType
          else raise (TypeCheckError("right operand of '^' is not of type string"))
        else raise (TypeCheckError("left openrand of '^' is not of type string"))

  | NumericApply(astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
      ( match tyf with
        | FuncType(tydom, tycod) ->
            if equivalent tydom tyl then tycod else
              raise (TypeCheckError("illegal apply; parameter is of unexpected type"))
        | _ -> raise (TypeCheckError("illegal apply; this is not a function"))
      )
(*
  | StringApply(cs, clsnm, idnm, argcons) ->
      let tycs = typecheck_ctrlseq cs in
        match tycs with
        | FuncType(tydom, tycod) ->
*)

  | BreakAndIndent -> StringType
  | DeeperIndent(astf) ->
      let tyf = typecheck astf in
        if equivalent StringType tyf then StringType
        else raise (TypeCheckError("illegal '\\deeper'"))
  | Times(astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
        if equivalent IntType tyf then
          if equivalent IntType tyl then IntType
          else raise (TypeCheckError("right operand of '+' is not of type int"))
        else raise (TypeCheckError("left operand of '+' is not of type int"))

  | Divides(astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
      if equivalent IntType tyf then
        if equivalent IntType tyl then IntType
        else raise (TypeCheckError("right operand of '/' is not of type int"))
      else raise (TypeCheckError("left operand of '/' is not of type int"))

  | Mod(astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
      if equivalent IntType tyf then
        if equivalent IntType tyl then IntType
        else raise (TypeCheckError("right operand of 'mod' is not of type int"))
      else raise (TypeCheckError("left operand of 'mod' is not of type int"))

  | Plus(astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
      if equivalent IntType tyf then
        if equivalent IntType tyl then IntType
        else raise (TypeCheckError("right operand of '+' is not of type int"))
      else raise (TypeCheckError("left operand of '+' is not of type int"))

  | Minus(astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
      if equivalent IntType tyf then
        if equivalent IntType tyl then IntType
        else raise (TypeCheckError("right operand of '-' is not of type int"))
      else raise (TypeCheckError("left operand of '-' is not of type int"))

  | GreaterThan(astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
      if equivalent IntType tyf then
        if equivalent IntType tyl then BoolType
        else raise (TypeCheckError("right operand of '>' is not of type int"))
      else raise (TypeCheckError("left operand of '>' is not of type int"))

  | LessThan(astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
      if equivalent IntType tyf then
        if equivalent IntType tyl then BoolType
        else raise (TypeCheckError("right operand of '<' is not of type int"))
      else raise (TypeCheckError("left operand of '<' is not of type int"))

  | EqualTo(astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
      if equivalent IntType tyf then
        if equivalent IntType tyl then BoolType
        else raise (TypeCheckError("right operand of '==' is not of type int"))
      else raise (TypeCheckError("left operand of '==' is not of type int"))

  | LogicalAnd(astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
      if equivalent BoolType tyf then
        if equivalent BoolType tyl then BoolType
        else raise (TypeCheckError("right operand of 'land' is not of type bool"))
      else raise (TypeCheckError("left operand of 'land' is not of type bool"))

  | LogicalOr(astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
      if equivalent BoolType tyf then
        if equivalent BoolType tyl then BoolType
        else raise (TypeCheckError("right operand of 'lor' is not of type bool"))
      else raise (TypeCheckError("left operand of 'lor' is not of type bool"))

  | LogicalNot(astf) ->
      let tyf = typecheck astf in
      if equivalent BoolType tyf then BoolType
      else raise (TypeCheckError("operand of 'not' is not of type bool"))

  | LetNumIn(_, astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
        if equivalent tyf tyl then tyl
        else raise (TypeCheckError("illegal 'let' expression"))

  | LetStrIn(_, astf, astl) ->
      let tyf = typecheck astf in
      let tyl = typecheck astl in
        if (equivalent StringType tyf) && (equivalent StringType tyl) then StringType
        else raise (TypeCheckError("illegal 'let' expression"))

  | IfThenElse(astb, astf, astl) ->
      let tyb = typecheck astb in
      let tyf = typecheck astf in
      let tyl = typecheck astl in
        if equivalent BoolType tyb then
          if equivalent tyf tyl then tyf
          else raise (TypeCheckError("illegal 'if' expression"))
        else raise (TypeCheckError("'illegal 'if' expression"))

  | _ -> raise (TypeCheckError("remains to be implemented"))
