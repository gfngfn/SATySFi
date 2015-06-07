type ctrlseq_name = string
type var_name = string
type id_name = string
type class_name = string

type id_name_arg =
  | IDName of id_name
  | NoIDName
type class_name_arg =
  | ClassName of class_name
  | NoClassName
type argument_variable_cons =
  | ArgumentVariableCons of var_name * argument_variable_cons
  | EndOfArgumentVariable
type argument_cons =
  | ArgumentCons of abstract_tree * argument_cons
  | EndOfArgument
and environment = (string, location) Hashtbl.t
and location = abstract_tree ref
and abstract_tree =
(* for parser *)
  | NumericEmpty
  | StringEmpty
  | NumericConstant of int
  | BooleanConstant of bool
  | StringConstant of string
  | ContentOf of var_name
  | Concat of abstract_tree * abstract_tree
  | ConcatOperation of abstract_tree * abstract_tree
  | NumericApply of abstract_tree * abstract_tree
(*
  | StringApply of ctrlseq_name * class_name_arg * id_name_arg * argument_cons
*)
  | Separated of abstract_tree * abstract_tree
  | BreakAndIndent
  | DeeperIndent of abstract_tree
  | Times of abstract_tree * abstract_tree
  | Divides of abstract_tree * abstract_tree
  | Mod of abstract_tree * abstract_tree
  | Plus of abstract_tree * abstract_tree
  | Minus of abstract_tree * abstract_tree
  | GreaterThan of abstract_tree * abstract_tree
  | LessThan of abstract_tree * abstract_tree
  | EqualTo of abstract_tree * abstract_tree
  | LogicalAnd of abstract_tree * abstract_tree
  | LogicalOr of abstract_tree * abstract_tree
  | LogicalNot of abstract_tree
  | LetIn of var_name * abstract_tree * abstract_tree
  | IfThenElse of abstract_tree * abstract_tree * abstract_tree
  | LambdaAbstract of var_name * abstract_tree
  | LiteralArea of abstract_tree
(* for inner procedure *)
  | FuncWithEnvironment of var_name * abstract_tree * environment
  | NoContent (* for @class and @id *)
  | UnderConstruction (* for 'compensate' *)
  | Invalid
  | FinishHeaderFile
  | PrimitiveSame of abstract_tree * abstract_tree
  | PrimitiveInclude of abstract_tree
  | PrimitiveArabic of abstract_tree


type type_variable_id = int
type type_struct =
  | UnitType
  | IntType
  | StringType
  | BoolType
  | FuncType of type_struct * type_struct
  | TypeVariable of type_variable_id

type type_environment = (var_name, type_struct) Hashtbl.t
type type_equation = ((type_struct * type_struct) Stacklist.t) ref
