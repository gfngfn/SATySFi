exception ParseErrorDetail of string

type ctrlseq_name = string
type var_name = string
type id_name = string
type class_name = string

type token_position = int * int * int
type code_range = int * int * int * int

type id_name_arg =
  | IDName of id_name
  | NoIDName
type class_name_arg =
  | ClassName of class_name
  | NoClassName
(* ---- untyped ---- *)
type untyped_argument_variable_cons =
  | UTArgumentVariableCons of var_name * untyped_argument_variable_cons
  | UTEndOfArgumentVariable
type untyped_argument_cons =
  | UTArgumentCons of untyped_abstract_tree * untyped_argument_cons
  | UTEndOfArgument
and untyped_mutual_let_cons =
  | UTMutualLetCons of var_name * untyped_abstract_tree * untyped_mutual_let_cons
  | UTEndOfMutualLet
and untyped_abstract_tree = code_range * untyped_abstract_tree_main
and untyped_abstract_tree_main =
  | UTStringEmpty
  | UTNumericConstant  of int
  | UTBooleanConstant  of bool
  | UTStringConstant   of string
  | UTUnitConstant
  | UTContentOf        of var_name
  | UTConcat           of untyped_abstract_tree * untyped_abstract_tree
  | UTConcatOperation  of untyped_abstract_tree * untyped_abstract_tree
  | UTApply            of untyped_abstract_tree * untyped_abstract_tree
  | UTListCons         of untyped_abstract_tree * untyped_abstract_tree
  | UTEndOfList
  | UTBreakAndIndent
  | UTLetIn            of untyped_mutual_let_cons * untyped_abstract_tree
  | UTIfThenElse       of untyped_abstract_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTLambdaAbstract   of var_name * untyped_abstract_tree
  | UTFinishHeaderFile
  | UTLetMutableIn     of var_name * untyped_abstract_tree * untyped_abstract_tree
  | UTSequential       of untyped_abstract_tree * untyped_abstract_tree
  | UTOverwrite        of var_name * untyped_abstract_tree
  | UTMutableValue     of untyped_abstract_tree
  | UTReference        of var_name
  | UTReferenceFinal   of var_name
  | UTIfClassIsValid   of untyped_abstract_tree * untyped_abstract_tree
  | UTIfIDIsValid      of untyped_abstract_tree * untyped_abstract_tree
  | UTApplyClassAndID of untyped_abstract_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTWhileDo          of untyped_abstract_tree * untyped_abstract_tree
  | UTNoContent
(* ---- typed ---- *)
type argument_variable_cons =
  | ArgumentVariableCons of var_name * argument_variable_cons
  | EndOfArgumentVariable
type argument_cons =
  | ArgumentCons of abstract_tree * argument_cons
  | EndOfArgument
and mutual_let_cons =
  | MutualLetCons of var_name * abstract_tree * mutual_let_cons
  | EndOfMutualLet
and environment = (var_name, location) Hashtbl.t
and location = abstract_tree ref
and abstract_tree =
(* for syntax *)
  | StringEmpty
  | NumericConstant of int
  | BooleanConstant of bool
  | StringConstant  of string
  | UnitConstant
  | ContentOf       of var_name
  | Concat          of abstract_tree * abstract_tree
  | Apply           of abstract_tree * abstract_tree
  | ListCons        of abstract_tree * abstract_tree
  | EndOfList
  | BreakAndIndent
  | LetIn          of mutual_let_cons * abstract_tree
  | IfThenElse     of abstract_tree * abstract_tree * abstract_tree
  | LambdaAbstract of var_name * abstract_tree
  | FinishHeaderFile
  | LetMutableIn   of var_name * abstract_tree * abstract_tree
  | Sequential     of abstract_tree * abstract_tree
  | Overwrite      of var_name * abstract_tree
  | MutableValue   of abstract_tree
  | Reference      of var_name
  | ReferenceFinal of var_name
  | IfClassIsValid of abstract_tree * abstract_tree
  | IfIDIsValid    of abstract_tree * abstract_tree
  | ApplyClassAndID of abstract_tree * abstract_tree * abstract_tree
  | WhileDo of abstract_tree * abstract_tree
(* only for inner procedure *)
  | NoContent (* for class and id *)
  | FuncWithEnvironment of var_name * abstract_tree * environment
  | EvaluatedEnvironment of environment
  | DeeperIndent of abstract_tree
  | Times   of abstract_tree * abstract_tree
  | Divides of abstract_tree * abstract_tree
  | Mod     of abstract_tree * abstract_tree
  | Plus    of abstract_tree * abstract_tree
  | Minus   of abstract_tree * abstract_tree
  | GreaterThan of abstract_tree * abstract_tree
  | LessThan    of abstract_tree * abstract_tree
  | EqualTo     of abstract_tree * abstract_tree
  | LogicalAnd  of abstract_tree * abstract_tree
  | LogicalOr   of abstract_tree * abstract_tree
  | LogicalNot  of abstract_tree
  | PrimitiveSame of abstract_tree * abstract_tree
  | PrimitiveStringSub of abstract_tree * abstract_tree * abstract_tree
  | PrimitiveStringLength of abstract_tree
  | PrimitiveInclude of abstract_tree
  | PrimitiveArabic of abstract_tree
  | PrimitiveListHead of abstract_tree
  | PrimitiveListTail of abstract_tree
  | PrimitiveIsEmpty  of abstract_tree


type type_variable_id = int
type type_struct =
  | TypeEnvironmentType of type_environment
  | UnitType
  | IntType
  | StringType
  | BoolType
  | FuncType of type_struct * type_struct
  | ListType of type_struct
(*  | PolyType of type_variable_id * type_struct *)
  | TypeVariable of type_variable_id * var_name
and type_environment = (var_name, type_struct) Hashtbl.t
type type_equation = ((type_struct * type_struct) Stacklist.t) ref
