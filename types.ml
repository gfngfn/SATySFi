
exception ParseErrorDetail of string
exception TypeCheckError of string


type ctrlseq_name = string
type var_name = string
type id_name = string
type class_name = string
type variant_type_name = string
type constructor_name = string

type token_position = int * int * int
type code_range = int * int * int * int

type type_variable_id = int
type type_environment = (var_name * type_struct) list
and type_struct =
  | TypeEnvironmentType of code_range * type_environment
  | UnitType            of code_range
  | IntType             of code_range
  | StringType          of code_range
  | BoolType            of code_range
  | FuncType            of code_range * type_struct * type_struct
  | ListType            of code_range * type_struct
  | RefType             of code_range * type_struct
  | ProductType         of code_range * (type_struct list)
  | TypeVariable        of code_range * type_variable_id
  | VariantType         of code_range * variant_type_name
  | ForallType          of type_variable_id * type_struct
(*  | TypeWithRestriction of type_variable_id * type_struct
*)

type id_name_arg =
  | IDName of id_name
  | NoIDName
type class_name_arg =
  | ClassName of class_name
  | NoClassName
(* ---- untyped ---- *)
type untyped_argument_variable_cons =
  | UTArgumentVariableCons of code_range * var_name * untyped_argument_variable_cons
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
  | UTApply            of untyped_abstract_tree * untyped_abstract_tree
  | UTListCons         of untyped_abstract_tree * untyped_abstract_tree
  | UTEndOfList
  | UTTupleCons        of untyped_abstract_tree * untyped_abstract_tree
  | UTEndOfTuple
  | UTBreakAndIndent
  | UTLetIn            of untyped_mutual_let_cons * untyped_abstract_tree
  | UTIfThenElse       of untyped_abstract_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTLambdaAbstract   of code_range * var_name * untyped_abstract_tree
  | UTFinishHeaderFile
  | UTLetMutableIn     of code_range * var_name * untyped_abstract_tree * untyped_abstract_tree
  | UTDeclareGlobalHash   of untyped_abstract_tree * untyped_abstract_tree
  | UTOverwriteGlobalHash of untyped_abstract_tree * untyped_abstract_tree
  | UTOverwrite        of code_range * var_name * untyped_abstract_tree
  | UTSequential       of untyped_abstract_tree * untyped_abstract_tree
  | UTReferenceFinal   of untyped_abstract_tree
  | UTIfClassIsValid   of untyped_abstract_tree * untyped_abstract_tree
  | UTIfIDIsValid      of untyped_abstract_tree * untyped_abstract_tree
  | UTApplyClassAndID  of untyped_abstract_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTWhileDo          of untyped_abstract_tree * untyped_abstract_tree
  | UTPatternMatch     of untyped_abstract_tree * untyped_pattern_match_cons
  | UTDeclareVariantIn of untyped_mutual_variant_cons * untyped_abstract_tree
  | UTConstructor      of constructor_name * untyped_abstract_tree
  | UTNoContent
and untyped_variant_cons = code_range * untyped_variant_cons_main
and untyped_variant_cons_main =
  | UTVariantCons      of constructor_name * type_struct * untyped_variant_cons
  | UTEndOfVariant
and untyped_mutual_variant_cons =
  | UTMutualVariantCons of variant_type_name * untyped_variant_cons * untyped_mutual_variant_cons
  | UTEndOfMutualVariant
and untyped_pattern_tree = code_range * untyped_pattern_tree_main
and untyped_pattern_tree_main =
  | UTPNumericConstant of int
  | UTPBooleanConstant of bool
  | UTPStringConstant  of untyped_abstract_tree
  | UTPUnitConstant
  | UTPListCons        of untyped_pattern_tree * untyped_pattern_tree
  | UTPEndOfList
  | UTPTupleCons       of untyped_pattern_tree * untyped_pattern_tree
  | UTPEndOfTuple
  | UTPWildCard
  | UTPVariable        of var_name
  | UTPAsVariable      of var_name * untyped_pattern_tree
  | UTPConstructor     of constructor_name * untyped_pattern_tree
and untyped_pattern_match_cons = code_range * untyped_pattern_match_cons_main
and untyped_pattern_match_cons_main =
  | UTPatternMatchCons     of untyped_pattern_tree * untyped_abstract_tree * untyped_pattern_match_cons
  | UTPatternMatchConsWhen of untyped_pattern_tree * untyped_abstract_tree * untyped_abstract_tree * untyped_pattern_match_cons
  | UTEndOfPatternMatch

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
  | TupleCons       of abstract_tree * abstract_tree
  | EndOfTuple
  | BreakAndIndent
  | LetIn          of mutual_let_cons * abstract_tree
  | IfThenElse     of abstract_tree * abstract_tree * abstract_tree
  | LambdaAbstract of var_name * abstract_tree
  | PatternMatch   of abstract_tree * pattern_match_cons
  | FinishHeaderFile
  | LetMutableIn   of var_name * abstract_tree * abstract_tree
  | Sequential     of abstract_tree * abstract_tree
  | Overwrite      of var_name * abstract_tree
  | MutableValue   of abstract_tree
  | Reference      of abstract_tree
  | ReferenceFinal of abstract_tree
  | IfClassIsValid of abstract_tree * abstract_tree
  | IfIDIsValid    of abstract_tree * abstract_tree
  | ApplyClassAndID of abstract_tree * abstract_tree * abstract_tree
  | WhileDo        of abstract_tree * abstract_tree
  | DeclareGlobalHash   of abstract_tree * abstract_tree
  | OverwriteGlobalHash of abstract_tree * abstract_tree
  | Constructor    of constructor_name * abstract_tree
(* only for inner procedure *)
  | NoContent (* for class and id *)
  | FuncWithEnvironment  of var_name * abstract_tree * environment
  | EvaluatedEnvironment of environment
  | DeeperIndent of abstract_tree
  | Times       of abstract_tree * abstract_tree
  | Divides     of abstract_tree * abstract_tree
  | Mod         of abstract_tree * abstract_tree
  | Plus        of abstract_tree * abstract_tree
  | Minus       of abstract_tree * abstract_tree
  | GreaterThan of abstract_tree * abstract_tree
  | LessThan    of abstract_tree * abstract_tree
  | EqualTo     of abstract_tree * abstract_tree
  | LogicalAnd  of abstract_tree * abstract_tree
  | LogicalOr   of abstract_tree * abstract_tree
  | LogicalNot  of abstract_tree
  | PrimitiveSame of abstract_tree * abstract_tree
  | PrimitiveStringSub of abstract_tree * abstract_tree * abstract_tree
  | PrimitiveStringLength of abstract_tree
  | PrimitiveInclude  of abstract_tree
  | PrimitiveArabic   of abstract_tree
  | PrimitiveListHead of abstract_tree
  | PrimitiveListTail of abstract_tree
  | PrimitiveIsEmpty  of abstract_tree
and pattern_match_cons =
  | PatternMatchCons     of pattern_tree * abstract_tree * pattern_match_cons
  | PatternMatchConsWhen of pattern_tree * abstract_tree * abstract_tree * pattern_match_cons
  | EndOfPatternMatch
and pattern_tree =
  | PNumericConstant of int
  | PBooleanConstant of bool
  | PStringConstant  of abstract_tree
  | PUnitConstant
  | PListCons        of pattern_tree * pattern_tree
  | PEndOfList
  | PTupleCons       of pattern_tree * pattern_tree
  | PEndOfTuple
  | PWildCard
  | PVariable        of var_name
  | PAsVariable      of var_name * pattern_tree
  | PConstructor     of constructor_name * pattern_tree


(* !!!! ---- global variable ---- !!!! *)
let global_hash_env : environment = Hashtbl.create 32

(* for parser *)
let untyped_finish : untyped_abstract_tree = ((-1, 0, 0, 0), UTFinishHeaderFile)

(* untyped_abstract_tree -> code_range *)
let get_range utast =
  let (rng, _) = utast in rng

let is_invalid_range rng =
  let (sttln, _, _, _) = rng in sttln <= 0


let print_for_debug msg =
(* enable below to see the process of type inference *)
(*
  print_string msg ;
*)
  ()
