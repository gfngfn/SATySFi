
exception ParseErrorDetail of string
exception TypeCheckError of string

type ctrlseq_name = string
type var_name = string
type id_name = string
type class_name = string
type type_name = string
type constructor_name = string
type module_name = string

type token_position = int * int * int
type code_range = int * int * int * int

type type_variable_id = int
and type_struct =
  | UnitType     of code_range
  | IntType      of code_range
  | StringType   of code_range
  | BoolType     of code_range
  | FuncType     of code_range * type_struct * type_struct
  | ListType     of code_range * type_struct
  | RefType      of code_range * type_struct
  | ProductType  of code_range * (type_struct list)
  | TypeVariable of code_range * type_variable_id
  | TypeSynonym  of code_range * type_name * type_struct
  | VariantType  of code_range * type_name
  | ForallType   of type_variable_id * type_struct

type id_name_arg =
  | IDName       of id_name
  | NoIDName

type class_name_arg =
  | ClassName    of class_name
  | NoClassName

(* ---- untyped ---- *)
type untyped_argument_variable_cons =
  | UTArgumentVariableCons of code_range * var_name * untyped_argument_variable_cons
  | UTEndOfArgumentVariable

type untyped_argument_cons =
  | UTArgumentCons         of untyped_abstract_tree * untyped_argument_cons
  | UTEndOfArgument
and untyped_mutual_let_cons =
  | UTMutualLetCons        of var_name * untyped_abstract_tree * untyped_mutual_let_cons
  | UTEndOfMutualLet
and untyped_abstract_tree = code_range * untyped_abstract_tree_main
and untyped_abstract_tree_main =
(* -- basic value -- *)
  | UTStringEmpty
  | UTNumericConstant      of int
  | UTBooleanConstant      of bool
  | UTStringConstant       of string
  | UTUnitConstant
  | UTConcat               of untyped_abstract_tree * untyped_abstract_tree
  | UTBreakAndIndent
  | UTNoContent
(* -- list value -- *)
  | UTListCons             of untyped_abstract_tree * untyped_abstract_tree
  | UTEndOfList
(* -- tuple value -- *)
  | UTTupleCons            of untyped_abstract_tree * untyped_abstract_tree
  | UTEndOfTuple
(* -- fundamental -- *)
  | UTContentOf            of var_name
  | UTApply                of untyped_abstract_tree * untyped_abstract_tree
  | UTLetIn                of untyped_mutual_let_cons * untyped_abstract_tree
  | UTIfThenElse           of untyped_abstract_tree * untyped_abstract_tree * untyped_abstract_tree
  | UTLambdaAbstract       of code_range * var_name * untyped_abstract_tree
  | UTFinishHeaderFile
(* -- pattern match -- *)
  | UTPatternMatch         of untyped_abstract_tree * untyped_pattern_match_cons
  | UTConstructor          of constructor_name * untyped_abstract_tree
(* -- declaration of type and module -- *)
  | UTDeclareTypeSynonymIn of type_name * type_struct * untyped_abstract_tree
  | UTDeclareVariantIn     of untyped_mutual_variant_cons * untyped_abstract_tree
  | UTModule               of module_name * untyped_module_tree * untyped_abstract_tree
(* -- implerative -- *)
  | UTLetMutableIn         of code_range * var_name * untyped_abstract_tree * untyped_abstract_tree
  | UTSequential           of untyped_abstract_tree * untyped_abstract_tree
  | UTWhileDo              of untyped_abstract_tree * untyped_abstract_tree
  | UTDeclareGlobalHash    of untyped_abstract_tree * untyped_abstract_tree
  | UTOverwriteGlobalHash  of untyped_abstract_tree * untyped_abstract_tree
  | UTOverwrite            of code_range * var_name * untyped_abstract_tree
  | UTReferenceFinal       of untyped_abstract_tree
  | UTLazyContent          of untyped_abstract_tree
(* -- class and id option -- *)
  | UTIfClassIsValid       of untyped_abstract_tree * untyped_abstract_tree
  | UTIfIDIsValid          of untyped_abstract_tree * untyped_abstract_tree
  | UTApplyClassAndID      of untyped_abstract_tree * untyped_abstract_tree * untyped_abstract_tree
and untyped_variant_cons = code_range * untyped_variant_cons_main
and untyped_variant_cons_main =
  | UTVariantCons          of constructor_name * type_struct * untyped_variant_cons
  | UTEndOfVariant
and untyped_mutual_variant_cons =
  | UTMutualVariantCons    of type_name * untyped_variant_cons * untyped_mutual_variant_cons
  | UTEndOfMutualVariant
and untyped_pattern_tree = code_range * untyped_pattern_tree_main
and untyped_pattern_tree_main =
  | UTPNumericConstant     of int
  | UTPBooleanConstant     of bool
  | UTPStringConstant      of untyped_abstract_tree
  | UTPUnitConstant
  | UTPListCons            of untyped_pattern_tree * untyped_pattern_tree
  | UTPEndOfList
  | UTPTupleCons           of untyped_pattern_tree * untyped_pattern_tree
  | UTPEndOfTuple
  | UTPWildCard
  | UTPVariable            of var_name
  | UTPAsVariable          of var_name * untyped_pattern_tree
  | UTPConstructor         of constructor_name * untyped_pattern_tree
and untyped_pattern_match_cons = code_range * untyped_pattern_match_cons_main
and untyped_pattern_match_cons_main =
  | UTPatternMatchCons     of untyped_pattern_tree * untyped_abstract_tree * untyped_pattern_match_cons
  | UTPatternMatchConsWhen of untyped_pattern_tree * untyped_abstract_tree * untyped_abstract_tree * untyped_pattern_match_cons
  | UTEndOfPatternMatch
and untyped_module_tree = code_range * untyped_module_tree_main
and untyped_module_tree_main =
  | UTMFinishModule
  | UTMPublicLetIn                 of untyped_mutual_let_cons * untyped_module_tree
  | UTMPublicLetMutableIn          of code_range * var_name * untyped_abstract_tree * untyped_module_tree
  | UTMPublicDeclareTypeSynonymIn  of type_name * type_struct * untyped_module_tree
  | UTMPublicDeclareVariantIn      of untyped_mutual_variant_cons * untyped_module_tree
  | UTMPrivateLetIn                of untyped_mutual_let_cons * untyped_module_tree
  | UTMPrivateLetMutableIn         of code_range * var_name * untyped_abstract_tree * untyped_module_tree
  | UTMPrivateDeclareTypeSynonymIn of type_name * type_struct * untyped_module_tree
  | UTMPrivateDeclareVariantIn     of untyped_mutual_variant_cons * untyped_module_tree
  | UTMDirectLetIn                 of untyped_mutual_let_cons * untyped_module_tree

(* ---- typed ---- *)
type argument_variable_cons =
  | ArgumentVariableCons  of var_name * argument_variable_cons
  | EndOfArgumentVariable
type argument_cons =
  | ArgumentCons          of abstract_tree * argument_cons
  | EndOfArgument
and mutual_let_cons =
  | MutualLetCons         of var_name * abstract_tree * mutual_let_cons
  | EndOfMutualLet
and environment = (var_name, location) Hashtbl.t
and location = abstract_tree ref
and abstract_tree =
(* -- basic value -- *)
  | StringEmpty
  | NumericConstant       of int
  | BooleanConstant       of bool
  | StringConstant        of string
  | UnitConstant
  | DeeperIndent          of abstract_tree
  | BreakAndIndent
  | Concat                of abstract_tree * abstract_tree
  | NoContent (* for class and id *)
  | FuncWithEnvironment   of var_name * abstract_tree * environment
  | EvaluatedEnvironment  of environment
(* -- list value -- *)
  | ListCons              of abstract_tree * abstract_tree
  | EndOfList
(* -- tuple value -- *)
  | TupleCons             of abstract_tree * abstract_tree
  | EndOfTuple
(* -- fundamental -- *)
  | LetIn                 of mutual_let_cons * abstract_tree
  | ContentOf             of var_name
  | IfThenElse            of abstract_tree * abstract_tree * abstract_tree
  | LambdaAbstract        of var_name * abstract_tree
  | Apply                 of abstract_tree * abstract_tree
  | FinishHeaderFile
(* -- pattern match -- *)
  | PatternMatch          of abstract_tree * pattern_match_cons
  | Constructor           of constructor_name * abstract_tree
(* -- imperative -- *)
  | LetMutableIn          of var_name * abstract_tree * abstract_tree
  | Sequential            of abstract_tree * abstract_tree
  | WhileDo               of abstract_tree * abstract_tree
  | Overwrite             of var_name * abstract_tree
  | MutableValue          of abstract_tree
  | Reference             of abstract_tree
  | DeclareGlobalHash     of abstract_tree * abstract_tree
  | OverwriteGlobalHash   of abstract_tree * abstract_tree
  | ReferenceFinal        of abstract_tree
  | LazyContent           of abstract_tree
  | LazyContentWithEnvironmentRef of abstract_tree * (environment ref)
(* -- class and id option -- *)
  | IfClassIsValid        of abstract_tree * abstract_tree
  | IfIDIsValid           of abstract_tree * abstract_tree
  | ApplyClassAndID       of abstract_tree * abstract_tree * abstract_tree
(* -- primitive operation -- *)
  | Times                 of abstract_tree * abstract_tree
  | Divides               of abstract_tree * abstract_tree
  | Mod                   of abstract_tree * abstract_tree
  | Plus                  of abstract_tree * abstract_tree
  | Minus                 of abstract_tree * abstract_tree
  | GreaterThan           of abstract_tree * abstract_tree
  | LessThan              of abstract_tree * abstract_tree
  | EqualTo               of abstract_tree * abstract_tree
  | LogicalAnd            of abstract_tree * abstract_tree
  | LogicalOr             of abstract_tree * abstract_tree
  | LogicalNot            of abstract_tree
  | PrimitiveSame         of abstract_tree * abstract_tree
  | PrimitiveStringSub    of abstract_tree * abstract_tree * abstract_tree
  | PrimitiveStringLength of abstract_tree
(*  | PrimitiveInclude      of abstract_tree *)
  | PrimitiveArabic       of abstract_tree
  | Module                of module_name * module_tree * abstract_tree
and pattern_match_cons =
  | PatternMatchCons      of pattern_tree * abstract_tree * pattern_match_cons
  | PatternMatchConsWhen  of pattern_tree * abstract_tree * abstract_tree * pattern_match_cons
  | EndOfPatternMatch
and pattern_tree =
  | PNumericConstant      of int
  | PBooleanConstant      of bool
  | PStringConstant       of abstract_tree
  | PUnitConstant
  | PListCons             of pattern_tree * pattern_tree
  | PEndOfList
  | PTupleCons            of pattern_tree * pattern_tree
  | PEndOfTuple
  | PWildCard
  | PVariable             of var_name
  | PAsVariable           of var_name * pattern_tree
  | PConstructor          of constructor_name * pattern_tree
and module_tree =
  | MFinishModule
  | MPublicLetIn                 of mutual_let_cons * module_tree
  | MPublicLetMutableIn          of var_name * abstract_tree * module_tree
(*  | MPublicDeclareTypeSynonymIn  of type_name * type_struct * module_tree *)
(*  | MPublicDeclareVariantIn      of mutual_variant_cons * module_tree *)
  | MPrivateLetIn                of mutual_let_cons * module_tree
  | MPrivateLetMutableIn         of var_name * abstract_tree * module_tree
(*  | MPrivateDeclareTypeSynonymIn of type_name * type_struct * module_tree *)
(*  | MPrivateDeclareVariantIn     of mutual_variant_cons * module_tree *)
  | MDirectLetIn                 of mutual_let_cons * module_tree


(* !!!! ---- global variable ---- !!!! *)
let global_hash_env : environment = Hashtbl.create 32

(* for parser *)
let end_header : untyped_abstract_tree = ((-1, 0, 0, 0), UTFinishHeaderFile)
let end_struct : untyped_module_tree = ((-1, 0, 0, 0), UTMFinishModule)

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
