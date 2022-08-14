
open Types
open StaticEnv


type type_error =
  | UndefinedVariable                    of Range.t * var_name * var_name list
  | UndefinedConstructor                 of Range.t * constructor_name * constructor_name list
  | UndefinedTypeName                    of Range.t * type_name
  | UndefinedTypeVariable                of Range.t * type_variable_name
  | UndefinedKindName                    of Range.t * kind_name
  | UndefinedModuleName                  of Range.t * module_name
  | UndefinedSignatureName               of Range.t * signature_name
  | UndefinedHorzMacro                   of Range.t * ctrlseq_name
  | UndefinedVertMacro                   of Range.t * ctrlseq_name
  | InvalidNumberOfMacroArguments        of Range.t * macro_parameter_type list
  | LateMacroArgumentExpected            of Range.t * mono_type
  | EarlyMacroArgumentExpected           of Range.t * mono_type
  | UnknownUnitOfLength                  of Range.t * length_unit_name
  | HorzCommandInMath                    of Range.t
  | MathCommandInHorz                    of Range.t
  | BreaksValueRestriction               of Range.t
  | MultiplePatternVariable              of Range.t * Range.t * var_name
  | LabelUsedMoreThanOnce                of Range.t * label
  | InvalidExpressionAsToStaging         of Range.t * stage
  | InvalidOccurrenceAsToStaging         of Range.t * var_name * stage
  | ApplicationOfNonFunction             of Range.t * mono_type
  | MultiCharacterMathScriptWithoutBrace of Range.t
  | IllegalNumberOfTypeArguments         of Range.t * type_name * int * int
  | ContradictionError                   of mono_type * mono_type
  | InclusionError                       of mono_type * mono_type
  | TypeParameterBoundMoreThanOnce       of Range.t * type_variable_name
  | ConflictInSignature                  of Range.t * string
  | NotAStructureSignature               of Range.t * EvalVarID.t functor_signature
  | NotAFunctorSignature                 of Range.t * EvalVarID.t StructSig.t
  | MissingRequiredValueName             of Range.t * var_name * poly_type
  | MissingRequiredConstructorName       of Range.t * constructor_name * constructor_entry
  | MissingRequiredTypeName              of Range.t * type_name * int
  | MissingRequiredModuleName            of Range.t * module_name * EvalVarID.t signature
  | MissingRequiredSignatureName         of Range.t * signature_name * virtual_signature abstracted
  | NotASubtypeAboutValue                of Range.t * var_name * poly_type * poly_type
  | NotASubtypeAboutConstructor          of Range.t * constructor_name * type_scheme * type_scheme
  | NotASubtypeAboutType                 of Range.t * type_name * type_entry * type_entry
  | NotASubtypeSignature                 of Range.t * virtual_signature * virtual_signature
  | UnexpectedOptionalLabel              of Range.t * label * mono_type
  | InvalidArityOfCommandApplication     of Range.t * int * int
  | CannotRestrictTransparentType        of Range.t * type_name
  | KindContradiction                    of Range.t * type_name * kind * kind
  | CyclicSynonymTypeDefinition          of (type_name * SynonymDependencyGraph.data) cycle
