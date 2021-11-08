
open Types
open StaticEnv


type type_error =
  | UndefinedVariable                    of Range.t * module_name list * var_name * var_name list
  | UndefinedConstructor                 of Range.t * var_name * var_name list
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
  | MultipleFieldInRecord                of Range.t * label
  | InvalidExpressionAsToStaging         of Range.t * stage
  | InvalidOccurrenceAsToStaging         of Range.t * var_name * stage
  | ApplicationOfNonFunction             of Range.t * mono_type
  | MultiCharacterMathScriptWithoutBrace of Range.t
  | IllegalNumberOfTypeArguments         of Range.t * type_name * int * int
  | ContradictionError                   of mono_type * mono_type
  | InclusionError                       of mono_type * mono_type
  | TypeParameterBoundMoreThanOnce       of Range.t * type_variable_name
  | ConflictInSignature                  of Range.t * string

  | NotAStructureSignature               of Range.t * functor_signature
  | MissingRequiredValueName             of Range.t * var_name * poly_type
  | MissingRequiredTypeName              of Range.t * type_name * int
  | MissingRequiredModuleName            of Range.t * module_name * signature
  | MissingRequiredSignatureName         of Range.t * signature_name * signature abstracted
  | NotASubtypeAboutType                 of Range.t * type_name
  | NotASubtypeSignature                 of Range.t * signature * signature
  | NotASubtypePolymorphicType           of Range.t * var_name * poly_type * poly_type
