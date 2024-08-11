
open Types
open StaticEnv
open SyntaxBase


type unification_error =
  | TypeContradiction                       of mono_type * mono_type
  | TypeVariableInclusion                   of FreeID.t * mono_type
  | RowContradiction                        of mono_row * mono_row
  | RowVariableInclusion                    of FreeRowID.t * mono_row
  | CommandArityMismatch                    of int * int
  | CommandOptionalLabelMismatch            of label
  | BreaksRowDisjointness                   of label
  | BreaksLabelMembershipByFreeRowVariable  of FreeRowID.t * label * LabelSet.t
  | BreaksLabelMembershipByBoundRowVariable of MustBeBoundRowID.t * label
  | BreaksLabelMembershipByEmptyRow         of label
  | InsufficientRowVariableConstraint       of MustBeBoundRowID.t * LabelSet.t * LabelSet.t

type type_error =
  | UndefinedVariable                    of Range.t * var_name * var_name list
  | UndefinedConstructor                 of Range.t * constructor_name * constructor_name list
  | UndefinedTypeName                    of Range.t * type_name
  | UndefinedTypeVariable                of Range.t * type_variable_name
  | UndefinedRowVariable                 of Range.t * row_variable_name
  | UndefinedKindName                    of Range.t * kind_name
  | UndefinedModuleName                  of Range.t * module_name
  | UndefinedSignatureName               of Range.t * signature_name
  | UndefinedMacro                       of Range.t * macro_name
  | InvalidNumberOfMacroArguments        of Range.t * mono_macro_parameter_type list
  | LateMacroArgumentExpected            of Range.t * mono_type
  | EarlyMacroArgumentExpected           of Range.t * mono_type
  | UnknownUnitOfLength                  of Range.t * length_unit_name
  | InlineCommandInMath                  of Range.t
  | MathCommandInInline                  of Range.t
  | BreaksValueRestriction               of Range.t
  | MultiplePatternVariable              of Range.t * Range.t * var_name
  | LabelUsedMoreThanOnce                of Range.t * label
  | InvalidExpressionAsToStaging         of Range.t * stage
  | InvalidOccurrenceAsToStaging         of Range.t * var_name * stage
  | ApplicationOfNonFunction             of Range.t * mono_type
  | MultiCharacterMathScriptWithoutBrace of Range.t
  | IllegalNumberOfTypeArguments         of Range.t * type_name * int * int
  | TypeUnificationError                 of mono_type * mono_type * unification_error
  | RowUnificationError                  of Range.t * mono_row * mono_row * unification_error
  | TypeParameterBoundMoreThanOnce       of Range.t * type_variable_name
  | ConflictInSignature                  of Range.t * string
  | NotAStructureSignature               of Range.t * functor_signature
  | NotAFunctorSignature                 of Range.t * StructSig.t
  | MissingRequiredValueName             of Range.t * var_name * poly_type
  | MissingRequiredMacroName             of Range.t * macro_name * poly_macro_type
  | MissingRequiredConstructorName       of Range.t * constructor_name * constructor_entry
  | MissingRequiredTypeName              of Range.t * type_name * int
  | MissingRequiredModuleName            of Range.t * module_name * signature
  | MissingRequiredSignatureName         of Range.t * signature_name * signature abstracted
  | NotASubtypeAboutValue                of Range.t * var_name * poly_type * poly_type
  | NotASubtypeAboutValueStage           of Range.t * var_name * stage * stage
  | NotASubtypeAboutMacro                of Range.t * macro_name * poly_macro_type * poly_macro_type
  | NotASubtypeAboutConstructor          of Range.t * constructor_name * type_scheme * type_scheme
  | NotASubtypeAboutType                 of Range.t * type_name * type_entry * type_entry
  | NotASubtypeSignature                 of Range.t * signature * signature
  | UnexpectedOptionalLabel              of Range.t * label * mono_type
  | InvalidArityOfCommandApplication     of Range.t * int * int
  | CannotRestrictTransparentType        of Range.t * type_name
  | KindContradiction                    of Range.t * type_name * kind * kind
  | CyclicSynonymTypeDefinition          of (type_name * SynonymDependencyGraph.data) cycle
  | MultipleSynonymTypeDefinition        of type_name * Range.t * Range.t
  | ValueAttributeError                  of ValueAttribute.error
  | TestMustBeStage1NonRec               of Range.t
