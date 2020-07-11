
open Types
open StaticEnv

exception UndefinedVariable              of Range.t * module_name list * var_name * var_name list
exception UndefinedConstructor           of Range.t * var_name * var_name list
exception InclusionError                 of mono_type * mono_type
exception ContradictionError             of mono_type * mono_type
exception UnknownUnitOfLength            of Range.t * length_unit_name
exception HorzCommandInMath              of Range.t
exception MathCommandInHorz              of Range.t
exception BreaksValueRestriction         of Range.t
exception MultiplePatternVariable        of Range.t * Range.t * var_name
exception InvalidOptionalCommandArgument of mono_type * Range.t
exception NeedsMoreArgument              of Range.t * mono_type * mono_type
exception TooManyArgument                of Range.t * mono_type
exception MultipleFieldInRecord          of Range.t * field_name
exception ApplicationOfNonFunction       of Range.t * mono_type
exception InvalidExpressionAsToStaging   of Range.t * stage
exception InvalidOccurrenceAsToStaging   of Range.t * var_name * stage
exception UndefinedHorzMacro             of Range.t * ctrlseq_name
exception UndefinedVertMacro             of Range.t * ctrlseq_name
exception InvalidNumberOfMacroArguments  of Range.t * macro_parameter_type list
exception LateMacroArgumentExpected      of Range.t * mono_type
exception EarlyMacroArgumentExpected     of Range.t * mono_type
exception IllegalNumberOfTypeArguments   of Range.t * type_name * int * int

val main_bindings : stage -> Typeenv.t -> untyped_binding list -> binding list * Typeenv.t

val main : stage -> Typeenv.t -> untyped_abstract_tree -> mono_type * abstract_tree
