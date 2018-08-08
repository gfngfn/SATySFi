
module Types = Types_
open Types
open Typeenv

exception UndefinedVariable     of Range.t * module_name list * var_name * var_name list
exception UndefinedConstructor  of Range.t * var_name * var_name list
exception InclusionError        of Typeenv.t * mono_type * mono_type
exception ContradictionError    of Typeenv.t * mono_type * mono_type
exception UnknownUnitOfLength   of Range.t * length_unit_name
exception HorzCommandInMath     of Range.t
exception MathCommandInHorz     of Range.t
exception BreaksValueRestriction of Range.t
exception MultiplePatternVariable of Range.t * Range.t * var_name
exception InvalidOptionalCommandArgument of Typeenv.t * mono_type * Range.t
exception NeedsMoreArgument              of Range.t * Typeenv.t * mono_type * mono_type
exception TooManyArgument                of Range.t * Typeenv.t * mono_type
exception MultipleFieldInRecord          of Range.t * field_name
exception ApplicationOfNonFunction       of Range.t * Typeenv.t * mono_type

val main : Typeenv.t -> untyped_abstract_tree -> (mono_type * Typeenv.t * abstract_tree)
