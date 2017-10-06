open Types
open Typeenv

exception InclusionError        of Typeenv.t * mono_type * mono_type
exception ContradictionError    of Typeenv.t * mono_type * mono_type
exception UndefinedVariable     of Range.t * var_name
exception UndefinedConstructor  of Range.t * var_name
exception InvalidArityOfCommand of Range.t * int * int
exception UnknownUnitOfLength   of Range.t * length_unit_name

val main : Typeenv.t -> untyped_abstract_tree -> (mono_type * Typeenv.t * abstract_tree)
