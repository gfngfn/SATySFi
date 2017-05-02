open Types
open Typeenv

exception InclusionError       of Variantenv.t * mono_type * mono_type
exception ContradictionError   of Variantenv.t * mono_type * mono_type
exception UndefinedVariable    of Range.t * var_name
exception UndefinedConstructor of Range.t * var_name

val main : Variantenv.t -> Typeenv.t -> untyped_abstract_tree -> (mono_type * Variantenv.t * Typeenv.t * abstract_tree)
