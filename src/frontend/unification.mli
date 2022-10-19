
open Types
open TypeError

val unify_type : mono_type -> mono_type -> (unit, unification_error) result

val unify_row : mono_row -> mono_row -> (unit, unification_error) result
