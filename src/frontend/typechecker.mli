
open Types
open StaticEnv
open TypeError

val typecheck : pre -> type_environment -> untyped_abstract_tree -> (abstract_tree * mono_type, type_error) result

val typecheck_letrec : pre -> type_environment -> untyped_let_binding list -> ((var_name * poly_type * EvalVarID.t * letrec_binding) list, type_error) result

val main : stage -> Typeenv.t -> untyped_abstract_tree -> (mono_type * abstract_tree, type_error) result

val are_unifiable : mono_type -> mono_type -> bool
