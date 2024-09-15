
open Types
open StaticEnv
open TypeError

val typecheck : pre -> type_environment -> untyped_abstract_tree -> (abstract_tree * mono_type, type_error) result

val typecheck_let_rec : pre -> type_environment -> untyped_let_binding list -> ((var_name * poly_type * EvalVarID.t * let_rec_binding) list, type_error) result

val typecheck_let_nonrec : always_polymorphic:bool -> pre -> type_environment -> untyped_let_binding -> (var_name * poly_type * EvalVarID.t * abstract_tree, type_error) result

val typecheck_let_mutable : pre -> type_environment -> var_name ranged -> untyped_abstract_tree -> (var_name * poly_type * EvalVarID.t * abstract_tree, type_error) result

val main : typecheck_config -> stage -> Typeenv.t -> untyped_abstract_tree -> (mono_type * abstract_tree, type_error) result

val are_unifiable : mono_type -> mono_type -> bool
