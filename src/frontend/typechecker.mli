
open Types
open StaticEnv

exception TypeError of TypeError.type_error

val main_bindings : stage -> target_type_environment -> untyped_signature option -> untyped_binding list -> target_struct_signature abstracted * binding list

val main : stage -> target_type_environment -> untyped_abstract_tree -> mono_type * abstract_tree

val are_unifiable : mono_type -> mono_type -> bool
