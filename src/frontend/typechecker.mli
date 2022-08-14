
open Types
open StaticEnv

exception TypeError of TypeError.type_error

val main_bindings : Typeenv.t -> untyped_signature option -> untyped_binding list -> StructSig.t abstracted * binding list

val main : stage -> Typeenv.t -> untyped_abstract_tree -> mono_type * abstract_tree

val are_unifiable : mono_type -> mono_type -> bool
