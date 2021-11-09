
open Types
open StaticEnv

exception TypeError of TypeError.type_error

val main_bindings : stage -> Typeenv.t -> untyped_signature option -> untyped_binding list -> abstract_tree * StructSig.t abstracted

val main : stage -> Typeenv.t -> untyped_abstract_tree -> mono_type * abstract_tree

val are_unifiable : mono_type -> mono_type -> bool
