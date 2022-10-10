
open Types
open StaticEnv
open TypeError

val main_bindings : Typeenv.t -> untyped_signature option -> untyped_binding list -> (StructSig.t abstracted * binding list, type_error) result

val main : stage -> Typeenv.t -> untyped_abstract_tree -> (mono_type * abstract_tree, type_error) result

val are_unifiable : mono_type -> mono_type -> bool
