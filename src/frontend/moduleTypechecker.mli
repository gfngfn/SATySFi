
open Types
open StaticEnv
open TypeError

val main : Typeenv.t -> untyped_signature option -> untyped_binding list -> (StructSig.t abstracted * binding list, type_error) result
