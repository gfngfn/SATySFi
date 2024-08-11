
open Types
open StaticEnv
open TypeError

val typecheck_signature : typecheck_config -> Typeenv.t -> untyped_signature -> (signature abstracted, type_error) result

val main : typecheck_config -> Typeenv.t -> (signature abstracted) option -> untyped_binding list -> (StructSig.t abstracted * binding list, type_error) result
