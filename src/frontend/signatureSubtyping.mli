
open StaticEnv
open TypecheckUtil

val substitute_abstract : substitution -> signature abstracted -> signature abstracted

val substitute_concrete : substitution -> signature -> signature

val subtype_concrete_with_abstract : Range.t -> signature -> signature abstracted -> substitution ok
