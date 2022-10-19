
open Types
open StaticEnv
open TypecheckUtil

val decode_manual_kind : pre -> type_environment -> manual_kind -> kind ok

val decode_manual_type : pre -> type_environment -> manual_type -> mono_type ok

val decode_manual_macro_type : pre -> type_environment -> manual_macro_type -> mono_macro_type ok
