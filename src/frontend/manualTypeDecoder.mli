
open Types
open StaticEnv
open TypecheckUtil

(** Convert manually written kinds into internal ones. *)
val decode_manual_kind : pre -> type_environment -> manual_kind -> kind ok

(** Convert manually written types into internal ones. *)
val decode_manual_type : pre -> type_environment -> manual_type -> mono_type ok

(** Convert manually written macro types into internal ones. *)
val decode_manual_macro_type : pre -> type_environment -> manual_macro_type -> mono_macro_type ok
