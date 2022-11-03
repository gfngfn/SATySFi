
open MyUtil
open Types
open StaticEnv
open ConfigError

val main : type_environment -> global_type_environment -> untyped_package -> (struct_signature * (abs_path * binding list) list, config_error) result

val main_document : type_environment -> global_type_environment -> (abs_path * untyped_library_file) list -> abs_path * untyped_document_file -> ((abs_path * binding list) list * abstract_tree, config_error) result
