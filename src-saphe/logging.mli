
open MyUtil
open LoggingUtil
open PackageSystemBase

val show_package_dependency_before_solving : logging_spec -> (dependency_flag * package_dependency) list -> unit

val show_package_dependency_solutions : logging_spec -> package_solution list -> unit

val end_lock_config_output : logging_spec -> abs_path -> unit

val end_envelope_config_output : logging_spec -> abs_path -> unit

val end_deps_config_output : logging_spec -> abs_path -> unit

val lock_already_installed : logging_spec -> lock_name -> abs_path -> unit

val lock_cache_exists : logging_spec -> lock_name -> abs_path -> unit

val store_root_config_updated : logging_spec -> created:bool -> abs_path -> unit

val package_registry_updated : logging_spec -> created:bool -> abs_path -> unit

val initialize_file : logging_spec -> abs_path -> unit

val initialize_package_config : logging_spec -> abs_path -> unit

val downloading_lock : logging_spec -> lock_name -> abs_path -> unit