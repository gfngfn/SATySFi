
open MyUtil
open CommonUtil
open PackageSystemBase

type config = {
  path_display_setting : path_display_setting;
  verbosity            : Verbosity.t;
}

val show_package_dependency_before_solving : config -> (dependency_flag * package_dependency) list -> unit

val show_package_dependency_solutions : config -> package_solution list -> unit

val end_lock_config_output : config -> abs_path -> unit

val end_envelope_config_output : config -> abs_path -> unit

val end_deps_config_output : config -> abs_path -> unit

val lock_already_installed : lock_name -> abs_path -> unit

val lock_cache_exists : lock_name -> abs_path -> unit

val store_root_config_updated : created:bool -> abs_path -> unit

val package_registry_updated : created:bool -> abs_path -> unit

val initialize_file : config -> abs_path -> unit

val initialize_package_config : config -> abs_path -> unit

val downloading_lock : lock_name -> abs_path -> unit

val report_canonicalized_url : url:string -> canonicalized_url:string -> hash_value:registry_hash_value -> unit
