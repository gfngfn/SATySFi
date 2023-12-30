
open MyUtil
open PackageSystemBase

val show_package_dependency_before_solving : (dependency_flag * package_dependency) list -> unit

val show_package_dependency_solutions : package_solution list -> unit

val end_lock_output : abs_path -> unit

val end_envelope_output : abs_path -> unit

val lock_already_installed : lock_name -> abs_path -> unit

val lock_cache_exists : lock_name -> abs_path -> unit

val downloading_lock : lock_name -> abs_path -> unit

val report_canonicalized_url : url:string -> canonicalized_url:string -> hash_value:registry_hash_value -> unit
