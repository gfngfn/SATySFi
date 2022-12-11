
open MyUtil
open PackageSystemBase

val begin_to_typecheck_file : abs_path -> unit

val begin_to_preprocess_file : abs_path -> unit

val begin_to_eval_file : abs_path -> unit

val begin_to_parse_file : abs_path -> unit

val pass_type_check : string option -> unit

val start_evaluation : int -> unit

val end_evaluation : unit -> unit

val start_page_break : unit -> unit

val achieve_count_max : unit -> unit

val achieve_fixpoint : string list -> unit

val end_output : abs_path -> unit

val target_file : abs_path -> unit

val dump_file : already_exists:bool -> abs_path -> unit

val lock_config_file : abs_path -> unit

val show_package_dependency_before_solving : (dependency_flag * package_dependency) list -> unit

val show_package_dependency_solutions : package_solution list -> unit

val begin_to_embed_fonts : unit -> unit

val begin_to_write_page : unit -> unit

val needs_another_trial : unit -> unit

val end_lock_output : abs_path -> unit

val warn_noninjective_cmap : Uchar.t -> Uchar.t -> Otfed.Value.glyph_id -> unit

val warn_noninjective_ligature : Otfed.Value.glyph_id -> unit

val warn_nonattachable_mark : Otfed.Value.glyph_id -> Otfed.Value.glyph_id -> unit

val warn_no_glyph : string -> Uchar.t -> unit

val warn_no_math_glyph : string -> Uchar.t -> unit

val warn_cmyk_image : abs_path -> unit

val warn_number_sign_end : Range.t -> unit

val warn_overfull_line : int -> unit

val warn_underfull_line : int -> unit

val warn_unreachable : int -> unit

val report_passed_test : test_name:string -> unit

val report_failed_test : test_name:string -> message:string -> unit

val all_tests_passed : unit -> unit

val some_test_failed : unit -> unit

val lock_already_installed : lock_name -> abs_path -> unit

val lock_cache_exists : lock_name -> abs_path -> unit

val downloading_lock : lock_name -> abs_path -> unit

val report_canonicalized_url : url:string -> canonicalized_url:string -> hash_value:registry_hash_value -> unit
