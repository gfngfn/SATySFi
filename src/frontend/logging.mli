
open MyUtil
open LoggingUtil

val show_path : logging_spec -> abs_path -> string

val begin_to_typecheck_file : logging_spec -> abs_path -> unit

val begin_to_preprocess_file : logging_spec -> abs_path -> unit

val begin_to_eval_file : logging_spec -> abs_path -> unit

val begin_to_parse_file : logging_spec -> abs_path -> unit

val pass_type_check : logging_spec -> string option -> unit

val start_evaluation : logging_spec -> int -> unit

val end_evaluation : logging_spec -> unit

val start_page_break : logging_spec -> unit

val needs_another_trial : logging_spec -> unit

val achieve_count_max : logging_spec -> unit

val achieve_fixpoint : logging_spec -> string list -> unit

val end_output : logging_spec -> abs_path -> unit

val target_file : logging_spec -> abs_path -> unit

val dump_file : logging_spec -> already_exists:bool -> abs_path -> unit

val deps_config_file : logging_spec -> abs_path -> unit

val begin_to_embed_fonts : logging_spec -> unit

val begin_to_write_page : logging_spec -> unit

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

val warn_wide_column_cell_overrides_nonempty_cell : unit -> unit

val warn_wide_row_cell_overrides_nonempty_cell : unit -> unit
