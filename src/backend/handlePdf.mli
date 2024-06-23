
open MyUtil
open LengthInterface
open HorzBox

type t

val create_empty_pdf : unit -> t

type config = {
  debug_show_bbox        : bool;
  debug_show_space       : bool;
  debug_show_block_bbox  : bool;
  debug_show_block_space : bool;
  debug_show_overfull    : bool;
}

type page

val write_page : config -> page -> page_parts_scheme_func -> t -> t

val write_to_file : abs_path -> t -> unit

val make_empty_page : paper_size:(length * length) -> page_break_info -> page_content_scheme -> page

val add_column_to_page : config -> page -> length -> evaled_vert_box list -> evaled_vert_box list -> page
