
open MyUtil


type build_option = {
    text_mode              : string option;
    page_number_limit      : int;
    show_full_path         : bool;
    debug_show_bbox        : bool;
    debug_show_space       : bool;
    debug_show_block_bbox  : bool;
    debug_show_block_space : bool;
    debug_show_overfull    : bool;
    type_check_only        : bool;
    bytecomp               : bool;
}

type input =
  | PackageInput  of {
      root     : abs_path;
      envelope : abs_path;
    }
  | DocumentInput of {
      doc  : abs_path;
      out  : abs_path;
      dump : abs_path;
    }


let build
  ~input:(_ : input)
  ~deps:(_abspath_deps_config : abs_path)
  ~options:(_ : build_option) : unit
=
  failwith "TODO: SatysfiCommand.build"
