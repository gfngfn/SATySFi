
open MyUtil


type envelope_name = string

type envelope_info = {
  envelope_name         : envelope_name;
  envelope_directory    : abs_path;
  envelope_dependencies : envelope_name list;
}

type input_kind =
  | InputSatysfi
  | InputMarkdown
