(** Glyph Lists *)

(** The Adobe Glyph List, which maps character names to sequences of unicode
codepoints. The source list is parsed into a hash table when called. *)
val glyph_hashes : unit -> (string, int list) Hashtbl.t

(** The reverse of [glyph_hashes]. The glyph list is not necessarily a
one-to-one map, so this reversal is heuristic. *)
val reverse_glyph_hashes : unit -> (int list, string) Hashtbl.t

(** Convert a glyph name to a PDF encoding number *)
val name_to_pdf : (string * int) list

(** Convert a glyph name to a Windows encoding number *)
val name_to_win : (string * int) list

(** Convert a glyph name to a Standard encoding number *)
val name_to_standard : (string * int) list

(** Convert a glyph name to a MacRoman encoding number *)
val name_to_macroman : (string * int) list

(** Convert a glyph name to a MacExpert encoding number *)
val name_to_macexpert : (string * int) list

(** Convert a glyph name to a Symbol encoding number *)
val name_to_symbol : (string * int) list

(** Convert a glyph name to a Dingbats encoding number *)
val name_to_dingbats : (string * int) list

(**/**)
val name_to_pdf_hashes : (string, int) Hashtbl.t
val reverse_name_to_pdf_hashes : (int, string) Hashtbl.t



