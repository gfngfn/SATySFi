(** Page Labels *)

(** The type for page labels. The page labels of a document, if well-formed,
are a list of [t]s where the [startpage] values are in increasing numerical
order. In the most basic case, [startvalue] will always be equal to
[startpage]. The default labelstyle is [DecimalArabic]. The default
[labelprefix] is the empty string.

For example, a document might have five pages of introduction with roman
numerals, followed by the rest of the pages in decimal arabic, numbered from
one.

For more details, see ISO 32000 12.4.2, but note that in our implementation,
pages are 1-based. *)
type labelstyle =
  | DecimalArabic
  | UppercaseRoman
  | LowercaseRoman
  | UppercaseLetters
  | LowercaseLetters
  | NoLabelPrefixOnly

type t =
  {labelstyle : labelstyle;
   labelprefix : string option;
   startpage : int;
   startvalue : int}

(** Debug string of page label *)
val string_of_pagelabel : t -> string

(** Read the page labels from a document *)
val read : Pdf.t -> t list

(** Return a list where every page has a label - pages which don't are given
arabic page numbers *)
val complete : t list -> t list 

(** Single label representing a given page. Raises [Not_found] if no label. *)
val pagelabel_of_pagenumber : int -> t list -> t

(** Return the text for a page label. Raises [Not_found] if no label. *)
val pagelabeltext_of_pagenumber : int -> t list -> string

(** Add a range starting at pagelabel.startpage, ending at the integer page
given. The first integer argument is the number of pages in the PDF. *)
val add_label : int -> t list -> t -> int -> t list

(** Optimise page labels, removing any which are not required. *)
val coalesce : t list -> t list

(** Merge some page labels for some PDFs and page ranges. *)
val merge_pagelabels : Pdf.t list -> int list list -> t list

(** Write page labels to a document, replacing any there. The list must contain
at least one element. *)
val write : Pdf.t -> t list -> unit

(** Remove all page labels. *)
val remove : Pdf.t -> unit

