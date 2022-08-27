
open MyUtil
open LengthInterface
open GraphicBase


type pure_badness = int
[@@deriving show]

type ratios =
  | TooShort    of { required : length; actual : length }
  | Permissible of float
  | TooLong     of { required : length; actual : length }
[@@deriving show]

type reachability =
  | Unreachable
  | Reachable of ratios
[@@deriving show]

type font_abbrev = string
[@@deriving show]

type math_font_abbrev = string
[@@deriving show]

type file_path = string
(*
type encoding_in_pdf =
  | Latin1
  | UTF16BE
  | IdentityH
*)
type font_with_size = font_abbrev * Length.t
[@@deriving show]

type font_with_ratio = font_abbrev * float * float
[@@deriving show]

type page_content_scheme =
  {
    page_content_origin : point;
    page_content_height : length;
  }

let pp_page_content_scheme fmt _ = Format.fprintf fmt "<page-content-scheme>"

type page_break_info = {
  current_page_number : int;
}
[@@deriving show {with_path = false }]

type page_content_scheme_func = page_break_info -> page_content_scheme

let pp_page_content_scheme_func fmt _ = Format.fprintf fmt "<page-content-scheme-func>"

type paddings =
  {
    paddingL : length;
    paddingR : length;
    paddingT : length;
    paddingB : length;
  }
[@@deriving show { with_path = false }]

(*
type line_dash =
  | SolidLine
  | DashedLine of length * length * length

type line_join =
  | MiterJoin
  | RoundJoin
  | BevelJoin

type line_cap =
  | ButtCap
  | RoundCap
  | ProjectingSquareCap

(* will be deprecated *)
type graphics_state =
  {
    line_width   : length;
    line_dash    : line_dash;
    line_join    : line_join;
    line_cap     : line_cap;
    miter_limit  : length;
    fill_color   : color;
    stroke_color : color;
  }

(* will be deprecated *)
type graphics_command =
  | DrawStroke
  | DrawFillByNonzero
  | DrawFillByEvenOdd
  | DrawBothByNonzero
  | DrawBothByEvenOdd
*)

type horz_string_info =
  {
    font_abbrev    : font_abbrev;
    text_font_size : length;
    text_color     : color;
    rising         : length;
  }

let pp_horz_string_info fmt info =
  Format.fprintf fmt "(HSinfo)"

type math_string_info =
  {
    math_font_abbrev : math_font_abbrev;
    math_font_size   : length;
    math_color       : color;
  }

(* -- internal representation of boxes -- *)

type math_kind =
  | MathOrdinary
  | MathBinary
  | MathRelation
  | MathOperator
  | MathPunct
  | MathOpen
  | MathClose
  | MathPrefix    (* -- mainly for differantial operator 'd', '\partial', etc. -- *)
  | MathInner
  | MathEnd
[@@deriving show { with_path = false }]

type math_char_class =
  | MathItalic
  | MathBoldItalic
  | MathRoman
  | MathBoldRoman
  | MathScript
  | MathBoldScript
  | MathFraktur
  | MathBoldFraktur
  | MathDoubleStruck
[@@deriving show { with_path = false }]
(* TEMPORARY; should add more *)

type math_variant_style =
  {
    math_italic        : Uchar.t list;
    math_bold_italic   : Uchar.t list;
    math_roman         : Uchar.t list;
    math_bold_roman    : Uchar.t list;
    math_script        : Uchar.t list;
    math_bold_script   : Uchar.t list;
    math_fraktur       : Uchar.t list;
    math_bold_fraktur  : Uchar.t list;
    math_double_struck : Uchar.t list;
  }

let pp_math_variant_style =
  (fun fmt _ -> Format.fprintf fmt "<math-variant-style>")


module MathVariantCharMap = Map.Make
  (struct
    type t = Uchar.t * math_char_class
    let compare = Pervasives.compare
  end)


module MathClassMap = Map.Make(Uchar)


type breakability =
  | Breakable
  | Unbreakable
[@@deriving show { with_path = false; }]


let ( &-& ) br1 br2 =
  match (br1, br2) with
  | (Breakable, Breakable) -> Breakable
  | _                      -> Unbreakable


type debug_margin_info =
  | Fixed
  | BetweenLines
  | LowerOnly of breakability
  | UpperOnly of breakability
  | Both      of (breakability * length) * (breakability * length)
[@@deriving show { with_path = false; }]


type context_main = {
  hyphen_dictionary      : LoadHyph.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<hyph>")]
  hyphen_badness         : int;
  font_size              : length;
  font_scheme            : font_with_ratio CharBasis.ScriptSchemeMap.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<map>")]
  langsys_scheme         : CharBasis.language_system CharBasis.ScriptSchemeMap.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<map>")]
  math_font              : math_font_abbrev;
  dominant_wide_script   : CharBasis.script;
  dominant_narrow_script : CharBasis.script;
  script_space_map       : (float * float * float) CharBasis.ScriptSpaceMap.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<map>")]
  space_natural          : float;
  space_shrink           : float;
  space_stretch          : float;
  adjacent_stretch       : float;
  paragraph_width        : length;
  paragraph_top          : length;
  paragraph_bottom       : length;
  min_first_line_ascender : length;
  min_last_line_descender : length;
  leading                : length;
  min_gap_of_lines       : length;
  text_color             : color;
  manual_rising          : length;
  space_badness          : pure_badness;
  math_variant_char_map  : (Uchar.t * math_kind) MathVariantCharMap.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<math-variant-char-map>")]
  math_class_map         : (Uchar.t * math_kind) MathClassMap.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<math-class-map>")]
  math_char_class        : math_char_class;
  before_word_break      : horz_box list;
  after_word_break       : horz_box list;
  space_math_bin         : float * float * float;
  space_math_rel         : float * float * float;
  space_math_op          : float * float * float;
  space_math_punct       : float * float * float;
  space_math_inner       : float * float * float;
  space_math_prefix      : float * float * float;
  left_hyphen_min        : int;
  right_hyphen_min       : int;
}

and decoration = point -> length -> length -> length -> (intermediate_horz_box list) GraphicD.t

and rules_func = length list -> length list -> (intermediate_horz_box list) GraphicD.t

and fixed_graphics = point -> (intermediate_horz_box list) GraphicD.t

and outer_fil_graphics = length -> point -> (intermediate_horz_box list) GraphicD.t

and pure_horz_box =
(* -- spaces inserted before text processing -- *)
  | PHSOuterEmpty     of length * length * length
  | PHSOuterFil
  | PHSFixedEmpty     of length
(* -- texts -- *)
  | PHCInnerString    of context_main * Uchar.t list
      [@printer (fun fmt _ -> Format.fprintf fmt "@[FixedString(...)@]")]
  | PHCInnerMathGlyph of math_string_info * length * length * length * OutputText.t
      [@printer (fun fmt _ -> Format.fprintf fmt "@[FixedMathGlyph(...)@]")]
(* -- groups -- *)
  | PHGRising         of length * horz_box list
  | PHGFixedFrame     of paddings * length * decoration * horz_box list
  | PHGInnerFrame     of paddings * decoration * horz_box list
  | PHGOuterFrame     of paddings * decoration * horz_box list
  | PHGEmbeddedVert   of length * length * length * intermediate_vert_box list
  | PHGFixedGraphics  of length * length * length * fixed_graphics
  | PHGOuterFilGraphics of length * length * outer_fil_graphics
  | PHGFixedTabular   of length * length * length * intermediate_row list * length list * length list * rules_func
  | PHGFixedImage     of length * length * ImageInfo.key
      [@printer (fun fmt _ -> Format.fprintf fmt "@[PHGFixedImage(...)@]")]
  | PHGHookPageBreak  of (page_break_info -> point -> unit)
  | PHGFootnote       of intermediate_vert_box list

and horz_box =
  | HorzPure           of pure_horz_box
  | HorzDiscretionary  of pure_badness * horz_box list * horz_box list * horz_box list
      [@printer (fun fmt _ -> Format.fprintf fmt "HorzDiscretionary(...)")]
  | HorzEmbeddedVertBreakable of length * vert_box list
  | HorzFrameBreakable of paddings * length * length * decoration * decoration * decoration * decoration * horz_box list
  | HorzScriptGuard    of CharBasis.script * CharBasis.script * horz_box list
  | HorzOmitSkipAfter

and intermediate_graphics =
  | ImGraphicsFixed    of fixed_graphics
  | ImGraphicsVariable of outer_fil_graphics

and intermediate_horz_box =
  | ImHorz               of evaled_horz_box
  | ImHorzRising         of length * length * length * length * intermediate_horz_box list
  | ImHorzFrame          of ratios * length * length * length * decoration * intermediate_horz_box list
  | ImHorzInlineTabular  of length * length * length * intermediate_row list * length list * length list * rules_func
  | ImHorzEmbeddedVert   of length * length * length * intermediate_vert_box list
  | ImHorzInlineGraphics of length * length * length * intermediate_graphics
  | ImHorzHookPageBreak  of (page_break_info -> point -> unit)
  | ImHorzFootnote       of intermediate_vert_box list

and evaled_horz_box =
  length * evaled_horz_box_main
      (* --
         (1) width
         (2) contents
         -- *)

and evaled_horz_box_main =
  | EvHorzString of horz_string_info * length * length * OutputText.t
      (* --
         (1) string information for writing string to PDF
         (2) content height
         (3) content depth
         (4) content string
         -- *)

  | EvHorzMathGlyph      of math_string_info * length * length * OutputText.t
      [@printer (fun fmt _ -> Format.fprintf fmt "EvHorzMathGlyph(...)")]
  | EvHorzRising         of length * length * length * evaled_horz_box list
  | EvHorzEmpty
  | EvHorzFrame          of ratios * length * length * decoration * evaled_horz_box list
  | EvHorzEmbeddedVert   of length * length * evaled_vert_box list
  | EvHorzInlineGraphics of length * length * intermediate_graphics
  | EvHorzInlineTabular  of length * length * evaled_row list * length list * length list * rules_func
  | EvHorzInlineImage    of length * ImageInfo.key
      [@printer (fun fmt _ -> Format.fprintf fmt "EvHorzInlineImage(...)")]
  | EvHorzHookPageBreak  of page_break_info * (page_break_info -> point -> unit)
      (* --
         (1) page number determined during the page breaking
         (2) hook function invoked during the construction of PDF data
         -- *)

and vert_box =
  | VertParagraph      of margins * paragraph_element list
  | VertFixedBreakable of length
      [@printer (fun fmt _ -> Format.fprintf fmt "Breakable")]
  | VertFrame          of margins * paddings * decoration * decoration * decoration * decoration * length * vert_box list
(*      [@printer (fun fmt (_, _, _, _, _, imvblst) -> Format.fprintf fmt "%a" (pp_list pp_intermediate_vert_box) imvblst)] *)
  | VertClearPage
  | VertHookPageBreak of (page_break_info -> point -> unit)

and margins = {
  margin_top    : (breakability * length) option;
  margin_bottom : (breakability * length) option;
}

and paragraph_element =
  | VertParagLine of reachability * length * length * intermediate_horz_box list
      [@printer (fun fmt _ -> Format.fprintf fmt "Line")]
  | VertParagSkip of length

and intermediate_vert_box =
  | ImVertLine       of reachability * length * length * intermediate_horz_box list
  | ImVertFixedEmpty of debug_margin_info * length
  | ImVertFrame      of paddings * decoration * length * intermediate_vert_box list
  | ImVertHookPageBreak of (page_break_info -> point -> unit)

and evaled_vert_box =
  | EvVertLine       of reachability * length * length * evaled_horz_box list
      [@printer (fun fmt _ -> Format.fprintf fmt "EvLine")]
      (* --
         (1) height of the contents
         (2) depth of the contents (nonpositive)
         (3) contents
         -- *)
  | EvVertFixedEmpty of debug_margin_info * length
      [@printer (fun fmt _ -> Format.fprintf fmt "EvEmpty")]
  | EvVertFrame      of paddings * page_break_info * decoration * length * evaled_vert_box list
  | EvVertHookPageBreak of (page_break_info -> point -> unit)

and page_parts_scheme = {
  header_origin  : point;
    [@printer (fun fmt _ -> Format.fprintf fmt "<point>")]
  header_content : intermediate_vert_box list;
  footer_origin  : point;
    [@printer (fun fmt _ -> Format.fprintf fmt "<point>")]
  footer_content : intermediate_vert_box list;
}

and page_content_info =
  page_break_info  (* temporary *)
(*
{
  page_number : int;
}
*)

and page_parts_scheme_func = page_content_info -> page_parts_scheme


and column_hook_func = unit -> vert_box list


and math_char_kern_func = length -> length -> length
  (* --
     takes the actual font size and the y-position,
     and returns a kerning value (positive for making characters closer)
     -- *)

and math_kern_func = length -> length
  (* -- takes a y-position as a correction height and then returns a kerning value -- *)

and math_variant_value = math_kind * math_variant_value_main

and math_variant_value_main =
  | MathVariantToChar         of bool * Uchar.t list
      [@printer (fun fmt _ -> Format.fprintf fmt "<to-char>")]
      (* --
         (1) whether it is big or not
         (2) contents
         -- *)

  | MathVariantToCharWithKern of bool * Uchar.t list * math_char_kern_func * math_char_kern_func
      [@printer (fun fmt _ -> Format.fprintf fmt "<to-char'>")]

and paren = length -> length -> length -> length -> color -> horz_box list * math_kern_func
  (* --
     'paren':
       the type for adjustable parentheses.
       An adjustable parenthesis takes as arguments
       (1-2) the height and the depth of the inner contents,
       (3)   the axis height,
       (4)   the font size, and
       (5)   the color for glyphs,
       and then returns its inline box representation and the function for kerning.
     -- *)

and radical = length -> length -> length -> length -> color -> horz_box list
  (* --
     'radical':
       the type for adjustable radicals.
       An adjustable radical takes as arguments
       (1-2) the height and the thickness of the bar required by the math font,
       (3)   the depth of the inner contents,
       (4)   the font size, and
       (5)   the color for glyphs,
       and then returns the inline box representation.
     -- *)

and cell =
  | NormalCell of paddings * horz_box list
  | EmptyCell
  | MultiCell  of int * int * paddings * horz_box list

and row = cell list

and intermediate_cell =
  | ImNormalCell of ratios * (length * length * length) * intermediate_horz_box list
  | ImEmptyCell  of length
  | ImMultiCell  of ratios * (int * int * length * length * length * length) * intermediate_horz_box list

and intermediate_row = length * intermediate_cell list

and evaled_cell =
  | EvNormalCell of ratios * (length * length * length) * evaled_horz_box list
  | EvEmptyCell  of length
  | EvMultiCell  of ratios * (int * int * length * length * length * length) * evaled_horz_box list

and evaled_row = length * evaled_cell list
[@@deriving show { with_path = false }]

type column = cell list


let normalize_script ctx script_raw =
  match script_raw with
  | CharBasis.CommonNarrow
  | CharBasis.Inherited
      -> ctx.dominant_narrow_script

  | CharBasis.CommonWide
      -> ctx.dominant_wide_script

  | _ -> script_raw


let get_font_with_ratio ctx script_raw =
  let script = normalize_script ctx script_raw in
    match ctx.font_scheme |> CharBasis.ScriptSchemeMap.find_opt script with
    | None          -> failwith "get_font_with_ratio"
    | Some(fontsch) -> fontsch


let get_language_system ctx script_raw =
  let script = normalize_script ctx script_raw in
    match ctx.langsys_scheme |> CharBasis.ScriptSchemeMap.find_opt script with
    | None          -> CharBasis.NoLanguageSystem
    | Some(langsys) -> langsys


let get_string_info ctx script_raw =
  let (font_abbrev, ratio, rising_ratio) = get_font_with_ratio ctx script_raw in
    {
      font_abbrev    = font_abbrev;
      text_font_size = ctx.font_size *% ratio;
      text_color     = ctx.text_color;
      rising         = ctx.manual_rising +% ctx.font_size *% rising_ratio;
    }


let get_metrics_of_evaled_horz_box ((wid, evhbmain) : evaled_horz_box) : length * length * length =
  let (hgt, dpt) =
    match evhbmain with
    | EvHorzEmpty
    | EvHorzHookPageBreak(_, _)
        -> (Length.zero, Length.zero)

    | EvHorzInlineImage(h, _) ->
        (h, Length.zero)

    | EvHorzString(_, h, d, _)
    | EvHorzRising(h, d, _, _)
    | EvHorzMathGlyph(_, h, d, _)
    | EvHorzEmbeddedVert(h, d, _)
    | EvHorzInlineGraphics(h, d, _)
    | EvHorzInlineTabular(h, d, _, _, _, _)
    | EvHorzFrame(_, h, d, _, _)
         -> (h, d)
  in
    (wid, hgt, dpt)


let rec get_height_of_evaled_vert_box_list evvblst =
  evvblst |> List.fold_left (fun l evvb ->
    match evvb with
    | EvVertLine(_, hgt, dpt, _)          -> l +% hgt +% (Length.negate dpt)
    | EvVertFixedEmpty(_, len)            -> l +% len
    | EvVertFrame(pads, _, _, _, evvblst) -> l +% pads.paddingB +% pads.paddingL +% get_height_of_evaled_vert_box_list evvblst
    | EvVertHookPageBreak(_)              -> l
  ) Length.zero


let get_metrics_of_intermediate_horz_box_list (imhblst : intermediate_horz_box list) : length * length * length =
  imhblst |> List.fold_left (fun (wid, hgt, dpt) imhb ->
    let (w, h, d) =
      match imhb with
      | ImHorz(evhb) ->
          get_metrics_of_evaled_horz_box evhb

      | ImHorzHookPageBreak(_)
      | ImHorzFootnote(_)
          -> (Length.zero, Length.zero, Length.zero)

      | ImHorzRising(w, h, d, _, _)
      | ImHorzFrame(_, w, h, d, _, _)
      | ImHorzInlineTabular(w, h, d, _, _, _, _)
      | ImHorzInlineGraphics(w, h, d, _)
      | ImHorzEmbeddedVert(w, h, d, _)
           -> (w, h, d)
    in
      (wid +% w, Length.max hgt h, Length.min dpt d)
  ) (Length.zero, Length.zero, Length.zero)


let rec extract_string (hblst : horz_box list) : string =
  let rec extract_one hb =
    match hb with
    | HorzPure(PHCInnerString(_, uchlst))            -> string_of_uchlst uchlst
    | HorzPure(PHCInnerMathGlyph(_, _, _, _, otxt))  -> ""
    | HorzPure(PHGRising(_, hblst))                  -> extract_string hblst
    | HorzPure(PHGFixedFrame(_, _, _, hblst))        -> extract_string hblst
    | HorzPure(PHGInnerFrame(_, _, hblst))           -> extract_string hblst
    | HorzPure(PHGOuterFrame(_, _, hblst))           -> extract_string hblst
    | HorzDiscretionary(_, hblst0, _, _)             -> extract_string hblst0
    | HorzFrameBreakable(_, _, _, _, _, _, _, hblst) -> extract_string hblst
    | HorzScriptGuard(_, _, hblst)                   -> extract_string hblst
    | _                                              -> ""
  in
    String.concat "" (List.map extract_one hblst)
