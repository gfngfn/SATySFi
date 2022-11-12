
open MyUtil
open LengthInterface
open GraphicBase


exception FontIsNotSet of {
  raw        : CharBasis.script;
  normalized : CharBasis.script;
}

exception MathFontIsNotSet


type pure_badness = int
[@@deriving show]

module PureBadness = struct
  type t = pure_badness
  let show = Int.to_string
  let add = Int.add
  let compare = Int.compare
  let zero = Int.zero
end

type ratios =
  | TooShort    of { required : length; actual : length }
  | Permissible of float
  | TooLong     of { required : length; actual : length }
[@@deriving show]

type reachability =
  | Unreachable
  | Reachable of ratios
[@@deriving show]

type file_path = string

type font_with_size = FontKey.t * Length.t
[@@deriving show]

type font_with_ratio = FontKey.t * float * float
[@@deriving show]

type page_content_scheme = {
  page_content_origin : point;
  page_content_height : length;
}
[@@deriving show {with_path = false }]

type page_break_info = {
  current_page_number : int;
}
[@@deriving show {with_path = false }]

type page_content_scheme_func = page_break_info -> page_content_scheme

let pp_page_content_scheme_func fmt _ =
  Format.fprintf fmt "<page-content-scheme-func>"

type paddings = {
  paddingL : length;
  paddingR : length;
  paddingT : length;
  paddingB : length;
}
[@@deriving show { with_path = false }]


type horz_string_info = {
  font_key       : FontKey.t;
  text_font_size : length;
  text_color     : color;
  rising         : length;
}
[@@deriving show { with_path = false }]

type math_string_info = {
  info_math_font_key  : FontKey.t;
  info_math_font_size : length;
  info_math_color     : color;
}
[@@deriving show { with_path = false }]

type math_kind =
  | MathOrdinary
  | MathBinary
  | MathRelation
  | MathOperator
  | MathPunct
  | MathOpen
  | MathClose
  | MathPrefix (* Mainly for differantial operator `d`, `\partial`, etc. *)
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
  | MathSansSerif
  | MathBoldSansSerif
  | MathItalicSansSerif
  | MathBoldItalicSansSerif
  | MathTypewriter
[@@deriving show { with_path = false }]


module MathVariantCharMap = Map.Make(Uchar)

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

type math_script_level =
  | BaseLevel
  | ScriptLevel
  | ScriptScriptLevel
[@@deriving show { with_path = false; }]

type math_variant_char_map =
  (math_char_class -> Uchar.t * math_kind) MathVariantCharMap.t

type context_main = {
  hyphen_dictionary       : LoadHyph.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<hyph>")]
  hyphen_badness          : int;
  font_size               : length;
  font_scheme             : font_with_ratio CharBasis.ScriptSchemeMap.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<map>")]
  langsys_scheme          : CharBasis.language_system CharBasis.ScriptSchemeMap.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<map>")]
  dominant_wide_script    : CharBasis.script;
  dominant_narrow_script  : CharBasis.script;
  script_space_map        : (float * float * float) CharBasis.ScriptSpaceMap.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<map>")]
  space_natural           : float;
  space_shrink            : float;
  space_stretch           : float;
  adjacent_stretch        : float;
  paragraph_width         : length;
  paragraph_top           : length;
  paragraph_bottom        : length;
  min_first_line_ascender : length;
  min_last_line_descender : length;
  leading                 : length;
  min_gap_of_lines        : length;
  text_color              : color;
  manual_rising           : length;
  space_badness           : pure_badness;
  math_variant_char_map   : math_variant_char_map;
    [@printer (fun fmt _ -> Format.fprintf fmt "<math-variant-char-map>")]
  math_class_map          : (Uchar.t * math_kind) MathClassMap.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<math-class-map>")]
  math_char_class         : math_char_class;
  before_word_break       : horz_box list;
  after_word_break        : horz_box list;
  space_math_bin          : float * float * float;
  space_math_rel          : float * float * float;
  space_math_op           : float * float * float;
  space_math_punct        : float * float * float;
  space_math_inner        : float * float * float;
  space_math_prefix       : float * float * float;
  left_hyphen_min         : int;
  right_hyphen_min        : int;
  math_font_key           : FontKey.t option;
  math_script_level       : math_script_level;
}

and decoration = point -> length -> length -> length -> (intermediate_horz_box list) GraphicD.t

and rules_func = length list -> length list -> (intermediate_horz_box list) GraphicD.t

and fixed_graphics = point -> (intermediate_horz_box list) GraphicD.t

and outer_fil_graphics = length -> point -> (intermediate_horz_box list) GraphicD.t

and pure_horz_box =
(* Spaces inserted before text processing: *)
  | PHSOuterEmpty of {
      natural     : length;
      shrinkable  : length;
      stretchable : length;
    }
  | PHSOuterFil
  | PHSFixedEmpty of {
      width : length;
    }
(* Texts: *)
  | PHCInnerString of {
      context : context_main;
      chars   : Uchar.t list;
    } [@printer (fun fmt _ _ -> Format.fprintf fmt "@[PHCInnerString(...)@]")]
  | PHCInnerMathGlyph of {
      info   : math_string_info;
      width  : length;
      height : length;
      depth  : length;
      output : OutputText.t;
    }
(* Groups: *)
  | PHGRising of {
      rising   : length;
      contents : horz_box list;
    }
  | PHGFixedFrame of {
      required_width : length;
      paddings       : paddings;
      decoration     : decoration;
      contents       : horz_box list;
    }
  | PHGInnerFrame of {
      paddings   : paddings;
      decoration : decoration;
      contents   : horz_box list;
    }
  | PHGOuterFrame of {
      paddings   : paddings;
      decoration : decoration;
      contents   : horz_box list;
    }
  | PHGEmbeddedVert of {
      width    : length;
      height   : length;
      depth    : length;
      contents : intermediate_vert_box list;
    }
  | PHGFixedGraphics of {
      width    : length;
      height   : length;
      depth    : length;
      graphics : fixed_graphics;
    }
  | PHGOuterFilGraphics of {
      height   : length;
      depth    : length;
      graphics : outer_fil_graphics;
    }
  | PHGFixedTabular of {
      width         : length;
      height        : length;
      depth         : length;
      rows          : intermediate_row list;
      column_widths : length list;
      row_heights   : length list;
      rule_graphics : rules_func;
    }
  | PHGFixedImage of {
      width  : length;
      height : length;
      key    : ImageInfo.key;
    } [@printer (fun fmt _ _ _ -> Format.fprintf fmt "@[PHGFixedImage(...)@]")]
  | PHGHookPageBreak of (page_break_info -> point -> unit)
  | PHGFootnote of intermediate_vert_box list

and horz_box =
  | HorzPure of pure_horz_box
  | HorzDiscretionary of {
      penalty  : pure_badness;
      no_break : horz_box list;
      pre      : horz_box list;
      post     : horz_box list;
    }
  | HorzEmbeddedVertBreakable of {
      width    : length;
      contents : vert_box list;
    }
  | HorzFrameBreakable of {
      paddings              : paddings;
      decoration_standalone : decoration;
      decoration_head       : decoration;
      decoration_middle     : decoration;
      decoration_tail       : decoration;
      contents              : horz_box list;
    }
  | HorzScriptGuard of {
      left     : CharBasis.script;
      right    : CharBasis.script;
      contents : horz_box list;
    }
  | HorzOmitSkipAfter

and intermediate_graphics =
  | ImGraphicsFixed    of fixed_graphics
  | ImGraphicsVariable of outer_fil_graphics

and intermediate_horz_box =
  | ImHorz of evaled_horz_box
  | ImHorzRising of {
      width    : length;
      height   : length;
      depth    : length;
      rising   : length;
      contents : intermediate_horz_box list;
    }
  | ImHorzFrame of {
      ratios     : ratios;
      width      : length;
      height     : length;
      depth      : length;
      decoration : decoration;
      contents   : intermediate_horz_box list;
    }
  | ImHorzInlineTabular of {
      width         : length;
      height        : length;
      depth         : length;
      rows          : intermediate_row list;
      column_widths : length list;
      row_heights   : length list;
      rule_graphics : rules_func;
    }
  | ImHorzEmbeddedVert of {
      width    : length;
      height   : length;
      depth    : length;
      contents : intermediate_vert_box list;
    }
  | ImHorzInlineGraphics of {
      width    : length;
      height   : length;
      depth    : length;
      graphics : intermediate_graphics;
    }
  | ImHorzHookPageBreak of (page_break_info -> point -> unit)
  | ImHorzFootnote of intermediate_vert_box list

and evaled_horz_box =
  length * evaled_horz_box_main
      (* (1) width
         (2) contents *)

and evaled_horz_box_main =
  | EvHorzString of {
      info   : horz_string_info;
      height : length;
      depth  : length;
      output : OutputText.t;
    }
  | EvHorzMathGlyph of {
      info   : math_string_info;
      height : length;
      depth  : length;
      output : OutputText.t;
    }
  | EvHorzRising of {
      height   : length;
      depth    : length;
      rising   : length;
      contents : evaled_horz_box list;
    }
  | EvHorzEmpty
  | EvHorzFrame of {
      ratios     : ratios;
      height     : length;
      depth      : length;
      decoration : decoration;
      contents   : evaled_horz_box list;
    }
  | EvHorzEmbeddedVert of {
      height   : length;
      depth    : length;
      contents : evaled_vert_box list;
    }
  | EvHorzInlineGraphics of {
      height   : length;
      depth    : length;
      graphics : intermediate_graphics;
    }
  | EvHorzInlineTabular of {
      height        : length;
      depth         : length;
      rows          : evaled_row list;
      column_widths : length list;
      row_heights   : length list;
      rule_graphics : rules_func;
    }
  | EvHorzInlineImage of {
      height : length;
      key    : ImageInfo.key;
    } [@printer (fun fmt _ _ -> Format.fprintf fmt "EvHorzInlineImage(...)")]
  | EvHorzHookPageBreak of page_break_info * (page_break_info -> point -> unit)

and vert_box =
  | VertParagraph      of margins * paragraph_element list
  | VertFixedBreakable of length
      [@printer (fun fmt _ -> Format.fprintf fmt "Breakable")]
  | VertFrame          of margins * paddings * decoration * decoration * decoration * decoration * length * vert_box list
  | VertClearPage
  | VertHookPageBreak   of (page_break_info -> point -> unit)

and margins = {
  margin_top    : (breakability * length) option;
  margin_bottom : (breakability * length) option;
}

and paragraph_element =
  | VertParagLine of reachability * length * length * intermediate_horz_box list
      [@printer (fun fmt _ -> Format.fprintf fmt "Line")]
  | VertParagSkip of length

and intermediate_vert_box =
  | ImVertLine          of reachability * length * length * intermediate_horz_box list
  | ImVertFixedEmpty    of debug_margin_info * length
  | ImVertFrame         of paddings * decoration * length * intermediate_vert_box list
  | ImVertHookPageBreak of (page_break_info -> point -> unit)

and evaled_vert_box =
  | EvVertLine       of reachability * length * length * evaled_horz_box list
      [@printer (fun fmt _ -> Format.fprintf fmt "EvLine")]
      (* (1) height of the contents
         (2) depth of the contents (nonpositive)
         (3) contents *)
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

and page_parts_scheme_func =
  page_content_info -> page_parts_scheme

and column_hook_func =
  unit -> vert_box list

and math_char_kern_func =
  length -> length -> length
    (* Takes the actual font size and the y-position,
       and returns a kerning value (positive for making characters closer). *)

and math_kern_func =
  length -> length
    (* Takes a y-position as a correction height and then returns a kerning value. *)

and radical =
  length -> length -> length -> length -> color -> horz_box list
    (* The type for adjustable radicals. An adjustable radical takes as arguments

       (1-2) the height and the thickness of the bar required by the math font,
       (3)   the depth of the inner contents,
       (4)   the font size, and
       (5)   the color for glyphs,

       and then returns the inline box representation. *)

and cell =
  | NormalCell of paddings * horz_box list
  | EmptyCell
  | MultiCell  of int * int * paddings * horz_box list

and row =
  cell list

and intermediate_cell =
  | ImNormalCell of ratios * (length * length * length) * intermediate_horz_box list
  | ImEmptyCell  of length
  | ImMultiCell  of ratios * (int * int * length * length * length * length) * intermediate_horz_box list

and intermediate_row =
  length * intermediate_cell list

and evaled_cell =
  | EvNormalCell of ratios * (length * length * length) * evaled_horz_box list
  | EvEmptyCell  of length
  | EvMultiCell  of ratios * (int * int * length * length * length * length) * evaled_horz_box list

and evaled_row =
  length * evaled_cell list
[@@deriving show { with_path = false }]

type column = cell list


let normalize_script ctx script_raw =
  match script_raw with
  | CharBasis.CommonNarrow
  | CharBasis.Inherited ->
      ctx.dominant_narrow_script

  | CharBasis.CommonWide ->
      ctx.dominant_wide_script

  | _ -> script_raw


let get_font_with_ratio ctx script_raw =
  let script = normalize_script ctx script_raw in
    match ctx.font_scheme |> CharBasis.ScriptSchemeMap.find_opt script with
    | None          -> raise (FontIsNotSet{ raw = script_raw; normalized = script })
    | Some(fontsch) -> fontsch


let get_language_system ctx script_raw =
  let script = normalize_script ctx script_raw in
    match ctx.langsys_scheme |> CharBasis.ScriptSchemeMap.find_opt script with
    | None          -> CharBasis.NoLanguageSystem
    | Some(langsys) -> langsys


let get_string_info ctx script_raw =
  let (fontkey, ratio, rising_ratio) = get_font_with_ratio ctx script_raw in
    {
      font_key       = fontkey;
      text_font_size = ctx.font_size *% ratio;
      text_color     = ctx.text_color;
      rising         = ctx.manual_rising +% ctx.font_size *% rising_ratio;
    }


let get_metrics_of_evaled_horz_box ((wid, evhbmain) : evaled_horz_box) : length * length * length =
  let (hgt, dpt) =
    match evhbmain with
    | EvHorzEmpty
    | EvHorzHookPageBreak(_, _) ->
        (Length.zero, Length.zero)

    | EvHorzInlineImage{ height; _ } ->
        (height, Length.zero)

    | EvHorzString{ height; depth; _ }
    | EvHorzMathGlyph{ height; depth; _ }
    | EvHorzRising{ height; depth; _ }
    | EvHorzFrame{ height; depth; _ }
    | EvHorzEmbeddedVert{ height; depth; _ }
    | EvHorzInlineGraphics{ height; depth; _ }
    | EvHorzInlineTabular{ height; depth; _ } ->
        (height, depth)
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
      | ImHorzFootnote(_) ->
          (Length.zero, Length.zero, Length.zero)

      | ImHorzRising{ width; height; depth; _ }
      | ImHorzFrame{ width; height; depth; _ }
      | ImHorzInlineTabular{ width; height; depth; _ }
      | ImHorzEmbeddedVert{ width; height; depth; _ }
      | ImHorzInlineGraphics{ width; height; depth; _ } ->
          (width, height, depth)
    in
    (wid +% w, Length.max hgt h, Length.min dpt d)
  ) (Length.zero, Length.zero, Length.zero)


let rec extract_string (hblst : horz_box list) : string =
  let extract_one hb =
    match hb with
    | HorzPure(PHCInnerString{ chars = uchs; _ })  -> string_of_uchar_list uchs
    | HorzPure(PHCInnerMathGlyph(_))               -> ""
    | HorzPure(PHGRising{ contents = hbs; _ })     -> extract_string hbs
    | HorzPure(PHGFixedFrame{ contents = hbs; _ }) -> extract_string hbs
    | HorzPure(PHGInnerFrame{ contents = hbs; _ }) -> extract_string hbs
    | HorzPure(PHGOuterFrame{ contents = hbs; _ }) -> extract_string hbs
    | HorzDiscretionary{ no_break = hbs0; _ }      -> extract_string hbs0
    | HorzFrameBreakable{ contents = hbs; _ }      -> extract_string hbs
    | HorzScriptGuard{ contents = hbs; _ }         -> extract_string hbs
    | _                                            -> ""
  in
  String.concat "" (List.map extract_one hblst)
