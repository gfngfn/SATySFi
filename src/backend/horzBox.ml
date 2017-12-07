
module Length
: sig
    type t  [@@deriving show]
    val zero : t
    val add : t -> t -> t
    val subtr : t -> t -> t
    val mult : t -> float -> t
    val div : t -> t -> float
    val max : t -> t -> t
    val min : t -> t -> t
    val negate : t -> t
    val abs : t -> t
    val less_than : t -> t -> bool
    val leq : t -> t -> bool
    val is_nearly_zero : t -> bool
    val of_pdf_point : float -> t
    val to_pdf_point : t -> float
    val of_centimeter : float -> t
    val of_millimeter : float -> t
    val of_inch : float -> t
    val show : t -> string
  end
= struct

    type t = float  [@@deriving show]
    let zero = 0.
    let add = ( +. )
    let subtr = ( -. )
    let mult = ( *. )
    let div = ( /. )
    let max = max
    let min = min
    let negate x = 0. -. x
    let abs x = if x < 0. then -.x else x
    let less_than = ( < )
    let leq = ( <= )
    let is_nearly_zero sl = (sl < 0.01)

    let of_pdf_point pt = pt
    let to_pdf_point len = len

    let convert pdfunit flt =
      let dpi = 72. in  (* temporary; dpi *)
        Pdfunits.convert dpi pdfunit Pdfunits.PdfPoint flt      

    let of_centimeter = convert Pdfunits.Centimetre
    let of_millimeter = convert Pdfunits.Millimetre
    let of_inch       = convert Pdfunits.Inch

    let show = string_of_float
  end

let ( +% ) = Length.add
let ( -% ) = Length.subtr
let ( *% ) = Length.mult
let ( *%! ) l n = l *% (float_of_int n)
let ( /% ) = Length.div
let ( <% ) = Length.less_than
let ( <=% ) = Length.leq


type length = Length.t  [@@deriving show]

type point = length * length

type stretchable =
  | FiniteStretch of length
  | Fils          of int

let add_stretchable strc1 strc2 =
  match (strc1, strc2) with
  | (FiniteStretch(w1), FiniteStretch(w2)) -> FiniteStretch(w1 +% w2)
  | (Fils(i1), Fils(i2))                   -> Fils(i1 + i2)
  | (Fils(i1), _)                          -> Fils(i1)
  | (_, Fils(i2))                          -> Fils(i2)
  
type length_info =
  {
    natural     : length;
    shrinkable  : length;
    stretchable : stretchable;
  }

type pure_badness = int

type ratios =
  | TooShort
  | PermissiblyShort of float
  | PermissiblyLong  of float
  | TooLong

type font_abbrev = string  [@@deriving show]

type math_font_abbrev = string  [@@deriving show]

type file_path = string

type encoding_in_pdf =
  | Latin1
  | UTF16BE
  | IdentityH

type font_with_size = font_abbrev * Length.t  [@@deriving show]

type font_with_ratio = font_abbrev * float * float  [@@deriving show]

type page_size =
  | A4Paper
  | UserDefinedPaper of length * length

type page_scheme =
  {
    page_size        : page_size;
    left_page_margin : length;
    top_page_margin  : length;
    area_width       : length;
    area_height      : length;
  }
    

type paddings =
  {
    paddingL : length;
    paddingR : length;
    paddingT : length;
    paddingB : length;
  }
[@@deriving show]

(* -- representation about graphics based on PDF 1.7 specification -- *)

type color =
  | DeviceGray of float
  | DeviceRGB  of float * float * float
  | DeviceCMYK of float * float * float * float

type 'a path_element =
  | LineTo              of 'a
  | CubicBezierTo       of point * point * 'a

type path =
  | GeneralPath of point * (point path_element) list * (unit path_element) option
  | Rectangle   of point * point

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

(* -- internal representation of boxes -- *)

type decoration = point -> length -> length -> length -> Pdfops.t list
[@@deriving show]

type horz_string_info =
  {
    font_abbrev : font_abbrev;
    font_size   : length;
    text_color  : color;
    rising      : length;
  }

type math_string_info =
  {
    math_font_abbrev : math_font_abbrev;
    math_font_size   : length;
  }

let pp_horz_string_info fmt info =
  Format.fprintf fmt "(HSinfo)"

type pure_horz_box =
  | PHOuterEmpty     of length * length * length
  | PHOuterFil
  | PHOuterFrame     of paddings * decoration * horz_box list
  | PHFixedString    of horz_string_info * Uchar.t list
      [@printer (fun fmt _ -> Format.fprintf fmt "@[FixedString(...)@]")]
  | PHFixedMathGlyph of math_string_info * length * length * length * FontFormat.glyph_id
      [@printer (fun fmt _ -> Format.fprintf fmt "@[FixedMathGlyph(...)@]")]
  | PHRising         of length * horz_box list
  | PHFixedEmpty     of length
  | PHFixedFrame     of paddings * length * decoration * horz_box list
  | PHInnerFrame     of paddings * decoration * horz_box list
  | PHEmbeddedVert   of length * length * length * evaled_vert_box list
  | PHInlineGraphics of length * length * length * (point -> Pdfops.t list)
(* -- core part of the definition of horizontal boxes -- *)

and horz_box =
  | HorzPure           of pure_horz_box
  | HorzDiscretionary  of pure_badness * pure_horz_box option * pure_horz_box option * pure_horz_box option
      [@printer (fun fmt _ -> Format.fprintf fmt "HorzDiscretionary(...)")]
  | HorzFrameBreakable of paddings * length * length * decoration * decoration * decoration * decoration * horz_box list

and evaled_horz_box_main =
  | EvHorzString         of horz_string_info * length * length * OutputText.t
  | EvHorzMathGlyph      of math_string_info * length * length * FontFormat.glyph_id
      [@printer (fun fmt _ -> Format.fprintf fmt "EvHorzMathGlyph(...)")]
  | EvHorzRising         of length * length * length * evaled_horz_box list
  | EvHorzEmpty
  | EvHorzFrame          of length * length * decoration * evaled_horz_box list
  | EvHorzEmbeddedVert   of length * length * evaled_vert_box list
  | EvHorzInlineGraphics of length * length * (point -> Pdfops.t list)

and evaled_horz_box =
  | EvHorz of length * evaled_horz_box_main

and intermediate_vert_box =
  | ImVertLine              of length * length * evaled_horz_box list
      [@printer (fun fmt _ -> Format.fprintf fmt "Line")]
  | ImVertFixedBreakable    of length
      [@printer (fun fmt _ -> Format.fprintf fmt "Breakable")]
  | ImVertTopMargin         of bool * length
      [@printer (fun fmt _ -> Format.fprintf fmt "Top")]
  | ImVertBottomMargin      of bool * length
      [@printer (fun fmt _ -> Format.fprintf fmt "Bottom")]
  | ImVertFrame             of paddings * decoration * decoration * decoration * decoration * length * intermediate_vert_box list
(*      [@printer (fun fmt (_, _, _, _, _, imvblst) -> Format.fprintf fmt "%a" (pp_list pp_intermediate_vert_box) imvblst)] *)
and evaled_vert_box =
  | EvVertLine       of length * length * evaled_horz_box list
      [@printer (fun fmt _ -> Format.fprintf fmt "EvLine")]
  | EvVertFixedEmpty of length
      [@printer (fun fmt _ -> Format.fprintf fmt "EvEmpty")]
  | EvVertFrame      of paddings * decoration * length * evaled_vert_box list
[@@deriving show]

type vert_box =
  | VertParagraph      of length * horz_box list  (* temporary; should contain more information as arguments *)
  | VertFixedBreakable of length


module MathContext
: sig
    type t
    val make : math_font_abbrev -> length -> t
    val enter_script : t -> t
    val is_in_base_level : t -> bool
  end
= struct
    type level =
      | BaseLevel
      | ScriptLevel
      | ScriptScriptLevel

    type t =
      {
        font_abbrev    : math_font_abbrev;
        base_font_size : length;
        level_int      : int;
        level          : level;
      }

    let make mfabbrev basesize =
      {
        font_abbrev    = mfabbrev;
        base_font_size = basesize;
        level_int      = 0;
        level          = BaseLevel;
      }

    let enter_script mctx =
      let levnew = mctx.level_int + 1 in
      match mctx.level with
      | BaseLevel         -> { mctx with level = ScriptLevel;       level_int = levnew; }
      | ScriptLevel       -> { mctx with level = ScriptScriptLevel; level_int = levnew; }
      | ScriptScriptLevel -> { mctx with                            level_int = levnew; }

    let is_in_base_level mctx =
      match mctx.level with
      | BaseLevel -> true
      | _         -> false

  end


type math_context =
  {
    math_context_font_abbrev : math_font_abbrev;
    math_context_font_size   : length;
  }

type math_element_main =
  | MathChar         of math_context * Uchar.t
  | MathEmbeddedHorz of horz_box list

type math_kind =
  | MathOrdinary
  | MathBinary
  | MathRelation
  | MathOperator
  | MathOpen
  | MathClose
  | MathPrefix    (* -- mainly for differantial operator 'd', '\partial', etc. -- *)
  | MathInner
  | MathEnd
[@@deriving show]

type math_element = math_kind * math_element_main

type math_kern_func = length -> length

type paren = length -> length -> length -> length -> horz_box list * math_kern_func
  (* --
     paren:
       the type for adjustable parentheses.
       An adjustable parenthesis takes as arguments
       (1-2) the height and the depth of the inner contents,
       (3)   the axis height, and
       (4)   the font size,
       and then returns its inline box representation and the function for kerning.
     -- *)

type radical = length -> length -> length -> length -> horz_box list
  (* --
     radical:
       the type for adjustable radicals.
       An adjustable radical takes as arguments
       (1-2) the height and the thickness of the bar required by the math font,
       (3)   the depth of the inner contents, and
       (4)   the font size,
       and then returns the inline box representation.
     -- *)

type math =
  | MathPure              of math_element
  | MathGroup             of math_kind * math_kind * math list
  | MathSubscript         of math list * math list
  | MathSuperscript       of math list * math list
  | MathFraction          of math list * math list
  | MathRadicalWithDegree of math list * math list
  | MathRadical           of radical * math list
  | MathParen             of paren * paren * math list
  | MathUpperLimit        of math list * math list
  | MathLowerLimit        of math list * math list
