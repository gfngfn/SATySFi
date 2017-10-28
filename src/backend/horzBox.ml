
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
let ( /% ) = Length.div
let ( <% ) = Length.less_than
let ( <=% ) = Length.leq


let ( @|> ) = ( |> )
  (* ----
      right-associative version;
      `y @|> x @|> f ` is equivalent to `f x y`
     ---- *)


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

type file_path = string

type encoding_in_pdf =
  | Latin1
  | UTF16BE
  | IdentityH

type font_info = font_abbrev * Length.t  [@@deriving show]

type paddings =
  {
    paddingL : length;
    paddingR : length;
    paddingT : length;
    paddingB : length;
  }
[@@deriving show]

(* -- representation about graphics based on PDF 1.7 specification -- *)

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

type color =
  | DeviceGray of float
  | DeviceRGB  of float * float * float
  | DeviceCMYK of float * float * float * float

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
  }

let pp_horz_string_info fmt info =
  Format.fprintf fmt "(HSinfo)"

type pure_horz_box =
  | PHOuterEmpty  of length * length * length
  | PHOuterFil
  | PHOuterFrame  of paddings * decoration * horz_box list
  | PHFixedString of horz_string_info * Uchar.t list
      [@printer (fun fmt _ -> Format.fprintf fmt "FixedString(...)")]
  | PHFixedEmpty  of length
  | PHFixedFrame  of paddings * length * decoration * horz_box list
  | PHInnerFrame  of paddings * decoration * horz_box list
  | PHEmbeddedVert of length * length * length * evaled_vert_box list
(* -- core part of the definition of horizontal boxes -- *)

and horz_box =
  | HorzPure           of pure_horz_box
  | HorzDiscretionary  of pure_badness * pure_horz_box option * pure_horz_box option * pure_horz_box option
      [@printer (fun fmt _ -> Format.fprintf fmt "HorzDiscretionary(...)")]
  | HorzFrameBreakable of paddings * length * length * decoration * decoration * decoration * decoration * horz_box list

and evaled_horz_box_main =
  | EvHorzString of horz_string_info * OutputText.t
  | EvHorzEmpty
  | EvHorzFrame  of length * length * decoration * evaled_horz_box list
  | EvHorzEmbeddedVert of length * length * evaled_vert_box list

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

(*
let rec pp_list pp fmt = function
| [] -> ()
| v :: vs ->
    pp fmt v; if vs <> [] then (Format.printf fmt "; "; pp_list pp fmt vs)

let pp_intermediate_vert_box ppf x imvb =
  Format.fprintf ppf "(imvb)"

let pp_horz_box ppf hb =
  Format.pp_print_string ppf "(hb)"
*)
