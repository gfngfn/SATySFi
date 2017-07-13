
module SkipLength
: sig
    type t
    val zero : t
    val add : t -> t -> t
    val subtr : t -> t -> t
    val mult : t -> float -> t
    val div : t -> t -> float
    val less_than : t -> t -> bool
    val is_nearly_zero : t -> bool
    val of_pdf_point : float -> t
    val to_pdf_point : t -> float
    val show : t -> string
  end
= struct

    type t = float

    let zero = 0.0

    let add = ( +. )

    let subtr = ( -. )

    let mult = ( *. )

    let div = ( /. )

    let less_than = ( < )

    let is_nearly_zero sl = (sl < 0.01)

    let of_pdf_point pt = pt

    let to_pdf_point sl = sl

    let show = string_of_float
  end

let ( +% ) = SkipLength.add
let ( -% ) = SkipLength.subtr
let ( *% ) = SkipLength.mult
let ( /% ) = SkipLength.div
let ( <% ) = SkipLength.less_than

type skip_width  = SkipLength.t
type skip_height = SkipLength.t
type skip_depth  = SkipLength.t

type pure_badness = int

type badness =
  | TooShort
  | Badness of pure_badness
  | TooLong

type font_abbrev = string

type font_info = font_abbrev * SkipLength.t

type horz_fixed_atom =
  | FixedString of font_info * string

type horz_outer_atom =
  | OuterEmpty of skip_width * skip_width * skip_width

type horz_box =
  | HorzFixedBoxAtom  of horz_fixed_atom
  | HorzOuterBoxAtom  of horz_outer_atom
  | HorzDiscretionary of horz_box option * horz_box option * horz_box option

type evaled_horz_box =
  | EvHorzFixedBoxAtom of skip_width * horz_fixed_atom
  | EvHorzOuterBoxAtom of skip_width * horz_outer_atom

type evaled_vert_box =
  | EvVertLine of evaled_horz_box list
