
type skip_length = int
type skip_width  = skip_length
type skip_height = skip_length
type skip_depth  = skip_length

type badness = int

type font_abbrev = string

type font_info = font_abbrev * int

type horz_fixed_atom =
  | FixedString of font_info * string

type horz_outer_atom =
  | OuterEmpty of skip_width * (skip_width -> badness)

type horz_box =
  | HorzFixedBoxAtom  of horz_fixed_atom
  | HorzOuterBoxAtom  of horz_outer_atom
  | HorzDiscretionary of horz_box * horz_box * horz_box
