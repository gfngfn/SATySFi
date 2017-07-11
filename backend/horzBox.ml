
type skip_length = int
type skip_width  = skip_length
type skip_height = skip_length
type skip_depth  = skip_length

type pure_badness = int

type badness =
  | TooShort
  | Badness of pure_badness
  | TooLong

type font_abbrev = string

type font_info = font_abbrev * int

type horz_fixed_atom =
  | FixedString of font_info * string

type horz_outer_atom =
  | OuterEmpty of skip_width * (skip_width -> pure_badness)

type horz_box =
  | HorzFixedBoxAtom  of horz_fixed_atom
  | HorzOuterBoxAtom  of horz_outer_atom
  | HorzDiscretionary of horz_box option * horz_box option * horz_box option

type evaled_horz_box =
  | EvHorzFixedBoxAtom of skip_width * horz_fixed_atom
  | EvHorzOuterBoxAtom of skip_width * horz_outer_atom

type evaled_vert_box =
  | EvVertLine of evaled_horz_box list
