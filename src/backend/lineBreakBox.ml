
open LengthInterface
open HorzBox
open CharBasis


type metrics = length_info * length * length

type lb_pure_box =
  | LBAtom of {
      metrics : metrics;
      main    : evaled_horz_box_main;
    }
  | LBRising of {
      metrics  : metrics;
      rising   : length;
      contents : lb_pure_box list;
    }
  | LBOuterFrame of {
      metrics    : metrics;
      decoration : decoration;
      contents   : lb_pure_box list;
    }
  | LBFixedFrame of {
      width      : length;
      height     : length;
      depth      : length;
      decoration : decoration;
      contents   : lb_pure_box list;
    }
  | LBEmbeddedVert of {
      width    : length;
      height   : length;
      depth    : length;
      contents : intermediate_vert_box list;
    }
  | LBFixedGraphics of {
      width    : length;
      height   : length;
      depth    : length;
      graphics : fixed_graphics;
    }
  | LBOuterFilGraphics of {
      height   : length;
      depth    : length;
      graphics : outer_fil_graphics;
    }
  | LBFixedTabular of {
      width         : length;
      height        : length;
      depth         : length;
      rows          : intermediate_row list;
      column_widths : length list;
      row_heights   : length list;
      rule_graphics : rules_func;
    }
  | LBFixedImage    of {
      width  : length;
      height : length;
      key    : ImageInfo.key;
    }
  | LBHookPageBreak of (page_break_info -> point -> unit)
  | LBFootnote of intermediate_vert_box list

type lb_box =
  | LBPure of lb_pure_box
  | LBDiscretionary of {
      penalty  : pure_badness;
      id       : DiscretionaryID.t;
      no_break : lb_pure_box list;
      pre      : lb_pure_box list;
      post     : lb_pure_box list;
    }
  | LBDiscretionaryList of {
      penalty    : pure_badness;
      no_break   : lb_pure_box list;
      candidates : (DiscretionaryID.t * lb_pure_box list * lb_pure_box list) list;
    }
  | LBFrameBreakable of {
      paddings              : paddings;
      decoration_standalone : decoration;
      decoration_head       : decoration;
      decoration_middle     : decoration;
      decoration_tail       : decoration;
      contents              : lb_box list;
    }
  | LBEmbeddedVertBreakable of {
      id       : DiscretionaryID.t;
      width    : length;
      contents : vert_box list;
    }


let make_width_info widnat widshrink widstretch =
  {
    natural     = widnat;
    shrinkable  = widshrink;
    stretchable = FiniteStretch(widstretch);
  }


let widinfo_zero =
  {
    natural     = Length.zero;
    shrinkable  = Length.zero;
    stretchable = FiniteStretch(Length.zero);
  }


let ( +%@ ) wi1 wi2 =
  {
    natural     = wi1.natural +% wi2.natural;
    shrinkable  = wi1.shrinkable +% wi2.shrinkable;
    stretchable = add_stretchable wi1.stretchable wi2.stretchable;
  }


let natural wid =
  {
    natural     = wid;
    shrinkable  = Length.zero;
    stretchable = FiniteStretch(Length.zero);
  }


type line_break_chunk_main =
  | AlphabeticChunk  of script * line_break_class * line_break_class * uchar_segment list * break_opportunity
      (* --
         (1) script
         (2) line break class for the previous chunk
         (3) line break class for the next chunk
         (4) text contents
         (5) whether breaking line immediate after the chunk is allowed
         -- *)
  | Space
  | UnbreakableSpace
  | IdeographicChunk of script * line_break_class * uchar_segment * break_opportunity
      (* --
         (1) script
         (2) line break class
         (3) character content
         (4) whether breaking line immediate after the chunk is allowed
         -- *)

type line_break_chunk = context_main * line_break_chunk_main
