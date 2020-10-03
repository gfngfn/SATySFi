
open LengthInterface
open HorzBox
open CharBasis


type metrics = length_info * length * length

type lb_pure_box =
  | LBAtom          of metrics * evaled_horz_box_main
  | LBRising        of metrics * length * lb_pure_box list
  | LBLinearTrans   of metrics * (float * float * float * float) * lb_pure_box list
  | LBOuterFrame    of metrics * decoration * lb_pure_box list
  | LBFixedFrame    of length * length * length * decoration * lb_pure_box list
  | LBEmbeddedVert  of length * length * length * intermediate_vert_box list
  | LBFixedGraphics of length * length * length * fixed_graphics
  | LBOuterFilGraphics of length * length * outer_fil_graphics
  | LBFixedTabular  of length * length * length * intermediate_row list * length list * length list * rules_func
  | LBFixedImage    of length * length * ImageInfo.key
  | LBHookPageBreak of (page_break_info -> point -> unit)
  | LBFootnote      of intermediate_vert_box list

type lb_box =
  | LBPure           of lb_pure_box
  | LBDiscretionary  of pure_badness * DiscretionaryID.t * lb_pure_box list * lb_pure_box list * lb_pure_box list
  | LBDiscretionaryList of pure_badness * lb_pure_box list * (DiscretionaryID.t * lb_pure_box list * lb_pure_box list) list
  | LBFrameBreakable of paddings * length * length * decoration * decoration * decoration * decoration * lb_box list
  | LBEmbeddedVertBreakable of DiscretionaryID.t * length * vert_box list


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
