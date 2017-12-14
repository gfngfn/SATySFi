
open HorzBox


type metrics = length_info * length * length

type lb_pure_box =
  | LBAtom          of metrics * evaled_horz_box_main
  | LBRising        of metrics * length * lb_pure_box list
  | LBOuterFrame    of metrics * decoration * lb_pure_box list
  | LBFixedFrame    of length * length * length * decoration * lb_pure_box list
  | LBEmbeddedVert  of length * length * length * evaled_vert_box list
  | LBFixedGraphics of length * length * length * (point -> Pdfops.t list)

type lb_box =
  | LBPure           of lb_pure_box
  | LBDiscretionary  of pure_badness * DiscretionaryID.t * lb_pure_box list * lb_pure_box list * lb_pure_box list
  | LBFrameBreakable of paddings * length * length * decoration * decoration * decoration * decoration * lb_box list


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
