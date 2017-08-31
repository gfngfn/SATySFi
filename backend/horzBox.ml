
module SkipLength
: sig
    type t
    val zero : t
    val add : t -> t -> t
    val subtr : t -> t -> t
    val mult : t -> float -> t
    val div : t -> t -> float
    val max : t -> t -> t
    val min : t -> t -> t
    val less_than : t -> t -> bool
    val leq : t -> t -> bool
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
    let max = max
    let min = min
    let less_than = ( < )
    let leq = ( <= )
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
let ( <=% ) = SkipLength.leq


let ( @|> ) = ( |> )
  (* ----
      right-associative version;
      `y @|> x @|> f ` is equivalent to `f x y`
     ---- *)


type skip_width  = SkipLength.t
type skip_height = SkipLength.t
type skip_depth  = SkipLength.t

type skip_info =
  {
    natural     : skip_width;
    shrinkable  : skip_width;
    stretchable : skip_width;
    fils        : int;
  }

type pure_badness = int

type badness =
  | TooShort
  | Badness of pure_badness
  | TooLong of pure_badness

type font_abbrev = string

type file_path = string

type encoding_in_pdf =
  | Latin1
  | UTF16BE
  | IdentityH

type font_info = font_abbrev * SkipLength.t

type pure_horz_box =
  | PHOuterEmpty  of skip_width * skip_width * skip_width
  | PHOuterFil
  | PHOuterFrame  of horz_box list
  | PHFixedString of font_info * InternalText.t
  | PHFixedEmpty  of skip_width
(* -- core part of the definition of horizontal boxes -- *)

and horz_box =
  | HorzPure          of pure_horz_box
  | HorzDiscretionary of pure_badness * pure_horz_box option * pure_horz_box option * pure_horz_box option

type evaled_horz_box_main =
  | EvHorzString of font_info * OutputText.t
  | EvHorzEmpty
  | EvHorzFrame  of evaled_horz_box list

and evaled_horz_box =
  | EvHorz of skip_width * evaled_horz_box_main

type vert_box =
  | VertParagraph      of skip_height * horz_box list  (* temporary; should contain more information as arguments *)
  | VertFixedBreakable of skip_height

type intermediate_vert_box =
  | ImVertLine           of skip_height * skip_depth * evaled_horz_box list
  | ImVertFixedBreakable of skip_height
(*
  | ImVertUnbreakableSkip of skip_height * skip_height * skip_height
*)

type evaled_vert_box =
  | EvVertLine       of skip_height * skip_depth * evaled_horz_box list
  | EvVertFixedEmpty of skip_height
