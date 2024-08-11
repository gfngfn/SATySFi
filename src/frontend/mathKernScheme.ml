
open Types
open LengthInterface


type t =
  | NoMathKern
  | DiscreteMathKern of FontFormat.math_kern
  | DenseMathKern    of HorzBox.math_kern_func


let zero : t =
  NoMathKern


let make_discrete (mkern : FontFormat.math_kern) : t =
  DiscreteMathKern(mkern)


let make_dense (kernf : HorzBox.math_kern_func) : t =
  DenseMathKern(kernf)


let calculate (ictx : input_context) (math_kern_scheme : t) (corrhgt : length) : length =
  let fontsize = Context.font_size ictx in
  let mathkey = Context.math_font_key_exn ictx in
  match math_kern_scheme with
  | NoMathKern ->
      Length.zero

  | DiscreteMathKern(mkern) ->
      let ratiok = FontInfo.get_math_kern_ratio mathkey mkern (corrhgt /% fontsize) in
      fontsize *% ratiok

  | DenseMathKern(kernf) ->
      Length.negate (kernf corrhgt)
