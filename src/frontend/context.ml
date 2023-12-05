
open Types
open LengthInterface


type t = input_context


let convert_math_variant_char ((ctx, _) : input_context) (uch : Uchar.t) =
  let open HorzBox in
  match ctx.math_class_map |> HorzBox.MathClassMap.find_opt uch with
  | Some((uch_after, mk)) ->
      (mk, uch_after)

  | None ->
      begin
        match ctx.math_variant_char_map |> HorzBox.MathVariantCharMap.find_opt uch with
        | Some(f) ->
            let (uch_after, mk) = f ctx.math_char_class in
            (mk, uch_after)

        | None ->
            (MathOrdinary, uch)
      end


let color ((ctx, _) : t) =
  ctx.HorzBox.text_color


let math_char_class ((ctx, _) : t) =
  ctx.HorzBox.math_char_class


let set_math_char_class mccls ((ctx, ctxsub) : t) =
  ({ ctx with HorzBox.math_char_class = mccls }, ctxsub)


let font_size ((ctx, _) : t)  =
  ctx.font_size


let math_font_key_exn ((ctx, _) : t) =
  match ctx.math_font_key with
  | None          -> raise HorzBox.MathFontIsNotSet
  | Some(mathkey) -> mathkey


let get_math_constants (ictx : input_context) =
  let mathkey = math_font_key_exn ictx in
  FontInfo.get_math_constants mathkey


let enter_script (ictx : t) : t =
  let mathkey = math_font_key_exn ictx in
  let mc = FontInfo.get_math_constants mathkey in
  let (ctx, ctxsub) = ictx in
  let size = ctx.font_size in
  let ctx =
    match ctx.math_script_level with
    | BaseLevel ->
        { ctx with
          font_size         = size *% mc.FontFormat.script_scale_down;
          math_script_level = ScriptLevel;
        }

    | ScriptLevel ->
        { ctx with
          font_size         = size *% (mc.FontFormat.script_script_scale_down /. mc.FontFormat.script_scale_down);
          math_script_level = ScriptScriptLevel;
        }

    | ScriptScriptLevel ->
        ctx
  in
  (ctx, ctxsub)


let is_in_base_level ((ctx, _) : t) =
  match ctx.math_script_level with
  | BaseLevel -> true
  | _         -> false


let get_math_string_info (ictx : t) : HorzBox.math_string_info =
  {
    info_math_font_key  = math_font_key_exn ictx;
    info_math_font_size = font_size ictx;
    info_math_color     = color ictx;
  }
