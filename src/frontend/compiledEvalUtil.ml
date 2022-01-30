
open SyntaxBase
open Types
open CompiledTypes


let report_bug_vm msg =
  Format.printf "[Bug]@ %s:" msg;
  failwith ("bug: " ^ msg)


let report_bug_value msg value =
  Format.printf "[Bug]@ %s:" msg;
  Format.printf "%a" pp_compiled_value value;
  failwith ("bug: " ^ msg)


let get_context (value : compiled_value) : input_context =
  match value with
  | CVContext(ictx) -> ictx
  | _               -> report_bug_value "get_context" value


let get_string (value : compiled_value) : string =
  match value with
  | CVBaseConstant(BCString(s)) -> s
  | _                           -> report_bug_value "get_string" value


let get_vert (value : compiled_value) : HorzBox.vert_box list =
  match value with
  | CVBaseConstant(BCVert(vblst)) -> vblst
  | _                             -> report_bug_value "get_vert" value


let get_text_mode_context (value : compiled_value) : TextBackend.text_mode_context =
  match value with
  | CVBaseConstant(BCTextModeContext(tctx)) -> tctx
  | _                                       -> report_bug_value "get_text_mode_context" value


let get_code (value : compiled_value) : code_value =
  match value with
  | CVCodeValue(cv) -> cv
  | _               -> report_bug_value "get_code" value


let make_string s = CVBaseConstant(BCString(s))
let make_vert v = CVBaseConstant(BCVert(v))
