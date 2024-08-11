open U


module Field = struct
  type t = {
    name  : string;
    type_ : string;
  }

  let name t = t.name

  let type_ t = t.type_
end


module Param = struct
  type t = {
    name  : string;
    type_ : string option;
  }

  let name t = t.name

  let type_ t = t.type_
end


type pp =
  | Default
  | Custom of string
  | Simple


type t = {
  inst                   : string;
  is_pdf_mode_primitive  : bool;
  is_text_mode_primitive : bool;
  needs_reducef          : bool;
  needs_runtime_config   : bool;
  no_interp              : bool;
  no_ircode              : bool;
  pp                     : pp;
  name                   : string option;
  type_                  : Type.t option;
  fields                 : Field.t list;
  params                 : Param.t list;
  code                   : string;
  code_interp            : string option;
}


let is_primitive t =
  t.is_pdf_mode_primitive || t.is_text_mode_primitive


let inst
    ?(is_pdf_mode_primitive = false)
    ?(is_text_mode_primitive = false)
    ?(needs_reducef = false)
    ?(needs_runtime_config = false)
    ?no_interp
    ?(no_ircode = false)
    ?(pp = Default)
    ?name
    ?type_
    ~fields
    ~params
    ?code_interp
    ~code
    inst =
  {
    inst;
    is_pdf_mode_primitive;
    is_text_mode_primitive;
    needs_reducef;
    needs_runtime_config;
    no_interp = default (not (is_pdf_mode_primitive || is_text_mode_primitive)) no_interp;
    no_ircode;
    pp;
    name;
    type_;
    fields;
    params;
    code_interp = opt_map trim code_interp;
    code = trim code;
  }


let field ~type_ name =
  { Field.name; type_ }


let param ?type_ name =
  { Param.name; type_ }
