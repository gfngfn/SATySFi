(**
   {1 SATySFi virtual machine instruction definitions}

   This file is a part of [gencode.exe].
   [gencode.exe] generates OCaml source code and
   type declarations included from

   - [types_.cppo.ml],
   - [ir_.cppo.ml],
   - [evaluator_.cppo.ml], and
   - [vm_.cppo.ml].

   To add a new primitive,

   + Add a new instruction definition to this file.
      ([is_pdf_mode_primitive] or [is_text_mode_primitive] should be [true].)
   + Add a new entry to [primitives.ml].
*)

let def =
  Instruction.(
    [ inst "AccessField"
        ~fields:[
          field "field_nm" ~type_:"field_name";
        ]
        ~params:[
          param "value1";
        ]
        ~code:{|
match value1 with
| RecordValue(asc1) ->
    begin
      match Assoc.find_opt asc1 field_nm with
      | Some(v) -> exec (v :: stack) env code dump
      | None    -> report_bug_vm ("AccessField field " ^ field_nm ^ " not found")
    end

| _ -> report_bug_vm "not a Record"
|}
    ; inst "Forward"
        ~fields:[
          field "n" ~type_:"int";
        ]
        ~params:[
        ]
        ~code:{|
let (vs, stack) = popn stack n in
match stack with
| v0 :: stack ->
    exec (v0 :: List.rev_append vs stack) env code dump
| _ ->
    report_bug_vm "Forward: stack underflow"
|}
    ; inst "Apply"
        ~fields:[
          field "n" ~type_:"int";
        ]
        ~params:[
          param "f";
        ]
        ~code:{|
match f with
| CompiledFuncWithEnvironment(optvars, arity, pargs, framesize, body, env1) ->
    let body =
      optvars |> List.fold_left (fun acc optvar ->
        let bindop =
          match optvar with
          | GlobalVar(loc, evid, refs)    -> OpBindGlobal(loc, evid, !refs)
          | LocalVar(lv, off, evid, refs) -> OpBindLocal(lv, off, evid, !refs)
        in
          OpPush(Constructor("None", UnitConstant)) :: bindop :: acc
      ) body
    in
    if arity = n then
      begin
       if pargs = [] then
          exec stack (newframe env1 framesize) body ((env, code) :: dump)
        else
          let (args, stack) = popn stack n in
          let allargs = List.rev (pargs @ args) in
            exec (allargs @ stack) (newframe env1 framesize) body ((env, code) :: dump)
      end

    else if arity > n then
      let (args, stack) = popn stack n in
      let applied = CompiledFuncWithEnvironment([], arity - n, pargs @ args, framesize, body, env1) in
        exec (applied :: stack) env code dump

    else
      let (surplus, stack) = popn stack (n - arity) in
      let (args, stack) = popn stack arity in
      let allargs = List.rev (pargs @ args) in
        exec (allargs @ stack) (newframe env1 framesize) body ((env, OpInsertArgs(surplus) :: OpApply(n - arity) :: code) :: dump)

| CompiledPrimitiveWithEnvironment(arity, [], framesize, body, env1, astf) ->
    if arity = n then
      exec stack (newframe env1 framesize) body ((env, code) :: dump)

    else if arity > n then
      let (args, stack) = popn stack n in
       let applied = CompiledFuncWithEnvironment([], arity - n, args, framesize, body, env1) in
         exec (applied :: stack) env code dump

    else
      let (surplus, stack) = popn stack (n - arity) in
        exec stack (newframe env1 framesize) body ((env, OpInsertArgs(surplus) :: OpApply(n - arity) :: code) :: dump)

 | _ ->
     report_bug_vm_value "Apply: not a function" f
|}
    ; inst "ApplyT"
        ~fields:[
          field "n" ~type_:"int";
        ]
        ~params:[
          param "f";
        ]
        ~code:{|
match f with
| CompiledFuncWithEnvironment(optvars, arity, pargs, framesize, body, env1) ->
    let body =
      optvars |> List.fold_left (fun acc optvar ->
        let bindop =
          match optvar with
          | GlobalVar(loc, evid, refs)    -> OpBindGlobal(loc, evid, !refs)
          | LocalVar(lv, off, evid, refs) -> OpBindLocal(lv, off, evid, !refs)
        in
          OpPush(Constructor("None", UnitConstant)) :: bindop :: acc
      ) body
    in
    if arity = n then
      begin
       if pargs = [] then
          exec stack (newframe env1 framesize) body dump
        else
          let (args, stack) = popn stack n in
          let allargs = List.rev (pargs @ args) in
            exec (allargs @ stack) (newframe env1 framesize) body dump
      end

    else if arity > n then
      let (args, stack) = popn stack n in
      let applied = CompiledFuncWithEnvironment([], arity - n, pargs @ args, framesize, body, env1) in
        exec (applied :: stack) env code dump

    else
      let (surplus, stack) = popn stack (n - arity) in
      let (args, stack) = popn stack arity in
      let allargs = List.rev (pargs @ args) in
        exec (allargs @ stack) (newframe env1 framesize) body ((env, OpInsertArgs(surplus) :: OpApplyT(n - arity) :: code) :: dump)

 | CompiledPrimitiveWithEnvironment(arity, [], framesize, body, env1, astf) ->
     if arity = n then
       exec stack (newframe env1 framesize) body dump

     else if arity > n then
       let (args, stack) = popn stack n in
        let applied = CompiledFuncWithEnvironment([], arity - n, args, framesize, body, env1) in
          exec (applied :: stack) env code dump

     else
       let (surplus, stack) = popn stack (n-arity) in
         exec stack (newframe env1 framesize) body ((env, OpInsertArgs(surplus) :: OpApplyT(n - arity) :: code) :: dump)

 | _ ->
     report_bug_vm_value "ApplyT: not a function" f
|}
    ; inst "ApplyOptional"
        ~fields:[
        ]
        ~params:[
          param "f";
          param "v";
        ]
        ~code:{|
match f with
| CompiledFuncWithEnvironment(var :: vars, arity, pargs, framesize, body, env1) ->
    let bindop =
      match var with
      | GlobalVar(loc, evid, refs)    -> OpBindGlobal(loc, evid, !refs)
      | LocalVar(lv, off, evid, refs) -> OpBindLocal(lv, off, evid, !refs)
    in
    let body = OpPush(Constructor("Some", v)) :: bindop :: body in
    let fnew = CompiledFuncWithEnvironment(vars, arity, pargs, framesize, body, env1) in
      exec (fnew :: stack) env code dump

| _ ->
    report_bug_vm_value "ApplyOptional: not a function with optional arguments" f
|}
    ; inst "ApplyOmission"
        ~fields:[
        ]
        ~params:[
          param "f";
        ]
        ~code:{|
match f with
| CompiledFuncWithEnvironment(var :: vars, arity, pargs, framesize, body, env1) ->
    let bindop =
      match var with
      | GlobalVar(loc, evid, refs)    -> OpBindGlobal(loc, evid, !refs)
      | LocalVar(lv, off, evid, refs) -> OpBindLocal(lv, off, evid, !refs)
    in
    let body = OpPush(Constructor("None", UnitConstant)) :: bindop :: body in
    let fnew = CompiledFuncWithEnvironment(vars, arity, pargs, framesize, body, env1) in
      exec (fnew :: stack) env code dump

| _ ->
    report_bug_vm_value "ApplyOmission: not a function with optional arguments" f
|}
    ; inst "BindGlobal"
        ~fields:[
          field "loc" ~type_:"syntactic_value ref";
          field "evid" ~type_:"EvalVarID.t";
          field "refs" ~type_:"int";
        ]
        ~params:[
          param "v";
        ]
        ~code:{|
loc := v;
exec stack env code dump
|}
    ; inst "BindLocal"
        ~fields:[
          field "lv" ~type_:"int";
          field "offset" ~type_:"int";
          field "evid" ~type_:"EvalVarID.t";
          field "refs" ~type_:"int";
        ]
        ~params:[
        ]
        ~code:{|
match stack with
| v :: stack ->
    local_set_value env lv offset v;
    exec stack env code dump

| _ ->
    report_bug_vm ("BindLocal(" ^ (EvalVarID.show_direct evid) ^ ")")
|}
    ; inst "BindClosuresRec"
        ~fields:[
          field "binds" ~type_:"(varloc * instruction list) list";
        ]
        ~params:[
        ]
        ~code:{|
binds |> List.iter (fun (var, code) ->
  let recfunc = exec [] env code [] in
    match var with
    | GlobalVar(loc, evid, refs) -> loc := recfunc
    | LocalVar(lv, offset, evid, refs) -> local_set_value env lv offset recfunc
);
exec stack env code dump
|}
    ; inst "Branch"
        ~fields:[
          field "body" ~type_:"instruction list";
        ]
        ~params:[
        ]
        ~pp:Simple
        ~code:{|
exec stack env body dump
|}
    ; inst "BranchIf"
        ~fields:[
          field "body" ~type_:"instruction list";
        ]
        ~params:[
          param "b" ~type_:"bool";
        ]
        ~pp:Simple
        ~code:{|
if b then
  exec stack env body dump
else
  exec stack env code dump
|}
    ; inst "BranchIfNot"
        ~fields:[
          field "body" ~type_:"instruction list";
        ]
        ~params:[
          param "b" ~type_:"bool";
        ]
        ~pp:Simple
        ~code:{|
if b then
  exec stack env code dump
else
  exec stack env body dump
|}
    ; inst "LoadGlobal"
        ~fields:[
          field "loc" ~type_:"syntactic_value ref";
          field "evid" ~type_:"EvalVarID.t";
          field "refs" ~type_:"int";
        ]
        ~params:[
        ]
        ~pp:(Custom "(fun fmt (r, evid, refs) -> Format.fprintf fmt \"OpLoadGlobal(%s)\" (EvalVarID.show_direct evid))")
        ~code:{|
let v = !loc in
  exec (v :: stack) env code dump
|}
    ; inst "LoadLocal"
        ~fields:[
          field "lv" ~type_:"int";
          field "offset" ~type_:"int";
          field "evid" ~type_:"EvalVarID.t";
          field "refs" ~type_:"int";
        ]
        ~params:[
        ]
        ~code:{|
let v = local_get_value env lv offset in
  exec (v :: stack) env code dump
|}
    ; inst "Dereference"
        ~fields:[
        ]
        ~params:[
          param "valuecont";
        ]
        ~is_pdf_mode_primitive:true
        ~no_interp:true
        ~code:{|
match valuecont with
| Location(stid) ->
    begin
      match find_location_value (vmenv_global env) stid with
      | Some(value) -> value
      | None        -> report_bug_vm "Dereference"
    end

| _ ->
    report_bug_vm "Dereference"
|}
    ; inst "Dup"
        ~fields:[
        ]
        ~params:[
        ]
        ~code:{|
let v =
  try List.hd stack with
  | Invalid_argument(_) -> report_bug_vm "Dup: stack underflow"
in
  exec (v :: stack) env code dump
|}
    ; inst "Error"
        ~fields:[
          field "msg" ~type_:"string";
        ]
        ~params:[
        ]
        ~code:{|
raise (ExecError(msg))
|}
    ; inst "MakeConstructor"
        ~fields:[
          field "ctor_nm" ~type_:"constructor_name";
        ]
        ~params:[
          param "valuecont";
        ]
        ~code:{|
exec (Constructor(ctor_nm, valuecont) :: stack) env code dump
|}
    ; inst "MakeRecord"
        ~fields:[
          field "keylst" ~type_:"Assoc.key list";
        ]
        ~params:[
        ]
        ~pp:Simple
        ~code:{|
let rec collect keys asc st =
  match keys with
  | [] ->
      (asc, st)

  | k :: rest ->
      begin
        match st with
        | v :: stnew -> collect rest (Assoc.add asc k v) stnew
        | _          -> report_bug_vm "MakeRecord: stack underflow"
      end
  in
  let (asc, stack) = collect (List.rev keylst) Assoc.empty stack in
    exec (RecordValue(asc) :: stack) env code dump
|}
    ; inst "MakeTuple"
        ~fields:[
          field "len" ~type_:"int";
        ]
        ~params:[
        ]
        ~code:{|
let rec iter n last st =
  if n <= 0 then
    (last, st)
  else
    match st with
    | value :: stnew -> iter (n - 1) (TupleCons(value, last)) stnew
    | []             -> report_bug_vm "MakeTuple: stack underflow"
in
let (tuple, stack) = iter len EndOfTuple stack in
  exec (tuple :: stack) env code dump
|}
    ; inst "Pop"
        ~fields:[
        ]
        ~params:[
        ]
        ~code:{|
let stack =
  try List.tl stack with
  | Invalid_argument(_) -> report_bug_vm "Pop: stack underflow"
in
  exec stack env code dump
|}
    ; inst "Push"
        ~fields:[
          field "v" ~type_:"syntactic_value";
        ]
        ~params:[
        ]
        ~code:{|
exec (v :: stack) env code dump
|}
    ; inst "PushEnv"
        ~fields:[
        ]
        ~params:[
        ]
        ~code:{|
exec (EvaluatedEnvironment(vmenv_global env) :: stack) env code dump
|}
    ; inst "CheckStackTopBool"
        ~fields:[
          field "b" ~type_:"bool";
          field "next" ~type_:"instruction list";
        ]
        ~params:[
          param "v";
        ]
        ~pp:Simple
        ~code:{|
match v with
| BooleanConstant(b0) when b = b0 -> exec stack env code dump
| _                               -> exec stack env next dump
|}
    ; inst "CheckStackTopCtor"
        ~fields:[
          field "ctor_nm" ~type_:"constructor_name";
          field "next" ~type_:"instruction list";
        ]
        ~params:[
          param "v";
        ]
        ~pp:Simple
        ~code:{|
match v with
| Constructor(nm, sub) when nm = ctor_nm -> exec (sub :: stack) env code dump
| _                                      -> exec stack env next dump
|}
    ; inst "CheckStackTopEndOfList"
        ~fields:[
          field "next" ~type_:"instruction list";
        ]
        ~params:[
          param "v";
        ]
        ~pp:Simple
        ~code:{|
match v with
| EndOfList -> exec stack env code dump
| _         -> exec stack env next dump
|}
    ; inst "CheckStackTopInt"
        ~fields:[
          field "i" ~type_:"int";
          field "next" ~type_:"instruction list";
        ]
        ~params:[
          param "v";
        ]
        ~pp:Simple
        ~code:{|
match v with
| IntegerConstant(i0) when i=i0 -> exec stack env code dump
| _                             -> exec stack env next dump
|}
    ; inst "CheckStackTopListCons"
        ~fields:[
          field "next" ~type_:"instruction list";
        ]
        ~params:[
          param "v";
        ]
        ~pp:Simple
        ~code:{|
match v with
| ListCons(car, cdr) -> exec (car :: cdr :: stack) env code dump
| _                  -> exec stack env next dump
|}
    ; inst "CheckStackTopStr"
        ~fields:[
          field "str" ~type_:"string";
          field "next" ~type_:"instruction list";
        ]
        ~params:[
          param "v";
        ]
        ~pp:Simple
        ~code:{|
match v with
| StringConstant(s0) when s0 = str -> exec stack env code dump
| _                                -> exec stack env next dump
|}
    ; inst "CheckStackTopTupleCons"
        ~fields:[
          field "next" ~type_:"instruction list";
        ]
        ~params:[
          param "v";
        ]
        ~pp:Simple
        ~code:{|
match v with
| TupleCons(car, cdr) -> exec (car :: cdr :: stack) env code dump
| _                   -> exec stack env next dump
|}
    ; inst "Closure"
        ~fields:[
          field "optvars" ~type_:"varloc list";
          field "arity" ~type_:"int";
          field "framesize" ~type_:"int";
          field "body" ~type_:"instruction list";
        ]
        ~params:[
        ]
        ~code:{|
exec (CompiledFuncWithEnvironment(optvars, arity, [], framesize, body, env) :: stack) env code dump
|}
    ; inst "ClosureInputHorz"
        ~fields:[
          field "imihlst" ~type_:"compiled_input_horz_element list";
        ]
        ~params:[
        ]
        ~code:{|
let imihclos = exec_input_horz_content env imihlst in
  exec (imihclos :: stack) env code dump
|}
    ; inst "ClosureInputVert"
        ~fields:[
          field "imivlst" ~type_:"compiled_input_vert_element list";
        ]
        ~params:[
        ]
        ~code:{|
let imivclos = exec_input_vert_content env imivlst in
  exec (imivclos :: stack) env code dump
|}
    ; inst "BindLocationGlobal"
        ~fields:[
          field "loc" ~type_:"syntactic_value ref";
          field "evid" ~type_:"EvalVarID.t";
        ]
        ~params:[
          param "valueini";
        ]
        ~code:{|
let stid = register_location (vmenv_global env) valueini in
loc := Location(stid);
  exec stack env code dump
|}
    ; inst "BindLocationLocal"
        ~fields:[
          field "lv" ~type_:"int";
          field "offset" ~type_:"int";
          field "evid" ~type_:"EvalVarID.t";
        ]
        ~params:[
          param "valueini";
        ]
        ~code:{|
let stid = register_location (vmenv_global env) valueini in
local_set_value env lv offset (Location(stid));
  exec stack env code dump
|}
    ; inst "UpdateGlobal"
        ~fields:[
          field "loc" ~type_:"syntactic_value ref";
          field "evid" ~type_:"EvalVarID.t";
        ]
        ~params:[
          param "valuenew";
        ]
        ~pp:Simple
        ~code:{|
match !loc with
| Location(stid) ->
    begin
      update_location (vmenv_global env) stid valuenew;
      exec (UnitConstant :: stack) env code dump
    end

| _ -> report_bug_vm "UpdateGlobal"
|}
    ; inst "UpdateLocal"
        ~fields:[
          field "lv" ~type_:"int";
          field "offset" ~type_:"int";
          field "evid" ~type_:"EvalVarID.t";
        ]
        ~params:[
          param "valuenew";
        ]
        ~pp:Simple
        ~code:{|
match local_get_value env lv offset with
| Location(stid) ->
     begin
       update_location (vmenv_global env) stid valuenew;
       exec (UnitConstant :: stack) env code dump
     end

| _ -> report_bug_vm "UpdateLocal"
|}
    ; inst "Sel"
        ~fields:[
          field "tpart" ~type_:"instruction list";
          field "fpart" ~type_:"instruction list";
        ]
        ~params:[
          param "b" ~type_:"bool";
        ]
        ~pp:Simple
        ~code:{|
if b then
  exec stack env tpart dump
else
  exec stack env fpart dump
|}
    ; inst "Concat"
        ~name:"^"
        ~type_:{|
~% (tS @-> tS @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "value1";
          param "value2";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
match (value1, value2) with
| (StringEmpty, _)                         -> value2
| (_, StringEmpty)                         -> value1
| (StringConstant(s1), StringConstant(s2)) -> StringConstant(s1 ^ s2)
| _                                        -> report_bug_vm "Concat"
|}
    ; inst "PrimitiveSetMathVariantToChar"
        ~name:"set-math-variant-char"
        ~type_:{|
~% (tMCCLS @-> tI @-> tI @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "mccls" ~type_:"math_char_class";
          param "cpfrom" ~type_:"int";
          param "cpto" ~type_:"int";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let uchfrom = Uchar.of_int cpfrom in
let uchto = Uchar.of_int cpto in
let mcclsmap = ctx.HorzBox.math_variant_char_map in
  Context(HorzBox.({ ctx with
    math_variant_char_map = mcclsmap |> MathVariantCharMap.add (uchfrom, mccls) uchto;
  }), valuecmd)
|}
    ; inst "PrimitiveConvertStringForMath"
        ~name:"convert-string-for-math"
        ~type_:{|
~% (tCTX @-> tMCCLS @-> tS @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, valuecmd)" ~type_:"context";
          param "mccls" ~type_:"math_char_class";
          param "s" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let ctx = HorzBox.({ ctx with math_char_class = mccls; }) in let (_, uchlst) = MathContext.convert_math_variant_char (ctx, valuecmd) s in StringConstant(InternalText.to_utf8 (InternalText.of_uchar_list uchlst))
|}
    ; inst "PrimitiveSetMathCommand"
        ~name:"set-math-command"
        ~type_:{|
~% (tCMD @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "valuecmd";
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(ctx, valuecmd)
|}
    ; inst "BackendMathVariantCharDirect"
        ~name:"math-variant-char"
        ~type_:{|
~% (tMATHCLS @-> tMCSTY @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls" ~type_:"math_class";
          param "valuercd";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let is_big = false in  (* temporary *)
let mvsty = get_math_variant_style valuercd in
  MathValue(HorzBox.([MathPure(MathVariantCharDirect(mathcls, is_big, mvsty))]))
|}
    ; inst "BackendGetLeftMathClass"
        ~name:"get-left-math-class"
        ~type_:{|
~% (tCTX @-> tMATH @-> tOPT tMATHCLS)
|}
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "mathlst" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
match mathlst with
| [] ->
    Constructor("None", UnitConstant)

| math :: _ ->
    let mathcls = Math.get_left_math_kind ictx math in
      make_math_class_option_value mathcls
|}
    ; inst "BackendGetRightMathClass"
        ~name:"get-right-math-class"
        ~type_:{|
~% (tCTX @-> tMATH @-> tOPT tMATHCLS)
|}
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "mathlst" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
match List.rev mathlst with
| [] ->
    Constructor("None", UnitConstant)

| math :: _ ->
    let mathcls = Math.get_right_math_kind ictx math in
      make_math_class_option_value mathcls
|}
    ; inst "BackendSpaceBetweenMaths"
        ~name:"space-between-maths"
        ~type_:{|
~% (tCTX @-> tMATH @-> tMATH @-> tOPT tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "mathlst1" ~type_:"math";
          param "mathlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let mathctx = MathContext.make ictx in
let hbspaceopt = Math.space_between_maths mathctx mathlst1 mathlst2 in
  match hbspaceopt with
  | None          -> Constructor("None", UnitConstant)
  | Some(hbspace) -> Constructor("Some", Horz([hbspace]))
|}
    ; inst "BackendMathConcat"
        ~name:"math-concat"
        ~type_:{|
~% (tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mlst1" ~type_:"math";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue(List.append mlst1 mlst2)
|}
    ; inst "BackendMathGroup"
        ~name:"math-group"
        ~type_:{|
~% (tMATHCLS @-> tMATHCLS @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls1" ~type_:"math_class";
          param "mathcls2" ~type_:"math_class";
          param "mlst" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue([MathGroup(mathcls1, mathcls2, mlst)])
|}
    ; inst "BackendMathSuperscript"
        ~name:"math-sup"
        ~type_:{|
~% (tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mlst1" ~type_:"math";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue([MathSuperscript(mlst1, mlst2)])
|}
    ; inst "BackendMathSubscript"
        ~name:"math-sub"
        ~type_:{|
~% (tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mlst1" ~type_:"math";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue([MathSubscript(mlst1, mlst2)])
|}
    ; inst "BackendMathFraction"
        ~name:"math-frac"
        ~type_:{|
~% (tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mlst1" ~type_:"math";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue([MathFraction(mlst1, mlst2)])
|}
    ; inst "BackendMathRadical"
        ~name:"math-radical"
        ~type_:{|
~% (tOPT tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "value1mopt";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let mlst1opt = get_option get_math value1mopt in
let radical = Primitives.default_radical in  (* temporary; should be variable *)
  match mlst1opt with
  | None        -> MathValue([MathRadical(radical, mlst2)])
  | Some(mlst1) -> MathValue([MathRadicalWithDegree(mlst1, mlst2)])
|}
    ; inst "BackendMathParen"
        ~name:"math-paren"
        ~type_:{|
~% (tPAREN @-> tPAREN @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "valueparenL";
          param "valueparenR";
          param "mlst1" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let parenL = make_paren reducef valueparenL in
let parenR = make_paren reducef valueparenR in
  MathValue([MathParen(parenL, parenR, mlst1)])
|}
    ; inst "BackendMathParenWithMiddle"
        ~name:"math-paren-with-middle"
        ~type_:{|
~% (tPAREN @-> tPAREN @-> tPAREN @-> tL tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "valueparenL";
          param "valueparenR";
          param "valuemiddle";
          param "mlstlst" ~type_:"math_list";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let parenL = make_paren reducef valueparenL in
let parenR = make_paren reducef valueparenR in
let middle = make_paren reducef valuemiddle in
  MathValue([MathParenWithMiddle(parenL, parenR, middle, mlstlst)])
|}
    ; inst "BackendMathUpperLimit"
        ~name:"math-upper"
        ~type_:{|
~% (tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mlst1" ~type_:"math";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue([MathUpperLimit(mlst1, mlst2)])
|}
    ; inst "BackendMathLowerLimit"
        ~name:"math-lower"
        ~type_:{|
~% (tMATH @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mlst1" ~type_:"math";
          param "mlst2" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue([MathLowerLimit(mlst1, mlst2)])
|}
    ; inst "BackendMathPullInScripts"
        ~name:"math-pull-in-scripts"
        ~type_:{|
~% (tMATHCLS @-> tMATHCLS @-> (tOPT tMATH @-> tOPT tMATH @-> tMATH) @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls1" ~type_:"math_class";
          param "mathcls2" ~type_:"math_class";
          param "valuef";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let mlstf = make_pull_in_scripts reducef valuef in
let mlst = [HorzBox.(MathPullInScripts(mathcls1, mathcls2, mlstf))] in
  MathValue(mlst)
|}
    ; inst "BackendMathChar"
        ~name:"math-char"
        ~type_:{|
~% (tMATHCLS @-> tS @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls" ~type_:"math_class";
          param "uchlst" ~type_:"uchar_list";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let mlst = [HorzBox.(MathPure(MathElement(mathcls, MathChar(false, uchlst))))] in
  MathValue(mlst)
|}
    ; inst "BackendMathBigChar"
        ~name:"math-big-char"
        ~type_:{|
~% (tMATHCLS @-> tS @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls" ~type_:"math_class";
          param "uchlst" ~type_:"uchar_list";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let mlst = [HorzBox.(MathPure(MathElement(mathcls, MathChar(true, uchlst))))] in
  MathValue(mlst)
|}
    ; inst "BackendMathCharWithKern"
        ~name:"math-char-with-kern"
        ~type_:{|
let mckf = tLN @-> tLN @-> tLN in
  ~% (tMATHCLS @-> tS @-> mckf @-> mckf @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls" ~type_:"math_class";
          param "uchlst" ~type_:"uchar_list";
          param "valuekernfL";
          param "valuekernfR";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let kernfL = make_math_char_kern_func reducef valuekernfL in
let kernfR = make_math_char_kern_func reducef valuekernfR in
let mlst = [HorzBox.(MathPure(MathElement(mathcls, MathCharWithKern(false, uchlst, kernfL, kernfR))))] in
  MathValue(mlst)
|}
    ; inst "BackendMathBigCharWithKern"
        ~name:"math-big-char-with-kern"
        ~type_:{|
let mckf = tLN @-> tLN @-> tLN in
  ~% (tMATHCLS @-> tS @-> mckf @-> mckf @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls" ~type_:"math_class";
          param "uchlst" ~type_:"uchar_list";
          param "valuekernfL";
          param "valuekernfR";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let kernfL = make_math_char_kern_func reducef valuekernfL in
let kernfR = make_math_char_kern_func reducef valuekernfR in
let mlst = [HorzBox.(MathPure(MathElement(mathcls, MathCharWithKern(true, uchlst, kernfL, kernfR))))] in
  MathValue(mlst)
|}
    ; inst "BackendMathText"
        ~name:"text-in-math"
        ~type_:{|
~% (tMATHCLS @-> (tCTX @-> tIB) @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mathcls" ~type_:"math_class";
          param "valuef";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let hblstf ictx =
  let valueh = reducef valuef [Context(ictx)] in
    get_horz valueh
in
  MathValue(HorzBox.([MathPure(MathElement(mathcls, MathEmbeddedText(hblstf)))]))
|}
    ; inst "BackendMathColor"
        ~name:"math-color"
        ~type_:{|
~% (tCLR @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "color" ~type_:"color";
          param "mlst" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue(HorzBox.([MathChangeContext(MathChangeColor(color), mlst)]))
|}
    ; inst "BackendMathCharClass"
        ~name:"math-char-class"
        ~type_:{|
~% (tMCCLS @-> tMATH @-> tMATH)
|}
        ~fields:[
        ]
        ~params:[
          param "mccls" ~type_:"math_char_class";
          param "mlst" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
MathValue(HorzBox.([MathChangeContext(MathChangeMathCharClass(mccls), mlst)]))
|}
    ; inst "BackendMathList"
        ~fields:[
          field "n" ~type_:"int";
        ]
        ~params:[
        ]
        ~no_ircode:true
        ~code:{|
let rec iter n st acc =
  if n <= 0 then
    (acc, st)
  else
    match st with
    | MathValue(m) :: stnew -> iter (n - 1) stnew (m :: acc)
    | _                     -> report_bug_vm "BackendMathList"
in
let (mlst, stack) = iter n stack [] in
  exec (MathValue(List.concat mlst) :: stack) env code dump
|}
    ; inst "BackendEmbeddedMath"
        ~name:"embed-math"
        ~type_:{|
~% (tCTX @-> tMATH @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "ictx" ~type_:"context";
          param "mlst" ~type_:"math";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let mathctx = MathContext.make ictx in
let hblst = Math.main mathctx mlst in
  Horz(hblst)
|}
    ; inst "BackendTabular"
        ~name:"tabular"
        ~type_:{|
~% ((tL (tL tCELL)) @-> tRULESF @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "valuetabular";
          param "valuerulesf";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let tabular = get_list (get_list get_cell) valuetabular in
let (imtabular, widlst, lenlst, wid, hgt, dpt) = Tabular.main tabular in
let rulesf xs ys =
  let valuexs = make_length_list xs in
  let valueys = make_length_list ys in
  let valueret = reducef valuerulesf [valuexs; valueys] in
    graphics_of_list valueret
in
  Horz(HorzBox.([HorzPure(PHGFixedTabular(wid, hgt, dpt, imtabular, widlst, lenlst, rulesf))]))
|}
    ; inst "BackendRegisterPdfImage"
        ~name:"load-pdf-image"
        ~type_:{|
~% (tS @-> tI @-> tIMG)
|}
        ~fields:[
        ]
        ~params:[
          param "relpathstr" ~type_:"string";
          param "pageno" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let abspath = MyUtil.make_abs_path (Filename.concat (OptionState.job_directory ()) relpathstr) in
let imgkey = ImageInfo.add_pdf abspath pageno in
  ImageKey(imgkey)
|}
    ; inst "BackendRegisterOtherImage"
        ~name:"load-image"
        ~type_:{|
~% (tS @-> tIMG)
|}
        ~fields:[
        ]
        ~params:[
          param "relpath" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let abspath = MyUtil.make_abs_path (Filename.concat (OptionState.job_directory ()) relpath) in
let imgkey = ImageInfo.add_image abspath in
  ImageKey(imgkey)
|}
    ; inst "BackendUseImageByWidth"
        ~name:"use-image-by-width"
        ~type_:{|
~% (tIMG @-> tLN @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "valueimg";
          param "wid" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
match valueimg with
| ImageKey(imgkey) ->
    let hgt = ImageInfo.get_height_from_width imgkey wid in
      Horz(HorzBox.([HorzPure(PHGFixedImage(wid, hgt, imgkey))]))

| _ -> report_bug_vm "BackendUseImage"
|}
    ; inst "BackendHookPageBreak"
        ~name:"hook-page-break"
        ~type_:{|
~% ((tPBINFO @-> tPT @-> tU) @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "hookf";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let hookf = make_hook reducef hookf in
  Horz(HorzBox.([HorzPure(PHGHookPageBreak(hookf))]))
|}
    ; inst "Path"
        ~fields:[
          field "c_pathcomplst" ~type_:"((instruction list) compiled_path_component) list";
          field "c_cycleopt" ~type_:"(unit compiled_path_component) option";
        ]
        ~params:[
          param "pt0" ~type_:"point";
        ]
        ~is_pdf_mode_primitive:true
        ~pp:Simple
        ~no_ircode:true
        ~no_interp:true
        ~code:{|
let (pathelemlst, closingopt) = get_path env c_pathcomplst c_cycleopt in
  PathValue([GeneralPath(pt0, pathelemlst, closingopt)])
|}
    ; inst "PathUnite"
        ~name:"unite-path"
        ~type_:{|
~% (tPATH @-> tPATH @-> tPATH)
|}
        ~fields:[
        ]
        ~params:[
          param "pathlst1" ~type_:"path_value";
          param "pathlst2" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
PathValue(List.append pathlst1 pathlst2)
|}
    ; inst "PathShift"
        ~name:"shift-path"
        ~type_:{|
~% (tPT @-> tPATH @-> tPATH)
|}
        ~fields:[
        ]
        ~params:[
          param "ptshift" ~type_:"point";
          param "pathlst" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
PathValue(List.map (shift_path ptshift) pathlst)
|}
    ; inst "PathGetBoundingBox"
        ~name:"get-path-bbox"
        ~type_:{|
~% (tPATH @-> tPROD [tPT; tPT])
|}
        ~fields:[
        ]
        ~params:[
          param "pathlst" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let (ptmin, ptmax) = get_path_list_bbox pathlst in
  TupleCons(make_point_value ptmin,
    TupleCons(make_point_value ptmax,
      EndOfTuple))
|}
    ; inst "PrePathBeginning"
        ~name:"start-path"
        ~type_:{|
~% (tPT @-> tPRP)
|}
        ~fields:[
        ]
        ~params:[
          param "pt0" ~type_:"point";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
PrePathValue(PrePath.start pt0)
|}
    ; inst "PrePathLineTo"
        ~name:"line-to"
        ~type_:{|
~% (tPT @-> tPRP @-> tPRP)
|}
        ~fields:[
        ]
        ~params:[
          param "pt1" ~type_:"point";
          param "prepath" ~type_:"prepath";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
PrePathValue(prepath |> PrePath.line_to pt1)
|}
    ; inst "PrePathCubicBezierTo"
        ~name:"bezier-to"
        ~type_:{|
~% (tPT @-> tPT @-> tPT @-> tPRP @-> tPRP)
|}
        ~fields:[
        ]
        ~params:[
          param "ptS" ~type_:"point";
          param "ptT" ~type_:"point";
          param "pt1" ~type_:"point";
          param "prepath" ~type_:"prepath";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
PrePathValue(prepath |> PrePath.bezier_to ptS ptT pt1)
|}
    ; inst "PrePathTerminate"
        ~name:"terminate-path"
        ~type_:{|
~% (tPRP @-> tPATH)
|}
        ~fields:[
        ]
        ~params:[
          param "prepath" ~type_:"prepath";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
PathValue([prepath |> PrePath.terminate])
|}
    ; inst "PrePathCloseWithLine"
        ~name:"close-with-line"
        ~type_:{|
~% (tPRP @-> tPATH)
|}
        ~fields:[
        ]
        ~params:[
          param "prepath" ~type_:"prepath";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
PathValue([prepath |> PrePath.close_with_line])
|}
    ; inst "PrePathCloseWithCubicBezier"
        ~name:"close-with-bezier"
        ~type_:{|
~% (tPT @-> tPT @-> tPRP @-> tPATH)
|}
        ~fields:[
        ]
        ~params:[
          param "ptS" ~type_:"point";
          param "ptT" ~type_:"point";
          param "prepath" ~type_:"prepath";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
PathValue([prepath |> PrePath.close_with_bezier ptS ptT])
|}
    ; inst "HorzConcat"
        ~name:"++"
        ~type_:{|
~% (tIB @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "hblst1" ~type_:"horz";
          param "hblst2" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Horz(List.append hblst1 hblst2)
|}
    ; inst "VertConcat"
        ~name:"+++"
        ~type_:{|
~% (tBB @-> tBB @-> tBB)
|}
        ~fields:[
        ]
        ~params:[
          param "vblst1" ~type_:"vert";
          param "vblst2" ~type_:"vert";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Vert(List.append vblst1 vblst2)
|}
    ; inst "HorzLex"
        ~name:"read-inline"
        ~type_:{|
~% (tCTX @-> tIT @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "valuectx";
          param "value1";
        ]
        ~is_pdf_mode_primitive:true
        ~code_interp:{|
match value1 with
| InputHorzWithEnvironment(imihlst, envi) -> interpret_pdf_mode_intermediate_input_horz envi valuectx imihlst
| _                                       -> report_bug_value "HorzLex" value1
|}
        ~code:{|
match value1 with
| CompiledInputHorzWithEnvironment(imihlst, envi) -> exec_pdf_mode_intermediate_input_horz envi valuectx imihlst
| _                                               -> report_bug_vm "HorzLex"
|}
    ; inst "VertLex"
        ~name:"read-block"
        ~type_:{|
~% (tCTX @-> tBT @-> tBB)
|}
        ~fields:[
        ]
        ~params:[
          param "valuectx";
          param "value1";
        ]
        ~is_pdf_mode_primitive:true
        ~code_interp:{|
match value1 with
| InputVertWithEnvironment(imivlst, envi) -> interpret_pdf_mode_intermediate_input_vert envi valuectx imivlst
| _                                       -> report_bug_value "VertLex" value1
|}
        ~code:{|
match value1 with
| CompiledInputVertWithEnvironment(imivlst, envi) -> exec_pdf_mode_intermediate_input_vert envi valuectx imivlst
| _                                               -> report_bug_vm "VertLex"
|}
    ; inst "TextHorzLex"
        ~name:"stringify-inline"
        ~type_:{|
~% (tTCTX @-> tIT @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "valuetctx";
          param "value1";
        ]
        ~is_text_mode_primitive:true
        ~code_interp:{|
match value1 with
| InputHorzWithEnvironment(imihlst, envi) -> interpret_text_mode_intermediate_input_horz envi valuetctx imihlst
| _                                       -> report_bug_value "TextHorzLex" value1
|}
        ~code:{|
match value1 with
| CompiledInputHorzWithEnvironment(imihlst, envi) -> exec_text_mode_intermediate_input_horz envi valuetctx imihlst
| _                                               -> report_bug_vm "TextHorzLex"
|}
    ; inst "TextVertLex"
        ~name:"stringify-block"
        ~type_:{|
~% (tTCTX @-> tBT @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "valuetctx";
          param "value1";
        ]
        ~is_text_mode_primitive:true
        ~code_interp:{|
match value1 with
| InputVertWithEnvironment(imivlst, envi) -> interpret_text_mode_intermediate_input_vert envi valuetctx imivlst
| _                                       -> report_bug_value "TextVertLex" value1
|}
        ~code:{|
match value1 with
| CompiledInputVertWithEnvironment(imivlst, envi) -> exec_text_mode_intermediate_input_vert envi valuetctx imivlst
| _                                               -> report_bug_vm "TextVertLex"
|}
    ; inst "TextDeepenIndent"
        ~name:"deepen-indent"
        ~type_:{|
~% (tI @-> tTCTX @-> tTCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "i" ~type_:"int";
          param "tctx" ~type_:"text_mode_context";
        ]
        ~is_text_mode_primitive:true
        ~code:{|
let tctx = tctx |> TextBackend.deepen_indent i in
  TextModeContext(tctx)
|}
    ; inst "TextBreak"
        ~name:"break"
        ~type_:{|
~% (tTCTX @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "tctx" ~type_:"text_mode_context";
        ]
        ~is_text_mode_primitive:true
        ~code:{|
let i = TextBackend.get_indent tctx in
let s = "\n" ^ (String.make i ' ') in
  StringConstant(s)
|}
    ; inst "TextGetInitialTextModeContext"
        ~name:"get-initial-text-info"
        ~type_:{|
~% (tU @-> tTCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "value1";
        ]
        ~is_text_mode_primitive:true
        ~code:{|
match value1 with
| UnitConstant ->
    let tctx = TextBackend.get_initial_text_mode_context () in
      TextModeContext(tctx)

| _ ->
    report_bug_value "TextGetInitialTextModeContext" value1
|}
    ; inst "PrimitiveEmbeddedVertBreakable"
        ~name:"embed-block-breakable"
        ~type_:{|
~% (tCTX @-> tBB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
          param "vblst" ~type_:"vert";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let wid = ctx.HorzBox.paragraph_width in
  Horz([HorzEmbeddedVertBreakable(wid, vblst)])
|}
    ; inst "BackendFont"
        ~fields:[
        ]
        ~params:[
          param "abbrev" ~type_:"string";
          param "size_ratio" ~type_:"float";
          param "rising_ratio" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_font_value (abbrev, size_ratio, rising_ratio)
|}
    ; inst "BackendLineBreaking"
        ~name:"line-break"
        ~type_:{|
~% (tB @-> tB @-> tCTX @-> tIB @-> tBB)
|}
        ~fields:[
        ]
        ~params:[
          param "is_breakable_top" ~type_:"bool";
          param "is_breakable_bottom" ~type_:"bool";
          param "(ctx,  _)" ~type_:"context";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let imvblst = HorzBox.(LineBreak.main is_breakable_top is_breakable_bottom ctx.paragraph_top ctx.paragraph_bottom ctx hblst) in
  Vert(imvblst)
|}
    ; inst "BackendPageBreaking"
        ~name:"page-break"
        ~type_:{|
~% (tPG @-> tPAGECONTF @-> tPAGEPARTSF @-> tBB @-> tDOC)
|}
        ~fields:[
        ]
        ~params:[
          param "pagesize" ~type_:"page_size";
          param "valuepagecontf";
          param "valuepagepartsf";
          param "vblst" ~type_:"vert";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let pagecontf = make_page_content_scheme_func reducef valuepagecontf in
let pagepartsf = make_page_parts_scheme_func reducef valuepagepartsf in
  DocumentValue(pagesize, pagecontf, pagepartsf, vblst)
|}
    ; inst "BackendVertFrame"
        ~name:"block-frame-breakable"
        ~type_:{|
~% (tCTX @-> tPADS @-> tDECOSET @-> (tCTX @-> tBB) @-> tBB)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, valuecmd)" ~type_:"context";
          param "pads" ~type_:"paddings";
          param "(valuedecoS, valuedecoH, valuedecoM, valuedecoT)" ~type_:"decoset";
          param "valuek";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let valuectxsub =
  Context(HorzBox.({ ctx with
    paragraph_width = HorzBox.(ctx.paragraph_width -% pads.paddingL -% pads.paddingR);
  }), valuecmd)
in
let vblst =
  let valuev = reducef valuek [valuectxsub] in
    get_vert valuev
in
  Vert(HorzBox.([
    VertTopMargin(true, ctx.paragraph_top);
    VertFrame(pads,
                make_frame_deco reducef valuedecoS,
                make_frame_deco reducef valuedecoH,
                make_frame_deco reducef valuedecoM,
                make_frame_deco reducef valuedecoT,
                ctx.paragraph_width, vblst);
    VertBottomMargin(true, ctx.paragraph_bottom);
]))
|}
    ; inst "BackendAddFootnote"
        ~name:"add-footnote"
        ~type_:{|
~% (tBB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "vblst" ~type_:"vert";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let imvblst = PageBreak.solidify vblst in
  Horz(HorzBox.([HorzPure(PHGFootnote(imvblst))]))
|}
    ; inst "BackendEmbeddedVertTop"
        ~name:"embed-block-top"
        ~type_:{|
~% (tCTX @-> tLN @-> (tCTX @-> tBB) @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, valuecmd)" ~type_:"context";
          param "wid" ~type_:"length";
          param "valuek";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let valuectxsub =
  Context(HorzBox.({ ctx with paragraph_width = wid; }), valuecmd)
in
let vblst =
  let valuev = reducef valuek [valuectxsub] in
    get_vert valuev
in
let imvblst = PageBreak.solidify vblst in
let (hgt, dpt) = PageBreak.adjust_to_first_line imvblst in
  Horz(HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, imvblst))]))
|}
    ; inst "BackendVertSkip"
        ~name:"block-skip"
        ~type_:{|
~% (tLN @-> tBB)
|}
        ~fields:[
        ]
        ~params:[
          param "len" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Vert(HorzBox.([VertFixedBreakable(len)]))
|}
    ; inst "BackendEmbeddedVertBottom"
        ~name:"embed-block-bottom"
        ~type_:{|
~% (tCTX @-> tLN @-> (tCTX @-> tBB) @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, valuecmd)" ~type_:"context";
          param "wid" ~type_:"length";
          param "valuek";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let valuectxsub =
  Context(HorzBox.({ ctx with paragraph_width = wid; }), valuecmd)
in
let vblst =
  let valuev = reducef valuek [valuectxsub] in
    get_vert valuev
in
let imvblst = PageBreak.solidify vblst in
let (hgt, dpt) = PageBreak.adjust_to_last_line imvblst in
  Horz(HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, imvblst))]))
|}
    ; inst "BackendLineStackTop"
        ~name:"line-stack-top"
        ~type_:{|
~% ((tL tIB) @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "valuehblstlst";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let hblstlst = get_list get_horz valuehblstlst in
let (wid, vblst) = make_line_stack hblstlst in
let imvblst = PageBreak.solidify vblst in
let (hgt, dpt) = PageBreak.adjust_to_first_line imvblst in
  Horz(HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, imvblst))]))
|}
    ; inst "BackendLineStackBottom"
        ~name:"line-stack-bottom"
        ~type_:{|
~% ((tL tIB) @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "valuehblstlst";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let hblstlst = get_list get_horz valuehblstlst in
let (wid, vblst) = make_line_stack hblstlst in
let imvblst = PageBreak.solidify vblst in
let (hgt, dpt) = PageBreak.adjust_to_last_line imvblst in
  Horz(HorzBox.([HorzPure(PHGEmbeddedVert(wid, hgt, dpt, imvblst))]))
|}
    ; inst "PrimitiveGetInitialContext"
        ~name:"get-initial-context"
        ~type_:{|
~% (tLN @-> tCMD @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "txtwid" ~type_:"length";
          param "valuecmd";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let ctx = Primitives.get_pdf_mode_initial_context txtwid in
  Context(ctx, valuecmd)
|}
    ; inst "PrimitiveSetHyphenMin"
        ~name:"set-hyphen-min"
        ~type_:{|
~% (tI @-> tI @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "lmin" ~type_:"int";
          param "rmin" ~type_:"int";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  left_hyphen_min = max 0 lmin;
  right_hyphen_min = max 0 rmin;
}), valuecmd)
|}
    ; inst "PrimitiveSetMinGapOfLines"
        ~name:"set-min-gap-of-lines"
        ~type_:{|
~% (tLN @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "lengap" ~type_:"length";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  min_gap_of_lines = Length.max Length.zero lengap;
}), valuecmd)
|}
    ; inst "PrimitiveSetSpaceRatio"
        ~name:"set-space-ratio"
        ~type_:{|
~% (tFL @-> tFL @-> tFL @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "ratio_natural" ~type_:"float";
          param "ratio_shrink" ~type_:"float";
          param "ratio_stretch" ~type_:"float";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  space_natural = max 0. ratio_natural;
  space_shrink  = max 0. ratio_shrink;
  space_stretch = max 0. ratio_stretch;
}), valuecmd)
|}
    ; inst "PrimitiveSetSpaceRatioBetweenScripts"
        ~name:"set-space-ratio-between-scripts"
        ~type_:{|
~% (tFL @-> tFL @-> tFL @-> tSCR @-> tSCR @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "ratio_natural" ~type_:"float";
          param "ratio_shrink" ~type_:"float";
          param "ratio_stretch" ~type_:"float";
          param "script1" ~type_:"script";
          param "script2" ~type_:"script";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  script_space_map =
    ctx.script_space_map |> CharBasis.ScriptSpaceMap.add
      (script1, script2)
      (max 0. ratio_natural, max 0. ratio_shrink, max 0. ratio_stretch)
}), valuecmd)
|}
    ; inst "PrimitiveGetSpaceRatioBetweenScripts"
        ~name:"get-space-ratio-between-scripts"
        ~type_:{|
~% (tCTX @-> tSCR @-> tSCR @-> tOPT (tPROD [tFL; tFL; tFL]))
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
          param "script1" ~type_:"script";
          param "script2" ~type_:"script";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
match ctx.script_space_map |> CharBasis.ScriptSpaceMap.find_opt (script1, script2) with
| None ->
    Constructor("None", UnitConstant)

| Some((r0, r1, r2)) ->
    Constructor("Some", TupleCons(FloatConstant(r0),
                          TupleCons(FloatConstant(r1),
                            TupleCons(FloatConstant(r2), EndOfTuple))))
|}
    ; inst "PrimitiveSetParagraphMargin"
        ~name:"set-paragraph-margin"
        ~type_:{|
~% (tLN @-> tLN @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "lentop" ~type_:"length";
          param "lenbottom" ~type_:"length";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  paragraph_top    = lentop;
  paragraph_bottom = lenbottom;
}), valuecmd)
|}
    ; inst "PrimitiveSetParagraphMinAscenderAndDescender"
        ~name:"set-min-paragraph-ascender-and-descender"
        ~type_:{|
~% (tLN @-> tLN @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "lenminasc" ~type_:"length";
          param "lenmindesc" ~type_:"length";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  min_first_line_ascender = lenminasc;
  min_last_line_descender = lenmindesc;
}), valuecmd)
|}
    ; inst "PrimitiveSetFontSize"
        ~name:"set-font-size"
        ~type_:{|
~% (tLN @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "size" ~type_:"length";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with font_size = size; }), valuecmd)
|}
    ; inst "PrimitiveGetFontSize"
        ~name:"get-font-size"
        ~type_:{|
~% (tCTX @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
LengthConstant(ctx.HorzBox.font_size)
|}
    ; inst "PrimitiveSetFont"
        ~name:"set-font"
        ~type_:{|
~% (tSCR @-> tFONT @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "font_info" ~type_:"font";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let font_scheme_new = HorzBox.(ctx.font_scheme |> CharBasis.ScriptSchemeMap.add script font_info) in
  Context(HorzBox.({ ctx with font_scheme = font_scheme_new; }), valuecmd)
|}
    ; inst "PrimitiveGetFont"
        ~name:"get-font"
        ~type_:{|
~% (tSCR @-> tCTX @-> tFONT)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let fontwr = HorzBox.get_font_with_ratio ctx script in
  make_font_value fontwr
|}
    ; inst "PrimitiveSetMathFont"
        ~name:"set-math-font"
        ~type_:{|
~% (tS @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "mfabbrev" ~type_:"string";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with math_font = mfabbrev; }), valuecmd)
|}
    ; inst "PrimitiveSetDominantWideScript"
        ~name:"set-dominant-wide-script"
        ~type_:{|
~% (tSCR @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with dominant_wide_script = script; }), valuecmd)
|}
    ; inst "PrimitiveGetDominantWideScript"
        ~name:"get-dominant-wide-script"
        ~type_:{|
~% (tCTX @-> tSCR)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_script_value ctx.HorzBox.dominant_wide_script
|}
    ; inst "PrimitiveSetDominantNarrowScript"
        ~name:"set-dominant-narrow-script"
        ~type_:{|
~% (tSCR @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with dominant_narrow_script = script; }), valuecmd)
|}
    ; inst "PrimitiveGetDominantNarrowScript"
        ~name:"get-dominant-narrow-script"
        ~type_:{|
~% (tCTX @-> tSCR)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
make_script_value ctx.HorzBox.dominant_narrow_script
|}
    ; inst "PrimitiveSetLangSys"
        ~name:"set-language"
        ~type_:{|
~% (tSCR @-> tLANG @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "langsys" ~type_:"language_system";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  langsys_scheme = ctx.langsys_scheme |> CharBasis.ScriptSchemeMap.add script langsys;
}), valuecmd)
|}
    ; inst "PrimitiveGetLangSys"
        ~name:"get-language"
        ~type_:{|
~% (tSCR @-> tCTX @-> tLANG)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let langsys = HorzBox.get_language_system ctx script in
  make_language_system_value langsys
|}
    ; inst "PrimitiveSetTextColor"
        ~name:"set-text-color"
        ~type_:{|
~% (tCLR @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "color" ~type_:"color";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with text_color = color; }), valuecmd)
|}
    ; inst "PrimitiveGetTextColor"
        ~name:"get-text-color"
        ~type_:{|
~% (tCTX @-> tCLR)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let color = ctx.HorzBox.text_color in
  make_color_value color
|}
    ; inst "PrimitiveSetLeading"
        ~name:"set-leading"
        ~type_:{|
~% (tLN @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "len" ~type_:"length";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with leading = len; }), valuecmd)
|}
    ; inst "PrimitiveGetTextWidth"
        ~name:"get-text-width"
        ~type_:{|
~% (tCTX @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
LengthConstant(ctx.HorzBox.paragraph_width)
|}
    ; inst "PrimitiveSetManualRising"
        ~name:"set-manual-rising"
        ~type_:{|
~% (tLN @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "rising" ~type_:"length";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with manual_rising = rising; }), valuecmd)
|}
    ; inst "PrimitiveRaise"
        ~name:"raise-inline"
        ~type_:{|
~% (tLN @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "rising" ~type_:"length";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Horz(HorzBox.([HorzPure(PHGRising(rising, hblst))]))
|}
    ; inst "PrimitiveSetHyphenPenalty"
        ~name:"set-hyphen-penalty"
        ~type_:{|
~% (tI @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "pnlty" ~type_:"int";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with hyphen_badness = pnlty; }), valuecmd)
|}
    ; inst "PrimitiveEmbed"
        ~name:"embed-string"
        ~type_:{|
~% (tS @-> tIT)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code_interp:{|
InputHorzWithEnvironment([ImInputHorzText(str)], env)
|}
        ~code:{|
CompiledInputHorzWithEnvironment([CompiledImInputHorzText(str)], env)
|}
    ; inst "PrimitiveExtract"
        ~name:"extract-string"
        ~type_:{|
~% (tIB @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
StringConstant(HorzBox.extract_string hblst)
|}
    ; inst "PrimitiveGetAxisHeight"
        ~name:"get-axis-height"
        ~type_:{|
~% (tCTX @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let fontsize = ctx.HorzBox.font_size in
let mfabbrev = ctx.HorzBox.math_font in
let hgt = FontInfo.get_axis_height mfabbrev fontsize in
  LengthConstant(hgt)
|}
    ; inst "BackendFixedEmpty"
        ~name:"inline-skip"
        ~type_:{|
~% (tLN @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "wid" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Horz([HorzBox.HorzPure(HorzBox.PHSFixedEmpty(wid))])
|}
    ; inst "BackendOuterEmpty"
        ~name:"inline-glue"
        ~type_:{|
~% (tLN @-> tLN @-> tLN @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "widnat" ~type_:"length";
          param "widshrink" ~type_:"length";
          param "widstretch" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Horz([HorzBox.HorzPure(HorzBox.PHSOuterEmpty(widnat, widshrink, widstretch))])
|}
    ; inst "BackendOuterFrame"
        ~name:"inline-frame-outer"
        ~type_:{|
~% (tPADS @-> tDECO @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "pads" ~type_:"paddings";
          param "valuedeco";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
Horz([HorzBox.HorzPure(HorzBox.PHGOuterFrame(
  pads,
  make_frame_deco reducef valuedeco,
  hblst))])
|}
    ; inst "BackendInnerFrame"
        ~name:"inline-frame-inner"
        ~type_:{|
~% (tPADS @-> tDECO @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "pads" ~type_:"paddings";
          param "valuedeco";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
Horz([HorzBox.HorzPure(HorzBox.PHGInnerFrame(
  pads,
  make_frame_deco reducef valuedeco,
  hblst))])
|}
    ; inst "BackendFixedFrame"
        ~name:"inline-frame-fixed"
        ~type_:{|
~% (tLN @-> tPADS @-> tDECO @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "wid" ~type_:"length";
          param "pads" ~type_:"paddings";
          param "valuedeco";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
Horz([HorzBox.HorzPure(HorzBox.PHGFixedFrame(
  pads, wid,
  make_frame_deco reducef valuedeco,
  hblst))])
|}
    ; inst "BackendOuterFrameBreakable"
        ~name:"inline-frame-breakable"
        ~type_:{|
~% (tPADS @-> tDECOSET @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "pads" ~type_:"paddings";
          param "(valuedecoS, valuedecoH, valuedecoM, valuedecoT)" ~type_:"decoset";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
Horz([HorzBox.HorzFrameBreakable(
  pads, Length.zero, Length.zero,
  make_frame_deco reducef valuedecoS,
  make_frame_deco reducef valuedecoH,
  make_frame_deco reducef valuedecoM,
  make_frame_deco reducef valuedecoT,
  hblst
)])
|}
    ; inst "BackendInlineGraphics"
        ~name:"inline-graphics"
        ~type_:{|
~% (tLN @-> tLN @-> tLN @-> tIGR @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "wid" ~type_:"length";
          param "hgt" ~type_:"length";
          param "dpt" ~type_:"length";
          param "valueg";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let graphics = make_inline_graphics reducef valueg in
  Horz(HorzBox.([HorzPure(PHGFixedGraphics(wid, hgt, Length.negate dpt, graphics))]))
|}
    ; inst "BackendInlineGraphicsOuter"
        ~name:"inline-graphics-outer"
        ~type_:{|
~% (tLN @-> tLN @-> tIGRO @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "hgt" ~type_:"length";
          param "dpt" ~type_:"length";
          param "valueg";
        ]
        ~is_pdf_mode_primitive:true
        ~needs_reducef:true
        ~code:{|
let graphics = make_inline_graphics_outer reducef valueg in
  Horz(HorzBox.([HorzPure(PHGOuterFilGraphics(hgt, Length.negate dpt, graphics))]))
|}
    ; inst "BackendScriptGuard"
        ~name:"script-guard"
        ~type_:{|
~% (tSCR @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "script" ~type_:"script";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Horz(HorzBox.([HorzScriptGuard(script, script, hblst)]))
|}
    ; inst "BackendScriptGuardBoth"
        ~name:"script-guard-both"
        ~type_:{|
~% (tSCR @-> tSCR @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "scriptL" ~type_:"script";
          param "scriptR" ~type_:"script";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Horz(HorzBox.([HorzScriptGuard(scriptL, scriptR, hblst)]))
|}
    ; inst "BackendGetLeftmostScript"
        ~name:"get-leftmost-script"
        ~type_:{|
~% (tIB @-> tOPT tSCR)
|}
        ~fields:[
        ]
        ~params:[
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let scriptopt = LineBreak.get_leftmost_script hblst in
make_option make_script_value scriptopt
|}
    ; inst "BackendGetRightmostScript"
        ~name:"get-rightmost-script"
        ~type_:{|
~% (tIB @-> tOPT tSCR)
|}
        ~fields:[
        ]
        ~params:[
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let scriptopt = LineBreak.get_rightmost_script hblst in
make_option make_script_value scriptopt
|}
    ; inst "BackendDiscretionary"
        ~name:"discretionary"
        ~type_:{|
~% (tI @-> tIB @-> tIB @-> tIB @-> tIB)
|}
        ~fields:[
        ]
        ~params:[
          param "pb" ~type_:"int";
          param "hblst0" ~type_:"horz";
          param "hblst1" ~type_:"horz";
          param "hblst2" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Horz(HorzBox.([HorzDiscretionary(pb, hblst0, hblst1, hblst2)]))
|}
    ; inst "BackendRegisterCrossReference"
        ~name:"register-cross-reference"
        ~type_:{|
~% (tS @-> tS @-> tU)
|}
        ~fields:[
        ]
        ~params:[
          param "k" ~type_:"string";
          param "v" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
CrossRef.register k v;
UnitConstant
|}
    ; inst "BackendGetCrossReference"
        ~name:"get-cross-reference"
        ~type_:{|
~% (tS @-> (tOPT tS))
|}
        ~fields:[
        ]
        ~params:[
          param "k" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
match CrossRef.get k with
| None    -> Constructor("None", UnitConstant)
| Some(v) -> Constructor("Some", StringConstant(v))
|}
    ; inst "PrimitiveGetNaturalMetrics"
        ~name:"get-natural-metrics"
        ~type_:{|
~% (tIB @-> tPROD [tLN; tLN; tLN])
|}
        ~fields:[
        ]
        ~params:[
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let (wid, hgt, dpt) = LineBreak.get_natural_metrics hblst in
  TupleCons(LengthConstant(wid),
    TupleCons(LengthConstant(hgt),
      TupleCons(LengthConstant(Length.negate dpt), EndOfTuple)))
|}
    ; inst "PrimitiveGetNaturalLength"
        ~name:"get-natural-length"
        ~type_:{|
~% (tBB @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "vblst" ~type_:"vert";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let imvblst = PageBreak.solidify vblst in
let (hgt, dpt) = PageBreak.adjust_to_first_line imvblst in
  LengthConstant(hgt +% (Length.negate dpt))
|}
    ; inst "PrimitiveDisplayMessage"
        ~name:"display-message"
        ~type_:{|
~% (tS @-> tU)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
print_endline str;
UnitConstant
|}
    ; inst "PrimitiveListCons"
        ~fields:[
        ]
        ~params:[
          param "valuehd";
          param "valuetl";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
ListCons(valuehd, valuetl)
|}
    ; inst "PrimitiveSame"
        ~name:"string-same"
        ~type_:{|
~% (tS @-> tS @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "str1" ~type_:"string";
          param "str2" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
BooleanConstant(String.equal str1 str2)
|}
    ; inst "PrimitiveStringSub"
        ~name:"string-sub"
        ~type_:{|
~% (tS @-> tI @-> tI @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
          param "pos" ~type_:"int";
          param "wid" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let resstr =
try BatUTF8.sub str pos wid with
| Invalid_argument(s) -> report_dynamic_error "illegal index for string-sub"
in
  StringConstant(resstr)
|}
    ; inst "PrimitiveStringSubBytes"
        ~name:"string-sub-bytes"
        ~type_:{|
~% (tS @-> tI @-> tI @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
          param "pos" ~type_:"int";
          param "wid" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let resstr =
try String.sub str pos wid with
| Invalid_argument(s) -> report_dynamic_error "illegal index for string-sub-bytes"
in
  StringConstant(resstr)
|}
    ; inst "PrimitiveStringLength"
        ~name:"string-length"
        ~type_:{|
~% (tS @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
IntegerConstant(BatUTF8.length str)
|}
    ; inst "PrimitiveStringByteLength"
        ~name:"string-byte-length"
        ~type_:{|
~% (tS @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
IntegerConstant(String.length str)
|}
    ; inst "PrimitiveStringScan"
        ~name:"string-scan"
        ~type_:{|
~% (tRE @-> tS @-> tOPT (tPROD [tS; tS]))
|}
        ~fields:[
        ]
        ~params:[
          param "pat" ~type_:"regexp";
          param "str" ~type_:"string";
        ]
        ~code:{|
if Str.string_match pat str 0 then
  let matched = Str.matched_string str in
  let start   = String.length matched in
  let rest    = String.sub str start (String.length str - start) in
  Constructor("Some", TupleCons(StringConstant(matched), TupleCons(StringConstant(rest), EndOfTuple)))
else
  Constructor("None", UnitConstant)
|}
    ; inst "PrimitiveStringUnexplode"
        ~name:"string-unexplode"
        ~type_:{|
~% ((tL tI) @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "valueilst";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let ilst = get_list get_int valueilst in
let s = (List.map Uchar.of_int ilst) |> InternalText.of_uchar_list |> InternalText.to_utf8 in
  StringConstant(s)
|}
    ; inst "PrimitiveRegExpOfString"
        ~name:"regexp-of-string"
        ~type_:{|
~% (tS @-> tRE)
|}
        ~fields:[
        ]
        ~params:[
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let regexp =
  try Str.regexp str with
  | Failure(msg) -> report_dynamic_error ("regexp-of-string: " ^ msg)
in
  RegExpConstant(regexp)
|}
    ; inst "PrimitiveStringMatch"
        ~name:"string-match"
        ~type_:{|
~% (tRE @-> tS @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "pat" ~type_:"regexp";
          param "s" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
BooleanConstant(Str.string_match pat s 0)
|}
    ; inst "PrimitiveSplitIntoLines"
        ~name:"split-into-lines"
        ~type_:{|
~% (tS @-> (tL (tPROD [tI; tS])))
|}
        ~fields:[
        ]
        ~params:[
          param "s" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let slst = String.split_on_char '\n' s in
let pairlst = slst |> List.map chop_space_indent in
  (pairlst |> make_list (fun (i, s) ->
    TupleCons(IntegerConstant(i), TupleCons(StringConstant(s), EndOfTuple))))
|}
    ; inst "PrimitiveSplitOnRegExp"
        ~name:"split-on-regexp"
        ~type_:{|
~% (tRE @-> tS @-> (tL (tPROD [tI; tS])))
|}
        ~fields:[
        ]
        ~params:[
          param "sep" ~type_:"regexp";
          param "str" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
let slst = Str.split sep str in
let pairlst = slst |> List.map chop_space_indent in
  (pairlst |> make_list (fun (i, s) ->
    TupleCons(IntegerConstant(i), TupleCons(StringConstant(s), EndOfTuple))))
|}
    ; inst "PrimitiveArabic"
        ~name:"arabic"
        ~type_:{|
~% (tI @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "num" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
StringConstant(string_of_int num)
|}
    ; inst "PrimitiveShowFloat"
        ~name:"show-float"
        ~type_:{|
~% (tFL @-> tS)
|}
        ~fields:[
        ]
        ~params:[
          param "fl" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
StringConstant(string_of_float fl)
|}
    ; inst "PrimitiveFloat"
        ~name:"float"
        ~type_:{|
~% (tI @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "ic1" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
FloatConstant(float_of_int ic1)
|}
    ; inst "PrimitiveRound"
        ~name:"round"
        ~type_:{|
~% (tFL @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "fc1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
IntegerConstant(int_of_float fc1)
|}
    ; inst "PrimitiveDrawText"
        ~name:"draw-text"
        ~type_:{|
~% (tPT @-> tIB @-> tGR)
|}
        ~fields:[
        ]
        ~params:[
          param "pt" ~type_:"point";
          param "hblst" ~type_:"horz";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let (imhblst, _, _) = LineBreak.natural hblst in
let grelem = GraphicD.make_text pt imhblst in
  GraphicsValue(grelem)
|}
    ; inst "PrimitiveDrawStroke"
        ~name:"stroke"
        ~type_:{|
~% (tLN @-> tCLR @-> tPATH @-> tGR)
|}
        ~fields:[
        ]
        ~params:[
          param "wid" ~type_:"length";
          param "color" ~type_:"color";
          param "pathlst" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let grelem = GraphicD.make_stroke wid color pathlst in
  GraphicsValue(grelem)
|}
    ; inst "PrimitiveDrawFill"
        ~name:"fill"
        ~type_:{|
~% (tCLR @-> tPATH @-> tGR)
|}
        ~fields:[
        ]
        ~params:[
          param "color" ~type_:"color";
          param "pathlst" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let grelem = GraphicD.make_fill color pathlst in
  GraphicsValue(grelem)
|}
    ; inst "PrimitiveDrawDashedStroke"
        ~name:"dashed-stroke"
        ~type_:{|
~% (tLN @-> tDASH @-> tCLR @-> tPATH @-> tGR)
|}
        ~fields:[
        ]
        ~params:[
          param "wid" ~type_:"length";
          param "valuetup3";
          param "color" ~type_:"color";
          param "pathlst" ~type_:"path_value";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let (len1, len2, len3) = get_tuple3 get_length valuetup3 in
let grelem = GraphicD.make_dashed_stroke wid (len1, len2, len3) color pathlst in
  GraphicsValue(grelem)
|}
    ; inst "PrimitiveShiftGraphics"
        ~name:"shift-graphics"
        ~type_:{|
~% (tPT @-> tGR @-> tGR)
|}
        ~fields:[
        ]
        ~params:[
          param "vec" ~type_:"point";
          param "grelem" ~type_:"graphics_element";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
GraphicsValue(GraphicD.shift_element vec grelem)
|}
    ; inst "PrimtiveGetGraphicsBBox"
        ~name:"get-graphics-bbox"
        ~type_:{|
~% (tGR @-> tPROD [tPT; tPT])
|}
        ~fields:[
        ]
        ~params:[
          param "grelem" ~type_:"graphics_element";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let (ptmin, ptmax) =
  GraphicD.get_element_bbox (fun (x, y) imhblst ->
    let (wid, hgt, dpt) = HorzBox.get_metrics_of_intermediate_horz_box_list imhblst in
      ((x, y +% dpt), (x +% wid, y +% hgt))
  ) grelem
in
  TupleCons(make_point_value ptmin,
    TupleCons(make_point_value ptmax,
      EndOfTuple))
|}
    ; inst "Times"
        ~name:"*"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
IntegerConstant(numl * numr)
|}
    ; inst "Divides"
        ~name:"/"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
try IntegerConstant(numl / numr) with
| Division_by_zero -> report_dynamic_error "division by zero"
|}
    ; inst "Mod"
        ~name:"mod"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
try IntegerConstant(numl mod numr) with
| Division_by_zero -> report_dynamic_error "division by zero"
|}
    ; inst "Plus"
        ~name:"+"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
IntegerConstant(numl + numr)
|}
    ; inst "Minus"
        ~name:"-"
        ~type_:{|
~% (tI @-> tI @-> tI)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
IntegerConstant(numl - numr)
|}
    ; inst "EqualTo"
        ~name:"=="
        ~type_:{|
~% (tI @-> tI @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
BooleanConstant(numl = numr)
|}
    ; inst "GreaterThan"
        ~name:">"
        ~type_:{|
~% (tI @-> tI @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
BooleanConstant(numl > numr)
|}
    ; inst "LessThan"
        ~name:"<"
        ~type_:{|
~% (tI @-> tI @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "numl" ~type_:"int";
          param "numr" ~type_:"int";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
BooleanConstant(numl < numr)
|}
    ; inst "LogicalAnd"
        ~name:"&&"
        ~type_:{|
~% (tB @-> tB @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "binl" ~type_:"bool";
          param "binr" ~type_:"bool";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
BooleanConstant(binl && binr)
|}
    ; inst "LogicalOr"
        ~name:"||"
        ~type_:{|
~% (tB @-> tB @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "binl" ~type_:"bool";
          param "binr" ~type_:"bool";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
BooleanConstant(binl || binr)
|}
    ; inst "LogicalNot"
        ~name:"not"
        ~type_:{|
~% (tB @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "binl" ~type_:"bool";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
BooleanConstant(not binl)
|}
    ; inst "FloatPlus"
        ~name:"+."
        ~type_:{|
~% (tFL @-> tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
          param "flt2" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
FloatConstant(flt1 +. flt2)
|}
    ; inst "FloatMinus"
        ~name:"-."
        ~type_:{|
~% (tFL @-> tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
          param "flt2" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
FloatConstant(flt1 -. flt2)
|}
    ; inst "FloatTimes"
        ~name:"*."
        ~type_:{|
~% (tFL @-> tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
          param "flt2" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
FloatConstant(flt1 *. flt2)
|}
    ; inst "FloatDivides"
        ~name:"/."
        ~type_:{|
~% (tFL @-> tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
          param "flt2" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
FloatConstant(flt1 /. flt2)
|}
    ; inst "FloatSine"
        ~name:"sin"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
FloatConstant(sin flt1)
|}
    ; inst "FloatArcSine"
        ~name:"asin"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
FloatConstant(asin flt1)
|}
    ; inst "FloatCosine"
        ~name:"cos"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
FloatConstant(cos flt1)
|}
    ; inst "FloatArcCosine"
        ~name:"acos"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
FloatConstant(acos flt1)
|}
    ; inst "FloatTangent"
        ~name:"tan"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
FloatConstant(tan flt1)
|}
    ; inst "FloatArcTangent"
        ~name:"atan"
        ~type_:{|
~% (tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
FloatConstant(atan flt1)
|}
    ; inst "FloatArcTangent2"
        ~name:"atan2"
        ~type_:{|
~% (tFL @-> tFL @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "flt1" ~type_:"float";
          param "flt2" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
FloatConstant(atan2 flt1 flt2)
|}
    ; inst "LengthPlus"
        ~name:"+'"
        ~type_:{|
~% (tLN @-> tLN @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
LengthConstant(HorzBox.(len1 +% len2))
|}
    ; inst "LengthMinus"
        ~name:"-'"
        ~type_:{|
~% (tLN @-> tLN @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
LengthConstant(HorzBox.(len1 -% len2))
|}
    ; inst "LengthTimes"
        ~name:"*'"
        ~type_:{|
~% (tLN @-> tFL @-> tLN)
|}
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "flt2" ~type_:"float";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
LengthConstant(HorzBox.(len1 *% flt2))
|}
    ; inst "LengthDivides"
        ~name:"/'"
        ~type_:{|
~% (tLN @-> tLN @-> tFL)
|}
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
FloatConstant(HorzBox.(len1 /% len2))
|}
    ; inst "LengthLessThan"
        ~name:"<'"
        ~type_:{|
~% (tLN @-> tLN @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
BooleanConstant(HorzBox.(len1 <% len2))
|}
    ; inst "LengthGreaterThan"
        ~name:">'"
        ~type_:{|
~% (tLN @-> tLN @-> tB)
|}
        ~fields:[
        ]
        ~params:[
          param "len1" ~type_:"length";
          param "len2" ~type_:"length";
        ]
        ~is_pdf_mode_primitive:true
        ~is_text_mode_primitive:true
        ~code:{|
BooleanConstant(HorzBox.(len2 <% len1))
|}
    ; inst "InsertArgs"
        ~fields:[
          field "lst" ~type_:"syntactic_value list";
        ]
        ~params:[
          param "func";
        ]
        ~code:{|
exec (func :: (List.rev_append lst stack)) env code dump
|}
    ; inst "PrimitiveSetWordBreakPenalty"
        ~name:"set-word-break-penalty"
        ~type_:{|
~% (tI @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "pnlty" ~type_:"int";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.{ ctx with
  space_badness = pnlty;
}, valuecmd)
|}
    ; inst "PrimitiveSetEveryWordBreak"
        ~name:"set-every-word-break"
        ~type_:{|
~% (tIB @-> tIB @-> tCTX @-> tCTX)
|}
        ~fields:[
        ]
        ~params:[
          param "hblst1" ~type_:"horz";
          param "hblst2" ~type_:"horz";
          param "(ctx, valuecmd)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Context(HorzBox.({ ctx with
  before_word_break = hblst1;
  after_word_break = hblst2;
}), valuecmd)
|}
    ; inst "PrimitiveGetEveryWordBreak"
        ~name:"get-every-word-break"
        ~type_:{|
~% (tCTX @-> tPROD [tIB; tIB])
|}
        ~fields:[
        ]
        ~params:[
          param "(ctx, _)" ~type_:"context";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let hblst1 = ctx.HorzBox.before_word_break in
let hblst2 = ctx.HorzBox.after_word_break in
  TupleCons(Horz(hblst1), TupleCons(Horz(hblst2), EndOfTuple))
|}
    ; inst "BackendProbeCrossReference"
        ~name:"probe-cross-reference"
        ~type_:{|
~% (tS @-> (tOPT tS))
|}
        ~fields:[
        ]
        ~params:[
          param "k" ~type_:"string";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
match CrossRef.probe k with
| None    -> Constructor("None", UnitConstant)
| Some(v) -> Constructor("Some", StringConstant(v))
|}
    ; inst "BackendRegisterDestination"
        ~name:"register-destination"
        ~type_:{|
~% (tS @-> tPT @-> tU)
|}
        ~fields:[
        ]
        ~params:[
          param "name" ~type_:"string";
          param "p" ~type_:"point";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
NamedDest.register name p;
UnitConstant
|}
    ; inst "BackendRegisterLinkToUri"
        ~name:"register-link-to-uri"
        ~type_:{|
~% (tS @-> tPT @-> tLN @-> tLN @-> tLN @-> (tOPT (tPROD [tLN; tCLR])) @-> tU)
|}
        ~fields:[
        ]
        ~params:[
          param "uri" ~type_:"string";
          param "pt" ~type_:"point";
          param "wid" ~type_:"length";
          param "hgt" ~type_:"length";
          param "dpt" ~type_:"length";
          param "vborderopt";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let borderopt = get_option (get_pair get_length get_color) vborderopt in
Annotation.register (Annotation.Link(Pdfaction.Uri(uri))) (pt, wid, hgt, dpt) borderopt;
UnitConstant
|}
    ; inst "BackendRegisterLinkToLocation"
        ~name:"register-link-to-location"
        ~type_:{|
~% (tS @-> tPT @-> tLN @-> tLN @-> tLN @-> (tOPT (tPROD [tLN; tCLR])) @-> tU)
|}
        ~fields:[
        ]
        ~params:[
          param "name" ~type_:"string";
          param "pt" ~type_:"point";
          param "wid" ~type_:"length";
          param "hgt" ~type_:"length";
          param "dpt" ~type_:"length";
          param "vborderopt";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
let borderopt = get_option (get_pair get_length get_color) vborderopt in
let destname = NamedDest.get name in
Annotation.register (Annotation.Link(Pdfaction.GotoName(destname))) (pt, wid, hgt, dpt) borderopt;
UnitConstant
|}
    ; inst "BackendRegisterOutline"
        ~name:"register-outline"
        ~type_:{|
~% ((tL(tPROD [tI; tS; tS; tB])) @-> tU)
|}
        ~fields:[
        ]
        ~params:[
          param "ol";
        ]
        ~is_pdf_mode_primitive:true
        ~code:{|
Outline.register (get_list get_outline ol);
UnitConstant
|}
    ])
