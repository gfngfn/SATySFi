
module Types = Types_
module Primitives = Primitives_
open MyUtil
open LengthInterface
open GraphicBase
open Types
open EvalUtil

exception ExecError of string


let report_dynamic_error msg =
  raise (ExecError(msg))


type compiled_nom_input_horz_element =
  | CompiledNomInputHorzText     of string
  | CompiledNomInputHorzEmbedded of instruction list
  | CompiledNomInputHorzContent  of compiled_nom_input_horz_element list * vmenv


let local_get_value env lv off =
  let (_, frames) = env in
    if lv = 0 then
      (List.hd frames).(off)
    else
      (List.nth frames lv).(off)


let local_set_value env lv off value =
  let (_, frames) = env in
    if lv = 0 then
      (List.hd frames).(off) <- value
    else
      (List.nth frames lv).(off) <- value


let vmenv_global env =
  let (global, _) = env in
    global


let newframe env size =
  let (global, local) = env in
    (global, (Array.make size Nil) :: local)


let newframe_recycle env preenv size =
  let (global, local) = env in
    match preenv with
    | (_, prefrm :: _) ->
        if size > Array.length prefrm then
          (global, (Array.make size Nil) :: local)
        else
          (global, prefrm :: local)

    | _ ->
        (global, (Array.make size Nil) :: local)


let get_input_horz_content env (value : syntactic_value) =
  match value with
  | CompiledInputHorzIntermediate(imihlist) -> CompiledInputHorzWithEnvironment(imihlist, env)
  | _                                       -> report_bug_vm "bad stack top item"


let get_input_vert_content env (value : syntactic_value) =
  match value with
  | CompiledInputVertIntermediate(imivlist) -> CompiledInputVertWithEnvironment(imivlist, env)
  | _                                       -> report_bug_vm "bad stack top item"


let lex_horz_text (ctx : HorzBox.context_main) (s_utf8 : string) : HorzBox.horz_box list =
  let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 s_utf8) in
    HorzBox.([HorzPure(PHCInnerString(ctx, uchlst))])


let popn stack n =
  let rec iter st n acc =
    if n = 0 then
      (acc, st)
    else
      match st with
      | x :: xs -> iter xs (n-1) (x::acc)
      | [] -> report_bug_vm "stack underflow!"
  in
    iter stack n []


let rec get_path env c_pathcomplst c_cycleopt =
  let pathelemlst =
    c_pathcomplst |> List.map (function
      | CompiledPathLineTo(ptcode) ->
          let pt = get_point @@ exec [] env ptcode [] in
            LineTo(pt)

      | CompiledPathCubicBezierTo(pt1code, pt2code, ptcode) ->
          let pt1 = get_point @@ exec [] env pt1code [] in
          let pt2 = get_point @@ exec [] env pt2code [] in
          let pt = get_point @@ exec [] env ptcode [] in
            CubicBezierTo(pt1, pt2, pt)
    )
  in
  let closingopt =
    c_cycleopt |> option_map (function
      | CompiledPathLineTo(()) ->
          LineTo(())

      | CompiledPathCubicBezierTo(pt1code, pt2code, ()) ->
          let pt1 = get_point @@ exec [] env pt1code [] in
          let pt2 = get_point @@ exec [] env pt2code [] in
            CubicBezierTo(pt1, pt2, ())
    )
  in
    (pathelemlst, closingopt)


and exec_intermediate_input_vert (env : vmenv) (valuectx : syntactic_value) (imivlst : compiled_intermediate_input_vert_element list) : syntactic_value =
  let rec interpret_commands env imivlst =
    imivlst |> List.map (fun imiv ->
        match imiv with
        | CompiledImInputVertEmbedded(code) ->
            let valueret = exec [valuectx] env (code) [] in
              get_vert valueret

        | CompiledImInputVertContent(ctcode) ->
            let value = exec [] env ctcode [] in
              begin
                match value with
                | CompiledInputVertWithEnvironment(imivlst, envsub) ->
                    interpret_commands envsub imivlst

                | _ -> report_bug_vm "interpret_input_vert_content"
              end

      ) |> List.concat
  in
  let imvblst = interpret_commands env imivlst in
    Vert(imvblst)


and exec_intermediate_input_horz (env : vmenv) (valuectx : syntactic_value) (imihlst : compiled_intermediate_input_horz_element list) : syntactic_value =
  let (ctx, valuemcmd) = get_context valuectx in
    begin
      let rec normalize imihlst =
        imihlst |> List.fold_left (fun acc imih ->
            match imih with
            | CompiledImInputHorzEmbedded(code) ->
                let nmih = CompiledNomInputHorzEmbedded(code) in
                  Alist.extend acc nmih

            | CompiledImInputHorzText(s2) ->
                begin
                  match Alist.chop_last acc with
                  | Some(accrest, CompiledNomInputHorzText(s1)) -> (Alist.extend accrest (CompiledNomInputHorzText(s1 ^ s2)))
                  | _                                           -> (Alist.extend acc (CompiledNomInputHorzText(s2)))
                end

            | CompiledImInputHorzEmbeddedMath(mathcode) ->
                let nmih = CompiledNomInputHorzEmbedded(mathcode @ [OpPush(valuemcmd); OpApplyT(2)]) in
                  Alist.extend acc nmih

            | CompiledImInputHorzContent(ctcode) ->
                let value = exec [] env ctcode [] in
                  begin
                    match value with
                    | CompiledInputHorzWithEnvironment(imihlst, envsub) ->
                        let nmihlstsub = normalize imihlst in
                        let nmih = CompiledNomInputHorzContent(nmihlstsub, envsub) in
                          Alist.extend acc nmih

                    | _ -> report_bug_vm "interpret_input_horz_content"
                  end

          ) Alist.empty |> Alist.to_list
      in

      let rec interpret_commands env (nmihlst : compiled_nom_input_horz_element list) : HorzBox.horz_box list =
        nmihlst |> List.map (fun nmih ->
            match nmih with
            | CompiledNomInputHorzEmbedded(code) ->
                let valueret = exec [valuectx] env code [] in
                  get_horz valueret

            | CompiledNomInputHorzText(s) ->
                lex_horz_text ctx s

            | CompiledNomInputHorzContent(nmihlstsub, envsub) ->
                interpret_commands envsub nmihlstsub

          ) |> List.concat
      in

      let nmihlst = normalize imihlst in
      let hblst = interpret_commands env nmihlst in
        Horz(hblst)
    end


and exec_application (env : vmenv) (vf : syntactic_value) (vargs : syntactic_value list) =
  let len = List.length vargs in
    if len = 0 then
      vf
    else
      exec (vf :: (List.rev vargs)) env [OpApplyT(len)] []


and exec (stack : syntactic_value list) (env : vmenv) (code : instruction list) dump =
  match code with
  | [] ->
      begin
        match dump with
        | (pe, pc) :: rest ->
            exec stack pe pc rest

        | [] ->
            begin match stack with
              | v :: _ -> v
              | [] -> report_bug_vm "stack underflow!"
            end
      end

  | c :: code ->
      (*Format.printf "\nexec ---> %a\n" pp_instruction c;*)
      match c with
      (**** include: __vm.ml ****)
