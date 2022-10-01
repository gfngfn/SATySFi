
open SyntaxBase
open Types
open EvalUtil


let compile_and_exec_0 (env : environment) (ast : abstract_tree) : syntactic_value * environment option =
  let (ir, env) = Ir.transform_ast_0 env ast in
  let instrs = Compiler.compile ir [] in
(*
  Format.printf "IR:\n%s\n" (show_ir ir);  (* for debug *)
  List.iter (fun inst -> Format.printf "%s\n" (show_instruction inst)) code;  (* for debug *)
*)
  Vm.exec_code env instrs


let compile_environment (env : environment) : unit =
  let (binds, _) = env in
  binds |> EvalVarIDMap.iter (fun _evid loc ->
    match !loc with
    | PrimitiveClosure(parbr, _env1, arity, astf) ->
        begin
          match compile_and_exec_0 env (Function(LabelMap.empty, parbr)) with
          | (CompiledClosure(varloc_labmap, _, _, framesize, body, env1), _) ->
              if LabelMap.cardinal varloc_labmap = 0 then
                loc := CompiledPrimitiveClosure(arity, [], framesize, body, env1, astf)
              else
                ()

          | _ ->
              ()
        end

    | _ ->
        ()
  )


let compile_and_exec_1 (env : environment) (ast : abstract_tree) : code_value * environment option =
  let (ir, env) = Ir.transform_ast_1 env ast in
  let instrs = Compiler.compile ir [] in
  let (value, envopt) = Vm.exec_code env instrs in
  match value with
  | CodeValue(cv) -> (cv, envopt)
  | _             -> report_bug_value "compile_and_exec_1: not a CodeValue(...)" value
