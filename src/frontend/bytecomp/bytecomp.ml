
open MyUtil
open LengthInterface
open Types


let compile_and_exec_0 (env : environment) (ast : abstract_tree) : syntactic_value =
  let (ir, env) = Ir.transform_ast env ast in
  let code = Compiler.compile ir [] in
(*
  Format.printf "IR:\n%s\n" (show_ir ir);  (* for debug *)
  List.iter (fun inst -> Format.printf "%s\n" (show_instruction inst)) code;  (* for debug *)
*)
  Vm.exec [] (env, []) code []


let compile_environment env =
  let (binds, _) = env in
  binds |> EvalVarIDMap.iter (fun evid loc ->
    match !loc with
    | PrimitiveClosure(parbr, env1, arity, astf) ->
        begin
          match compile_and_exec_0 env (Function([], parbr)) with
          | CompiledClosure([], _, _, framesize, body, env1) ->
              loc := CompiledPrimitiveClosure(arity, [], framesize, body, env1, astf)
          | _ ->
              ()
        end

    | _ ->
        ()
  )
