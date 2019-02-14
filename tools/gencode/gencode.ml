open U


module Const = struct
  let stack = "stack"

  let environment = "env"

  let code = "code"

  let dump = "dump"

  let vmexec = "exec"

  let func_prefix = "get_"

  let ret = "ret"

  let trans_prim = "transform_primitive"

  let destructuring_rules =
    let open Printf in
    [ "int", sprintf "IntegerConstant(%s)"
    ; "bool", sprintf "BooleanConstant(%s)"
    ; "context", sprintf "Context(%s)"
    ; "float", sprintf "FloatConstant(%s)"
    ; "horz", sprintf "Horz(%s)"
    ; "vert", sprintf "Vert(%s)"
    ; "length", sprintf "LengthConstant(%s)"
    ; "math", sprintf "MathValue(%s)"
    ; "path_value", sprintf "PathValue(%s)"
    ; "prepath", sprintf "PrePathValue(%s)"
    ; "regexp", sprintf "RegExpConstant(%s)"
    ]
end


let is_pdf_mode_primitive def =
  def.Instruction.is_pdf_mode_primitive


let is_text_mode_primitive def =
  def.Instruction.is_text_mode_primitive


let gen_prims is_prims =
  let open Instruction in
  Vminst.def |> List.iter (function
  | {
      name = Some(name);
      type_ = Some(t);
      inst;
      params;
      _
    } as def  when is_prims def ->
      let args = params |> List.mapi (fun i _ -> "_v%d" @% i + 1) in
      let len = List.length args in
      puts "        (\"%s\"," name;
      puts "          begin";
      split_lines t |> List.iter (puts "            %s");
      puts "          end,";
      puts "          lambda%d (fun %s -> %s(%s))"
        len (String.concat " " args) inst (String.concat ", " args);
      puts "        );"

  | _ ->
      ()
  )


let gen_pdf_mode_prims () =
  gen_prims is_pdf_mode_primitive


let gen_text_mode_prims () =
  gen_prims is_text_mode_primitive


let gen_interps () =
  let open Instruction in
  Vminst.def |> List.iter (function
  | {
      no_interp = false;
      inst;
      params;
      needs_reducef;
      code_interp;
      code;
      _
    } as def  when is_primitive def ->
      let astargs = params |> List.mapi (fun i _ -> "_ast%d" @% i) in
      puts "  | %s(%s) ->" inst (String.concat ", " astargs);
      List.combine params astargs |> List.iter (function
      | ({ Param.name; type_ = None }, astident) ->
          puts "      let %s = interpret_0 env %s in"
            name astident

      | _ ->
          ()
      );
      List.combine params astargs |> List.iter (function
      | ({ Param.name; type_ = Some t }, astident) ->
          puts "      let %s = %s%s (interpret_0 env %s) in"
            name Const.func_prefix t astident

      | _ ->
          ()
      );
      if needs_reducef then begin
        puts "      let reducef = reduce_beta_list in"
      end;
      puts "        begin";
      default code code_interp |> split_lines |> List.iter
        (puts "          %s");
      puts "        end";
      puts ""

  | _ ->
      ()
  )


let gen_vminstrs () =
  let open Instruction in
  Vminst.def |> List.iter (function
  | {
      inst;
      params;
      fields;
      needs_reducef;
      code;
      _
    } as def ->
      let i = ref 0 in
      let ps =
        params |> List.map (function
        | { Param.name; type_ = None } as p ->
            (p, name, false)

        | { Param.name; type_ = Some typ } as p ->
            begin
              match List.assoc_opt typ Const.destructuring_rules with
              | Some(rule) ->
                  (p, rule name, false)

              | None ->
                  let tmp = "_tmp%d" @% !i in
                  i := !i + 1;
                  (p, tmp, true)
            end
        )
      in
      let destruct = ps |> List.rev_map (fun (_, x, _) -> x) in
      let funcapp = ps |> List.filter (fun (_, _, x) -> x) in
      begin
        match fields with
        | [] ->
            puts "  | Op%s ->" inst

        | fs ->
            puts "  | Op%s(%s) ->" inst @@
              String.concat ", " @@ List.map Field.name fs
      end;
      puts "      begin";
      if not @@ nullp params then begin
        puts "        match %s with" Const.stack
      end;
      begin
        match destruct with
        | [] ->
            ()

        | ds ->
            puts "        | %s :: %s ->" (String.concat " :: " ds) Const.stack
      end;
      funcapp |> List.iter (function
      | ({ Param.name = dest; type_ = Some(func) }, src, _) ->
          puts "            let %s = %s%s %s in"
            dest Const.func_prefix func src

      | _ ->
          ()   (* hmm... *)
      );
      if needs_reducef then begin
        puts "            let reducef = exec_application %s in"
          Const.environment
      end;
      let print_code () =
        split_lines code |> List.iter
          (puts "              %s");
      in
      if is_primitive def then begin
        puts "            let %s =" Const.ret;
        print_code ();
        puts "            in %s (%s :: %s) %s %s %s"
          Const.vmexec Const.ret Const.stack Const.environment Const.code Const.dump
      end else begin
        puts "            begin";
        print_code ();
        puts "            end"
      end;
      puts "";
      if not @@ nullp params then begin
        puts "        | _ -> report_bug_vm \"invalid argument for Op%s\"" inst
      end;
      puts "      end";
      puts ""
  )


let gen_insttype () =
  let open Instruction in
  puts "and instruction =";
  Vminst.def |> List.iter (function
  | {
      inst;
      fields;
      pp;
      _
    } ->
      begin
        match fields with
        | [] ->
            puts "  | Op%s" inst

        | fs ->
            puts "  | Op%s of %s"
              inst (String.concat " * " @@ List.map Field.type_ fs)
      end;
      begin
        match pp with
        | Default ->
            ()

        | Simple ->
            puts "      [@printer (fun fmt _ -> Format.fprintf fmt \"Op%s(...)\")]"
              inst

        | Custom pp ->
            puts "      [@printer (%s)]" pp
      end
  );
  puts "  [@@deriving show { with_path = false; }]"


let gen_attype () =
  let open Instruction in
  Vminst.def |> List.iter (function
  | {
      no_ircode = false;
      inst;
      params;
      _
    } as def  when is_primitive def ->
      begin
        match params with
        | [] ->
            puts "  | %s" inst

        | ps ->
            puts "  | %s of %s"
              inst
              (String.concat " * "
               @@ List.map (const "abstract_tree") ps)
      end

  | _ ->
      ()
  )


let gen_ircases () =
  let open Instruction in
  Vminst.def |> List.iter (function
  | {
      no_ircode = false;
      inst;
      params;
      _
    } as def  when is_primitive def ->
      let ps = params |> List.mapi (fun i _ -> "p%d" @% i + 1) in
      puts "    | %s(%s) ->"
        inst
        (String.concat ", " ps);
      puts "        %s env [%s] Op%s"
        Const.trans_prim
        (String.concat "; " ps)
        inst;
      puts ""

  | _ ->
      ()
  )


let () =
  let opts =
    [
      ("--gen-vm"             , gen_vminstrs       );
      ("--gen-ir"             , gen_ircases        );
      ("--gen-insttype"       , gen_insttype       );
      ("--gen-attype"         , gen_attype         );
      ("--gen-interps"        , gen_interps        );
      ("--gen-pdf-mode-prims" , gen_pdf_mode_prims );
      ("--gen-text-mode-prims", gen_text_mode_prims);
    ]
  in
  let opt = Sys.argv.(1) in
  match List.assoc_opt opt opts with
  | Some(func) -> func ()
  | None       -> failwith @@ Printf.sprintf "unknown option: %s" opt
