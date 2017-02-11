open Types
open Domstd


exception OnBrowserError of string


external documentUnsafe : < .. > Js.t = "document" [@@bs.val]

let setBackgroundColor_ext : string -> element -> unit =
  [%bs.raw{|
    function(clr, domobj) { domobj.style.backgroundColor = clr; }
  |}]

let setBackgroundColor clr domobj = setBackgroundColor_ext clr domobj


let varntenv_default = Primitives.make_variant_environment
let kdenv_default    = Kindenv.empty
let tyenv_default    = Primitives.make_type_environment
let env_default      = Primitives.make_environment ()


let output inputCode =
  try
    let _ = Lexer.reset_to_numexpr () in
    let utast = Parser.main Lexer.cut_token (Lexing.from_string inputCode) in
    let (ty, _, newkdenv, _, ast) = Typechecker.main varntenv_default kdenv_default tyenv_default utast in
    begin
      match ty with
      | (_, StringType) -> let evaled = Evaluator.interpret env_default ast in Out.main evaled
      | _               -> raise (OnBrowserError("the output is not string"))
    end
  with
  | Lexer.LexError(s)               -> ("! [ERROR AT LEXER] " ^ s ^ ".")
  | Parsing.Parse_error             -> ("! [ERROR AT PARSER] something is wrong.")
  | ParseErrorDetail(s)             -> ("! [ERROR AT PARSER] " ^ s ^ "")
  | ( Typechecker.Error(s)
    | Variantenv.Error(s)
    | Subst.ContradictionError(s) ) -> ("! [ERROR AT TYPECHECKER] " ^ s ^ ".")
  | Evaluator.EvalError(s)          -> ("! [ERROR AT EVALUATOR] " ^ s ^ ".")
  | Out.IllegalOut(s)               -> ("! [ERROR AT OUTPUT] " ^ s ^ ".")
  | OnBrowserError(s)               -> ("! [ERROR] " ^ s ^ ".")
  | Sys_error(s)                    -> ("! [ERROR] System error - " ^ s)


let _ =
  afterLoadingHTML (fun () ->
    let inputArea = documentUnsafe##inputForm##inputArea in
    let outputArea = document |> getElementById "output-side" in
    let submissionButton = document |> getElementById "submission-button" in
    let nowConverting = ref false in
    begin
      submissionButton |> addEventListener MouseOver (fun e ->
        submissionButton |> setBackgroundColor "#cccccc"
      ) ;
      submissionButton |> addEventListener MouseOut  (fun e ->
        submissionButton |> setBackgroundColor "#eeeeee"
      ) ;
      submissionButton |> addEventListener Click (fun e ->
        if !nowConverting then () else
        begin
          nowConverting := true ;
          submissionButton |> setBackgroundColor "#aaaaaa" ;
          let outputText = output inputArea##value in
          begin
            outputArea |> setInnerHtml outputText |> ignore ; (* temporary *)
            nowConverting := false ;
          end
       end
      ) ;
    end
  )
