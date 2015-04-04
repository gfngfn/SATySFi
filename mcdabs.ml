(* module Mcdabs *)
  open Types

  let report_error errmsg =
    print_string ("[ERROR IN MCDABS] " ^ errmsg ^ ".") ; print_newline ()

  let print_process stat =
    (* print_string stat ; print_newline () *)
    ()

  (* tree -> abstract_tree *)
  let rec concrete_to_abstract conctr =
    match conctr with
      NonTerminal(Total, [tr; Terminal(END_OF_INPUT)]) -> (
          print_process "#Total" ;
          concrete_to_abstract tr
        )
    | NonTerminal(Block, chdrn) -> (
          match chdrn with
            [] -> (
                print_process "#Block []" ;
                EmptyAbsBlock
              )
          | [stc; NonTerminal(Block, [])] -> (
                print_process "#Block [stc; blk] 1" ;
                concrete_to_abstract stc
              )
          | [stc; blk] -> (
                print_process "#Block [stc; blk] 2" ;
                AbsBlock(concrete_to_abstract stc, concrete_to_abstract blk)
              )
          | _ -> (
                report_error "illegal Block" ;
                Invalid
              )
        )
    | NonTerminal(Group, [Terminal(BGRP); lstbysp; Terminal(EGRP)]) -> (
          print_process "#Group" ;
            concrete_to_abstract lstbysp
        )
    | NonTerminal(ListBySep, [NonTerminal(Block, chdrn)]) -> (
          print_process "#ListBySep []" ;
          concrete_to_abstract (NonTerminal(Block, chdrn))
        )
    | NonTerminal(ListBySep, [tr; Terminal(SEP); lstbysp]) -> (
          print_process "#ListBySep _" ;
          Separated(concrete_to_abstract tr, concrete_to_abstract lstbysp)
        )
    | NonTerminal(Sentence, chdrn) -> (
        match chdrn with
        (* S -> [char] *)
          [Terminal(CHAR(c))] -> (
              print_process "#Sentence CHAR" ;
              Output(c)
            )
        (* S -> [finalbreak] *)
        | [Terminal(FINALBREAK)] -> (
              print_process "#Sentence FINALBREAK" ;
              AbsBlock(ShallowIndent,
                AbsBlock(Output("\n"),
                  AbsBlock(ContentOf("~indent"),
                    DeepenIndent
                  )
                )
              )
            )
        (* S -> [var] *)
        | [Terminal(VAR(v)); Terminal(END)] -> (
              print_process "#Sentence VAR" ;
              ContentOf(v)
            )
        (* S -> [pop] [var] [var] G G *)
        | [Terminal(POP); Terminal(VAR(u)); Terminal(VAR(v)); grp1; grp2]
            -> (
              print_process "#Sentence POP" ;
              Pop(u, v, concrete_to_abstract grp1, concrete_to_abstract grp2)
            )
        (* S -> [macro] [ctrlseq] A G *)
        (* not [Terminal(MACRO); Terminal(CTRLSEQ(f)); args; grp1]
            since it is not consistent with indent system *)
        | [Terminal(MACRO); Terminal(CTRLSEQ(f)); args; grp1]
            -> (
              print_process "#Sentence MACRO" ;
              Macro(f, make_args_list args, concrete_to_abstract grp1, EmptyAbsBlock)
            )
        (* S -> [macrowid] [ctrlseq] A G G *)
  (*    | [Terminal(MACROWID); Terminal(CTRLSEQ(f)); args; grp1; grp2]
            -> Macro(f, make_args_list args, concrete_to_abstract grp1, concrete_to_abstract grp2) *)
        (* S -> [ctrlseq] [end] *)
        | [Terminal(CTRLSEQ(f)); Terminal(END)]
            -> (
              print_process "#Sentence CTRLSEQ1" ;
              Apply(f, NoID, [])
            )
        (* S -> [ctrlseq] [id] [end] *)
        | [Terminal(CTRLSEQ(f)); Terminal(ID(i)); Terminal(END)]
            -> (
              print_process "#Sentence CTRLSEQ2" ;
              Apply(f, RealID(i), [])
            )
        (* S -> [ctrlseq] G P *)
        | [Terminal(CTRLSEQ(f)); grp; prms]
            -> (
              print_process "#Sentence CTRLSEQ3" ;
            (* call by value *)
              Apply(f, NoID, make_params_list (NonTerminal(Params, [grp; prms])))
            )
        (* S -> [ctrlseq] [id] G P *)
        | [Terminal(CTRLSEQ(f)); Terminal(ID(i)); grp; prms]
            -> (
              print_process "#Sentence CTRLSEQ4" ;
            (* call by value *)
              Apply(f, RealID(i), make_params_list (NonTerminal(Params, [grp; prms])))
            )
        (* S -> [bltrl] C [eltrl] *)
        | [Terminal(BLTRL(lb)); chofltrl; Terminal(ELTRL)]
            -> (
              print_process "#Sentence BLTRL-ELTRL" ;
              LiteralBlock(lb, concrete_to_abstract chofltrl)
            )
        | _ -> ( report_error "illegal child of sentence" ; Invalid )
      )
    | NonTerminal(CharOfLiteral, []) -> (
          EmptyAbsBlock
        )
    | NonTerminal(CharOfLiteral, [Terminal(CHAR(c)); chofltrl]) -> (
          let abstr_chofltrl = concrete_to_abstract chofltrl in
            match abstr_chofltrl with
              EmptyAbsBlock -> OutputOfLiteral(c)
            | _ -> AbsBlock(OutputOfLiteral(c), abstr_chofltrl)
        )
    | Terminal(END_OF_INPUT) -> (
          print_process "#END_OF_INPUT" ;
          EmptyAbsBlock
        )
    | _ -> (
        report_error "illegal concrete tree" ;
        Invalid
      )

    (* tree -> var_name list *)
    and make_args_list args =
      match args with
        NonTerminal(Args, []) -> (
            print_process "#Args []" ;
            []
          )
      | NonTerminal(Args, [Terminal(VAR(v)); argssub])
          -> (
            print_process "#Args v :: argssub" ;
            v :: (make_args_list argssub)
          )
      | _ -> ( report_error "illegal argument" ; ["error"] )

    (* tree -> abstract_tree list *)
    and make_params_list prms =
      match prms with
        NonTerminal(Params, []) -> []
      | NonTerminal(Params, [grp; paramssub])
          -> (manufact_indent (concrete_to_abstract grp)) :: (make_params_list paramssub)
      | _ -> ( report_error "illegal parameter" ; [Invalid] )

    and manufact_indent abstr =
      AbsBlock(DeepenIndent, AbsBlock(abstr, ShallowIndent))