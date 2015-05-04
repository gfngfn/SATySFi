(* module Mcdabs *)
  open Types

  let report_bug errmsg =
    print_string ("[BUG IN MCDABS] " ^ errmsg ^ ".") ; print_newline ()

  (* tree -> abstract_tree *)
  let rec concrete_to_abstract conctr =
    match conctr with
      (* T -> B [EOI] *)
      NonTerminal(Total, [tr; Terminal(END_OF_INPUT)])
        -> concrete_to_abstract tr

      (*B -> . | S B *)
    | NonTerminal(Block, chdrn) -> (
          match chdrn with
            [] -> EmptyAbsBlock

          | [stc; NonTerminal(Block, [])]
              -> concrete_to_abstract stc

          | [stc; blk]
              -> AbsBlock(concrete_to_abstract stc, concrete_to_abstract blk)

          | _ -> ( report_bug "illegal Block" ; Invalid )
        )
      (* G -> [{] L [}] *)
    | NonTerminal(Group, [Terminal(BGRP); lstbysp; Terminal(EGRP)])
        -> concrete_to_abstract lstbysp

      (* G -> [`] C ['] *)
    | NonTerminal(Group, [Terminal(OPENQT); chofltrl; Terminal(CLOSEQT)])
        -> concrete_to_abstract chofltrl

    | NonTerminal(ListBySep, [NonTerminal(Block, chdrn)])
        -> concrete_to_abstract (NonTerminal(Block, chdrn))

    | NonTerminal(ListBySep, [tr; Terminal(SEP); lstbysp])
        -> Separated(concrete_to_abstract tr, concrete_to_abstract lstbysp)

    | NonTerminal(Sentence, chdrn) -> (
        match chdrn with
        (* S -> [char] *)
          [Terminal(CHAR(c))] -> Output(c)

        (* S -. [space] *)
        | [Terminal(SPACE)] -> Output(" ")

        (* S -. [break] *)
        | [Terminal(BREAK)] -> BreakAndIndent

        (* S -> [var] *)
        | [Terminal(VAR(v)); Terminal(END)] -> ContentOf(v)

        (* S -> [pop] [var] [var] G G *)
        | [Terminal(POP); Terminal(VAR(u)); Terminal(VAR(v)); grp1; grp2]
            -> Pop(u, v, concrete_to_abstract grp1, concrete_to_abstract grp2)

        (* S -> [macro] [ctrlseq] A G *)
        | [Terminal(MACRO); Terminal(CTRLSEQ(f)); args; grp1]
            -> Macro(f, make_args_list args, concrete_to_abstract grp1, EmptyAbsBlock)

        (* S -> [macrowid] [ctrlseq] A G G *)
        | [Terminal(MACROWID); Terminal(CTRLSEQ(f)); args; grp1; grp2]
            -> Macro(f, make_args_list args, concrete_to_abstract grp1, concrete_to_abstract grp2)

        (* S -> [ctrlseq] [end] *)
        | [Terminal(CTRLSEQ(f)); Terminal(END)]
            -> Apply(f, NoID, [])

        (* S -> [ctrlseq] [id] [end] *)
        | [Terminal(CTRLSEQ(f)); Terminal(ID(i)); Terminal(END)]
            -> Apply(f, RealID(i), [])

        (* S -> [ctrlseq] G P *)
        | [Terminal(CTRLSEQ(f)); grp; prms]
            -> Apply(f, NoID, make_params_list (NonTerminal(Params, [grp; prms])))

        (* S -> [ctrlseq] [id] G P *)
        | [Terminal(CTRLSEQ(f)); Terminal(ID(i)); grp; prms]
            -> Apply(f, RealID(i), make_params_list (NonTerminal(Params, [grp; prms])))

        | _ -> ( report_bug "illegal child of sentence" ; Invalid )
      )

    (* C -> . *)
    | NonTerminal(CharOfLiteral, []) -> EmptyAbsBlock

    (* C -> [char] C *)
    | NonTerminal(CharOfLiteral, [Terminal(CHAR(c)); chofltrl]) -> (
          let abstr_chofltrl = concrete_to_abstract chofltrl in
            match abstr_chofltrl with
              EmptyAbsBlock -> Output(c)
            | _ -> AbsBlock(Output(c), abstr_chofltrl)
        )

    | Terminal(END_OF_INPUT) -> EmptyAbsBlock

    | _ -> ( report_bug "illegal concrete tree" ; Invalid )

    (* tree -> var_name list *)
    and make_args_list args =
      match args with
        NonTerminal(Args, []) -> []

      | NonTerminal(Args, [Terminal(VAR(v)); argssub])
          -> v :: (make_args_list argssub)

      | _ -> ( report_bug "illegal argument" ; ["error"] )

    (* tree -> abstract_tree list *)
    and make_params_list prms =
      match prms with
        NonTerminal(Params, []) -> []
      | NonTerminal(Params, [grp; paramssub])
          -> (manufact_indent (concrete_to_abstract grp)) :: (make_params_list paramssub)
      | _ -> ( report_bug "illegal parameter" ; [Invalid] )

    and manufact_indent abstr = abstr
