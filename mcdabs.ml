
type abstract_id_name = NoID | RealID of id_name

type abstract_tree = EmptyAbsBlock
                   | AbsBlock of abstract_tree * abstract_tree
                   | ContentOf of var_name
                   | Output of letter
                   | Pop of var_name * var_name * abstract_tree * abstract_tree
                   | Macro of macro_name * (var_name list) * abstract_tree * abstract_tree
                   | Apply of macro_name * abstract_id_name * (abstract_tree list)
                   | Invalid
                   | Separated of abstract_tree * abstract_tree
                   | DeepenIndent
                   | ShallowIndent

module McdAbs : sig

  val concrete_to_abstract : tree -> abstract_tree

end = struct

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
        )
    | NonTerminal(Group, [Terminal(BGRP); lstbysp; Terminal(EGRP)]) -> (
          print_process "#Group" ;
          concrete_to_abstract lstbysp
        )
    | NonTerminal(ListBySep, [NonTerminal(Block, chdrn)]) -> (
          print_process "#ListBySep []" ;
          concrete_to_abstract (NonTerminal(Block, chdrn))
    	    (* Separated(concrete_to_abstract (NonTerminal(Block, chdrn)), EndOfSeparated) *)
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
              Apply(f, NoID, (concrete_to_abstract grp) :: (make_params_list prms))
            )
        (* S -> [ctrlseq] [id] G P *)
        | [Terminal(CTRLSEQ(f)); Terminal(ID(i)); grp; prms]
            -> (
              print_process "#Sentence CTRLSEQ4" ;
              Apply(f, RealID(i), (concrete_to_abstract grp) :: (make_params_list prms))
            )
        | _ -> ( report_error "illegal child of sentence" ; Invalid )
      )
    | Terminal(END_OF_INPUT) -> (
    	    print_process "#END_OF_INPUT" ;
    	    EmptyAbsBlock
        )
    | _ -> (
    	  report_error "illegal concrete tree" ;
    	  McdParser.print_tree_node conctr ;
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
          -> (concrete_to_abstract grp) :: (make_params_list paramssub)
      | _ -> ( report_error "illegal parameter" ; [Invalid] )

end
