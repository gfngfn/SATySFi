(* module Mcdabs *)
  open Types

  type literal_reading_state = Normal | ReadingSpace

  let report_bug errmsg =
    print_string ("[BUG IN MCDABS] " ^ errmsg ^ ".") ; print_newline ()

  (* tree -> abstract_tree *)
  let rec concrete_to_abstract conctr =
    match conctr with
      (* T -> B [EOI] *)
    | NonTerminal(Total, [tr; Terminal(END_OF_INPUT)])
        -> concrete_to_abstract tr

      (*B -> . | S B *)
    | NonTerminal(Block, chdrn) -> (
          match chdrn with
          | [] -> EmptyAbsBlock

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
        -> omit_spaces chofltrl

    | NonTerminal(ListBySep, [NonTerminal(Block, chdrn)])
        -> concrete_to_abstract (NonTerminal(Block, chdrn))

    | NonTerminal(ListBySep, [tr; Terminal(SEP); lstbysp])
        -> Separated(concrete_to_abstract tr, concrete_to_abstract lstbysp)

    | NonTerminal(Sentence, chdrn) -> (
        match chdrn with
        (* S -> [char] *)
        | [Terminal(CHAR(c))] -> Output(c)

        (* S -. [space] *)
        | [Terminal(SPACE)] -> Output(" ")

        (* S -. [break] *)
        | [Terminal(BREAK)] -> BreakAndIndent

        (* S -> [var] *)
        | [Terminal(VAR(v)); Terminal(END)] -> ContentOf(v)

        (* S -> [pop] [var] [var] G G *)
        | [Terminal(POP); Terminal(VAR(u)); Terminal(VAR(v)); grp1; grp2]
            -> Pop(u, v, concrete_to_abstract grp1, concrete_to_abstract grp2)

        (* S -> [popchar] [var] [var] G G *)
        | [Terminal(POPCHAR); Terminal(VAR(u)); Terminal(VAR(v)); grp1; grp2]
            -> PopChar(u, v, concrete_to_abstract grp1, concrete_to_abstract grp2)

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
      | NonTerminal(Args, []) -> []

      | NonTerminal(Args, [Terminal(VAR(v)); argssub])
          -> v :: (make_args_list argssub)

      | _ -> ( report_bug "illegal argument" ; ["error"] )

    (* tree -> abstract_tree list *)
    and make_params_list prms =
      match prms with
      | NonTerminal(Params, []) -> []
      | NonTerminal(Params, [grp; paramssub])
          -> (concrete_to_abstract grp) :: (make_params_list paramssub)
      | _ -> ( report_bug "illegal parameter" ; [Invalid] )

    and stringify_literal chofltrl =
      match chofltrl with
      | NonTerminal(CharOfLiteral, []) -> ""

      | NonTerminal(CharOfLiteral, [Terminal(CHAR(c)); chofltrlsub])
          -> c ^ (stringify_literal chofltrlsub)

      | _ -> ( report_bug "illegal literal" ; "" )

    (* tree -> abstract_tree *)
    and omit_spaces chofltrl =
      let str_ltrl = stringify_literal chofltrl in
        let min_indent = min_indent_space str_ltrl in
          let str_shaved = shave_indent str_ltrl min_indent in
            if str_shaved.[(String.length str_shaved) - 1] = '\n' then
              let str_no_last_break = String.sub str_shaved 0 ((String.length str_shaved) - 1) in
                AbsBlock(Output(str_no_last_break), BreakAndIndent)
            else
              Output(str_shaved)

    (* string -> int *)
    and min_indent_space str_ltrl =
      min_indent_space_sub str_ltrl 0 Normal 0 (String.length str_ltrl)

    (* string -> int -> literal_reading_state -> int -> int -> int *)
    and min_indent_space_sub str_ltrl index lrstate spnum minspnum =
      if index >= (String.length str_ltrl) then
        ( print_string ("min_indent: " ^ (string_of_int minspnum) ^ "\n") ;(* for test *)
          minspnum
        )(* for test *)
      else
        match lrstate with
        | Normal -> (
              match str_ltrl.[index] with
              | '\n' -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace 0 minspnum
              | _  -> min_indent_space_sub str_ltrl (index + 1) Normal 0 minspnum
            )
        | ReadingSpace -> (
              match str_ltrl.[index] with
              | ' ' -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace (spnum + 1) minspnum
              | '\n' -> min_indent_space_sub str_ltrl (index + 1) ReadingSpace 0 minspnum
                  (* does not take space-only line into account *)
              | _ -> min_indent_space_sub str_ltrl (index + 1) Normal 0 (if spnum < minspnum then spnum else minspnum)
            )

      and shave_indent str_ltrl minspnum =
        shave_indent_sub str_ltrl minspnum 0 "" Normal 0

      and shave_indent_sub str_ltrl minspnum index str_constr lrstate spnum =
        if index >= (String.length str_ltrl) then
          str_constr
        else
          match lrstate with
          | Normal -> (
                match str_ltrl.[index] with
                | '\n' -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ "\n") ReadingSpace 0
                | ch -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ (String.make 1 ch)) Normal 0
              )
          | ReadingSpace -> (
                match str_ltrl.[index] with
                | ' ' ->
                    if spnum < minspnum then
                      shave_indent_sub str_ltrl minspnum (index + 1) str_constr ReadingSpace (spnum + 1)
                    else
                      shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ " ") ReadingSpace (spnum + 1)
                | '\n' -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ "\n") ReadingSpace 0
                | ch -> shave_indent_sub str_ltrl minspnum (index + 1) (str_constr ^ (String.make 1 ch)) Normal 0
              )
