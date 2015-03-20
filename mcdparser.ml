
module McdParser = struct

  type nonterminal = Total | Sentence | Block | Group | Args | Params

  type tree = Empty | Terminal of token | NonTerminal of nonterminal * (tree list)

  type state = (unit -> unit)

  type stacked_tree = tree * state

  let input_line : tree list ref = ref []
  let output_stack : stacked_tree list ref = ref []

  (* string -> unit *)
  let report_error errmsg =
    print_string ("[Error in mcdparser] " ^ errmsg ^ ":") ; print_newline ()

  (* unit -> tree *)
  let pop_from_line () =
    match !input_line with
      [] -> Empty (*error*)
    | head :: tail -> (input_line := tail ; head)

  (* unit -> tree *)
  let top_of_line () =
    match !input_line with
      [] -> Empty (*error*)
    | head :: tail -> head

  (* token list -> tree list *)
  let rec convert_token_list_into_tree_list toklst =
    match toklst with
      [] -> []
    | head :: tail -> (Terminal(head)) :: (convert_token_list_into_tree_list tail)

  (* token list -> unit *)
  let make_line input =
    input_line := convert_token_list_into_tree_list input

  (* stacked_tree list -> stacked_tree list *)
  let rec append_element lst elem =
    match lst with
      [] -> [elem]
    | head :: tail -> head :: (append_element tail elem)

  (* stacked_tree -> unit *)
  let append_stack elem =
    output_stack := append_element !output_stack elem

  let rec pop_from_stack lst =
    match lst with
      [] -> report_error "pop_from_stack failed"
    | [last] -> last
    | head :: tail -> pop_from_stack tail

  let pop_from_stack_times num =
    pop_from_stack


  let rec mcdparse (input: token list) =
    make_line input ;
    shift Empty q_first

  (* tree -> state -> unit *)
  and shift content q =
    append_stack (content, q) ; q ()

  (* nonterminal * int -> unit *)
  and reduce nontm num = ()
  (* ****NOW WRITING**** *)

  and q_first () =
  (*
    T -> .B [$]
    B -> .S B
    B -> .              (reduce [$])
    S -> .[var] [end]
    S -> .[char]
    S -> .[pop] [var] [var] G
    S -> .[macro] [ctrlseq] A G
    S -> .[macrowid] [ctrlseq] A G G
    S -> .[ctrlseq] [end]
    S -> .[ctrlseq] [id] [end]
    S -> .[ctrlseq] G P
    S -> .[ctrlseq] [id] G P
  *)
    match top_of_line () with
      Terminal(END_OF_INPUT) -> reduce Block 0
    | _ -> (
    	let popped = pop_from_line () in
        match popped with
          NonTerminal(Block, lst) -> shift popped q_end
        | NonTerminal(Sentence, lst) -> shift popped q_after_sentence
        | Terminal(VAR(c)) -> q_var1 ()
        | Terminal(CHAR(c)) -> q_char ()
        | Terminal(POP) -> q_pop1 ()
        | Terminal(MACRO) -> q_macro1 ()
(*        | Terminal(MACROWID) -> q_macrowid1 () *)
        | Terminal(CTRLSEQ(c)) -> q_after_ctrlseq ()
        | _ -> report_error "illegal first token"
    )

  and q_after_sentence () =
  (*
    B -> S.B
    B -> .S B
    B -> .             (reduce [$], [}])
    S -> .[var] [end]
    S -> .[char]
    S -> .[pop] [var] [var] G
    S -> .[macro] [ctrlseq] A G
    S -> .[macrowid] [ctrlseq] A G G
    S -> .[ctrlseq] [end]
    S -> .[ctrlseq] [id] [end]
    S -> .[ctrlseq] G P
    S -> .[ctrlseq] [id] G P
  *)
    match top_of_line () with
      Terminal(EGRP) -> reduce Block 0
    | Terminal(END_OF_INPUT) -> reduce Block 0
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Block, lst) -> shift popped q_after_block
        | NonTerminal(Sentence, lst) -> shift popped q_after_sentence
        | Terminal(VAR(c)) -> shift popped q_var1
        | Terminal(CHAR(c)) -> shift popped q_char
        | Terminal(POP) -> shift popped q_pop1
        | Terminal(MACRO) -> shift popped q_macro1
(*        | Terminal(MACROWID) -> shift popped q_macrowid1 *)
        | Terminal(CTRLSEQ(c)) -> shift popped q_after_ctrlseq
        | _ -> report_error "inappropriate token after sentence"
    )

  and q_inner_of_group () =
  (*
    B -> [{].B [}]
    B -> .S B
    B -> .             (reduce [$], [}])
    S -> .[var] [end]
    S -> .[char]
    S -> .[pop] [var] [var] G
    S -> .[macro] [ctrlseq] A G
    S -> .[macrowid] [ctrlseq] A G G
    S -> .[ctrlseq] [end]
    S -> .[ctrlseq] [id] [end]
    S -> .[ctrlseq] G P
    S -> .[ctrlseq] [id] G P
  *)
    match top_of_line () with
      Terminal(EGRP) -> reduce Block 0
    | Terminal(END_OF_INPUT) -> reduce Block 0
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Block, lst) -> shift popped q_after_block
        | NonTerminal(Sentence, lst) -> shift popped q_after_sentence
        | Terminal(VAR(c)) -> shift popped q_var1
        | Terminal(CHAR(c)) -> shift popped q_char
        | Terminal(POP) -> shift popped q_pop1
        | Terminal(MACRO) -> shift popped q_macro1
(*        | Terminal(MACROWID) -> shift popped q_macrowid1 *)
        | Terminal(CTRLSEQ(c)) -> shift popped q_after_ctrlseq
        | _ -> report_error "inappropriate token after sentence"
    )

  and q_var1 () =
  (*
    S -> [var].[end]
  *)
    let popped = pop_from_line () in
      match popped with
        Terminal(END) -> shift popped q_var2
      | _ -> report_error "missing semicolon after variable"

  and q_var2 () =
  (*
    S -> [var] [end].
  *)
    reduce Sentence 2

  and q_char () =
  (*
    S -> [char].
  *)
    reduce Sentence 1

  and q_pop1 () =
  (*
    S -> [pop].[var] [var] G
  *)
    let popped = pop_from_line () in
      match popped with
        Terminal(VAR(c)) -> shift popped q_pop2
      | _ -> report_error "missing first variable after \\pop"

  and q_pop2 () =
  (*
    S -> [pop] [var].[var] G
  *)
    let popped = pop_from_line () in
      match popped with
        Terminal(VAR(c)) -> shift popped q_pop3
      | _ -> report_error "missing second variable after \\pop"

  and q_pop3 () =
  (*
    S -> [pop] [var] [var].G
    G -> .[{] B [}]
  *)
    let popped = pop_from_line () in
      match popped with
        Terminal(BGRP) -> shift popped q_inner_of_group
      | _ -> report_error "missing { after \\pop"

  and q_macro1 () =
  (*
    S -> [macro].[ctrlseq] A G
  *)
    let popped = pop_from_line () in
      match popped with
        Terminal(CTRLSEQ(c)) -> shift popped q_macro2
      | _ -> report_error "missing control sequence after \\macro"

  and q_macro2 () =
  (*
    S -> [macro] [ctrlseq].A G
    A -> .[var] A
    A -> .
  *)
    match top_of_line () with
      Terminal(BGRP) -> reduce Args 0 (*?*)
    | Terminal(EGRP) -> reduce Args 0
    | Terminal(END_OF_INPUT) -> reduce Args 0
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Args, lst) -> shift popped q_macro3
        | Terminal(VAR(c)) -> shift popped q_args
        | _ -> report_error "inappropriate argument in \\macro declaration"
    )

  and q_macro3 () =
  (*
    S -> [macro] [ctrlseq] A.G
    G -> .[{] B [}]
  *)
    let popped = pop_from_line () in
      match popped with
        NonTerminal(Group, lst) -> shift popped q_macro4
      | Terminal(BGRP) -> shift popped q_inner_of_group
      | _ -> report_error "missing group in \\macro declaration"

  and q_macro4 () =
  (*
    S -> [macro] [ctrlseq] A G.
  *)
    reduce Sentence 4

  and q_args () =
  (*
  	A -> [var].A
  	A -> .[var] A
  	A -> .
  *)
    match top_of_line () with
      Terminal(BGRP) -> reduce Args 0 (*?*)
    | Terminal(EGRP) -> reduce Args 0
    | Terminal(END_OF_INPUT) -> reduce Args 0
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Args, lst) -> shift popped q_macro3
        | Terminal(VAR(c)) -> shift popped q_args
        | _ -> report_error "inappropriate argument in \\macro declaration"
    )

  and q_after_ctrlseq () =
  (*
    S -> [ctrlseq].[end]
    S -> [ctrlseq].[id] [end]
    S -> [ctrlseq].G P
    S -> [ctrlseq].[id] G P
    G -> .[{] B [}]
  *)
    let popped = pop_from_line () in
      match popped with
        NonTerminal(Group, lst) -> shift popped q_after_first_group
      | Terminal(END) -> shift popped q_ctrlseq_A
      | Terminal(ID(c)) -> shift popped q_after_id
      | Terminal(BGRP) -> shift popped q_inner_of_group
      | _ -> report_error "illegal token after control sequence"

  and q_after_first_group () =
  (*
    S -> [ctrlseq] G.P
    P -> .G P
    P -> .
    G -> .[{] B [}]
  *)
    match top_of_line () with
      Terminal(EGRP) -> reduce Params 0
    | Terminal(END_OF_INPUT) -> reduce Params 0
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Params, lst) -> shift popped q_ctrlseq_C
        | NonTerminal(Group, lst) -> shift popped q_params
        | Terminal(BGRP) -> shift popped q_inner_of_group
        | _ -> report_error "inappropriate token after group"
    )

  and q_after_id () =
  (*
    S -> [ctrlseq] [id].[end]
    S -> [ctrlseq] [id].G P
    G -> .[{] B [}]
  *)
    let popped = pop_from_line() in
      match popped with
        NonTerminal(Group, lst) -> shift popped q_after_id_and_first_group
      | Terminal(END) -> shift popped q_ctrlseq_C
      | Terminal(BGRP) -> shift popped q_inner_of_group
      | _ -> report_error "inappropriate token after ID"

  and q_after_id_and_first_group () =
  (*
    S -> [ctrlseq] [id] G.P
    P -> .G P
    P -> .
    G -> .[{] B [}]
  *)
    match top_of_line () with
      Terminal(EGRP) -> reduce Params 0
    | Terminal(END_OF_INPUT) -> reduce Params 0
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Params, lst) -> shift popped q_ctrlseq_D
        | NonTerminal(Group, lst) -> shift popped q_params
        | Terminal(BGRP) -> shift popped q_inner_of_group
        | _ -> report_error "inappropriate token after id and first group"
    )

  and q_params () =
  (*
  	P -> G.P
  	P -> .G P
  	P -> .
  	G -> .[{] B [}]
  *)
    match top_of_line () with
      Terminal(EGRP) -> reduce Params 0
    | Terminal(END_OF_INPUT) -> reduce Params 0
    | _ -> (
    	let popped = pop_from_line () in
    	  match popped with
    	    NonTerminal(Params, lst) -> shift popped q_params_end
    	  | NonTerminal(Group, lst) -> shift popped q_params
    	  | Terminal(BGRP) -> shift popped q_inner_of_group
    	  | _ -> report_error "inappropriate parameter"
    )

  and q_params_end () =
  (*
  	  P -> G P.
  *)
    reduce Params 2

  and q_ctrlseq_A () =
  (*
    S -> [ctrlseq] [end].
  *)
    reduce Sentence 2

  and q_ctrlseq_B () =
  (*
    S -> [ctrlseq] [id] [end].
  *)
    reduce Sentence 3

  and q_ctrlseq_C () =
  (*
    S -> [ctrlseq] G P.
  *)
    reduce Sentence 3

  and q_ctrlseq_D () =
  (*
    S -> [ctrlseq] [id] G P.
  *)
    reduce Sentence 4

  and q_after_block () =
  (*
    B -> S B.
  *)
    reduce Block 2

  and q_end () =
  (*
    T -> B.[$]
  *)
    match pop_from_line () with
     | Terminal(END_OF_INPUT) -> () (*end of parsing*)
     | _ -> report_error "illegal end"


end
