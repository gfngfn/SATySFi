
exception StackUnderflow

exception LineUnderflow

module Stack (* : sig

  type 'a t

  val empty : 'a t
  val pop : ('a t ref) -> 'a
  val push : ('a t ref) -> 'a -> unit
  val to_list : ('a t) -> ('a list)

end *) = struct

  type 'a t = 'a list

  let empty = []

  (* 'a t ref -> 'a *)
  let pop rfstk =
    match !rfstk with
      [] -> raise StackUnderflow
    | head :: tail -> (rfstk := tail ; head)

  (* 'a t ref -> 'a -> unit *)
  let push rfstk cnt =
    rfstk := cnt :: !rfstk

  (* 'a t ref -> 'a *)
  let top rfstk =
    match !rfstk with
      [] -> raise StackUnderflow
    | head :: tail -> head

  (* 'a t -> 'a t -> 'a t *)
  let rec concat lsta lstb =
    match lsta with
      [] -> lstb
    | head :: tail -> head :: (concat tail lstb)

  (* 'a t -> 'a list *)
  let rec to_list stk =
    match stk with
      [] -> []
    | head :: tail -> concat (to_list tail) [head]

end

type nonterminal = Total | Sentence | Block | Group | Args | Params | ListBySep
type tree = Terminal of token | NonTerminal of nonterminal * (tree list)

(*
module McdParser : sig

  type state
  type tree_and_state

  val mcdparser : (token list) -> tree

end = struct
*)

  type state = (unit -> unit)
  type tree_and_state = tree * state

  let input_line : tree list ref = ref []
  let output_stack : tree_and_state Stack.t ref = ref Stack.empty


  (* for test *)
  let print_tree_node tr =
    match tr with
      NonTerminal(nontm, lst) -> (
        match nontm with
          Total -> print_string "T "
        | Sentence -> print_string "S "
        | Block -> print_string "B "
        | Group -> print_string "G "
        | Args -> print_string "A "
        | Params -> print_string "P "
        | ListBySep -> print_string "L "
      )
    | Terminal(tm) -> (
        match tm with
          CTRLSEQ(c) -> print_string "[ctrlseq] "
        | VAR(c) -> print_string "[var] "
        | ID(c) -> print_string "[id] "
        | END -> print_string "[end] "
        | BGRP -> print_string "[{] "
        | EGRP -> print_string "[}] "
        | CHAR(c) -> print_string "[char] "
        | SEP -> print_string "[sep] "
        | BEGINNING_OF_INPUT -> print_string "[!] "
        | END_OF_INPUT -> print_string "[$] "
        | POP -> print_string "[pop] "
        | MACRO -> print_string "[macro] "
      )

  (* for test *)
  let rec print_output_sub stk =
    match stk with
      [] -> print_string "output: "
    | (tr, st) :: tail -> ( print_output_sub tail ; print_tree_node tr )

  let print_output stk =
  (*
    print_output_sub stk ; print_newline ()
  *)
    ()

  (* for test *)
  let rec print_input ln =
  (*
    match ln with
      [] -> ( print_string ":input" ; print_newline () )
    | head :: tail -> ( print_tree_node head ; print_input tail )
  *)
    ()

  (* string -> unit *)
  let report_error errmsg =
    print_string ("[ERROR IN MCDPARSER] " ^ errmsg ^ ":") ; print_newline ()
    (* ; output_stack := Stack.empty *)

  let print_process stat = 
    (* print_string stat ; print_newline () *)
    ()

  (* unit -> tree *)
  let pop_from_line () =
    match !input_line with
      [] -> raise LineUnderflow (*error*)
    | head :: tail -> (input_line := tail ; head)

  (* unit -> tree *)
  let top_of_line () =
    match !input_line with
      [] -> raise LineUnderflow (*error*)
    | head :: tail -> head

  (* token list -> tree list *)
  let rec convert_token_list_into_tree_list toklst =
    match toklst with
      [] -> []
    | head :: tail -> (Terminal(head)) :: (convert_token_list_into_tree_list tail)

  (* 'a list -> 'a list *)
  let elim_first lst =
    match lst with
      [] -> []
    | head :: tail -> tail

  (* tree list -> tree *)
  let get_first lst =
    match lst with
      [] -> raise LineUnderflow
    | head :: tail -> head

  (* token list -> unit *)
  let make_line input =
    input_line := convert_token_list_into_tree_list input

  (* (tree * state) list -> tree list *)
  let rec eliminate_state lst =
    match lst with
      [] -> []
    | (tr, st) :: tail -> tr :: (eliminate_state tail)


  let rec mcdparser (input : token list) =
    make_line input ;
    output_stack := Stack.empty ;
    Stack.push output_stack (Terminal(BEGINNING_OF_INPUT), q_first) ;

    q_first () ;
    print_process "[END OF MCDPASER]" ;
    get_first (eliminate_state (elim_first (Stack.to_list !output_stack)))

  (* tree -> state -> unit *)
  and shift content q =
    Stack.push output_stack (content, q) ;
    print_output !output_stack ;
    print_input !input_line ;
    q ()

  (* nonterminal * int -> unit *)
  and reduce nontm num =
    reduce_sub [] nontm num

  (* tree list -> nonterminal -> int -> unit *)
  and reduce_sub trlst nontm num =
    if num == 0 then (
      match Stack.top output_stack with
        (tr, st) -> (
          input_line := (NonTerminal(nontm, trlst)) :: !input_line ;
          print_process "reduce" ;
          print_output !output_stack ;
          print_input !input_line ;
          st ()
        )
    ) else
      match Stack.pop output_stack with
        (tr, st) -> reduce_sub (tr :: trlst) nontm (num - 1)

  and reduce_empty nontm q =
    input_line := (NonTerminal(nontm, [])) :: !input_line ; 
    print_process "reduce_empty" ;
    print_output !output_stack ;
    print_input !input_line ;
    q ()

  and q_dummy () =
    print_process "q_dummy" ;
    ()

  and q_first () =
    print_process "q_first" ;
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
      Terminal(END_OF_INPUT) -> reduce_empty Block q_first
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Total, lst) -> shift popped q_total
        | NonTerminal(Block, lst) -> shift popped q_end
        | NonTerminal(Sentence, lst) -> shift popped q_after_sentence
        | Terminal(VAR(c)) -> shift popped q_var1
        | Terminal(CHAR(c)) -> shift popped q_char
        | Terminal(POP) -> shift popped q_pop1
        | Terminal(MACRO) -> shift popped q_macro1
(*        | Terminal(MACROWID) -> q_macrowid1 () *)
        | Terminal(CTRLSEQ(c)) -> shift popped q_after_ctrlseq
        | _ -> report_error "illegal first token"
    )

  and q_total () =
    print_process "q_total" ;
    ()

  and q_after_sentence () =
    print_process "q_after_sentence" ;
  (*
    B -> S.B
    B -> .S B
    B -> .             (reduce [$], [}], [sep])
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
      Terminal(EGRP) -> reduce_empty Block q_after_sentence
    | Terminal(END_OF_INPUT) -> reduce_empty Block q_after_sentence
    | Terminal(SEP) -> reduce_empty Block q_after_sentence
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
    print_process "q_inner_of_group" ;
  (*
    G -> [{].L [}]
    L -> .B [sep] L
    L -> .B
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
      Terminal(EGRP) -> reduce_empty Block q_inner_of_group
    | Terminal(END_OF_INPUT) -> reduce_empty Block q_inner_of_group
    | Terminal(SEP) -> reduce_empty Block q_inner_of_group
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Block, lst) -> shift popped q_inner_of_list_by_sep
        | NonTerminal(ListBySep, lst) -> shift popped q_after_inner_of_group
        | NonTerminal(Sentence, lst) -> shift popped q_after_sentence
        | Terminal(VAR(c)) -> shift popped q_var1
        | Terminal(CHAR(c)) -> shift popped q_char
        | Terminal(POP) -> shift popped q_pop1
        | Terminal(MACRO) -> shift popped q_macro1
(*        | Terminal(MACROWID) -> shift popped q_macrowid1 *)
        | Terminal(CTRLSEQ(c)) -> shift popped q_after_ctrlseq
        | _ -> report_error "inappropriate token after sentence"
    )

  and q_inner_of_list_by_sep () =
    print_process "q_inner_of_list_by_sep" ;
  (*
    L -> B.[sep] L
    L -> B.           (reduce [$], [}])
  *)
    match top_of_line () with
      Terminal(EGRP) -> reduce ListBySep 1
    | Terminal(END_OF_INPUT) -> reduce ListBySep 1
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          Terminal(SEP) -> shift popped q_after_sep
        | _ -> report_error "illegal end of list in group"
      )

  and q_after_sep () =
    print_process "q_after_sep" ;
  (*
    L -> B [sep]. L
    L -> .B [sep] L
    L -> .B
    B -> .S B
    B -> .             (reduce [$], [}], [sep])
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
      Terminal(EGRP) -> reduce_empty Block q_inner_of_group
    | Terminal(END_OF_INPUT) -> reduce_empty Block q_inner_of_group
    | Terminal(SEP) -> reduce_empty Block q_inner_of_group
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Block, lst) -> shift popped q_inner_of_list_by_sep
        | NonTerminal(ListBySep, lst) -> shift popped q_end_of_list
        | NonTerminal(Sentence, lst) -> shift popped q_after_sentence
        | Terminal(VAR(c)) -> shift popped q_var1
        | Terminal(CHAR(c)) -> shift popped q_char
        | Terminal(POP) -> shift popped q_pop1
        | Terminal(MACRO) -> shift popped q_macro1
(*        | Terminal(MACROWID) -> shift popped q_macrowid1 *)
        | Terminal(CTRLSEQ(c)) -> shift popped q_after_ctrlseq
        | _ -> report_error "inappropriate token after sentence"
    )

  and q_end_of_list () =
    print_process "q_end_of_list" ;
  (*
    L -> B [sep] L.
  *)
    reduce ListBySep 3

  and q_after_inner_of_group () =
    print_process "q_after_inner_of_group" ;
  (*
    G -> [{] L.[}]
  *)
    let popped = pop_from_line () in
      match popped with
        Terminal(EGRP) -> shift popped q_end_of_group
      | _ -> report_error "inappropriate end of group"

  and q_end_of_group () =
    print_process "q_end_of_group" ;
  (*
    G -> [{] L [}].
  *)
    reduce Group 3

  and q_var1 () =
    print_process "q_var1" ;
  (*
    S -> [var].[end]
  *)
    let popped = pop_from_line () in
      match popped with
        Terminal(END) -> shift popped q_var2
      | _ -> report_error "missing semicolon after variable"

  and q_var2 () =
    print_process "q_var2" ;
  (*
    S -> [var] [end].
  *)
    reduce Sentence 2

  and q_char () =
    print_process "q_char" ;
  (*
    S -> [char].
  *)
    reduce Sentence 1

  and q_pop1 () =
    print_process "q_pop1" ;
  (*
    S -> [pop].[var] [var] G
  *)
    let popped = pop_from_line () in
      match popped with
        Terminal(VAR(c)) -> shift popped q_pop2
      | _ -> report_error "missing first variable after \\pop"

  and q_pop2 () =
    print_process "q_pop2" ;
  (*
    S -> [pop] [var].[var] G
  *)
    let popped = pop_from_line () in
      match popped with
        Terminal(VAR(c)) -> shift popped q_pop3
      | _ -> report_error "missing second variable after \\pop"

  and q_pop3 () =
    print_process "q_pop3" ;
  (*
    S -> [pop] [var] [var].G
    G -> .[{] L [}]
  *)
    let popped = pop_from_line () in
      match popped with
        Terminal(BGRP) -> shift popped q_inner_of_group
      | _ -> report_error "missing { after \\pop"

  and q_macro1 () =
    print_process "q_macro1" ;
  (*
    S -> [macro].[ctrlseq] A G
  *)
    let popped = pop_from_line () in
      match popped with
        Terminal(CTRLSEQ(c)) -> shift popped q_macro2
      | _ -> report_error "missing control sequence after \\macro"

  and q_macro2 () =
    print_process "q_macro2" ;
  (*
    S -> [macro] [ctrlseq].A G
    A -> .[var] A
    A -> .
  *)
    match top_of_line () with
      Terminal(BGRP) -> reduce_empty Args q_macro2
    | Terminal(END_OF_INPUT) -> reduce_empty Args q_macro2
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Args, lst) -> shift popped q_macro3
        | Terminal(VAR(c)) -> shift popped q_args
        | _ -> report_error "inappropriate argument in \\macro declaration"
    )

  and q_macro3 () =
    print_process "q_macro3" ;
  (*
    S -> [macro] [ctrlseq] A.G
    G -> .[{] L [}]
  *)
    let popped = pop_from_line () in
      match popped with
        NonTerminal(Group, lst) -> shift popped q_macro4
      | Terminal(BGRP) -> shift popped q_inner_of_group
      | _ -> report_error "missing group in \\macro declaration"

  and q_macro4 () =
    print_process "q_macro4" ;
  (*
    S -> [macro] [ctrlseq] A G.
  *)
    reduce Sentence 4

  and q_args () =
    print_process "q_args" ;
  (*
    A -> [var].A
    A -> .[var] A
    A -> .
  *)
    match top_of_line () with
      Terminal(BGRP) -> reduce_empty Args q_args
    | Terminal(END_OF_INPUT) -> reduce_empty Args q_args
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Args, lst) -> shift popped q_end_of_args
        | Terminal(VAR(c)) -> shift popped q_args
        | _ -> report_error "inappropriate argument in \\macro declaration"
    )

  and q_end_of_args () =
  (*
    A -> [var].A
  *)
    reduce Args 2

  and q_after_ctrlseq () =
    print_process "q_after_ctrlseq" ;
  (*
    S -> [ctrlseq].[end]
    S -> [ctrlseq].[id] [end]
    S -> [ctrlseq].G P
    S -> [ctrlseq].[id] G P
    G -> .[{] L [}]
  *)
    let popped = pop_from_line () in
      match popped with
        NonTerminal(Group, lst) -> shift popped q_after_first_group
      | Terminal(END) -> shift popped q_ctrlseq_A
      | Terminal(ID(c)) -> shift popped q_after_id
      | Terminal(BGRP) -> shift popped q_inner_of_group
      | _ -> report_error "illegal token after control sequence"

  and q_after_first_group () =
    print_process "q_after_first_group" ;
  (*
    S -> [ctrlseq] G.P
    P -> .G P
    P -> .
    G -> .[{] L [}]
  *)
    match top_of_line () with
      Terminal(EGRP) -> reduce_empty Params q_after_first_group
    | Terminal(END_OF_INPUT) -> reduce_empty Params q_after_first_group
    | Terminal(SEP) -> reduce_empty Params q_after_first_group
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Params, lst) -> shift popped q_ctrlseq_C
        | NonTerminal(Group, lst) -> shift popped q_params
        | Terminal(BGRP) -> shift popped q_inner_of_group
        | _ -> report_error "inappropriate token after group"
    )

  and q_after_id () =
    print_process "q_after_id" ;
  (*
    S -> [ctrlseq] [id].[end]
    S -> [ctrlseq] [id].G P
    G -> .[{] L [}]
  *)
    let popped = pop_from_line() in
      match popped with
        NonTerminal(Group, lst) -> shift popped q_after_id_and_first_group
      | Terminal(END) -> shift popped q_ctrlseq_C
      | Terminal(BGRP) -> shift popped q_inner_of_group
      | _ -> report_error "inappropriate token after ID"

  and q_after_id_and_first_group () =
    print_process "q_after_id_and_first_group" ;
  (*
    S -> [ctrlseq] [id] G.P
    P -> .G P
    P -> .
    G -> .[{] L [}]
  *)
    match top_of_line () with
      Terminal(EGRP) -> reduce_empty Params q_after_id_and_first_group
    | Terminal(END_OF_INPUT) -> reduce_empty Params q_after_id_and_first_group
    | Terminal(SEP) -> reduce_empty Params q_after_id_and_first_group
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Params, lst) -> shift popped q_ctrlseq_D
        | NonTerminal(Group, lst) -> shift popped q_params
        | Terminal(BGRP) -> shift popped q_inner_of_group
        | _ -> report_error "inappropriate token after id and first group"
    )

  and q_params () =
    print_process "q_params" ;
  (*
    P -> G.P
    P -> .G P
    P -> .
    G -> .[{] L [}]
  *)
    match top_of_line () with
      Terminal(EGRP) -> reduce_empty Params q_params
    | Terminal(END_OF_INPUT) -> reduce_empty Params q_params
    | Terminal(SEP) -> reduce_empty Params q_params
    | _ -> (
      let popped = pop_from_line () in
        match popped with
          NonTerminal(Params, lst) -> shift popped q_params_end
        | NonTerminal(Group, lst) -> shift popped q_params
        | Terminal(BGRP) -> shift popped q_inner_of_group
        | _ -> report_error "inappropriate parameter"
    )

  and q_params_end () =
    print_process "q_params_end" ;
  (*
      P -> G P.
  *)
    reduce Params 2

  and q_ctrlseq_A () =
    print_process "q_ctrlseq_A" ;
  (*
    S -> [ctrlseq] [end].
  *)
    reduce Sentence 2

  and q_ctrlseq_B () =
    print_process "q_ctrlseq_B" ;
  (*
    S -> [ctrlseq] [id] [end].
  *)
    reduce Sentence 3

  and q_ctrlseq_C () =
    print_process "q_ctrlseq_C" ;
  (*
    S -> [ctrlseq] G P.
  *)
    reduce Sentence 3

  and q_ctrlseq_D () =
    print_process "q_ctrlseq_D" ;
  (*
    S -> [ctrlseq] [id] G P.
  *)
    reduce Sentence 4

  and q_after_block () =
    print_process "q_after_block" ;
  (*
    B -> S B.
  *)
    reduce Block 2

  and q_end () =
    print_process "q_end" ;
  (*
    T -> B.[$]
  *)
    let popped = pop_from_line () in
      match popped with
        Terminal(END_OF_INPUT) -> shift popped q_end_of_end (*end of parsing*)
      | _ -> report_error "illegal end"

  and q_end_of_end () =
    print_process "q_end_of_end" ;
  (*
    T -> B [$].
  *)
    reduce Total 2
(*
end
*)
