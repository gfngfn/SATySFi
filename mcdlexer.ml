(* module Mcdlexer *)

  open Types

  type token_type = CTRLSEQ_TYPE | VAR_TYPE | ID_TYPE | END_TYPE
                  | BGRP_TYPE | EGRP_TYPE | CHAR_TYPE | SEP_TYPE | INVALID_TYPE
                  | SPACE_TYPE | BREAK_TYPE | INDENT_TYPE | COMMENT_TYPE
                  | BLTRL_TYPE | ELTRL_TYPE

  let input_buffer : string ref = ref ""
  let pos_start : int ref = ref 0
  let pos_last : int ref = ref 0
  let pos_current : int ref = ref 0
  let last_token_type : token_type ref = ref INVALID_TYPE
  let output_sequence : token list ref = ref []
  let ignore_space : bool ref = ref false
  let in_comment : bool ref = ref false
  let in_literal : bool ref = ref false
  let top_of_line : bool ref = ref false

  let get_last_token () =
    String.sub !input_buffer !pos_start (!pos_last - !pos_start)

  let save_token_type toktp =
    last_token_type := toktp ;
    pos_last := !pos_current

  let read_char () =
     let ch = !input_buffer.[!pos_current] in (pos_current := !pos_current + 1 ; ch)

  let is_basic_char ch =
    ('0' <= ch && ch <= '9') || ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || (ch == '-')

  let rec append_element lst elem =
    match lst with
      [] -> [elem]
    | head :: tail -> head :: (append_element tail elem)

  let append_to_sequence elem =
    output_sequence := append_element !output_sequence elem

  let rec omit_last_element lst = 
    match lst with
      [] -> []
    | [e] -> []
    | head :: tail -> head :: (omit_last_element tail)

  let omit_last_from_sequence () =
    output_sequence := omit_last_element !output_sequence

  let rec get_last_element lst =
    match lst with
      [] -> raise SequenceUnderflow
    | [e] -> e
    | head :: tail -> get_last_element tail

  let get_last_of_sequence () =
    get_last_element !output_sequence

  let report_error errmsg =
    print_string ("[ERROR IN LEXER] " ^ errmsg ^ ":") ; print_newline () ;
    print_string (" from " ^ (string_of_int !pos_start)) ; print_newline () ;
    print_string (" to " ^ (string_of_int !pos_current)) ; print_newline () ;
    output_sequence := [END_OF_INPUT]

  let print_process stat =
  (*
    print_string stat ; print_newline () ;
  *)
    ()

  let output_token () = 
    let lasttok = get_last_token () in
      match !last_token_type with
        CTRLSEQ_TYPE
          -> if !in_comment then () else (
            append_to_sequence (CTRLSEQ(lasttok)) ;
            ignore_space := true ;
            top_of_line := false
          )
      | VAR_TYPE
          -> if !in_comment then () else (
            append_to_sequence (VAR(lasttok)) ;
            ignore_space := true ;
            top_of_line := false
          )
      | ID_TYPE
          -> if !in_comment then () else (
            append_to_sequence (ID(lasttok)) ;
            ignore_space := true ;
            top_of_line := false
          )
      | END_TYPE
          -> if !in_comment then () else (
            append_to_sequence END ;
            ignore_space := false ;
            top_of_line := false
          )
      | BGRP_TYPE
          -> if !in_comment then () else (
            append_to_sequence BGRP ;
            ignore_space := false ;
            top_of_line := false
          )
      | EGRP_TYPE
          -> if !in_comment then () else (
              (
                match get_last_of_sequence () with
                  BREAK -> (
                      (* delete BREAK *)
                      omit_last_from_sequence () ;
                      append_to_sequence FINALBREAK
                    )
                | _ -> ()
              ) ;
              append_to_sequence EGRP ;
              ignore_space := false ;
              top_of_line := false
            )
      | CHAR_TYPE
          -> if !in_comment then () else (
            append_to_sequence (CHAR(lasttok)) ;
            ignore_space := false ;
            top_of_line := false
          )
      | SEP_TYPE
          -> if !in_comment then () else (
              (
                match get_last_of_sequence () with
                  BREAK -> (
                      (* delete BREAK *)
                      omit_last_from_sequence () ;
                      append_to_sequence FINALBREAK ;
                    )
                | _ -> ()
              ) ;
              append_to_sequence SEP ;
              ignore_space := false ;
              top_of_line := false
          )
      | INDENT_TYPE
          ->
            if !in_comment then () else (
              append_to_sequence (VAR("~indent")) ;
              append_to_sequence END ;
              ignore_space := true ;
              top_of_line := false
            )
      | SPACE_TYPE
          -> (
            (
            	if !in_comment then () else
                if !ignore_space then () else
                  append_to_sequence (CHAR(lasttok))
            ) ;
            top_of_line := false
          )
      | BREAK_TYPE
          -> (
            if !in_comment then (
              in_comment := false ;
              ignore_space := true ;
              top_of_line := true
            ) else (
            	if !in_literal then (
            	  append_to_sequence (CHAR("\n")) ;
                top_of_line := true
              ) else (
                if !ignore_space then () else
                  append_to_sequence BREAK
                ) ;
                ignore_space := true ;
                top_of_line := true
              )
          )
      | BLTRL_TYPE
          -> (
            if !in_comment then () else (
              append_to_sequence (BLTRL(String.sub lasttok 0 ((String.length lasttok) - 1)))
            ) ;
            ignore_space := false ;
            top_of_line := true
          )
      | ELTRL_TYPE
          -> (
            if !in_comment then () else (
              append_to_sequence ELTRL
            ) ;
            ignore_space := true ;
            top_of_line := true
          )
      | COMMENT_TYPE -> (
            in_comment := true ;
            top_of_line := false
          )
      | INVALID_TYPE -> report_error ("invalid token \"" ^ lasttok ^ "\"")

  let rec refine_ctrlseq lst =
    match lst with
      [] -> []
    | head :: tail -> (
      match head with
        CTRLSEQ("\\macro") -> MACRO :: (refine_ctrlseq tail)
      | CTRLSEQ("\\macro-with-id") -> MACROWID :: (refine_ctrlseq tail)
      | CTRLSEQ("\\pop") -> POP :: (refine_ctrlseq tail)
      | _ -> head :: (refine_ctrlseq tail)
    )

  let rec mcdlex (input: string) =
    input_buffer := input ^ "\000" ;
    pos_start := 0 ;
    pos_last := 0 ;
    pos_current := 0 ;
    last_token_type := INVALID_TYPE ;
    output_sequence := [] ;
    ignore_space := true ;
    in_comment := false ;
    in_literal := false ;
    top_of_line := true ;

    q_ini () ;
    append_to_sequence END_OF_INPUT ;
    refine_ctrlseq !output_sequence

  and q_ini () =
    if !in_literal then (
      print_process "q_ini (in literal)" ;
      match read_char () with
        '~' -> (
            if !top_of_line then (
              save_token_type CHAR_TYPE ; q_end_literal ()
            ) else (
              save_token_type CHAR_TYPE ; next ()
            )
          )
      | '\000' -> report_error "input ended while reading literal block"
      | '\n' -> ( save_token_type BREAK_TYPE ; next () )
      | _ -> ( save_token_type CHAR_TYPE ; next () )
    ) else (
      match read_char () with
        ';' -> ( save_token_type END_TYPE ; next () )
      | '{' -> ( save_token_type BGRP_TYPE ; next () )
      | '}' -> ( save_token_type EGRP_TYPE ; next () )
      | '\\' -> ( save_token_type CTRLSEQ_TYPE ; q_escape () )
      | '@' -> ( save_token_type VAR_TYPE ; q_var () )
      | '#' -> ( save_token_type ID_TYPE ; q_id () )
      | '|' -> ( save_token_type SEP_TYPE ; next () )
      | ' ' -> ( save_token_type SPACE_TYPE ; next () )
      | '\t' -> ( save_token_type SPACE_TYPE ; next () )
      | '\n' -> ( save_token_type BREAK_TYPE ; next () )
      | '~' -> (
            if !top_of_line then (
              save_token_type BLTRL_TYPE ; q_begin_literal ()
            ) else (
              save_token_type CHAR_TYPE ; next ()
            )
          )
      | '%' -> (save_token_type COMMENT_TYPE ; next ())
      | '\000' -> print_process "[END OF LEXER]"
      | _ -> (save_token_type CHAR_TYPE ; next ())
    )

  and q_escape () =
    print_process "q_escape" ;
    let rdch = read_char () in
      match rdch with
        '\000' -> (report_error "input ended by escape")
      | _ -> (
          if (is_basic_char rdch) then (
            save_token_type CTRLSEQ_TYPE ; q_ctrlseq ()
          ) else (
            save_token_type CHAR_TYPE ; pos_start := !pos_start + 1 ; next ()
          )
        )

  and q_ctrlseq () =
    print_process "q_ctrlseq" ;
    let rdch = read_char () in
      if (is_basic_char rdch) then (
        save_token_type CTRLSEQ_TYPE ; q_ctrlseq ()
      ) else (
        next ()
      )

  and q_var () =
    print_process "q_var" ;
    let rdch = read_char () in
      if (is_basic_char rdch) then (
        save_token_type VAR_TYPE ; q_var ()
      ) else (
        next ()
      )

  and q_id () =
    print_process "q_id" ;
    let rdch = read_char () in
      if (is_basic_char rdch) then (
        save_token_type ID_TYPE ; q_id ()
      ) else (
        next ()
      )

  and q_begin_literal () =
    print_process "q_begin_literal" ;
    let rdch = read_char () in
      match rdch with
        '\n' -> (
            save_token_type BLTRL_TYPE ;
            in_literal := true ;
            ignore_space := true ;
            next ()
          )
      | '\000' -> report_error "input ended while reading the beginning of literal block"
      | _ -> ( save_token_type BLTRL_TYPE ; q_begin_literal () )

  and q_end_literal () =
    print_process "q_end_literal" ;
    let rdch = read_char () in
      match rdch with
        '/' -> ( save_token_type ELTRL_TYPE ; q_end_literal2 () )
      | '\000' -> report_error "input ended while reading the end of literal block"
      | _ -> next ()

  and q_end_literal2 () =
    print_process "q_end_literal2" ;
    let rdch = read_char () in
      match rdch with
        '\n' -> (
            save_token_type ELTRL_TYPE ;
            in_literal := false ;
            next ()
          )
      | _ -> report_error "literal block ended without break"

  and next () =
    output_token () ;
    pos_start := !pos_last ;
    pos_current := !pos_start ;
    last_token_type := INVALID_TYPE ;
    q_ini ()
