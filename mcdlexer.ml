(* module Mcdlexer *)

  open Types

  type token_type = CTRLSEQ_TYPE | VAR_TYPE | ID_TYPE | END_TYPE
                  | BGRP_TYPE | EGRP_TYPE | CHAR_TYPE | SEP_TYPE | INVALID_TYPE
                  | SPACE_TYPE | BREAK_TYPE | INDENT_TYPE | COMMENT_TYPE

  let input_buffer : string ref = ref ""
  let pos_start : int ref = ref 0
  let pos_last : int ref = ref 0
  let pos_current : int ref = ref 0
  let last_token_type : token_type ref = ref INVALID_TYPE
  let output_sequence : token list ref = ref []
  let ignore_space : bool ref = ref false
  let in_comment : bool ref = ref false
  let after_break : bool ref = ref false

  let get_last_token () = (
    String.sub !input_buffer !pos_start (!pos_last - !pos_start)
  )
  let save_token_type toktp = (
    last_token_type := toktp ;
    pos_last := !pos_current
  )
  let read_char () = (
     let ch = !input_buffer.[!pos_current] in (pos_current := !pos_current + 1 ; ch)
  )
  let is_basic_char ch = (
    ('0' <= ch && ch <= '9') || ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || (ch == '-')
  )
  let rec append_element lst elem = (
    match lst with
      [] -> [elem]
    | head :: tail -> head :: (append_element tail elem)
  )
  let append_to_sequence elem = (
    output_sequence := append_element !output_sequence elem
  )
  let rec omit_last_element lst = 
    match lst with
      [] -> []
    | [e] -> []
    | head :: tail -> head :: (omit_last_element tail)

  let omit_last_from_sequence () =
    output_sequence := omit_last_element !output_sequence

  let report_error errmsg =
    print_string ("[ERROR IN MCDLEXER] " ^ errmsg ^ ":") ; print_newline () ;
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
            after_break := false
          )
      | VAR_TYPE
          -> if !in_comment then () else (
            append_to_sequence (VAR(lasttok)) ;
            ignore_space := true ;
            after_break := false
          )
      | ID_TYPE
          -> if !in_comment then () else (
            append_to_sequence (ID(lasttok)) ;
            ignore_space := true ;
            after_break := false
          )
      | END_TYPE
          -> if !in_comment then () else (
            append_to_sequence END ;
            ignore_space := false ;
            after_break := false
          )
      | BGRP_TYPE
          -> if !in_comment then () else (
            append_to_sequence BGRP ;
            ignore_space := false ;
            after_break := false
          )
      | EGRP_TYPE
          -> if !in_comment then () else (
              (
                if !after_break then (
                  (* delete 3 tokens CHAR("\n"), VAR("~indent"), END *)
                  omit_last_from_sequence () ;
                  omit_last_from_sequence () ;
                  omit_last_from_sequence () ;
                  append_to_sequence FINALBREAK ;
                ) else ()
              ) ;
              append_to_sequence EGRP ;
              ignore_space := false ;
              after_break := false
            )
      | CHAR_TYPE
          -> if !in_comment then () else (
            append_to_sequence (CHAR(lasttok)) ;
            ignore_space := false ;
            after_break := false
          )
      | SEP_TYPE
          -> if !in_comment then () else (
              (
                if !after_break then (
                  (* delete 3 tokens CHAR("\n"), VAR("~indent"), END *)
                  omit_last_from_sequence () ;
                  omit_last_from_sequence () ;
                  omit_last_from_sequence () ;
                  append_to_sequence FINALBREAK ;
                ) else ()
              ) ;
              append_to_sequence SEP ;
              ignore_space := false ;
              after_break := false
          )
      | INDENT_TYPE
          ->
            if !in_comment then () else (
              append_to_sequence (VAR("~indent")) ;
              append_to_sequence END ;
              ignore_space := true ;
              after_break := false
            )
      | SPACE_TYPE
          -> (
            if !in_comment then () else
              if !ignore_space then () else
                append_to_sequence (CHAR(lasttok))
          )
      | BREAK_TYPE
          -> (
            if !in_comment then (
              in_comment := false ;
              ignore_space := true
            ) else (
              (
                if !ignore_space then () else (
                  append_to_sequence (CHAR(lasttok)) ;
                  append_to_sequence (VAR("~indent")) ;
                  append_to_sequence END
                )
              ) ;
              ignore_space := true ;
              after_break := true
            )
          )
      | COMMENT_TYPE -> (
            in_comment := true ;
            after_break := false
          )
      | INVALID_TYPE -> report_error ("invalid token \"" ^ lasttok ^ "\"")

  let rec refine_ctrlseq lst =
    match lst with
      [] -> []
    | head :: tail -> (
      match head with
        CTRLSEQ("\\macro") -> MACRO :: (refine_ctrlseq tail)
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
    after_break := false ;

    q_ini () ;
    append_to_sequence END_OF_INPUT ;
    refine_ctrlseq !output_sequence

  and q_ini () =
    match read_char () with
      ';' -> (save_token_type END_TYPE ; next ())
    | '{' -> (save_token_type BGRP_TYPE ; next ())
    | '}' -> (save_token_type EGRP_TYPE ; next ())
    | '\\' -> (save_token_type CTRLSEQ_TYPE ; q_escape ())
    | '@' -> (save_token_type VAR_TYPE ; q_var ())
    | '#' -> (save_token_type ID_TYPE ; q_id ())
    | '|' -> (save_token_type SEP_TYPE ; next ())
    | ' ' -> (save_token_type SPACE_TYPE ; next ())
    | '\t' -> (save_token_type SPACE_TYPE ; next ())
    | '\n' -> (save_token_type BREAK_TYPE ; next ())
    | '~' -> (save_token_type INDENT_TYPE ; next ())
    | '%' -> (save_token_type COMMENT_TYPE ; next ())
    | '\000' -> print_process "[END OF MCDLEXER]"
    | _ -> (save_token_type CHAR_TYPE ; next ())

  and q_escape () =
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
    let rdch = read_char () in
      if (is_basic_char rdch) then (
        save_token_type CTRLSEQ_TYPE ; q_ctrlseq ()
      ) else (
        next ()
      )

  and q_var () =
    let rdch = read_char () in
      if (is_basic_char rdch) then (
        save_token_type VAR_TYPE ; q_var ()
      ) else (
        next ()
      )

  and q_id () =
    let rdch = read_char () in
      if (is_basic_char rdch) then (
        save_token_type ID_TYPE ; q_id ()
      ) else (
        next ()
      )

  and next () =
    output_token () ;
    pos_start := !pos_last ;
    pos_current := !pos_start ;
    last_token_type := INVALID_TYPE ;
    q_ini ()
