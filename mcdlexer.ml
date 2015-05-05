(* module Mcdlexer *)
  open Types

  type token_type = CTRLSEQ_TYPE | VAR_TYPE | ID_TYPE | END_TYPE
                  | BGRP_TYPE | EGRP_TYPE | CHAR_TYPE | SEP_TYPE | INVALID_TYPE
                  | SPACE_TYPE | BREAK_TYPE
                  | COMMENT_TYPE | END_OF_COMMENT_TYPE | IGNORED_TYPE
                  | OPENQT_TYPE | CLOSEQT_TYPE

  let input_buffer : string ref = ref ""
  let pos_start : int ref = ref 0
  let pos_last : int ref = ref 0
  let pos_current : int ref = ref 0
  let last_token_type : token_type ref = ref INVALID_TYPE
  let output_sequence : token Sequence.t ref = ref Sequence.empty
  let ignore_space : bool ref = ref false
  let in_comment : bool ref = ref false
  let in_literal : bool ref = ref false
  let openqtdepth : int ref = ref 0
  let closeqtdepth : int ref = ref 0

  let get_last_token () =
    String.sub !input_buffer !pos_start (!pos_last - !pos_start)

  let save_token_type toktp =
    last_token_type := toktp ;
    pos_last := !pos_current

  let read_char () =
     let ch = !input_buffer.[!pos_current] in (pos_current := !pos_current + 1 ; ch)

  let is_basic_char ch =
    ('0' <= ch && ch <= '9') || ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || (ch == '-')


  let report_error errmsg =
    print_string ("! [ERROR IN LEXER] " ^ errmsg ^ ":") ; print_newline () ;
    print_string ("    from " ^ (string_of_int !pos_start)) ; print_newline () ;
    print_string ("    to " ^ (string_of_int !pos_current)) ; print_newline () ;
    output_sequence := Sequence.of_list [END_OF_INPUT]

  let print_process stat =
  (*
    print_string stat ; print_newline () ;
  *)
    ()

  let output_token () = 
    let lasttok = get_last_token () in
      match !last_token_type with
        CTRLSEQ_TYPE
          -> (
            (
              match lasttok with
                "\\macro" -> Sequence.append output_sequence (MACRO)
              | "\\macro-with-id" -> Sequence.append output_sequence (MACROWID)
              | "\\pop" -> Sequence.append output_sequence (POP)
              | _ -> Sequence.append output_sequence (CTRLSEQ(lasttok))
            ) ;
            ignore_space := true
          )
      | VAR_TYPE
          -> ( Sequence.append output_sequence (VAR(lasttok)) ; ignore_space := true )

      | ID_TYPE
          -> ( Sequence.append output_sequence (ID(lasttok)) ; ignore_space := true )

      | END_TYPE
          -> ( Sequence.append output_sequence END ; ignore_space := false )

      | BGRP_TYPE
          -> (
            (
              match Sequence.get_last output_sequence with
                BREAK -> Sequence.omit_last output_sequence
              | SPACE -> Sequence.omit_last output_sequence
              | _ -> ()
            ) ;
            Sequence.append output_sequence BGRP ; ignore_space := true
          )
      | EGRP_TYPE
          -> (
              (
                match Sequence.get_last output_sequence with
                  BREAK -> Sequence.omit_last output_sequence
                | SPACE -> Sequence.omit_last output_sequence
                | _ -> ()
              ) ;
              Sequence.append output_sequence EGRP ; ignore_space := false
            )
      | CHAR_TYPE
          -> ( Sequence.append output_sequence (CHAR(lasttok)) ; ignore_space := false )

      | SEP_TYPE
          -> (
              (
                match Sequence.get_last output_sequence with
                  BREAK -> Sequence.omit_last output_sequence
                | SPACE -> Sequence.omit_last output_sequence
                | _ -> ()
              ) ;
              Sequence.append output_sequence SEP ;
              ignore_space := true
          )
      | SPACE_TYPE
          -> (
            if !ignore_space then () else
              Sequence.append output_sequence SPACE
          )
      | BREAK_TYPE
          -> (
              if !in_literal then (
                Sequence.append output_sequence (CHAR("\n"))
              ) else (
                if !ignore_space then () else
                  Sequence.append output_sequence BREAK
              ) ;
              ignore_space := true
            )
      | OPENQT_TYPE
          -> (
            (
              match Sequence.get_last output_sequence with
                BREAK -> Sequence.omit_last output_sequence
              | SPACE -> Sequence.omit_last output_sequence
              | _ -> ()
            ) ;
            Sequence.append output_sequence OPENQT ;
            in_literal := true ;
            ignore_space := true
          )

      | CLOSEQT_TYPE
          -> (
            Sequence.append output_sequence CLOSEQT ;
            in_literal := false ;
            ignore_space := false
          )

      | COMMENT_TYPE -> in_comment := true

      | END_OF_COMMENT_TYPE -> ( in_comment := false ; ignore_space := true )

      | IGNORED_TYPE -> ()

      | INVALID_TYPE -> report_error ("invalid token \"" ^ lasttok ^ "\"")

  let rec mcdlex (input: string) =
    input_buffer := input ^ "\000" ;
    pos_start := 0 ;
    pos_last := 0 ;
    pos_current := 0 ;
    last_token_type := INVALID_TYPE ;
    output_sequence := Sequence.empty ;
    ignore_space := true ;
    in_comment := false ;
    in_literal := false ;

    q_ini () ;
    Sequence.append output_sequence END_OF_INPUT ;
    Sequence.to_list !output_sequence

  and q_ini () =
    if !in_comment then
      match read_char () with
        '\n' -> ( save_token_type END_OF_COMMENT_TYPE ; next () )
      | _ -> ( save_token_type IGNORED_TYPE ; next () )

    else if !in_literal then (
      print_process "q_ini (in literal)" ;
      match read_char () with
        '`' -> (
            closeqtdepth := 1 ;
            (
              if !openqtdepth == 1 then
                save_token_type CLOSEQT_TYPE
              else
                save_token_type CHAR_TYPE
            ) ;
            q_end_literal ()
          )
    (*  | ' ' -> ( save_token_type SPACE_TYPE ; next () ) *)
    (*  | '\n' -> ( save_token_type BREAK_TYPE ; next () ) *)
      | '\000' -> report_error "input ended while reading literal block"
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
      | '`' -> (
            openqtdepth := 1 ;
            save_token_type OPENQT_TYPE ;
            q_begin_literal ()
          )
      | '%' -> ( save_token_type COMMENT_TYPE ; next () )
      | '\000' -> print_process "[END OF LEXER]"
      | _ -> ( save_token_type CHAR_TYPE ; next () )
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
    print_process "q_begin_literal_sub" ;
    let rdch = read_char () in
      match rdch with
        '`' -> (
              openqtdepth := !openqtdepth + 1 ;
              save_token_type OPENQT_TYPE ;
              q_begin_literal ()
            )
      | '\000' -> report_error "input ended while reading the beginning of literal block"
      | _ -> next ()

  and q_end_literal () =
    print_process "q_end_literal" ;
    let rdch = read_char () in
      match rdch with
        '`' -> (
          closeqtdepth := !closeqtdepth + 1 ;
          (
            if !closeqtdepth < !openqtdepth then
              save_token_type CHAR_TYPE
            else if !closeqtdepth == !openqtdepth then
              save_token_type CLOSEQT_TYPE
            else
              report_error "literal block closed with too many '`'s"
          )
          ; q_end_literal ()
        )
      | '\000' -> report_error "input ended while reading the end of literal block"
      | _ -> ( closeqtdepth := 0 ; next () )

  and next () =
    output_token () ;
    pos_start := !pos_last ;
    pos_current := !pos_start ;
    last_token_type := INVALID_TYPE ;
    q_ini ()
