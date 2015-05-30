(* module Mcdlexer *)
  open Types

  type token_type = CTRLSEQ_TYPE | STRVAR_TYPE | IDNAME_TYPE | CLASSNAME_TYPE | END_TYPE
                  | BGRP_TYPE | EGRP_TYPE | CHAR_TYPE | SEP_TYPE | INVALID_TYPE
                  | SPACE_TYPE | BREAK_TYPE
                  | COMMENT_TYPE | END_OF_COMMENT_TYPE | IGNORED_TYPE
                  | OPENQT_TYPE | CLOSEQT_TYPE
                  | OPENNUM_TYPE | CLOSENUM_TYPE
                  | OPENSTR_TYPE | CLOSESTR_TYPE
                  | NUMCONST_TYPE | NUMVAR_TYPE
                  | LPAREN_TYPE | RPAREN_TYPE
                  | PLUS_TYPE | MINUS_TYPE
                  | TIMES_TYPE | DIVIDES_TYPE
                  | EQUAL_TYPE
                  | LAND_TYPE | LOR_TYPE
                  | CONCAT_TYPE

  let input_buffer : string ref = ref ""
  let pos_start : int ref = ref 0
  let pos_last : int ref = ref 0
  let pos_current : int ref = ref 0
  let last_token_type : token_type ref = ref INVALID_TYPE
  let output_sequence : token Sequence.t ref = ref Sequence.empty
  let ignore_space : bool ref = ref false
  let numdepth_stack : (int Stacklist.t) ref = ref Stacklist.empty
  let strdepth_stack : (int Stacklist.t) ref = ref Stacklist.empty
  let numdepth : int ref = ref 0
  let strdepth : int ref = ref 0
  let openqtdepth : int ref = ref 0
  let closeqtdepth : int ref = ref 0

  let increment rn =
    rn := !rn + 1

  let decrement rn =
    rn := !rn - 1

  let get_last_token () =
    String.sub !input_buffer !pos_start (!pos_last - !pos_start)

  let save toktp =
    last_token_type := toktp ;
    pos_last := !pos_current

  let read_char () =
    let ch = !input_buffer.[!pos_current] in (
      increment pos_current ;
      print_process ("token: [" ^ (String.make 1 ch) ^ "]") ;
      ch
    )

  let is_numeric_char ch =
    '0' <= ch && ch <= '9'

  let is_basic_char ch =
    (is_numeric_char ch) || ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || (ch == '-')

  let report_error errmsg =
    print_string ("! [ERROR IN LEXER] " ^ errmsg ^ ":\n") ;
    print_string ("    from " ^ (string_of_int !pos_start) ^ "\n") ;
    print_string ("    to " ^ (string_of_int !pos_current) ^ "\n") ;
    output_sequence := Sequence.of_list [EOI]

  let print_process stat =
  
    print_string (stat ^ "\n") ;
  
    ()

  let output_token () = 
    let lasttok = get_last_token () in
      match !last_token_type with
      | CTRLSEQ_TYPE
          -> ( Sequence.append output_sequence (CTRLSEQ(lasttok)) ; ignore_space := true )
      | STRVAR_TYPE
          -> ( Sequence.append output_sequence (STRVAR(lasttok)) ; ignore_space := true )

      | IDNAME_TYPE
          -> ( Sequence.append output_sequence (IDNAME(lasttok)) ; ignore_space := true )

      | CLASSNAME_TYPE
          -> ( Sequence.append output_sequence (CLASSNAME(lasttok)) ; ignore_space := true )

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
                | BREAK -> Sequence.omit_last output_sequence
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
                | BREAK -> Sequence.omit_last output_sequence
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
              if !ignore_space then () else
                Sequence.append output_sequence BREAK
              ;
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
            ignore_space := true
          )
      | CLOSEQT_TYPE
          -> (
            Sequence.append output_sequence CLOSEQT ;
            ignore_space := false
          )

      | COMMENT_TYPE -> ()

      | END_OF_COMMENT_TYPE -> ( ignore_space := true )

      | IGNORED_TYPE -> ()

      | NUMCONST_TYPE -> Sequence.append output_sequence (NUMCONST(lasttok))

      | NUMVAR_TYPE -> Sequence.append output_sequence (NUMVAR(lasttok))

      | OPENNUM_TYPE -> Sequence.append output_sequence OPENNUM
      | CLOSENUM_TYPE -> Sequence.append output_sequence CLOSENUM
      | OPENSTR_TYPE -> Sequence.append output_sequence OPENSTR
      | CLOSESTR_TYPE -> Sequence.append output_sequence CLOSESTR

      | LPAREN_TYPE -> Sequence.append output_sequence LPAREN
      | RPAREN_TYPE -> Sequence.append output_sequence RPAREN
      | PLUS_TYPE -> Sequence.append output_sequence PLUS
      | MINUS_TYPE -> Sequence.append output_sequence MINUS
      | TIMES_TYPE -> Sequence.append output_sequence TIMES
      | DIVIDES_TYPE -> Sequence.append output_sequence DIVIDES
      | EQUAL_TYPE -> Sequence.append output_sequence EQ
      | LAND_TYPE -> Sequence.append output_sequence LAND
      | LOR_TYPE -> Sequence.append output_sequence LOR
      | CONCAT_TYPE -> Sequence.append output_sequence CONCAT

      | INVALID_TYPE -> report_error ("invalid token \"" ^ lasttok ^ "\"")

  let rec lex input =
    input_buffer := input ^ "\000" ;
    pos_start := 0 ;
    pos_last := 0 ;
    pos_current := 0 ;
    last_token_type := INVALID_TYPE ;
    output_sequence := Sequence.empty ;
    numdepth_stack := Stacklist.empty ;
    strdepth_stack := Stacklist.empty ;
    numdepth := 0 ;
    strdepth := 0 ;

    q_numexpr () ;
    Sequence.append output_sequence EOI ;
    Sequence.to_list !output_sequence

  and q_comment () =
      print_process "q_comment" ;
      match read_char () with
        '\n' -> ( save END_OF_COMMENT_TYPE ; cut () ; )
      | _ -> q_comment ()

  and q_literal () =
      print_process "q_literal" ;
      match read_char () with
        '`' -> (
            closeqtdepth := 1 ;
            save (if !openqtdepth == 1 then CLOSEQT_TYPE else CHAR_TYPE) ;
            q_closeqt ()
          )
      | '\000' -> report_error "input ended while reading literal block"
      | _ -> ( save CHAR_TYPE ; cut () ; q_literal () )

  and q_active () =
      print_process "q_active" ;
    match read_char () with
    | ' ' -> ( save IGNORED_TYPE ; cut () ; q_active () )
    | '\t' -> ( save IGNORED_TYPE ; cut () ; q_active () )
    | '\n' -> ( save IGNORED_TYPE ; cut () ; q_active () )
    | ';' -> ( save END_TYPE ; cut () ; q_strexpr () )
    | '{' -> (
          increment strdepth ;
          save BGRP_TYPE ; cut () ; q_strexpr ()
        )
    | '@' -> ( save STRVAR_TYPE ; q_var () )
    | '#' -> ( save IDNAME_TYPE ; q_id () )
    | '.' -> ( save CLASSNAME_TYPE ; q_class () )
    | '(' -> (
          Stacklist.push numdepth_stack !numdepth ; increment numdepth ;
          save OPENNUM_TYPE ; cut () ; q_numexpr ()
        )
    | _ -> report_error "unexpected token at active area"

  and q_numexpr () =
    print_process "q_numexpr" ;
    match read_char () with
    | ' ' -> ( save IGNORED_TYPE ; cut () ; q_numexpr () )
    | '\t' -> ( save IGNORED_TYPE ; cut () ; q_numexpr () )
    | '\n' -> ( save IGNORED_TYPE ; cut () ; q_numexpr () )
    | '(' -> (
          increment numdepth ;
          save LPAREN_TYPE ; cut () ; q_numexpr ()
        )
    | ')' -> (
          decrement numdepth ;
          if !numdepth == Stacklist.top numdepth_stack then (
            Stacklist.delete_top numdepth_stack ;
            save CLOSENUM_TYPE ; cut () ; q_active ()
          ) else (
            save RPAREN_TYPE ; cut () ; q_numexpr ()
          )
        )
    | '+' -> ( save PLUS_TYPE ; cut () ; q_numexpr () )
    | '-' -> ( save MINUS_TYPE ; cut () ; q_numexpr () )
    | '*' -> ( save TIMES_TYPE ; cut () ; q_numexpr () )
    | '/' -> ( save DIVIDES_TYPE ; cut () ; q_numexpr () )
    | '=' -> ( save EQUAL_TYPE ; cut () ; q_numexpr () )
    | '&' -> ( save LAND_TYPE ; cut () ; q_numexpr () )
    | '|' -> ( save LOR_TYPE ; cut () ; q_numexpr () )
    | '^' -> ( save CONCAT_TYPE ; cut () ; q_numexpr () )
    | '{' -> (
          Stacklist.push strdepth_stack !strdepth ; increment strdepth ;
          save OPENSTR_TYPE ; cut () ; q_strexpr ()
        )
    | '\000' -> print_process "[END OF LEXER]"
    | c -> (
          if is_numeric_char c then (
            save NUMCONST_TYPE ; q_constnum ()
          ) else if is_basic_char c then (
            save NUMVAR_TYPE ; q_numvar ()
          ) else (
            report_error "unexpected character in numeric expression"
          )
        )

  and q_constnum () =
    print_process "q_constnum" ;
    let rc = read_char () in
      if is_numeric_char rc then (
        save NUMCONST_TYPE ; q_constnum ()
      ) else (
        cut () ; q_numexpr ()
      )

  and q_numvar () =
    print_process "q_numvar" ;
    let rc = read_char () in
      if is_basic_char rc then (
        save NUMVAR_TYPE ; q_numvar ()
      ) else (
        cut () ; q_numexpr ()
      )

  and q_strexpr () =
    print_process "q_strexpr" ;
    match read_char () with
    | '\\' -> ( save CTRLSEQ_TYPE ; q_escape () )
    | '{' -> (
          increment strdepth ;
          save BGRP_TYPE ; cut () ; q_strexpr ()
        )
    | '}' -> (
          decrement strdepth ;
          if !strdepth == Stacklist.top strdepth_stack then (
            Stacklist.delete_top strdepth_stack ;
            save CLOSESTR_TYPE ; cut () ; q_numexpr ()
          ) else (
            save EGRP_TYPE ; cut () ; q_strexpr ()
          )
        )
    | '@' -> ( save STRVAR_TYPE ; q_var () )
    | '#' -> ( save IDNAME_TYPE ; q_id () )
    | '|' -> ( save SEP_TYPE ; cut () ; q_strexpr () )
    | ' ' -> ( save SPACE_TYPE ; cut () ; q_strexpr () )
    | '\t' -> ( save SPACE_TYPE ; cut () ; q_strexpr () )
    | '\n' -> ( save BREAK_TYPE ; cut () ; q_strexpr () )
    | '`' -> (
          openqtdepth := 1 ;
          save OPENQT_TYPE ; q_openqt ()
        )
    | '%' -> ( save COMMENT_TYPE ; q_comment () )
    | '\000' -> report_error "input ended while reading string expression"
    | _ -> ( save CHAR_TYPE ; cut () ; q_strexpr () )

  and q_escape () =
    print_process "q_escape" ;
    let rdch = read_char () in
      match rdch with
      | '\000' -> report_error "input ended by escape"
      | _ -> (
          if is_basic_char rdch then (
            save CTRLSEQ_TYPE ; q_ctrlseq ()
          ) else (
            save CHAR_TYPE ; increment pos_start ; cut () ;
          )
        )

  and q_ctrlseq () =
    print_process "q_ctrlseq" ;
    let rdch = read_char () in
      if is_basic_char rdch then (
        save CTRLSEQ_TYPE ; q_ctrlseq ()
      ) else (
        cut () ; q_active ()
      )

  and q_var () =
    print_process "q_var" ;
    let rdch = read_char () in
      if is_basic_char rdch then (
        save STRVAR_TYPE ; q_var ()
      ) else (
        cut () ; q_active ()
      )

  and q_id () =
    print_process "q_id" ;
    let rdch = read_char () in
      if is_basic_char rdch then (
        save IDNAME_TYPE ; q_id ()
      ) else (
        cut () ; q_active ()
      )

  and q_class () =
    print_process "q_class" ;
    let rdch = read_char () in
      if is_basic_char rdch then (
        save CLASSNAME_TYPE ; q_class ()
      ) else (
        cut () ; q_active ()
      )

  and q_openqt () =
    print_process "q_openqt" ;
    let rdch = read_char () in
      match rdch with
        '`' -> (
              openqtdepth := !openqtdepth + 1 ;
              save OPENQT_TYPE ; q_openqt ()
            )
      | '\000' -> report_error "input ended while reading the beginning of literal block"
      | _ -> cut () ; q_literal ()

  and q_closeqt () =
    print_process "q_closeqt" ;
    let rdch = read_char () in
      match rdch with
        '`' -> (
          closeqtdepth := !closeqtdepth + 1 ;
          (
            if !closeqtdepth < !openqtdepth then
              save CHAR_TYPE
            else if !closeqtdepth == !openqtdepth then
              save CLOSEQT_TYPE
            else
              report_error "literal block closed with too many '`'s"
          ) ;
          q_closeqt ()
        )
      | '\000' -> report_error "input ended while reading the end of literal block"
      | _ -> ( closeqtdepth := 0 ; cut () ; q_strexpr () )

  and cut () =
    output_token () ;
    pos_start := !pos_last ;
    pos_current := !pos_start ;
    last_token_type := INVALID_TYPE
