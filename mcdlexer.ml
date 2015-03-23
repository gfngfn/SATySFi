
type macro_name = string
type var_name = string
type id_name = string
type letter = string

type token = CTRLSEQ of macro_name | VAR of var_name | ID of id_name
           | END | BGRP | EGRP | SEP | CHAR of letter
           | BEGINNING_OF_INPUT | END_OF_INPUT
           | MACRO | POP

module McdLexer : sig

  val mcdlex : string -> (token list)

end = struct

  type token_type = CTRLSEQ_TYPE | VAR_TYPE | ID_TYPE | END_TYPE
                  | BGRP_TYPE | EGRP_TYPE | CHAR_TYPE | SEP_TYPE | INVALID_TYPE
                  | SPACE_TYPE

  let input_buffer : string ref = ref ""
  let pos_start : int ref = ref 0
  let pos_last : int ref = ref 0
  let pos_current : int ref = ref 0
  let last_token_type : token_type ref = ref INVALID_TYPE
  let output_sequence : token list ref = ref []
  let ignore_space : bool ref = ref false

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
  let report_error errmsg =
    print_string ("[ERROR IN MCDLEXER] " ^ errmsg ^ ":") ; print_newline () ;
    print_string (" from " ^ (string_of_int !pos_start)) ; print_newline () ;
    print_string (" to " ^ (string_of_int !pos_current)) ; print_newline () ;
    output_sequence := [END_OF_INPUT]

  let print_process stat =
  (*
    print_string stat ; print_newline ()
  *)
    ()

  let output_token () = 
    let lasttok = get_last_token () in
      match !last_token_type with
        CTRLSEQ_TYPE -> (append_to_sequence (CTRLSEQ(lasttok)) ; ignore_space := true)
      | VAR_TYPE -> (append_to_sequence (VAR(lasttok)) ; ignore_space := true)
      | ID_TYPE -> (append_to_sequence (ID(lasttok)) ; ignore_space := true)
      | END_TYPE -> (append_to_sequence END ; ignore_space := false)
      | BGRP_TYPE -> (append_to_sequence BGRP ; ignore_space := false)
      | EGRP_TYPE -> (append_to_sequence EGRP ; ignore_space := false)
      | CHAR_TYPE -> (append_to_sequence (CHAR(lasttok)) ; ignore_space := false)
      | SEP_TYPE -> (append_to_sequence SEP ; ignore_space := false)
      | SPACE_TYPE -> if !ignore_space then () else append_to_sequence (CHAR(lasttok))
        (* maybe the specification of space letter needs changing *)
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
    ignore_space := false ;

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
    | '\n' -> (save_token_type SPACE_TYPE ; next ())
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

end
