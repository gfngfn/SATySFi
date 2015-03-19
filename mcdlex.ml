
type token_type = CTRLSEQ_TYPE | VAR_TYPE | ID_TYPE | END_TYPE
                | BGRP_TYPE | EGRP_TYPE | CHAR_TYPE | INVALID_TYPE

type token = CTRLSEQ of string | VAR of string | ID of string | END | BGRP | EGRP | CHAR of string
           | MACRO | POP | IFEMPTY | IFSAME

let input_buffer : string ref = ref ""
let pos_start : int ref = ref 0
let pos_last : int ref = ref 0
let pos_current : int ref = ref 0
let last_token_type : token_type ref = ref INVALID_TYPE

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

let output_sequence : token list ref = ref []

let rec append_element lst elem = (
  match lst with
    [] -> [elem]
  | head :: tail -> head :: (append_element tail elem)
)
let append_to_sequence elem = (
  output_sequence := append_element !output_sequence elem
)

let mcdlex_error errmsg = (
  print_string ("[Error in mcdlex] " ^ errmsg ^ ":") ; print_newline () ;
  print_string (" from " ^ (string_of_int !pos_start)) ; print_newline () ;
  print_string (" to " ^ (string_of_int !pos_current)) ; print_newline ()
)

let output_token () = 
  let lasttok = get_last_token () in
    match !last_token_type with
      CTRLSEQ_TYPE -> append_to_sequence (CTRLSEQ(lasttok))
    | VAR_TYPE -> append_to_sequence (VAR(lasttok))
    | ID_TYPE -> append_to_sequence (ID(lasttok))
    | END_TYPE -> append_to_sequence END
    | BGRP_TYPE -> append_to_sequence BGRP
    | EGRP_TYPE -> append_to_sequence EGRP
    | CHAR_TYPE -> append_to_sequence (CHAR(lasttok))
    | INVALID_TYPE -> mcdlex_error ("invalid token \"" ^ lasttok ^ "\"")

let rec refine_ctrlseq lst =
  match lst with
    [] -> []
  | head :: tail -> (
     match head with
      CTRLSEQ(tok) ->
        if tok == "\\macro" then
          MACRO :: (refine_ctrlseq tail)
        else if tok == "\\pop" then
          POP :: (refine_ctrlseq tail)
        else if tok == "\\ifempty" then
          IFEMPTY :: (refine_ctrlseq tail)
        else if tok == "\\ifsame" then
          IFSAME :: (refine_ctrlseq tail)
        else
          head :: (refine_ctrlseq tail)
    | _ -> head :: (refine_ctrlseq tail)
  )

let rec mcdlex (input: string) =
  input_buffer := input ^ "\000" ;
  pos_start := 0 ;
  pos_last := 0 ;
  pos_current := 0 ;
  output_sequence := [] ;
  q_ini () ;
  refine_ctrlseq !output_sequence

and q_ini () =
  match read_char () with
    ';' -> (save_token_type END_TYPE ; next ())
  | '{' -> (save_token_type BGRP_TYPE ; next ())
  | '}' -> (save_token_type EGRP_TYPE ; next ())
  | '\\' -> (save_token_type CTRLSEQ_TYPE ; q_escape ())
  | '@' -> (save_token_type VAR_TYPE ; q_var ())
  | '#' -> (save_token_type ID_TYPE ; q_id ())
  | '\000' -> (print_string "[END of mcdlex]" ; print_newline ())
  | _ -> (save_token_type CHAR_TYPE ; next ())

and q_escape () =
  match read_char () with
    '\000' -> mcdlex_error "Input ended while scanning an escape sequence"
  | rdch ->
    if (is_basic_char rdch) then (
      save_token_type CTRLSEQ_TYPE ; q_ctrlseq ()
    ) else (
      save_token_type CHAR_TYPE ; next()
    )

and q_ctrlseq () =
  match read_char () with
    '\000' -> mcdlex_error "Input ended while scanning a control sequence"
  | rdch ->
    if (is_basic_char rdch) then (
      save_token_type CTRLSEQ_TYPE ; q_ctrlseq ()
    ) else (
      next ()
    )

and q_var () =
  match read_char () with
    '\000' -> mcdlex_error "Input ended while scanning a variable"
  | rdch ->
    if (is_basic_char rdch) then (
      save_token_type VAR_TYPE ; q_var ()
    ) else (
      next ()
    )

and q_id () =
  match read_char () with
    '\000' -> mcdlex_error "Input ended while scanning an ID"
  | rdch ->
    if (is_basic_char rdch) then (
      save_token_type ID_TYPE ; q_id ()
    ) else (
      next ()
    )

and next () =
  output_token ();
  pos_start := !pos_last ;
  pos_current := !pos_start ;
  last_token_type := INVALID_TYPE ;
  q_ini ()
