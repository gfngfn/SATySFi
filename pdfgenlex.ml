(* A simple lexer, which distinguishes integers, floats and single character
delimiters. Quoted strings are also distinguished, and allow escaped quotes.
Any other non-whitespace-including string is returned as an [Ident]. *)
open Pdfutil

(* To avoid too much storage allocation (and hence garbage collection), we use
the same data type for this very basic lexing module as for the main lexing in
Pdfread. Eventually, we may unify this with the parsing type too. *)
type t =
  | LexNull
  | LexBool      of bool
  | LexInt       of int
  | LexReal      of float
  | LexString    of string
  | LexStringHex of string
  | LexName      of string
  | LexLeftSquare
  | LexRightSquare
  | LexLeftDict
  | LexRightDict
  | LexStream    of Pdf.stream
  | LexEndStream
  | LexObj
  | LexEndObj
  | LexR
  | LexComment
  | StopLexing
  | LexNone

let string_of_token = function
  | LexInt i -> "Int " ^ string_of_int i
  | LexReal f -> "Float " ^ string_of_float f
  | LexString s -> "String " ^ s
  | LexName s -> "Ident " ^ s
  | LexNull -> "Nothing"
  | _ -> "GenLexNone"

let string_of_tokens ts =
  fold_left (fun a b -> a ^ "\n " ^ b) "" (map string_of_token ts)

let is_delimiter = function
  | '(' | ')' | '<' | '>' | '[' | ']' | '{' | '}' | '%' | '/' -> true
  | _ -> false

let is_not_whitespace = function
  | '\000' | '\009' | '\010' | '\012' | ' ' | '\013' -> false
  | _ -> true

let is_whitespace_or_delimiter = function
  | '\000' | '\009' | '\010' | '\012' | ' ' | '\013'
  | '(' | ')' | '<' | '>' | '[' | ']' | '{' | '}' | '%' | '/' -> true
  | _ -> false

let lex_item s =
  let len = String.length s in
    if len = 0 then LexNull else
      try
        match String.unsafe_get s 0 with
        | 'a'..'z' | 'A'..'Z' ->
            LexName (String.copy s)
        | '\"' when len >= 2 ->
            LexString (String.sub s 1 (len - 2))
        | _ ->
            let rec isint s pos =
              pos = ~-1 ||
                match String.unsafe_get s pos with
                | '.' -> false
                | _ -> isint s (pos - 1)
            in
              if isint s (len - 1)
                then LexInt (int_of_string s)
                else LexReal (float_of_string s)
      with
        _ -> LexName (String.copy s)

(* Return the string between and including the current position and before the
next character satisfying a given predicate, leaving the position at the
character following the last one returned. End of input is considered a
delimiter, and the characters up to it are returned if it is reached. *)
let rec lengthuntil i n =
  match i.Pdfio.input_byte () with
  | x when x = Pdfio.no_more -> n
  | x ->
     if is_whitespace_or_delimiter (Char.unsafe_chr x)
       then n
       else lengthuntil i (n + 1)

(* Pre-built strings to prevent allocation just to do int_of_string,
float_of_string etc. What we actually need is int_of_substring etc, but this
will require patching OCaml. *)
let strings =
 [|"";
   " ";
   "  ";
   "   ";
   "    ";
   "     ";
   "      ";
   "       ";
   "        ";
   "         ";
   "          ";
   "           ";
   "            ";
   "             ";
   "              ";
   "               ";
   "                "|]

let getuntil i =
  let p = i.Pdfio.pos_in () in
    let l = lengthuntil i 0 in
      i.Pdfio.seek_in p;
      let s = if l <= 16 then Array.unsafe_get strings l else String.create l in
        Pdfio.setinit_string i s 0 l;
        s

(* The same, but don't return anything. *)
let rec ignoreuntil f i =
  match i.Pdfio.input_byte () with
  | x when x = Pdfio.no_more -> ()
  | x -> if f (Char.unsafe_chr x) then Pdfio.rewind i else ignoreuntil f i

(* Position on the next non-whitespace character. *)
let dropwhite i =
  ignoreuntil is_not_whitespace i

(* Get a quoted string, including the quotes. Any quotes inside must be
escaped. *)
let rec get_string_inner b i =
  match i.Pdfio.input_byte () with
  | x when x = Pdfio.no_more -> raise End_of_file
  | x when x = int_of_char '\"' ->
       Buffer.add_char b '\"'
  | x when x = int_of_char '\\' ->
      begin match i.Pdfio.input_byte () with
      | x when x = Pdfio.no_more-> raise End_of_file
      | x when x = int_of_char '\"' ->
            Buffer.add_char b '\"';
            get_string_inner b i
      | x ->
         Buffer.add_char b '\\';
         Buffer.add_char b (Char.unsafe_chr x);
         get_string_inner b i
      end
  | x ->
     Buffer.add_char b (Char.unsafe_chr x);
     get_string_inner b i

let b = Buffer.create 30

let get_string i =
  Pdfio.nudge i;
  Buffer.clear b;
  Buffer.add_char b '\"';
  get_string_inner b i;
  Buffer.contents b

(* Repeatedly take a whitespace-or-delimiter-delimited section from the input,
and scan it *)
let get_section i =
  match Pdfio.peek_byte i with
  | x when x = Pdfio.no_more -> ""
  | _ ->
      dropwhite i;
      match Pdfio.peek_byte i with
      | x when x = Pdfio.no_more -> ""
      | x when Char.unsafe_chr x = '\"' -> get_string i
      | x ->
          let x = Char.unsafe_chr x in
            if is_delimiter x
              then (Pdfio.nudge i; string_of_char x)
              else getuntil i

let lex_single i =
  lex_item (get_section i)

let rec lex_inner prev i =
  match lex_item (get_section i) with
  | LexNull -> rev prev
  | x -> lex_inner (x::prev) i

let lex = lex_inner []

let lex_string s =
  lex (Pdfio.input_of_string s)

