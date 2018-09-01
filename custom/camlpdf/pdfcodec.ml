open Pdfutil
open Pdfio

(* Zlib inflate level *)
let flate_level = ref 6

(* Get the next non-whitespace character in a stream. *)
let rec get_streamchar skipped s =
  match s.input_byte () with
  | x when x = Pdfio.no_more -> raise End_of_file
  | x ->
    let chr = Char.unsafe_chr x in
      if Pdf.is_whitespace chr then
        begin
          incr skipped;
          get_streamchar skipped s
        end
      else chr

(* Raised if there was bad data. *)
exception Couldn'tDecodeStream of string

(* Raised if the codec was not supported. *)
exception DecodeNotSupported of string

(* ASCIIHex *)

(* We build a list of decoded characters from the input stream, and then
convert this to the output stream. *)
let encode_ASCIIHex stream =
  let size = bytes_size stream in
    let stream' = mkbytes (size * 2 + 1) in
      bset stream' (size * 2) (int_of_char '>'); (*r ['>'] is end-of-data *)
      for p = 0 to size - 1 do
        let chars = explode (Printf.sprintf "%02X" (bget stream p)) in
          bset stream' (p * 2) (int_of_char (hd chars));
          bset stream' (p * 2 + 1) (int_of_char (hd (tl chars)))
      done;
      stream'

(* Decode ASCIIHex *)

(* Calulate a character from two hex digits a and b *)
let char_of_hex a b =
  char_of_int (int_of_string ("0x" ^ string_of_char a ^ string_of_char b))

let decode_ASCIIHex i =
  let output = ref []
  in let enddata = ref false in
    try
      while not !enddata do
        let b = get_streamchar (ref 0) i in
          let b' = get_streamchar (ref 0) i in
            match b, b' with
            | '>', _ ->
                (* Rewind for inline images *)
                set enddata; rewind i
            | c, '>'
                when
                  (c >= '0' && c <= '9') ||
                  (c >= 'a' && c <= 'f') ||
                  (c >= 'A' && c <= 'F')
                ->
                  output =| char_of_hex c '0';
                  set enddata
            | c, c'
                when
                    ((c >= '0' && c <= '9') ||
                     (c >= 'a' && c <= 'f') ||
                     (c >= 'A' && c <= 'F'))
                  &&
                    ((c' >= '0' && c' <= '9') ||
                     (c' >= 'a' && c' <= 'f') ||
                     (c' >= 'A' && c' <= 'F')) 
                ->
                  output =| char_of_hex c c'
            | _ -> raise Not_found (*r Bad data. *)
      done;
      bytes_of_charlist (rev !output)
    with
      | End_of_file ->
          (* We ran out of data. This is a normal exit. *)
          bytes_of_charlist (rev !output)
      | Not_found ->
          raise (Couldn'tDecodeStream "ASCIIHex")

(* ASCII85 *)

(* Decode five characters. *)
let decode_5bytes c1 c2 c3 c4 c5 n o =
  let d x p =
    i32mul (i32ofi (int_of_char x - 33)) (i32ofi (pow p 85))
  in
    let total =
      i32add
        (i32add (d c1 4) (d c2 3))
        (i32add (d c3 2) (i32add (d c4 1) (d c5 0)))
    in
      let extract t =
        char_of_int (i32toi (lsr32 (lsl32 total (24 - t)) 24))
      in
        if n = 4 then extract 0::extract 8::extract 16::extract 24::o
        else if n = 3 then extract 8::extract 16::extract 24::o
        else if n = 2 then extract 16::extract 24::o
        else if n = 1 then extract 24::o
        else o

let conso cs o =
  match cs with
  | [c1; c2; c3; c4; c5] -> decode_5bytes c1 c2 c3 c4 c5 4 o
  | [c1; c2; c3; c4] -> decode_5bytes c1 c2 c3 c4 '~' 3 o
  | [c1; c2; c3] -> decode_5bytes c1 c2 c3 '~' '>' 2 o
  | [c1; c2] -> decode_5bytes c1 c2 '~' '>' '!' 1 o
  | _ -> o

let rec decode_ASCII85_i i cs o =
  match i.input_byte () with
  | x when x = Pdfio.no_more -> raise End_of_file
  | x ->
     match char_of_int x with
     | c when Pdf.is_whitespace c ->
       if length cs = 5
         then decode_ASCII85_i i [] (conso (rev cs) o)
         else decode_ASCII85_i i cs o
     | 'z' ->
       decode_ASCII85_i i [] ('\000'::'\000'::'\000'::'\000'::conso (rev cs) o)
     | '~' ->
       begin match i.input_byte () with
       | x when x = Pdfio.no_more -> raise End_of_file
       | x ->
          match char_of_int x with
          | '>' ->
             bytes_of_charlist (rev (conso (rev cs) o))
          | c ->
             if length cs = 5
               then decode_ASCII85_i i [c] (conso (rev cs) o)
               else decode_ASCII85_i i (c::cs) o
       end
     | c when c >= '!' && c <= 'u' ->
       if length cs = 5
         then (decode_ASCII85_i i [c] (conso (rev cs) o))
         else (decode_ASCII85_i i (c::cs) o)
     | _ -> raise (Failure "A")

let decode_ASCII85 i =
  try decode_ASCII85_i i [] [] with
    _ -> raise (Pdf.PDFError "Error decoding ASCII85 stream")

(* Encode a single symbol set. *)
let encode_4bytes = function
  | [b1; b2; b3; b4] ->
      let ( * ) = Int64.mul
      in let ( - ) = Int64.sub
      in let ( / ) = Int64.div 
      in let rem = Int64.rem in
        let numbers =
          [i64ofi (int_of_char b1) * i64ofi (pow 3 256);
           i64ofi (int_of_char b2) * i64ofi (pow 2 256);
           i64ofi (int_of_char b3) * i64ofi (pow 1 256);
           i64ofi (int_of_char b4) * i64ofi (pow 0 256)]
        in
          let t = fold_left Int64.add Int64.zero numbers
          in let one85 = i64ofi (pow 1 85) in let two85 = i64ofi (pow 2 85)
          in let three85 = i64ofi (pow 3 85) in let zero85 = i64ofi (pow 0 85)
          in let four85 = i64ofi (pow 4 85) in
            let t, c5 = t - rem t one85, rem t one85 / zero85 in
              let t, c4 = t - rem t two85, rem t two85 / one85 in
                let t, c3 = t - rem t three85, rem t three85 / two85 in
                  let t, c2 = t - rem t four85, rem t four85 / three85 in
                    let i1, i2, i3, i4, i5 =
                      i64toi (t / four85),
                      i64toi c2, i64toi c3, i64toi c4, i64toi c5
                    in
                      i1, i2, i3, i4, i5
  | _ -> assert false

(* Encode a stream. *)
let encode_ASCII85 stream =
  let output = ref []
  in let enddata = ref false
  in let istream = input_of_bytes stream in
    while not !enddata do
      let b1 = istream.input_char () in
      let b2 = istream.input_char () in
      let b3 = istream.input_char () in
      let b4 = istream.input_char () in
        match b1, b2, b3, b4 with
        | Some b1, Some b2, Some b3, Some b4 ->
            output := [b1; b2; b3; b4]::!output
        | Some b1, Some b2, Some b3, None ->
            set enddata; output := [b1; b2; b3]::!output
        | Some b1, Some b2, None, None ->
            set enddata; output := [b1; b2]::!output
        | Some b1, None, None, None ->
            set enddata; output := [b1]::!output
        | None, _, _, _ -> set enddata
        | _ -> assert false
    done;
    let fix k = char_of_int (k + 33) in
      let charlists' =
        rev_map
          (fun l ->
             let len = length l in
               if len < 4
               then
                 let l' = l @ (many '\000' (4 - len)) in
                   let c1, c2, c3, c4, c5 = encode_4bytes l' in
                     take [fix c1; fix c2; fix c3; fix c4; fix c5] (len + 1)
               else
                   let c1, c2, c3, c4, c5 = encode_4bytes l in
                     if c1 + c2 + c3 + c4 + c5 = 0
                       then ['z']
                       else [fix c1; fix c2; fix c3; fix c4; fix c5])
          !output
        in
          bytes_of_charlist (flatten charlists' @ ['~'; '>'])

(* Flate *)

(* Make a bytes from a list of strings by taking the contents, in order
from the items, in order. *)
let rec total_length n = function
 | [] -> n
 | h::t -> total_length (n + String.length h) t

let bytes_of_strings_rev strings =
  let len = total_length 0 strings in
    let s = mkbytes len
    and pos = ref (len - 1) in
      iter
        (fun str ->
           for x = String.length str - 1 downto 0 do
             bset_unsafe s !pos (int_of_char (String.unsafe_get str x));
             decr pos
           done)
        strings;
      s

let flate_process f data =
  let strings = ref []
  and pos = ref 0
  and inlength = bytes_size data in
    let input =
      (fun buf ->
         let s = String.length buf in
           let towrite = min (inlength - !pos) s in
             for x = 0 to towrite - 1 do
               String.unsafe_set
                 buf x (Char.unsafe_chr (bget_unsafe data !pos));
                 incr pos
             done;
             towrite)
    and output =
      (fun buf length ->
         if length > 0 then strings =| String.sub buf 0 length)
    in
      f input output;
      bytes_of_strings_rev !strings

(* This is for reading zlib compressed things in streams where we need to
commence lexing straightaway afterwards. And so, we can only supply one byte at
a time, continuing until zlib has finished uncompressing. *)
let decode_flate_input i =
  let strings = ref [] in
    let input =
      (fun buf ->
         let s = String.length buf in
           if s > 0 then
             begin
               match i.input_byte () with
               | x when x = Pdfio.no_more -> raise End_of_file
               | x -> String.unsafe_set buf 0 (char_of_int x); 1
             end
           else 0)
    and output =
      (fun buf length ->
         if length > 0 then strings =| String.sub buf 0 length)
    in
      Pdfflate.uncompress input output;
      bytes_of_strings_rev !strings

let encode_flate stream =
  flate_process (Pdfflate.compress ~level:!flate_level) stream

let decode_flate stream =
  if bytes_size stream = 0 then mkbytes 0 else (* Accept the empty stream. *)
    try flate_process Pdfflate.uncompress stream with
      Pdfflate.Error (a, b) ->
        raise (Couldn'tDecodeStream ("Flate" ^ " " ^ a ^ " " ^ b))

(* LZW *)

(* Decode LZW. *)
let decode_lzw early i =
  let prefix_code = Array.make 4096 0
  and append_character = Array.make 4096 0
  and bit_count = ref 0
  and bit_buffer = ref 0l
  and endflush = ref 4
  and code_length = ref 9
  and next_code = ref 258
  and new_code = ref 0
  and old_code = ref 256
  and character = ref 0 in
    let rec decode_string code str =
      if code > 255 then
        decode_string prefix_code.(code) (append_character.(code)::str)
      else
        code::str
    and input_code stream =
      while !bit_count <= 24 do
        let streambyte =
          match stream.input_byte () with
          | b when b = Pdfio.no_more ->
              if !endflush = 0 then raise End_of_file else (decr endflush; 0)
          | b -> b
        in
          bit_buffer :=
            lor32 !bit_buffer (lsl32 (i32ofi streambyte) (24 - !bit_count));
          bit_count += 8
      done;
      let result = Int32.to_int (lsr32 !bit_buffer (32 - !code_length)) in
        bit_buffer := lsl32 !bit_buffer !code_length;
        bit_count -= !code_length;
        result
    and strip_cleartable_codes stream =
      while !old_code = 256 do
        old_code := input_code stream
      done
    and reset_table () =
      next_code := 258;
      code_length := 9;
      old_code := 256
    in
      bit_count := 0; bit_buffer := 0l;
      endflush := 4; reset_table ();
        let outstream, data = input_output_of_bytes 16034
        and finished = ref false in
          strip_cleartable_codes i;
          match !old_code with
          | 257 -> mkbytes 0
          | _ ->
              character := !old_code;
              outstream.output_byte !old_code;
              while not !finished do
                new_code := input_code i;
                match !new_code with
                | 257 -> set finished
                | 256 ->
                   reset_table ();
                   set_array prefix_code 0;
                   set_array append_character 0;
                   strip_cleartable_codes i;
                   character := !old_code;
                   outstream.output_byte !old_code
                | _ ->
                  let chars =
                    if !new_code >= !next_code
                      then (decode_string !old_code []) @ [!character]
                      else decode_string !new_code []
                  in
                    character := hd chars;
                    iter outstream.output_byte chars;
                    prefix_code.(!next_code) <- !old_code;
                    append_character.(!next_code) <- !character;
                    incr next_code;
                    old_code := !new_code;
                    match !next_code + early with
                    | 512 | 1024 | 2048 -> incr code_length
                    | _ -> ()
              done;
              extract_bytes_from_input_output outstream data 

(* CCITT *)

(* Decode a CCITT-encoded stream. Parameter names:
 eol -- /EndOfLine
 eba -- /EncodedByteAlign
 eob -- /EndOfBlock
 bone -- /BlackIs1
 dra -- /DamagedRowsBeforeError
 c -- /Columns
 r -- /Rows
*)

let rec read_white_code i =
  let a = getbitint i in
  let b = getbitint i in
  let c = getbitint i in
  let d = getbitint i in
    match a, b, c, d with
    | 0, 1, 1, 1 -> 2
    | 1, 0, 0, 0 -> 3
    | 1, 0, 1, 1 -> 4
    | 1, 1, 0, 0 -> 5
    | 1, 1, 1, 0 -> 6
    | 1, 1, 1, 1 -> 7
    | _ ->
  let e = getbitint i in
    match a, b, c, d, e with
    | 1, 0, 0, 1, 1 -> 8
    | 1, 0, 1, 0, 0 -> 9
    | 0, 0, 1, 1, 1 -> 10
    | 0, 1, 0, 0, 0 -> 11
    | 1, 1, 0, 1, 1 -> 64 + read_white_code i
    | 1, 0, 0, 1, 0 -> 128 + read_white_code i
    | _ ->
  let f = getbitint i in
    match a, b, c, d, e, f with
    | 0, 0, 0, 1, 1, 1 -> 1
    | 0, 0, 1, 0, 0, 0 -> 12
    | 0, 0, 0, 0, 1, 1 -> 13
    | 1, 1, 0, 1, 0, 0 -> 14
    | 1, 1, 0, 1, 0, 1 -> 15
    | 1, 0, 1, 0, 1, 0 -> 16
    | 1, 0, 1, 0, 1, 1 -> 17
    | 0, 1, 0, 1, 1, 1 -> 192 + read_white_code i
    | 0, 1, 1, 0, 0, 0 -> 1664 + read_white_code i
    | _ ->
  let g = getbitint i in
    match a, b, c, d, e, f, g with
    | 0, 1, 0, 0, 1, 1, 1 -> 18
    | 0, 0, 0, 1, 1, 0, 0 -> 19
    | 0, 0, 0, 1, 0, 0, 0 -> 20
    | 0, 0, 1, 0, 1, 1, 1 -> 21
    | 0, 0, 0, 0, 0, 1, 1 -> 22
    | 0, 0, 0, 0, 1, 0, 0 -> 23
    | 0, 1, 0, 1, 0, 0, 0 -> 24
    | 0, 1, 0, 1, 0, 1, 1 -> 25
    | 0, 0, 1, 0, 0, 1, 1 -> 26
    | 0, 1, 0, 0, 1, 0, 0 -> 27
    | 0, 0, 1, 1, 0, 0, 0 -> 28
    | 0, 1, 1, 0, 1, 1, 1 -> 256 + read_white_code i
    | _ ->
  let h = getbitint i in
    match a, b, c, d, e, f, g, h with
    | 0, 0, 1, 1, 0, 1, 0, 1 -> 0
    | 0, 0, 0, 0, 0, 0, 1, 0 -> 29
    | 0, 0, 0, 0, 0, 0, 1, 1 -> 30
    | 0, 0, 0, 1, 1, 0, 1, 0 -> 31
    | 0, 0, 0, 1, 1, 0, 1, 1 -> 32
    | 0, 0, 0, 1, 0, 0, 1, 0 -> 33
    | 0, 0, 0, 1, 0, 0, 1, 1 -> 34
    | 0, 0, 0, 1, 0, 1, 0, 0 -> 35
    | 0, 0, 0, 1, 0, 1, 0, 1 -> 36
    | 0, 0, 0, 1, 0, 1, 1, 0 -> 37
    | 0, 0, 0, 1, 0, 1, 1, 1 -> 38
    | 0, 0, 1, 0, 1, 0, 0, 0 -> 39
    | 0, 0, 1, 0, 1, 0, 0, 1 -> 40
    | 0, 0, 1, 0, 1, 0, 1, 0 -> 41
    | 0, 0, 1, 0, 1, 0, 1, 1 -> 42
    | 0, 0, 1, 0, 1, 1, 0, 0 -> 43
    | 0, 0, 1, 0, 1, 1, 0, 1 -> 44
    | 0, 0, 0, 0, 0, 1, 0, 0 -> 45
    | 0, 0, 0, 0, 0, 1, 0, 1 -> 46
    | 0, 0, 0, 0, 1, 0, 1, 0 -> 47
    | 0, 0, 0, 0, 1, 0, 1, 1 -> 48
    | 0, 1, 0, 1, 0, 0, 1, 0 -> 49
    | 0, 1, 0, 1, 0, 0, 1, 1 -> 50
    | 0, 1, 0, 1, 0, 1, 0, 0 -> 51
    | 0, 1, 0, 1, 0, 1, 0, 1 -> 52
    | 0, 0, 1, 0, 0, 1, 0, 0 -> 53
    | 0, 0, 1, 0, 0, 1, 0, 1 -> 54
    | 0, 1, 0, 1, 1, 0, 0, 0 -> 55
    | 0, 1, 0, 1, 1, 0, 0, 1 -> 56
    | 0, 1, 0, 1, 1, 0, 1, 0 -> 57
    | 0, 1, 0, 1, 1, 0, 1, 1 -> 58
    | 0, 1, 0, 0, 1, 0, 1, 0 -> 59
    | 0, 1, 0, 0, 1, 0, 1, 1 -> 60
    | 0, 0, 1, 1, 0, 0, 1, 0 -> 61
    | 0, 0, 1, 1, 0, 0, 1, 1 -> 62
    | 0, 0, 1, 1, 0, 1, 0, 0 -> 63
    | 0, 0, 1, 1, 0, 1, 1, 0 -> 320 + read_white_code i
    | 0, 0, 1, 1, 0, 1, 1, 1 -> 384 + read_white_code i
    | 0, 1, 1, 0, 0, 1, 0, 0 -> 448 + read_white_code i
    | 0, 1, 1, 0, 0, 1, 0, 1 -> 512 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 0, 0 -> 576 + read_white_code i
    | 0, 1, 1, 0, 0, 1, 1, 1 -> 640 + read_white_code i
    | _ ->
  let j = getbitint i in
    match a, b, c, d, e, f, g, h, j with
    | 0, 1, 1, 0, 0, 1, 1, 0, 0 -> 704 + read_white_code i
    | 0, 1, 1, 0, 0, 1, 1, 0, 1 -> 768 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 0, 1, 0 -> 832 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 0, 1, 1 -> 896 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 1, 0, 0 -> 960 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 1, 0, 1 -> 1024 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 1, 1, 0 -> 1088 + read_white_code i
    | 0, 1, 1, 0, 1, 0, 1, 1, 1 -> 1152 + read_white_code i
    | 0, 1, 1, 0, 1, 1, 0, 0, 0 -> 1216 + read_white_code i
    | 0, 1, 1, 0, 1, 1, 0, 0, 1 -> 1280 + read_white_code i
    | 0, 1, 1, 0, 1, 1, 0, 1, 0 -> 1344 + read_white_code i
    | 0, 1, 1, 0, 1, 1, 0, 1, 1 -> 1408 + read_white_code i
    | 0, 1, 0, 0, 1, 1, 0, 0, 0 -> 1472 + read_white_code i
    | 0, 1, 0, 0, 1, 1, 0, 0, 1 -> 1536 + read_white_code i
    | 0, 1, 0, 0, 1, 1, 0, 1, 0 -> 1600 + read_white_code i
    | 0, 1, 0, 0, 1, 1, 0, 1, 1 -> 1728 + read_white_code i
    | _ ->
  let k = getbitint i in
  let l = getbitint i in
    match a, b, c, d, e, f, g, h, j, k, l with
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 -> 1792 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0 -> 1856 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1 -> 1920 + read_white_code i
    | _ ->
  let m = getbitint i in
    match a, b, c, d, e, f, g, h, j, k, l, m with
    | 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 -> -1
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0 -> 1984 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1 -> 2048 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0 -> 2112 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1 -> 2176 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0 -> 2240 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1 -> 2304 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0 -> 2368 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1 -> 2432 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0 -> 2496 + read_white_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1 -> 2560 + read_white_code i
    | _ -> raise (Failure "bad white code")

let rec read_black_code i =
  let a = getbitint i in
  let b = getbitint i in
    match a, b with
    | 1, 1 -> 2
    | 1, 0 -> 3
    | _ ->
  let c = getbitint i in
    match a, b, c with
    | 0, 1, 0 -> 1
    | 0, 1, 1 -> 4
    | _ ->
  let d = getbitint i in
    match a, b, c, d with
    | 0, 0, 1, 1 -> 5
    | 0, 0, 1, 0 -> 6
    | _ ->
  let e = getbitint i in
    match a, b, c, d, e with
    | 0, 0, 0, 1, 1 -> 7
    | _ ->
  let f = getbitint i in
    match a, b, c, d, e, f with
    | 0, 0, 0, 1, 0, 1 -> 8
    | 0, 0, 0, 1, 0, 0 -> 9
    | _ ->
  let g = getbitint i in
    match a, b, c, d, e, f, g with
    | 0, 0, 0, 0, 1, 0, 0 -> 10
    | 0, 0, 0, 0, 1, 0, 1 -> 11
    | 0, 0, 0, 0, 1, 1, 1 -> 12
    | _ ->
  let h = getbitint i in
    match a, b, c, d, e, f, g, h with
    | 0, 0, 0, 0, 0, 1, 0, 0 -> 13
    | 0, 0, 0, 0, 0, 1, 1, 1 -> 14
    | _ ->
  let j = getbitint i in
    match a, b, c, d, e, f, g, h, j with
    | 0, 0, 0, 0, 1, 1, 0, 0, 0 -> 15
    | _ ->
  let k = getbitint i in
    match a, b, c, d, e, f, g, h, j, k with
    | 0, 0, 0, 0, 1, 1, 0, 1, 1, 1 -> 0
    | 0, 0, 0, 0, 0, 1, 0, 1, 1, 1 -> 16
    | 0, 0, 0, 0, 0, 1, 1, 0, 0, 0 -> 17
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 -> 18
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 1 -> 64 + read_black_code i
    | _ ->
  let l = getbitint i in
    match a, b, c, d, e, f, g, h, j, k, l with
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1 -> 19
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0 -> 20
    | 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0 -> 21
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1 -> 22
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0 -> 23
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1 -> 24
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0 -> 25
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0 -> 1792 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0 -> 1856 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1 -> 1920 + read_black_code i
    | _ ->
  let m = getbitint i in
    match a, b, c, d, e, f, g, h, j, k, l, m with
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0 -> 26
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1 -> 27
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0 -> 28
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1 -> 29
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0 -> 30
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1 -> 31
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0 -> 32
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1 -> 33
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0 -> 34
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1 -> 35
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0 -> 36
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1 -> 37
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0 -> 38
    | 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1 -> 39
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0 -> 40
    | 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1 -> 41
    | 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0 -> 42
    | 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1 -> 43
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0 -> 44
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1 -> 45
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0 -> 46
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 1 -> 47
    | 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0 -> 48
    | 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1 -> 49
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0 -> 50
    | 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1 -> 51
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0 -> 52
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1 -> 53
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0 -> 54
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1 -> 55
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0 -> 56
    | 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0 -> 57
    | 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 1 -> 58
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1 -> 59
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0 -> 60
    | 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0 -> 61
    | 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0 -> 62
    | 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1 -> 63
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0 -> 128 + read_black_code i
    | 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1 -> 192 + read_black_code i
    | 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1 -> 256 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1 -> 320 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0 -> 384 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1 -> 448 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0 -> 1984 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1 -> 2048 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0 -> 2112 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1 -> 2176 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0 -> 2240 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1 -> 2304 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0 -> 2368 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1 -> 2432 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0 -> 2496 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1 -> 2560 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 -> -1
    | _ ->
  let n = getbitint i in
    match a, b, c, d, e, f, g, h, j, k, l, m, n with
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0 -> 512 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1 -> 576 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0 -> 640 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1 -> 704 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0 -> 768 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1 -> 832 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0 -> 896 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1 -> 960 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0 -> 1024 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1 -> 1088 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0 -> 1152 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1 -> 1216 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0 -> 1280 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 1 -> 1344 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0 -> 1408 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1 -> 1472 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0 -> 1536 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 1 -> 1600 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0 -> 1664 + read_black_code i
    | 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1 -> 1728 + read_black_code i
    | _ -> raise (Failure "bad black code")

(* Group 4 Fax decoder. *)
type modes =
  | Pass
  | Horizontal
  | Vertical of int
  | Uncompressed
  | EOFB

let read_mode i =
  let a = getbitint i in
    match a with
    | 1 -> Vertical 0
    | _ ->
  let b = getbitint i in
  let c = getbitint i in
    match a, b, c with
    | 0, 1, 1 -> Vertical (-1)
    | 0, 1, 0 -> Vertical 1
    | 0, 0, 1 -> Horizontal
    | _ ->
  let d = getbitint i in
    match a, b, c, d with
    | 0, 0, 0, 1 -> Pass
    | _ ->
  let e = getbitint i in
  let f = getbitint i in
    match a, b, c, d, e, f with
    | 0, 0, 0, 0, 1, 1 -> Vertical (-2)
    | 0, 0, 0, 0, 1, 0 -> Vertical 2
    | _ ->
  let g = getbitint i in
    match a, b, c, d, e, f, g with
    | 0, 0, 0, 0, 0, 1, 1 -> Vertical (-3)
    | 0, 0, 0, 0, 0, 1, 0 -> Vertical 3
    | _ ->
  let h = getbitint i in
  let j = getbitint i in
  let k = getbitint i in
  let l = getbitint i in
  let m = getbitint i in
    match a, b, c, d, e, f, g, h, j, k, l, m with
    | 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1 -> Uncompressed
    | 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 ->
      let a = getbitint i in
      let b = getbitint i in
      let c = getbitint i in
      let d = getbitint i in
      let e = getbitint i in
      let f = getbitint i in
      let g = getbitint i in
      let h = getbitint i in
      let j = getbitint i in
      let k = getbitint i in
      let l = getbitint i in
      let m = getbitint i in
        begin match a, b, c, d, e, f, g, h, j, k, l, m with
        | 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 -> EOFB
        | _ -> raise (Failure "Not a valid code on EOFB")
        end
  | _ -> raise (Failure "Not a valid code")

let decode_CCITTFax k eol eba c r eob bone dra input =
  if k > 0 then raise (DecodeNotSupported "CCITTFax k > 0") else
    let whiteval, blackval = if bone then 0, 1 else 1, 0
    in let output = make_write_bitstream () in
      let b = bitbytes_of_input input
      in let column = ref 0
      in let row = ref 0
      in let refline = ref (Array.make c whiteval)
      in let currline = ref (Array.make c 0)
      in let white = ref true
      in let output_line line =
        Array.iter (putbit output) line;
        align_write output
      in
        let output_span l v =
          if l < 0 then raise (Failure "Bad CCITT stream") else
            begin
              for x = !column to !column + l - 1 do
                let r = !currline in r.(x) <- v
              done;
              column += l
            end
        in let find_b1 () =
          let pos = ref !column
          in let curr, opp =
            if !white then whiteval, blackval else blackval, whiteval
          in
            let find v =
              while
                let r = !refline in
                  if !pos >= 0 && !pos < Array.length r then
                    r.(!pos) <> v
                  else
                    false
              do
                incr pos
              done; !pos
            in
              try
                (* Careful to skip imaginary black at beginning *)
                ignore (if !column = 0 && !white then 0 else find curr);
                find opp
              with
                _ -> c
        in let find_b2 () =
          let pos = ref !column
          in let curr, opp =
            if !white then whiteval, blackval else blackval, whiteval
          in
            let find v =
              while
                let r = !refline in
                  if !pos >=0 && !pos < Array.length r then
                    r.(!pos) <> v
                  else
                    false
              do
                incr pos
              done; !pos
            in
              try
                (* Careful to skip imaginary black at beginning *)
                ignore (if !column = 0 && !white then 0 else find curr);
                ignore (find opp);
                find curr
              with
                _ -> c
        in
          try
            while true do
              if !column >= c then
                begin
                  output_line !currline;
                  refline := !currline;
                  column := 0;
                  set white;
                  if eba then align b;
                  incr row;
                  if !row >= r && r > 0 then raise End_of_file
                end
              else
               begin
                  if k < 0 then
                    (* Group 4 *)
                    match read_mode b with
                    | Pass ->
                        output_span
                          (find_b2 () - !column)
                          (if !white then whiteval else blackval)
                    | Horizontal ->
                        if !white then
                          begin
                            output_span (read_white_code b) whiteval;
                            output_span (read_black_code b) blackval;
                          end
                        else
                          begin
                            output_span (read_black_code b) blackval;
                            output_span (read_white_code b) whiteval;
                          end
                    | Vertical n ->
                        output_span
                          (find_b1 () - !column - n)
                          (if !white then whiteval else blackval);
                        flip white
                    | EOFB -> raise End_of_file
                    | Uncompressed ->
                        raise (DecodeNotSupported "CCITT Uncompressed")
                  else if k = 0 then
                    (* Group 3 *)
                    begin match
                      (if !white then read_white_code else read_black_code) b
                    with
                    | -1 ->
                       (* Pad it out *)
                       if !column > 0 then output_span (c - !column) whiteval
                    | l ->
                      begin
                        output_span l (if !white then whiteval else blackval);
                        flip white
                      end
                    end
                  else
                    raise (DecodeNotSupported "CCITT k")
                end
            done;
            mkbytes 0
          with
            | End_of_file -> bytes_of_write_bitstream output
            | _ -> raise (Failure "Bad CCITT Stream") 

(* PNG and TIFF Predictors *)

(* Get the value at index i from an int array a, giving zero if the index is
too low. Fails in the usual manner if the index is too high. *)
let get0 a i =
  if i < 0 then 0 else a.(i)

(* TIFF prediction. 8bpp only for now. We don't have example files for the
others yet, so we'll have to wait to create our own examples. *)
let decode_tiff_predictor colors bpc columns stream =
  match bpc with
  | 1 -> raise (DecodeNotSupported "TIFF Predictor for 1bpc not supported")
  | 2 -> raise (DecodeNotSupported "TIFF Predictor for 2bpc not supported")
  | 4 -> raise (DecodeNotSupported "TIFF Predictor for 4bpc not supported")
  | 8 ->
      let scanline_width = (colors * bpc * columns + 7) / 8 in
        for line = 0 to bytes_size stream / scanline_width - 1 do
          let linestart = line * scanline_width in
            let p = ref colors in
              while !p < scanline_width do
                bset stream (linestart + !p)
                  ((bget stream (linestart + !p - colors) +
                    bget stream (linestart + !p)) mod 256);
                p := !p + 1 
              done
        done;
        stream
  | 16 -> raise (DecodeNotSupported "TIFF Predictor for 16bpc not supported")
  | _ -> raise (DecodeNotSupported "Tiff predictor for unknown color depth")

(* Given two scanlines, the previous and current, and the predictor function
p, calculate the output scanline as a list of bytes. *)
let decode_scanline_pair prior_encoded prior_decoded current pred bpc cols =
  let output = Array.copy current in
    begin match pred with
    | 0 -> (* None *)
        ()
    | 1 -> (* Sub *)
        for x = 0 to Array.length output - 1 do
          output.(x) <- (get0 current x + get0 output (x - cols)) mod 256
        done
    | 2 -> (* Up *)
        for x = 0 to Array.length output - 1 do
          output.(x) <- (get0 current x + get0 prior_decoded x) mod 256
        done
    | 3 -> (* Average -- No test case yet found. *)
        for x = 0 to Array.length output - 1 do
          output.(x) <-
            (get0 current x +
              (get0 output (x - cols) + get0 prior_decoded x) / 2) mod 256
        done
    | 4 -> (* Paeth *)
        let paeth a b c =
          let p = a + b - c in
            let pa = abs (p - a) in
            let pb = abs (p - b) in
            let pc = abs (p - c) in
              if pa <= pb && pa <= pc then a
              else if pb <= pc then b
              else c
          in
            for x = 0 to Array.length output - 1 do
              output.(x) <-
                let curr = get0 current x
                in let currback = get0 output (x - cols)
                in let decoded = get0 prior_decoded x
                in let decodedback = get0 prior_decoded (x - cols) in
                  (curr + paeth currback decoded decodedback) mod 256
            done
    | _ -> raise (DecodeNotSupported "unknown PNG predictor")
    end;
    output

(* Main function. Given predictor, number of channels, bits-per-channel,
columns and the stream data, perform the decoding. *)
let decode_predictor pred colors bpc columns stream =
  if pred = 2 then decode_tiff_predictor colors bpc columns stream else
    let i = input_of_bytes stream
    in let scanline_width = (colors * bpc * columns + 7) / 8 in
      let blank () = ref (Array.make scanline_width 0) in
        let prev, curr, prior_decoded = blank (), blank (), blank ()
        in let outputlines = ref []
        in let finished = ref false
        in let pred = ref 0
        in let got_predictor = ref false in
          while not !finished do
            clear got_predictor;
            begin match i.input_byte () with
            | x when x = Pdfio.no_more -> set finished
            | x -> pred := x
            end;
            if !finished then () else
              begin
                set got_predictor;
                prev := !curr;
                for x = 0 to scanline_width - 1 do
                  match i.input_byte () with
                  | x when x = Pdfio.no_more -> set finished
                  | b -> (!curr).(x) <- b
                done
              end;
            (* We allow an unfinished final line only if we managed to get a
            predictor byte *)
            if !got_predictor then
              begin
                prior_decoded :=
                decode_scanline_pair
                  !prev !prior_decoded !curr !pred bpc ((bpc * colors + 7) / 8);
                outputlines =| !prior_decoded
              end
          done;
          bytes_of_arraylist (rev !outputlines)

(* Assumes 12 (PNGUp, no column padding required, colors, bpc standard values.
This is just for xref streams for now *)
let encode_predictor pred colors bpc columns stream =
  if pred = 12 then
    begin
      let get0 a i =
        if i < 0 then 0 else bget a i
      in
        let o, bytes = Pdfio.input_output_of_bytes 4096 in
          for scanline = 0 to bytes_size stream / columns - 1 do
            o.Pdfio.output_byte 2; (* tag for Up *)
            for byte = 0 to columns - 1 do
              o.Pdfio.output_byte
                ((get0 stream (scanline * columns + byte) -
                 get0 stream ((scanline - 1) * columns + byte))
                 mod 256)
            done
          done;
      Pdfio.extract_bytes_from_input_output o bytes
    end
  else
    raise (Pdf.PDFError "encode_predictor: not supported")

(* Run Length Encoding *)
let encode_runlength stream =
  let i = input_of_bytes stream in
    let data_in = ref [] in
      begin try
        while true do
          data_in =|
            begin match i.input_byte () with
            | x when x = Pdfio.no_more -> raise End_of_file
            | x -> x
            end
        done
      with
        End_of_file -> data_in := rev !data_in
      end;
      let rec runs_of_data prev = function
        | [] -> rev prev
        | h::t ->
            let same, rest = cleavewhile (eq h) (h::t) in
              runs_of_data ((length same, hd same)::prev) rest
      in
        let runs = ref (runs_of_data [] !data_in)
        in let outbytes = ref []
        in let chunksize = ref 0
        in let chunkdata = ref [] in
          let writechunk () =
            if !chunksize > 0 then
              begin
                outbytes =| !chunksize - 1; 
                iter (( =| ) outbytes) (rev !chunkdata);
                chunkdata := [];
                chunksize := 0;
              end
          in
              while !runs <> [] do
                begin match hd !runs with
                | (l, _) when l < 1 ->
                    assert false
                | (l, x) when l < 3 ->
                    if l + !chunksize > 128 then writechunk ();
                    chunkdata =@ many x l;
                    chunksize += l 
                | (l, x) ->
                    writechunk ();
                    let l = ref l in
                      while !l > 0 do
                        outbytes =| 257 - min !l 128;
                        outbytes =| x;
                        l -= 128
                      done
                end;
                runs := tl !runs
              done;
              writechunk ();
              outbytes =| 128; (*r End-of-data *)
              bytes_of_list (rev !outbytes)

let decode_runlength i =
  let o, data = input_output_of_bytes 4096 in
    let eod = ref false in
      begin try
        while not !eod do
          let l =
            match i.input_byte () with
            | x when x = Pdfio.no_more -> raise End_of_file
            | x -> x
          in
            if l < 128 then
              for x = 1 to l + 1 do
                o.output_byte
                  begin match i.input_byte () with
                    | x when x = Pdfio.no_more -> raise End_of_file
                    | x -> x
                  end
              done
            else if l > 128 then
              let towrite =
                begin match i.input_byte () with
                | x when x = Pdfio.no_more -> raise End_of_file
                | x -> x
                end;
              in
                for x = 1 to 257 - l do o.output_byte towrite done
            else
              set eod
        done
      with
        End_of_file ->
          Printf.eprintf "Warning: Missing EOD marker in runlength decode...\n"
      end;
      extract_bytes_from_input_output o data

(* Decoding PDF streams *)
type source =
  | StreamSource of bytes
  | InputSource of input

let decoder pdf dict source name =
  let input_of_source = function
    | InputSource i -> i
    | StreamSource s -> input_of_bytes s
  in
    let i = input_of_source source in
      match name with
      | "/ASCIIHexDecode" | "/AHx" -> decode_ASCIIHex i
      | "/ASCII85Decode" | "/A85" -> decode_ASCII85 i
      | "/FlateDecode" | "/Fl" ->
          begin match source with
          | StreamSource s -> decode_flate s
          | InputSource i -> decode_flate_input i
          end
      | "/RunLengthDecode" | "/RL" -> decode_runlength i
      | "/LZWDecode" | "/LZW" ->
          let early =
            match Pdf.lookup_direct_orelse pdf "/DecodeParms" "/DP" dict with
            | Some (Pdf.Array (Pdf.Null::_)) -> 1
            | Some (Pdf.Dictionary _ as d)
            | Some (Pdf.Array (Pdf.Dictionary _ as d::_)) ->
                begin match Pdf.lookup_direct pdf "/EarlyChange" d with
                | Some (Pdf.Integer n) -> n
                | None -> 1
                | _ -> raise (Pdf.PDFError "malformed /EarlyChange")
                end
            | _ -> 1
          in
            decode_lzw early i
      | "/CCITTFaxDecode" | "/CCF" ->
          begin match
            Pdf.lookup_direct_orelse pdf "/DecodeParms" "/DP" dict
          with
          | None -> decode_CCITTFax 0 false false 1728 0 true false 0 i
          | Some (Pdf.Dictionary _ as dparms)
          | Some (Pdf.Array (dparms::_)) ->
              (* Copes with null ok, because lookup_direct used below *)
              let dparms = Pdf.direct pdf dparms in
              let k =
                match Pdf.lookup_direct pdf "/K" dparms with
                | Some (Pdf.Integer i) -> i
                | _ -> 0
              in let eol =
                match Pdf.lookup_direct pdf "/EndOfLine" dparms with
                | Some (Pdf.Boolean b) -> b
                | _ -> false
              in let eba =
                match Pdf.lookup_direct pdf "/EncodedByteAlign" dparms with
                | Some (Pdf.Boolean b) -> b
                | _ -> false
              in let c =
                match Pdf.lookup_direct pdf "/Columns" dparms with
                | Some (Pdf.Integer i) -> i
                | _ -> 1728
              in let r =
                match Pdf.lookup_direct pdf "/Rows" dparms with
                | Some (Pdf.Integer i) -> i
                | _ -> 0
              in let eob =
                match Pdf.lookup_direct pdf "/EndOfBlock" dparms with
                | Some (Pdf.Boolean b) -> b
                | _ -> true
              in let bone =
                match Pdf.lookup_direct pdf "/BlackIs1" dparms with
                | Some (Pdf.Boolean b) -> b
                | _ -> false
              in let dra =
                match
                  Pdf.lookup_direct pdf "/DamagedRowsBeforeError" dparms
                with
                | Some (Pdf.Integer i) -> i
                | _ -> 0
              in
                decode_CCITTFax k eol eba c r eob bone dra i
            | _ -> raise (Pdf.PDFError "bad Decodeparms")
            end
      | name ->
          raise (DecodeNotSupported (Printf.sprintf "Unknown: %s" name))

(* Decode at most one stage. *)
let decode_one pdf dict source =
  match Pdf.lookup_direct_orelse pdf "/Filter" "/F" dict with
  | None | Some (Pdf.Array []) ->
      begin match source with
      | StreamSource s -> s
      | InputSource _ -> raise (DecodeNotSupported "decode_one")
      end
  | Some (Pdf.Name n) | Some (Pdf.Array (Pdf.Name n::_)) ->
      let decoded = decoder pdf dict source n in
        let decodeparms =
          match Pdf.lookup_direct_orelse pdf "/DecodeParms" "/DP" dict with
          | Some (Pdf.Dictionary d)
          | Some (Pdf.Array (Pdf.Dictionary d::_)) -> Pdf.Dictionary d
          | _ -> Pdf.Dictionary []
        in
          begin match Pdf.lookup_direct pdf "/Predictor" decodeparms with
          | None | Some (Pdf.Integer 1) -> decoded
          | Some (Pdf.Integer pred) ->
              let colors =
                match Pdf.lookup_direct pdf "/Colors" decodeparms with
                | Some (Pdf.Integer n) -> n
                | None -> 1
                | _ -> raise (Pdf.PDFError "malformed /Colors")
              in let bits_per_component =
                match Pdf.lookup_direct pdf "/BitsPerComponent" decodeparms with
                | Some (Pdf.Integer n) -> n
                | None -> 8
                | _ -> raise (Pdf.PDFError "malformed /BitsPerComponent")
              in let columns =
                match Pdf.lookup_direct pdf "/Columns" decodeparms with
                | Some (Pdf.Integer n) -> n
                | None -> 1
                | _ -> raise (Pdf.PDFError "malformed /Columns")
              in
                begin try
                  decode_predictor
                    pred colors bits_per_component columns decoded
                with
                  _ -> raise (Couldn'tDecodeStream "Predictor")
                end
          | _ -> raise (Pdf.PDFError "Malformed /Predictor")
          end
  | _ ->
    raise (Pdf.PDFError "PDF.decode: Bad filter specification")

(* Need to make sure /Filter, /F, /DecodeParms, /DP are not indirect. d on entry
is a name -> pdfobject map *)
let prepare_decoder pdf d =
  map
    (function
        (("/Filter" | "/F" | "/DecodeParms" | "/DP") as k, v) ->
          k, Pdf.direct pdf v
     | x -> x)
    d

(* Remove a single decoder from a filter list. Also remove the first entry of a
 DecodeParms array *)
let remove_decoder d =
  let d' =
    match lookup "/Filter" d, lookup "/F" d with
    | None, None -> d
    | Some (Pdf.Name _ | Pdf.Array [_]), None ->
        lose (fun (n, _) -> n = "/Filter") d
    | None, Some (Pdf.Name _ | Pdf.Array [_]) ->
        lose (fun (n, _) -> n = "/F") d
    | Some (Pdf.Array (_::t)), _ -> replace "/Filter" (Pdf.Array t) d
    | _, Some (Pdf.Array (_::t)) -> replace "/F" (Pdf.Array t) d
    | _ -> raise (Pdf.PDFError "PDF.remove_decoder: malformed /Filter")
  in
    match lookup "/DecodeParms" d', lookup "/DP" d' with
    | None, None -> d'
    | Some (Pdf.Dictionary _ | Pdf.Array []), _ -> remove "/DecodeParms" d'
    | _, Some (Pdf.Dictionary _ | Pdf.Array []) -> remove "/DP" d'
    | Some (Pdf.Array (_::t)), _ -> replace "/DecodeParms" (Pdf.Array t) d'
    | _, Some (Pdf.Array (_::t)) -> replace "/DP" (Pdf.Array t) d'
    | _ -> raise (Pdf.PDFError "PDF.remove_decoder: malformed /DecodeParms")

(* Decode at most one stage. *)
let decode_pdfstream_onestage pdf stream =
  Pdf.getstream stream;
  match stream with
  | Pdf.Stream
      ({contents = (Pdf.Dictionary d as dict, Pdf.Got s)} as stream_contents) ->
      begin match
        Pdf.direct pdf (Pdf.lookup_fail "no /Length" pdf "/Length" dict)
      with
      | Pdf.Integer _ -> ()
      (*i if l <> bytes_size s then raise (PDFError "Wrong /Length") i*)
      | _ -> raise (Pdf.PDFError "No /Length")
      end;
      let stream' = decode_one pdf dict (StreamSource s) in
        let d' =
          replace
            "/Length"
            (Pdf.Integer (bytes_size stream'))
            (remove_decoder (prepare_decoder pdf d))
        in
          stream_contents := (Pdf.Dictionary d', Pdf.Got stream')
  | _ -> raise (Pdf.PDFError "Pdf.decode_pdfstream: not a valid Stream")

let string_of_pdf = ref (fun _ -> "")

(* Decode until there's nothing left to do. *)
let rec decode_pdfstream pdf = function
  | Pdf.Stream {contents = d, _} as stream ->
      Pdf.getstream stream;
      begin match Pdf.lookup_direct_orelse pdf "/Filter" "/F" d with
      | None -> ()
      | Some (Pdf.Name _ | Pdf.Array _) ->
            begin
              decode_pdfstream_onestage pdf stream;
              match stream with
              | Pdf.Stream {contents = d', _} ->
                  if !string_of_pdf d = !string_of_pdf d' then () else
                  decode_pdfstream pdf stream
              | _ -> assert false
            end
      | _ -> raise (Pdf.PDFError "Pdf.remove_decoder: malformed /Filter")
      end
  | Pdf.Indirect i ->
      decode_pdfstream pdf (Pdf.direct pdf (Pdf.Indirect i))
  | _ -> raise (Pdf.PDFError "Pdf.decode_pdfstream: malformed Stream")

(* Decode a stream until a decoding isn't supported. *)
let decode_pdfstream_until_unknown pdf s =
  try decode_pdfstream pdf s with
    DecodeNotSupported _ -> ()

(* Decode from an input. *)
let decode_from_input i dict =
  match Pdf.lookup_direct_orelse (Pdf.empty ()) "/F" "/Filter" dict with
  | Some (Pdf.Name _) ->
      Some (decode_one (Pdf.empty ()) dict (InputSource i))
  | Some (Pdf.Array (_::t)) ->
      let stream = decode_one (Pdf.empty ()) dict (InputSource i) in
        let rec decode_rest stream = function
          | [] -> stream
          | Pdf.Name _::more ->
              (* Removing the filter just done, and its decodeparms. Note that
              /F can denote a file specification, but this can never be in an
              inline image (which is currrently the only use of
              decode_from_input, so that's ok for now. *)
              let filters =
                Pdf.lookup_direct_orelse (Pdf.empty ()) "/F" "/Filter" dict
              in let decodeparms =
                Pdf.lookup_direct_orelse
                  (Pdf.empty ()) "/DP" "/DecodeParms" dict
              in
                let dict = Pdf.remove_dict_entry dict "/Filter" in
                let dict = Pdf.remove_dict_entry dict "/F" in
                let dict = Pdf.remove_dict_entry dict "/DP" in
                let dict = Pdf.remove_dict_entry dict "/DecodeParms" in
                  let strip = function
                  | Some (Pdf.Array (_::t)) -> Pdf.Array t
                  | _ -> Pdf.Array []
                  in
                    let decodeparms = strip decodeparms
                    and filters = strip filters in
                      let dict = Pdf.add_dict_entry dict "/DP" decodeparms in
                      let dict = Pdf.add_dict_entry dict "/F" filters in 
                        let stream =
                          decode_one (Pdf.empty ()) dict (StreamSource stream)
                        in
                          decode_rest stream more
          | _ -> raise (Pdf.PDFError "Malformed filter array")
        in
          Some (decode_rest stream t)
  | _ -> raise (Couldn'tDecodeStream "No or bad filter")

(* Encoding streams *)

(* Supported encodings. *)
type encoding =
  | ASCIIHex
  | ASCII85
  | RunLength
  | Flate

type predictor =
    TIFF2
  | PNGNone
  | PNGSub
  | PNGUp
  | PNGAverage
  | PNGPaeth
  | PNGOptimum

(* The name of an encoding. *)
let name_of_encoding = function
  | ASCIIHex -> "/ASCIIHexDecode"
  | ASCII85 -> "/ASCII85Decode"
  | RunLength -> "/RunLengthDecode"
  | Flate -> "/FlateDecode"

(* Add an encoding to the dictionary d. *)
let add_encoding length pdf encoding d =
  let filter' =
    match Pdf.lookup_direct pdf "/Filter" d with
    | None ->
        Pdf.Name (name_of_encoding encoding)
    | Some (Pdf.Name n) ->
        Pdf.Array (Pdf.Name (name_of_encoding encoding)::[Pdf.Name n])
    | Some (Pdf.Array a) ->
        Pdf.Array (Pdf.Name (name_of_encoding encoding)::a)
    | _ -> raise (Pdf.PDFError "Malformed /Filter")
  in
    Pdf.replace_dict_entry
      (Pdf.add_dict_entry d "/Filter" filter') "/Length" (Pdf.Integer length)

(* Find the encoding function. *)
let encoder_of_encoding = function
  | ASCIIHex -> encode_ASCIIHex
  | ASCII85 -> encode_ASCII85
  | RunLength -> encode_runlength
  | Flate -> encode_flate

(* For now, just for xref streams *)
let process_prediction_data predictor predictor_columns d =
  match predictor with
    PNGUp -> encode_predictor 12 1 8 predictor_columns d
  | _ -> raise (Pdf.PDFError "process_predction_data")

let process_prediction predictor predictor_columns stream =
  match stream with
    {contents = d, Pdf.Got s} ->
      begin match predictor with
        Some PNGUp ->
          let data =
            process_prediction_data PNGUp predictor_columns s
          and d' =
            let decodeparms =
              Pdf.Dictionary
                [("/Columns", Pdf.Integer predictor_columns);
                 ("/Predictor", Pdf.Integer 12)]
            in
              Pdf.add_dict_entry d "/DecodeParms" decodeparms
          in
            (d', data)
      | _ -> raise (Pdf.PDFError "Encode predictor not supported")
      end
  | _ -> assert false

(* Encode a PDF stream with an encoding. *)
let encode_pdfstream pdf encoding ?predictor ?(predictor_columns = 1) stream =
  Pdf.getstream stream;
  match stream with
  | Pdf.Stream ({contents = d, Pdf.Got s} as stream) ->
      let d', predicted =
        if predictor <> None
          then process_prediction predictor predictor_columns stream
          else d, s
      in
        let data = encoder_of_encoding encoding predicted in
          let d'' = add_encoding (bytes_size data) pdf encoding d' in
            stream := d'', Pdf.Got data
  | _ -> raise (Pdf.PDFError "Pdf.encode_pdfstream: malformed Stream")

