(* JPEG images *)
open Pdfutil
open Pdfio

(* This function is used by the inline image code to return the JPEG data
without decoding it, placing the stream pointer at the byte following the data.
This is done by searching for the two-byte sequence &FF, &D9 (the end of image
marker). This sequence cannot occur in the entropy-encoded image data because
the encoder always puts a &00 after any &FF. *)
let get_jpeg_data i =
  let s, data = input_output_of_bytes 4096 in
    let fin = ref false and last = ref 0 in
    while not !fin do
      match i.input_byte () with
      | x when x = no_more ->
          raise (Failure "Could not read JPEG data - end of stream")
      | 0xD9 ->
          if !last = 0xFF then set fin else last := 0xD9;
          s.output_byte 0xD9
      | n -> last := n; s.output_byte n
    done;
    extract_bytes_from_input_output s data

