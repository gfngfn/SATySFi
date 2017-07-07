
open Core.Result


type file_name = string

exception FailToLoadFontFormatOwingToSize   of file_name
exception FailToLoadFontFormatOwingToSystem of string


let file_name_hash_table : (string, file_name) Hashtbl.t = Hashtbl.create 32

let decoder_hash_table : (file_name, Otfm.decoder) Hashtbl.t = Hashtbl.create 32


let string_of_file (flnmin : file_name) : string =
  try
    let ic = open_in_bin flnmin in
    let bufsize = 65536 in
    let buf : Buffer.t = Buffer.create bufsize in
    let byt : bytes = Bytes.create bufsize in
      try
        begin
          while true do
            let c = input ic byt 0 bufsize in
              if c = 0 then raise Exit else
                Buffer.add_substring buf (Bytes.unsafe_to_string byt) 0 c
          done ;
          assert false
        end
      with
      | Exit           -> begin close_in ic ; Buffer.contents buf end
      | Failure(_)     -> begin close_in ic ; raise (FailToLoadFontFormatOwingToSize(flnmin)) end
      | Sys_error(msg) -> begin close_in ic ; raise (FailToLoadFontFormatOwingToSystem(msg)) end
  with
  | Sys_error(msg) -> raise (FailToLoadFontFormatOwingToSystem(msg))


let get_decoder (flnmin : file_name) : Otfm.decoder =
  try
    Hashtbl.find decoder_hash_table flnmin
  with
  | Not_found ->
      let s = string_of_file flnmin in
      let dcdr = Otfm.decoder (`String(s)) in
      begin
        Hashtbl.add decoder_hash_table flnmin dcdr ;
        dcdr
      end


