(* Pdfcrypt primitives, split out *)
open Pdfutil
open Pdfio

type encryption = 
  | ARC4 of int * int
  | AESV2
  | AESV3 of bool (* true = iso, false = old algorithm *)

external aes_cook_encrypt_key : string -> string = "caml_aes_cook_encrypt_key"

external aes_cook_decrypt_key : string -> string = "caml_aes_cook_decrypt_key"

external aes_encrypt : string -> string -> int -> string -> int -> unit =
  "caml_aes_encrypt"

external aes_decrypt : string -> string -> int -> string -> int -> unit =
  "caml_aes_decrypt"

external sha_256 : string -> string = "caml_sha256"

external sha_384 : string -> string = "caml_sha384"

external sha_512 : string -> string = "caml_sha512"

let key_expansion nk key =
  aes_cook_encrypt_key (string_of_int_array key)

let key_expansion_decrypt nk key =
  aes_cook_decrypt_key (string_of_int_array key)

(* 40bit / 128bit Encryption/Decryption Primitives *)

(* Encryption / Decryption given a key. *)
let ksa s key =
  let keylength = Array.length key in
    for i = 0 to 255 do s.(i) <- i done;
    let j = ref 0 in
      for i = 0 to 255 do
        j := (!j + s.(i) + key.(i mod keylength)) mod 256;
        swap s i !j
      done

let prga s pi pj =
  pi := (!pi + 1) mod 256;
  pj := (!pj + s.(!pi)) mod 256;
  swap s !pi !pj;
  s.((s.(!pi) + s.(!pj)) mod 256)

let crypt key data =
  let s, pi, pj, out =
    Array.make 256 0, ref 0, ref 0, mkbytes (bytes_size data)
  in
    ksa s key;
    for x = 0 to bytes_size data - 1 do
      bset out x (bget data x lxor prga s pi pj)
    done;
    out

let _ = Random.self_init ()

(* Pad the input data (RFC2898, PKCS #5), then encrypt using a 16 byte AES
cipher in cipher block chaining mode, with a random initialisation vector, which
is stored as the first 16 bytes of the result. *)
let ran255 () =
  Random.int 255

let mkiv () =
  let r = ran255 in
    [| r (); r (); r (); r ();
       r (); r (); r (); r ();
       r (); r (); r (); r ();
       r (); r (); r (); r () |]

(* Build blocks for encryption, including padding. *)
let get_blocks data =
  let l = bytes_size data in
    let fullblocks =
      if l < 16 then [] else
        let blocks = ref [] in
          for x = 0 to l / 16 - 1 do
            blocks =|
              let a = Array.make 16 0 in
                for y = 0 to 15 do
                  Array.unsafe_set a y (bget_unsafe data (x * 16 + y))
                done;
                a
          done;
          rev !blocks
    in let lastblock =
      let getlast n =
        if n = 0 then [] else
          let bytes = ref [] in
            for x = 0 to n - 1 do
              bytes =| bget data (l - 1 - x)
            done;
            !bytes
      in let pad n =
        many n n
      in
        let overflow = l mod 16 in
          Array.of_list (getlast overflow @ pad (16 - overflow))
    in
      fullblocks @ [lastblock]

(* Flatten a list of blocks into a bytes *)
let bytes_of_blocks blocks =
  let len = 16 * length blocks in
    let s = mkbytes len
    in let p = ref 0 in
      iter
        (fun a ->
          Array.iter (fun v -> bset s !p v; incr p) a)
        blocks;
      s

(* These two functions strip the padding from a stream once it's been decoded.*)
let get_padding s =
  let l = bytes_size s in
    assert (l >= 16);
    let potential = bget s (l - 1) in
      if potential > 0x10 || potential < 0x01 then None else
        let rec elts_equal p f t =
          if f = t then p = bget s t else
            p = bget s f && elts_equal p (f + 1) t
        in
          if elts_equal potential (l - potential) (l - 1)
            then Some potential
            else None

let cutshort s =
  if bytes_size s = 0 then mkbytes 0 else
    if bytes_size s < 16 then s else
      match get_padding s with
      | None -> s
      | Some padding ->
          let s' = mkbytes (bytes_size s - padding) in
            for x = 0 to bytes_size s' - 1 do
              bset_unsafe s' x (bget_unsafe s x)
            done;
            s'

(* Decrypt data *)
let print_txt d p =
  for x = p to p + 15 do Printf.printf "%02x" (bget d x) done; flprint "\n"

let aes_decrypt_data ?(remove_padding = true) nk key data =
  let key = key_expansion_decrypt nk key in
  let len = bytes_size data in
    if len <= 16 then mkbytes 0 else
      let output = mkbytes (len - 16)
      and prev_ciphertext = mkbytes 16 in
        for x = 0 to 15 do
          bset_unsafe prev_ciphertext x (bget_unsafe data x)
        done;
        let pos = ref 16 in
          while !pos < len do
            let i = String.make 16 ' '
            and o = String.make 16 ' ' in
              for x = 0 to 15 do
                i.[x] <- char_of_int (bget_unsafe data (x + !pos))
              done;
              aes_decrypt key i 0 o 0;
              for x = 0 to 15 do
                bset_unsafe output (x + !pos - 16) (int_of_char o.[x])
              done;
              for x = 0 to 15 do
                bset_unsafe
                  output
                  (x + !pos - 16)
                  (bget_unsafe
                    prev_ciphertext x lxor bget_unsafe output (x + !pos - 16));
                bset_unsafe prev_ciphertext x (bget_unsafe data (x + !pos))
              done;
              pos += 16
          done;
          if remove_padding then cutshort output else output

(* With ECB instead. Data on input must be a multiple of 16. *)
let aes_decrypt_data_ecb ?(remove_padding = true) nk key data =
  let key = key_expansion_decrypt nk key in
    let size = bytes_size data in
      if size = 0 then mkbytes 0 else
        let output = mkbytes size
        and pos = ref 0 in
          while !pos < size do
            let i = String.make 16 ' '
            and o = String.make 16 ' ' in
              for x = 0 to 15 do i.[x] <-
                char_of_int (bget_unsafe data (x + !pos))
              done;
              aes_decrypt key i 0 o 0;
              for x = 0 to 15 do
                bset_unsafe output (x + !pos) (int_of_char o.[x])
              done;
              pos += 16
          done;
          (if remove_padding then cutshort else ident) output

(* Encrypt data *)
let aes_encrypt_data ?(firstblock = mkiv ()) nk key data =
  let key = key_expansion nk key in
  let outblocks = ref [] in
    let prev_ciphertext = ref firstblock in
      iter
        (fun block ->
          let ciphertext =
            let src =
              string_of_int_array ((array_map2 (lxor)) block !prev_ciphertext)
            and dst = String.make 16 ' ' in
            aes_encrypt key src 0 dst 0;
            (int_array_of_string dst)
          in
            prev_ciphertext := ciphertext;
            outblocks =| ciphertext)
        (get_blocks data);
        bytes_of_blocks (firstblock::rev !outblocks)

(* With ECB instead. Input length is multiple of 16. *)
let aes_encrypt_data_ecb nk key data =
  let key = key_expansion nk key in
    let size = bytes_size data in
      if size = 0 then mkbytes 0 else
        let output = mkbytes size
        and pos = ref 0 in
          while !pos < size do
            let i = String.make 16 ' '
            and o = String.make 16 ' ' in
              for x = 0 to 15 do i.[x] <-
                char_of_int (bget data (x + !pos))
              done;
              aes_encrypt key i 0 o 0;
              for x = 0 to 15 do
                bset output (x + !pos) (int_of_char o.[x])
              done;
              pos += 16
          done;
          output

let string_of_input i =
  let b = Buffer.create 100 in
    try
      while true do
        match i.input_char () with
          Some c -> Buffer.add_char b c
        | None -> raise End_of_file
      done;
      assert false
    with
      End_of_file -> Buffer.contents b

let sha256 i =
  sha_256 (string_of_input i)

let sha384 i =
  sha_384 (string_of_input i)

let sha512 i =
  sha_512 (string_of_input i)

(* Given an object number, generation number, input key and key length in bits,
apply Algorithm 3.1 from the PDF Reference manual to obtain the hash to be used
by the encryption function. *)
let find_hash crypt_type obj gen key keylength =
  let from_obj =
    [| i32toi (land32 obj 0x000000ffl);
       i32toi (lsr32 (land32 obj 0x0000ff00l) 8);
       i32toi (lsr32 (land32 obj 0x00ff0000l) 16) |]
  in let from_gen =
    [| i32toi (land32 gen 0x000000ffl);
       i32toi (lsr32 (land32 gen 0x0000ff00l) 8) |]
  in let extra =
    if crypt_type = AESV2 then [| 0x73; 0x41; 0x6C; 0x54 |] else [| |]
  in
    let digest_input = string_of_int_arrays [key; from_obj; from_gen; extra] in
      int_array_of_string
        (String.sub (Digest.string digest_input) 0 (min 16 (keylength / 8 + 5)))

let decrypt_stream_data crypt_type encrypt file_encryption_key obj gen key keylength r data =
  let f =
    (if crypt_type = AESV2 then
       (if encrypt
          then aes_encrypt_data 4
          else aes_decrypt_data 4)
     else if
       (match crypt_type with AESV3 _ -> true | _ -> false)
     then
       (if encrypt
          then aes_encrypt_data 8
          else aes_decrypt_data 8)
     else
       crypt)
  in
    if r = 5 || r = 6 then
      let key =
        match file_encryption_key with
          Some k -> k
        | None -> failwith "decrypt: no key C"
      in
        f (int_array_of_string key) data
    else
      let hash =
        find_hash crypt_type (i32ofi obj) (i32ofi gen) key keylength
      in
        f hash data

