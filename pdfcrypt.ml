(* Encryption and Decryption *)
open Pdfutil
open Pdfio

let crypt_debug = ref false

(* Find a key, given a password, O entry, P entry, id entry, and key length in
bits. *)
let paddings =
  [| 0x28; 0xbf; 0x4e; 0x5e; 0x4e; 0x75; 0x8a; 0x41;
     0x64; 0x00; 0x4e; 0x56; 0xff; 0xfa; 0x01; 0x08;
     0x2e; 0x2e; 0x00; 0xb6; 0xd0; 0x68; 0x3e; 0x80;
     0x2f; 0x0c; 0xa9; 0xfe; 0x64; 0x53; 0x69; 0x7a |]

let pad_password password =
  let pw = Array.make 32 0 in
    Array.iteri (fun i v -> if i < 32 then pw.(i) <- v) password;
    let n = Array.length password in
      if n < 32 then
        for x = n to 31 do
          pw.(x) <- paddings.(x - n)
        done;
  pw

let find_key no_encrypt_metadata password r o p id keylength =
  let password = int_array_of_string password
  in let o = int_array_of_string o
  in let id = int_array_of_string id in
    let pw = pad_password password in
      let from_p =
        [| i32toi (land32 p 0x000000ffl);
           i32toi (lsr32 (land32 p 0x0000ff00l) 8);
           i32toi (lsr32 (land32 p 0x00ff0000l) 16);
           i32toi (lsr32 (land32 p 0xff000000l) 24) |]
      in let rev4_no_metadata =
        if r >= 4 && no_encrypt_metadata then [|255; 255; 255; 255|] else [||]
      in
        let todigest = [pw; o; from_p; id; rev4_no_metadata] in
          let hash_input = string_of_int_arrays todigest in
            let hashed = Digest.string hash_input in
              let hashed' =
                if r >= 3 then
                  let h = ref hashed in
                    for x = 1 to 50 do
                      let hashed = Digest.string !h in
                        h :=
                          string_of_int_array
                            (Array.sub (int_array_of_string hashed)
                            0 (keylength / 8))
                    done;
                    !h
                else
                  hashed
              in
                Array.sub (int_array_of_string hashed') 0 (keylength / 8)

(* Authenticate the user password, given the password string and U, O, P, id
and key length entry. *)
let authenticate_user no_encrypt_metadata password r u o p id keylength =
(*flprint "AUTHENTICATE_USER\n";
  Printf.printf "no_encrypt_metadata: %b\n" no_encrypt_metadata;
  Printf.printf "user_pw = %S\n" password;
  Printf.printf "r = %i\n" r;
  Printf.printf "u = %S\n" u;
  Printf.printf "o = %S\n" o;
  Printf.printf "p = %li\n" p;
  Printf.printf "id = %S\n" id;
  Printf.printf "keylength = %i\n" keylength;
flprint "END_AUTHENTICATE_USER\n";*)
  let u = int_array_of_string u in
    let key = find_key no_encrypt_metadata password r o p id keylength in
      if r >= 3 then
        let id = int_array_of_string id in
          let todigest = [paddings; id] in
            let hash_input = string_of_int_arrays todigest in
              let hashed = Digest.string hash_input in
                let encrypted_hashed =
                  int_array_of_bytes
                    (Pdfcryptprimitives.crypt key (bytes_of_string hashed))
                in
                  let u' = ref [||] in
                    u' := encrypted_hashed;
                    for x = 1 to 19 do
                      let key' = Array.make (keylength / 8) 0 in
                        for k = 0 to (keylength / 8) - 1 do
                          key'.(k) <- key.(k) lxor x 
                        done;
                        u' :=
                          int_array_of_bytes
                            (Pdfcryptprimitives.crypt
                               key' (bytes_of_int_array !u'))
                    done;
                    Array.sub u 0 16 = !u'
      else
          u
        =
          int_array_of_bytes
            (Pdfcryptprimitives.crypt key (bytes_of_int_array paddings))

(* Decrypt a PDF file, given the user password. *)

(* For debug. Filled in by Pdfwrite *)
let string_of_pdf : (Pdf.pdfobject -> string) ref = ref (function _ -> "")



let rec decrypt
  crypt_type pdf no_encrypt_metadata encrypt obj gen key keylength r
  file_encryption_key l
=
  match l with
  | Pdf.String s ->
      (* Avoid decrypting an object which came from an object stream, since the
      object stream has been decrypted en-masse already. *)
      begin match
        fst (Pdf.pdfobjmap_find obj pdf.Pdf.objects.Pdf.pdfobjects)
      with
      | {contents = Pdf.ParsedAlreadyDecrypted _} -> Pdf.String s
      | _ (* Will always be Parsed for now...*) ->
        let f =
          (if crypt_type = Pdfcryptprimitives.AESV2 then
            (if encrypt
               then Pdfcryptprimitives.aes_encrypt_data 4
               else Pdfcryptprimitives.aes_decrypt_data 4)
           else if (match crypt_type with Pdfcryptprimitives.AESV3 _ -> true | _ -> false) then
            (if encrypt
               then Pdfcryptprimitives.aes_encrypt_data 8
               else Pdfcryptprimitives.aes_decrypt_data 8)
           else
            Pdfcryptprimitives.crypt)
        in
          let s_ints = bytes_of_string s in
            if r = 5 || r = 6 then
              let key =
                match file_encryption_key with
                  Some k -> k
                | None -> raise (Pdf.PDFError "decrypt: no key B")
              in
                Pdf.String
                  (string_of_bytes (f (int_array_of_string key) s_ints))
            else
              let hash =
                Pdfcryptprimitives.find_hash crypt_type (i32ofi obj) (i32ofi gen) key keylength
              in
                Pdf.String (string_of_bytes (f hash s_ints))
      end
  | (Pdf.Stream _) as stream ->
      decrypt_stream
         crypt_type pdf no_encrypt_metadata encrypt obj gen key keylength r
         file_encryption_key stream
  | Pdf.Array a ->
      begin match
        fst (Pdf.pdfobjmap_find obj pdf.Pdf.objects.Pdf.pdfobjects)
      with
      | {contents = Pdf.ParsedAlreadyDecrypted _} -> Pdf.Array a
      | _ ->
          Pdf.recurse_array
            (decrypt crypt_type pdf no_encrypt_metadata encrypt obj
             gen key keylength r file_encryption_key)
            a
      end
  | Pdf.Dictionary d ->
      begin match
        fst (Pdf.pdfobjmap_find obj pdf.Pdf.objects.Pdf.pdfobjects)
      with
      | {contents = Pdf.ParsedAlreadyDecrypted _} -> Pdf.Dictionary d
      | _ ->
          Pdf.recurse_dict
            (decrypt crypt_type pdf no_encrypt_metadata encrypt obj
             gen key keylength r file_encryption_key)
            d
      end
  | x -> x

and is_identity no_encrypt_metadata pdf d =
  let identity_crypt_filter_present =
    match Pdf.lookup_direct pdf "/Filter" d with
    | Some (Pdf.Name "/Crypt")
    | Some (Pdf.Array (Pdf.Name "/Crypt"::_)) ->
        begin match Pdf.lookup_direct pdf "/DecodeParms" d with
        | Some (Pdf.Dictionary decodeparmsdict)
        | Some (Pdf.Array (Pdf.Dictionary decodeparmsdict::_)) ->
            begin match
              Pdf.lookup_direct
                pdf "/Name" (Pdf.Dictionary decodeparmsdict)
            with
            | Some (Pdf.Name "/Identity") | None -> true
            | _ -> false
            end
        | _ -> true
        end
    | _ -> false
  in
    (no_encrypt_metadata &&
       (match
          Pdf.lookup_direct pdf "/Type" d with
            Some (Pdf.Name "/Metadata") -> true
          | _ -> false))
    || identity_crypt_filter_present

and decrypt_stream
  crypt_type pdf no_encrypt_metadata encrypt obj gen key keylength r
  file_encryption_key stream
=
  begin match stream with
  | Pdf.Stream {contents = (Pdf.Dictionary dict as d, data)} ->
      if is_identity no_encrypt_metadata pdf d then stream else
        let data' =
          let rec f unhashed_key key data =
            let crypt =
              Pdf.ToDecrypt
                {Pdf.crypt_type = crypt_type;
                 Pdf.file_encryption_key = file_encryption_key;
                 Pdf.obj = obj;
                 Pdf.gen = gen;
                 Pdf.key = unhashed_key;
                 Pdf.keylength = keylength;
                 Pdf.r = r}
            in
            match data with
              Pdf.Got data ->
                (*Printf.printf "decrypt_stream: Got, encrypt = %b\n" encrypt;*)
                (if crypt_type = Pdfcryptprimitives.AESV2 then
                   (if encrypt
                      then
                        Pdf.Got (Pdfcryptprimitives.aes_encrypt_data 4 key data)
                      else
                        Pdf.ToGet
                          (Pdf.toget ~crypt (Pdfio.input_of_bytes data) 0 (bytes_size data)))
                 else if
                   (match crypt_type with Pdfcryptprimitives.AESV3 _ -> true | _ -> false)
                 then
                   (if encrypt
                      then
                        Pdf.Got (Pdfcryptprimitives.aes_encrypt_data 8 key data)
                      else
                        Pdf.ToGet
                          (Pdf.toget ~crypt (Pdfio.input_of_bytes data) 0 (bytes_size data)))
                 else
                   if encrypt then
                     Pdf.Got (Pdfcryptprimitives.crypt key data)
                   else
                     Pdf.ToGet
                       (Pdf.toget ~crypt (Pdfio.input_of_bytes data) 0 (bytes_size data)))
            | Pdf.ToGet toget ->
                (*Printf.printf "decrypt_stream: ToGet, encrypt = %b\n" encrypt;*)
                if encrypt then 
                  (* If encrypting, call getstream, then go again *)
                  begin
                    Pdf.getstream stream;
                    match stream with
                      Pdf.Stream {contents = (_, data)} -> f unhashed_key key data
                    | _ -> assert false
                  end
                else
                  (* Otherwise, do the deferred decryption magic *)
                  let crypt =
                    Pdf.ToDecrypt
                      {Pdf.crypt_type = crypt_type;
                       Pdf.file_encryption_key = file_encryption_key;
                       Pdf.obj = obj;
                       Pdf.gen = gen;
                       Pdf.key = key;
                       Pdf.keylength = keylength;
                       Pdf.r = r}
                  in
                    Pdf.ToGet
                      (Pdf.toget ~crypt
                        (Pdf.input_of_toget toget) (Pdf.position_of_toget toget) (Pdf.length_of_toget toget))
          in
            if r = 5 || r = 6 then
              let key =
                match file_encryption_key with
                  Some k -> k
                | None -> raise (Pdf.PDFError "decrypt: no key C")
              in
                f (int_array_of_string key) (int_array_of_string key) data
            else
              let hash =
                Pdfcryptprimitives.find_hash crypt_type (i32ofi obj) (i32ofi gen) key keylength
              in
                f key hash data
        in let dict' =
          Pdf.recurse_dict
            (decrypt crypt_type pdf no_encrypt_metadata encrypt
             obj gen key keylength r file_encryption_key)
            dict
        in
          let dict'' =
            match data' with Pdf.Got data' ->
              Pdf.replace_dict_entry
                dict' "/Length" (Pdf.Integer (bytes_size data'))
            | _ -> dict'
          in
            Pdf.Stream {contents = (dict'', data')}
  | _ -> assert false
  end

let process_cryption
  no_encrypt_metadata encrypt pdf crypt_type user_pw r u o p id keylength
  file_encryption_key
=
  let encryption_object_number =
    match pdf.Pdf.trailerdict with
    | Pdf.Dictionary d ->
        begin match lookup "/Encrypt" d with
        | Some (Pdf.Indirect i) -> i
        | _ -> -1
        end
    | _ -> -1
  in
  let do_encryption key =
    Pdf.objiter_gen
      (fun objnum gennum obj ->
         if objnum <> encryption_object_number then
           begin
             ignore
              (Pdf.addobj_given_num
                 pdf
                 (objnum,
                  decrypt crypt_type pdf no_encrypt_metadata encrypt objnum
                  gennum key keylength r file_encryption_key obj))
           end)
      pdf;
    let trailerdict' = Pdf.remove_dict_entry pdf.Pdf.trailerdict "/Encrypt" in
      pdf.Pdf.trailerdict <- trailerdict';
      Some pdf
  in
    if r = 5 || r = 6 then (* AESV3 *)
      begin match file_encryption_key with
      | Some k -> do_encryption (int_array_of_string k)
      | None -> None
      end
    else if
      authenticate_user no_encrypt_metadata user_pw r u o p id keylength
    then
      do_encryption (find_key no_encrypt_metadata user_pw r o p id keylength)
    else None

let printable_of_string s =
  String.concat
    "" (map (fun c -> Printf.sprintf "%02x" (int_of_char c)) (explode s))

let get_encryption_values pdf =
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | None -> raise (Pdf.PDFError "get_encryption_values: unencrypted pdf")
  | Some encryptdict ->
      let crypt_type =
        match
          Pdf.lookup_direct pdf "/Filter" encryptdict,
          Pdf.lookup_direct pdf "/V" encryptdict,
          Pdf.lookup_direct pdf "/Length" encryptdict,
          Pdf.lookup_direct pdf "/R" encryptdict
        with
        | Some (Pdf.Name "/Standard"), Some (Pdf.Integer 1), _, Some (Pdf.Integer r)
        | Some (Pdf.Name "/Standard"), Some (Pdf.Integer 2), None, Some (Pdf.Integer r) ->
            Some (Pdfcryptprimitives.ARC4 (40, r))
        | Some (Pdf.Name "/Standard"), Some (Pdf.Integer 2), Some (Pdf.Integer n), _
            when n mod 8 = 0 && n >= 40 && n <= 128 ->
              Some (Pdfcryptprimitives.ARC4 (n, 3))
        | Some (Pdf.Name "/Standard"), Some (Pdf.Integer (4 | 5)), length, Some (Pdf.Integer r) ->
            begin match Pdf.lookup_direct pdf "/CF" encryptdict with
            | Some cfdict ->
                begin match Pdf.lookup_direct pdf "/StdCF" cfdict with
                | Some stdcfdict ->
                    begin match Pdf.lookup_direct pdf "/CFM" stdcfdict with
                    | Some (Pdf.Name "/V2") ->
                        begin match length with
                        | Some (Pdf.Integer i) -> Some (Pdfcryptprimitives.ARC4 (i, 4))
                        | _ ->
                            begin match Pdf.lookup_direct pdf "/Length" cfdict with
                            | Some (Pdf.Integer i) -> Some (Pdfcryptprimitives.ARC4 (i, 4))
                            | _ -> None
                            end
                        end
                    | Some (Pdf.Name "/AESV2") -> Some Pdfcryptprimitives.AESV2
                    | Some (Pdf.Name "/AESV3") -> Some (Pdfcryptprimitives.AESV3 (r = 6))
                    | _ -> None
                    end
                | _ -> None
                end
            | _ -> None
            end
        | _ -> None
      in
        let chop_string3248 n s =
          let need = match crypt_type with Some (Pdfcryptprimitives.AESV3 _) -> 48 | _ -> 32 in
            if String.length s < need then
              raise (Pdf.PDFError (n ^ ": too small in get_encryption_values"))
            else
              String.sub s 0 need
        in
          match crypt_type with
          | None -> raise (Pdf.PDFError "No encryption method")
          | Some crypt_type ->
              let o =
                match Pdf.lookup_direct pdf "/O" encryptdict with
                | Some (Pdf.String o) -> chop_string3248 "/O" o
                | _ -> raise (Pdf.PDFError "Bad or missing /O entry")
              and u =
                match Pdf.lookup_direct pdf "/U" encryptdict with
                | Some (Pdf.String u) -> chop_string3248 "/U" u
                | _ -> raise (Pdf.PDFError "Bad or missing /U entry")
              and p =
                match Pdf.lookup_direct pdf "/P" encryptdict with
                | Some (Pdf.Integer flags) -> i32ofi flags
                | _ -> raise (Pdf.PDFError "Bad or missing /P entry")
              and id =
                match Pdf.lookup_direct pdf "/ID" pdf.Pdf.trailerdict with
                | Some (Pdf.Array [Pdf.String s; _]) -> s
                | _ -> raise (Pdf.PDFError "Bad or missing /ID element")
              and oe =
                match Pdf.lookup_direct pdf "/OE" encryptdict with
                | Some (Pdf.String s) -> Some s
                | _ -> None
              and ue =
                match Pdf.lookup_direct pdf "/UE" encryptdict with
                | Some (Pdf.String s) -> Some s
                | _ -> None
              in
                (*Printf.printf "Encryption Values...\n";
                Printf.printf "crypt_type = %s\n" (string_of_encryption crypt_type);
                Printf.printf "p = %li\n" p;
                Printf.printf "u = %s\n" (printable_of_string u);
                Printf.printf "o = %s\n" (printable_of_string o);
                if ue <> None then Printf.printf "ue = %s\n" (printable_of_string (unopt ue));
                if oe <> None then Printf.printf "oe = %s\n" (printable_of_string (unopt oe));*)
                crypt_type, u, o, p, id, ue, oe

(* Permissions *)
type permission =
  | NoEdit (* R2, Bit 4 *)
  | NoPrint (* R2, Bit 3 *)
  | NoCopy (* R2, Bit 5 *)
  | NoAnnot (* R2, Bit 6 *)
  | NoForms (* R3 only, Bit 9 *)
  | NoExtract (* R3 only, Bit 10 *)
  | NoAssemble (* R3 only, Bit 11 *)
  | NoHqPrint (* R3 only, Bit 12 *)

let string_of_permission = function
  | NoEdit -> "NoEdit"
  | NoPrint -> "NoPrint"
  | NoCopy -> "NoCopy"
  | NoAnnot -> "NoAnnot"
  | NoForms -> "NoForms"
  | NoExtract -> "NoExtract"
  | NoAssemble -> "NoAssemble"
  | NoHqPrint -> "NoHqPrint"

let string_of_bans bans =
  fold_left ( ^ ) "" (interleave " " (map string_of_permission bans))
  
let p_of_banlist toban =
  let p = ref 0l in
    let setbit n b =
      if b then p := Int32.logor !p (Int32.shift_left 1l (n - 1))
    in let notin =
      notpred (mem' toban)
    in
      setbit 3 (notin NoPrint);
      setbit 4 (notin NoEdit);
      setbit 5 (notin NoCopy);
      setbit 6 (notin NoAnnot);
      setbit 7 true;
      setbit 8 true;
      setbit 9 (notin NoForms);
      setbit 10 (notin NoExtract);
      setbit 11 (notin NoAssemble);
      setbit 12 (notin NoHqPrint);
      iter (fun x -> setbit x true) (ilist 13 32);
      !p

let banlist_of_p p =
  let l = ref []
  in let bitset n =
    Int32.logand (Int32.shift_right p (n - 1)) 1l = 0l
  in
    if bitset 3 then l =| NoPrint;
    if bitset 4 then l =| NoEdit;
    if bitset 5 then l =| NoCopy;
    if bitset 6 then l =| NoAnnot;
    if bitset 9 then l =| NoForms;
    if bitset 10 then l =| NoExtract;
    if bitset 11 then l =| NoAssemble;
    if bitset 12 then l =| NoHqPrint;
    !l


let print_string name s =
  flprint name;
  iter (Printf.printf "%i ") (map int_of_char (explode s));
  flprint "\n"

(* New r = 6 algorithm (2.B in the standard) *)

(* modulus 3 of the first 16 bytes of a string taken as a 128 bit big-endian
number. Since (xy mod n) is equal to (x mod n + y mod n) and 256 mod 3 is 1, we
can just sum the bytes and take the modulus afterward. Logic due to Jay
Birkenbilt. *)
let mod3 b =
  let x = ref 0 in
    for i = 0 to 15 do x += int_of_char b.[i] done;
    !x mod 3

let prs s =
  String.iter
    (fun x -> Printf.printf "%02x" (int_of_char x))
    (if String.length s > 16 then String.sub s 0 16 else s);
  flprint "\n"

let shamix_cache = Hashtbl.create 16

let shamix password udata s =
  try Hashtbl.find shamix_cache (password, udata, s) with
    Not_found ->
      let i = input_of_string s in
      if !crypt_debug then
        begin
          flprint "Beginning of shamix\n Password is\n"; prs password; flprint "udata is\n";
          prs (match udata with None -> "" | Some x -> x);
        end;
      let k = ref (Pdfcryptprimitives.sha256 i)
      and fin = ref false
      and round = ref 0
      and last_e = ref 0 in
        while not !fin do
          round += 1;
          let k1 = password ^ !k ^ match udata with None -> "" | Some x -> x in
            let k1_64 = String.concat "" (many k1 64) in
              let e =
                let key = int_array_of_string (String.sub !k 0 16) 
                and firstblock = int_array_of_string (String.sub !k 16 16) in
                  let raw =
                    string_of_bytes
                      (Pdfcryptprimitives.aes_encrypt_data
                         ~firstblock:firstblock 4 key (Pdfio.bytes_of_string k1_64))
                  in
                    String.sub raw 16 (String.length raw - 32)
              in
                last_e := int_of_char e.[String.length e - 1];
                k := (match mod3 e with
                          0 -> Pdfcryptprimitives.sha256
                        | 1 -> Pdfcryptprimitives.sha384
                        | _ -> Pdfcryptprimitives.sha512) (Pdfio.input_of_string e);
          fin := !round >= 64 && !last_e <= !round - 32
        done;
        let result = String.sub !k 0 32 in
          if !crypt_debug then
            begin flprint "RESULT:\n"; prs result end;
          Hashtbl.add shamix_cache (password, udata, s) result;
          result

(* Conversion via unicode and SASLprep required here for complicated passwords. *)
let make_utf8 pw =
  if String.length pw > 127 then String.sub pw 0 127 else pw

let zero_iv = String.make 16 '\000'

(* Part of Algorithm 3.2a - making the intermediate key using owner password. *)
let file_encryption_key_aesv3 ?digest iso utf8pw o oe u =
  if String.length o < 48 || String.length u < 48 then raise (Pdf.PDFError "/O too short in make_intermediate_owner_key_aesv3") else
    let d =
      match digest with
      | Some d -> 
         if Array.length d <> 32 then Printf.printf "file_encryption_key_aesv3 pre-made length %i\n" (Array.length d);
         d
      | None ->
          let i =
            int_array_of_string
            ((if iso then shamix utf8pw (Some u) else (fun x ->
              Pdfcryptprimitives.sha256 (Pdfio.input_of_string x)))
                (String.concat "" [utf8pw; String.sub o 40 8; String.sub u 0 48]))
          in
            if Array.length i <> 32 then Printf.printf "file_encryption_key_aesv3 made length %i\n" (Array.length i);
            i
    in
      Pdfcryptprimitives.aes_decrypt_data ~remove_padding:false 8 d (bytes_of_string (zero_iv ^ oe))

let file_encryption_key_aesv3_user iso utf8pw u ue =
  if String.length u < 48 then raise (Pdf.PDFError "/U too short in file_encryption_key_aesv3_user") else
    Pdfcryptprimitives.aes_decrypt_data ~remove_padding:false
      8
      (int_array_of_string
        ((if iso then shamix utf8pw None else (fun x ->
          Pdfcryptprimitives.sha256 (Pdfio.input_of_string x)))
         (String.concat "" [utf8pw; String.sub u 40 8])))
      (bytes_of_string (zero_iv ^ ue))

(* Algorithm 3.12 - Authenticating the owner password. *)
let authenticate_owner_password_aesv3 iso utf8pw u o =
  if String.length o < 48 || String.length u < 48 then
    raise (Pdf.PDFError "/O too short in authenticate_owner_password")
  else
      (if iso then shamix utf8pw (Some u) else (fun x ->
        Pdfcryptprimitives.sha256 (Pdfio.input_of_string x)))
      (String.concat "" [utf8pw; String.sub o 32 8; String.sub u 0 48])
    =
      String.sub o 0 32

(* Algorithm 3.11 - Authenticating the user password. *)
let authenticate_user_password_aesv3 iso utf8pw u =
  if String.length u < 48 then raise (Pdf.PDFError "/U too short in authenticate_owner_password") else
    (if iso then shamix utf8pw None else (fun x -> Pdfcryptprimitives.sha256
    (Pdfio.input_of_string x)))
    (String.concat "" [utf8pw; String.sub u 32 8])
  = String.sub u 0 32

(* Part of algorithm 3.2a - return p from perms so we can check they match *)
let p_of_perms key perms =
  if String.length perms < 16 then raise (Pdf.PDFError "Wrong length in /Perms") else
  let ps = Pdfcryptprimitives.aes_decrypt_data_ecb ~remove_padding:false 8 (int_array_of_bytes key) (bytes_of_string perms) in
    let ints = int_array_of_bytes ps in
      if ints.(9) <> int_of_char 'a' || ints.(10) <> int_of_char 'd' || ints.(11) <> int_of_char 'b'
        then None
        else
          Some
            (lor32
              (lor32 (lsl32 (i32ofi ints.(0)) 0) (lsl32 (i32ofi ints.(1)) 8))
              (lor32 (lsl32 (i32ofi ints.(2)) 16) (lsl32 (i32ofi ints.(3)) 24)))

(* Main function for decryption. *)
let decrypt_pdf ?keyfromowner user_pw pdf =
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | None -> Some pdf, []
  | Some encrypt_dict ->
     let crypt_type, u, o, p, id, ue, oe = get_encryption_values pdf in
       let r, keylength, file_encryption_key =
         match crypt_type with
         | Pdfcryptprimitives.AESV2 -> 4, 128, None
         | Pdfcryptprimitives.AESV3 iso ->
             begin match oe, ue with
             | Some _, Some ue ->
                 begin match keyfromowner with
                 | Some k -> 5, 256, Some k
                 | None ->
                     let perms =
                       match Pdf.lookup_direct pdf "/Perms" encrypt_dict with
                       | Some (Pdf.String s) -> s
                       | _ -> raise (Pdf.PDFError "Missing /Perms in encryption dictionary")
                     in
                       let auth = authenticate_user_password_aesv3 iso (make_utf8 user_pw) u in
                         if not auth then 5, 256, None else
                           let key = file_encryption_key_aesv3_user iso (make_utf8 user_pw) u ue in
                             match p_of_perms key perms with
                             | None -> raise (Failure "/Perms file permissions corrupted")
                             | Some x when x = p -> 5, 256, Some (string_of_bytes key)
                             | Some _ -> raise (Failure "Mismatched /Perms and /P permissions")
                 end
             | _ -> raise (Failure "decrypt_pdf: no oe")
             end
         | Pdfcryptprimitives.ARC4 (k, r) -> r, k, None
       in let encrypt_metadata =
         match Pdf.lookup_direct pdf "/EncryptMetadata" encrypt_dict with
         | Some (Pdf.Boolean false) -> false
         | _ -> true
       in
         let perms =
           match Pdf.lookup_direct pdf "/Perms" encrypt_dict with
           | Some (Pdf.String s) -> s
           | _ -> ""
         in
           pdf.Pdf.saved_encryption <-
             Some
               {Pdf.from_get_encryption_values = (crypt_type, u, o, p, id, ue, oe);
                Pdf.encrypt_metadata = encrypt_metadata;
                Pdf.perms = perms};
           (process_cryption
             (not encrypt_metadata) false pdf crypt_type user_pw r u o p id
             keylength file_encryption_key,
            banlist_of_p p)

(* Calculate the owner key from the padded owner password (as calculated by
pad_password) *)
let owner_key padded_owner keylength r =
  let digest1 = Digest.string (string_of_int_array padded_owner) in
    let digest2 =
      if r >= 3 then
        let d = ref digest1 in
          for x = 1 to 50 do
            d := Digest.string !d
          done;
          !d
        else
          digest1
    in
      int_array_of_string (String.sub digest2 0 (keylength / 8))

(* Calculate XOR keys *)
let mkkey key x =
  let key' = Array.copy key in
    for k = 0 to Array.length key - 1 do
      key'.(k) <- key.(k) lxor x 
    done;
    key'

(* Just decrypt a single stream, given the user password, and pdf. This is used
to decrypt cross-reference streams during the reading of a file -- the PDF is
only partially formed at this stage. *)
let decrypt_single_stream user_pw owner_pw pdf obj gen stream =
  (*i Printf.printf "decrypt_single_stream\n";
  begin match user_pw with None -> flprint "no user password\n" | Some x -> Printf.printf "user password |%s|\n" x end;
  begin match owner_pw with None -> flprint "no owner password\n" | Some x -> Printf.printf "owner password |%s|\n" x end; i*)
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | None -> stream 
  | Some encrypt_dict ->
     let crypt_type, u, o, p, id, ue, oe = get_encryption_values pdf in
       let r, keylength =
         match crypt_type with
         | Pdfcryptprimitives.AESV2 -> 4, 128
         | Pdfcryptprimitives.AESV3 b -> (if b then 6 else 5), 256
         | Pdfcryptprimitives.ARC4 (k, r) -> r, k
       in let no_encrypt_metadata =
         match Pdf.lookup_direct pdf "/EncryptMetadata" encrypt_dict with
         | Some (Pdf.Boolean false) -> true
         | _ -> false
       in
           match r, keylength, ue, oe with
           | (5 | 6), 256, Some ue, Some oe ->
               let owner_pw = match owner_pw with Some x -> x | None -> ""
               and user_pw = match user_pw with Some x -> x | None -> "" in
                 (* Generate a file encryption key from either the owner or user password *)
                 let file_encryption_key =
                   if authenticate_user_password_aesv3 (r = 6) (make_utf8 user_pw) u
                     then file_encryption_key_aesv3_user (r = 6) (make_utf8 user_pw) u ue
                     else if authenticate_owner_password_aesv3 (r = 6) (make_utf8 owner_pw) u o
                       then file_encryption_key_aesv3 (r = 6) (make_utf8 owner_pw) o oe u
                       else raise (Pdf.PDFError "Encryption: Could not decrypt single stream: Bad AESV3 user or owner password")
                 in
                   decrypt_stream
                     crypt_type pdf no_encrypt_metadata false obj gen (int_array_of_bytes file_encryption_key)
                     keylength r (Some (string_of_bytes file_encryption_key)) stream
           | _ ->
         if owner_pw <> None then
           (* Decode with owner password *)
           let owner_pw_string = unopt owner_pw in
             (* Authenticate the owner password and calculate the key *)
             let padded_owner = pad_password (int_array_of_string owner_pw_string) in
               let key = owner_key padded_owner keylength r in
                 let user_pw =
                   if r = 2 then
                     string_of_bytes (Pdfcryptprimitives.crypt key (bytes_of_string o))
                   else (* r >= 3 *)
                     begin
                       let acc = ref (bytes_of_string o) in
                         for x = 19 downto 0 do
                           acc := Pdfcryptprimitives.crypt (mkkey key x) !acc
                         done;
                         string_of_bytes !acc 
                     end
                 in
                   if authenticate_user no_encrypt_metadata user_pw r u o p id keylength then
                     let key = find_key no_encrypt_metadata user_pw r o p id keylength in
                       decrypt_stream crypt_type pdf no_encrypt_metadata false obj gen key keylength r None stream
                   else
                     raise (Pdf.PDFError "Encryption: Bad owner password when decrypting single stream")
         else
           (* We're using user password. If none, assume it's blank *)
           let user_pw_string = match user_pw with None -> "" | Some x -> x in
             if authenticate_user no_encrypt_metadata user_pw_string r u o p id keylength then
               let key = find_key no_encrypt_metadata user_pw_string r o p id keylength in
                 decrypt_stream crypt_type pdf no_encrypt_metadata false obj gen key keylength r None stream
             else
               raise (Pdf.PDFError "Encryption: Bad password when decrypting single stream")

let key_or_user_password_from_owner ?encryption_values owner_pw pdf =
  let padded_owner = pad_password (int_array_of_string owner_pw) in
    let crypt_type, u, o, oe =
      match encryption_values with
        Some e -> e
      | None ->
          let crypt_type, u, o, _, _, _, oe = get_encryption_values pdf in
            crypt_type, u, o, oe
    in
      let r, keylength =
        match crypt_type with
        | Pdfcryptprimitives.AESV2 -> 4, 128
        | Pdfcryptprimitives.AESV3 x -> (if x then 6 else 5), 256
        | Pdfcryptprimitives.ARC4 (k, r) -> r, k
      in
        if r = 5 || r = 6 then
          if authenticate_owner_password_aesv3 (r = 6) (make_utf8 owner_pw) u o then
          begin
            match oe with
            | None -> raise (Pdf.PDFError "decrypt_pdf_owner: No /OE entry found")
            | Some oe ->
                let key = string_of_bytes (file_encryption_key_aesv3 (r = 6) (make_utf8 owner_pw) o oe u) in
                  Some (key, "")
          end
          else
             None
        else
          let user_pw =
            let key = owner_key padded_owner keylength r in
              if r = 2 then
                string_of_bytes (Pdfcryptprimitives.crypt key (bytes_of_string o))
              else (* r >= 3 *)
                begin
                  let acc = ref (bytes_of_string o) in
                    for x = 19 downto 0 do
                      acc := Pdfcryptprimitives.crypt (mkkey key x) !acc
                    done;
                    string_of_bytes !acc 
                end
          in
            Some ("", user_pw)

(* Decrypt with the owner password. *)
let decrypt_pdf_owner owner_pw pdf =
  (*Printf.printf "decrypt_pdf_owner with owner pw A%sA\n" owner_pw; flprint "\n";*)
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
    None -> Some pdf
  | _ ->
    match key_or_user_password_from_owner owner_pw pdf with
      None -> None
    | Some (key, user_pw) -> fst (decrypt_pdf user_pw ~keyfromowner:key pdf)

(* Make an owner password *)
let mk_owner r owner_pw user_pw keylength =
  let padded_owner =
    let source =
      if owner_pw = "" then user_pw else owner_pw
    in
     pad_password (int_array_of_string source)
  in
    let key = owner_key padded_owner keylength r in
      let padded_user = pad_password (int_array_of_string user_pw) in
        if r = 2 then
          string_of_bytes (Pdfcryptprimitives.crypt key (bytes_of_int_array padded_user))
        else (* r >= 3 *)
          let acc = ref (Pdfcryptprimitives.crypt key (bytes_of_int_array padded_user)) in
            for x = 1 to 19 do
              acc := Pdfcryptprimitives.crypt (mkkey key x) !acc
            done;
            string_of_bytes !acc
            
(* Make a user password *)
let mk_user no_encrypt_metadata user_pw o p id r keylength =
  let key = find_key no_encrypt_metadata user_pw r o p id keylength in
    if r = 2 then
      string_of_bytes (Pdfcryptprimitives.crypt key (bytes_of_int_array paddings))
    else (* r >= 3 *)
      let digest_input = [paddings; int_array_of_string id] in
        let d = Digest.string (string_of_int_arrays digest_input) in
          let acc = ref (Pdfcryptprimitives.crypt key (bytes_of_string d)) in
            for x = 1 to 19 do
              acc := Pdfcryptprimitives.crypt (mkkey key x) !acc
            done;
            string_of_bytes !acc ^ (implode (many '\000' 16))

(* Get the ID, or add one if there's not one there. Return the updated pdf and
the ID *)
let get_or_add_id pdf =    
  match Pdf.lookup_direct pdf "/ID" pdf.Pdf.trailerdict with
  | Some (Pdf.Array [Pdf.String s; _]) ->
      s, pdf
  | _ ->
      let idobj = Pdf.generate_id pdf "" (fun () -> Random.float 1.) in
        let pdf' =
          {pdf with
            Pdf.trailerdict =
              Pdf.add_dict_entry pdf.Pdf.trailerdict "/ID" idobj}
        in
          match idobj with
          | Pdf.Array [Pdf.String s; _] -> s, pdf'
          | _ -> assert false

(* 40bit encryption *)
let encrypt_pdf_40bit_inner owner user p user_pw id pdf =
  let crypt_dict =
    Pdf.Dictionary
      ["/Filter", Pdf.Name "/Standard";
       "/V", Pdf.Integer 1;
       "/R", Pdf.Integer 2;
       "/O", Pdf.String owner;
       "/U", Pdf.String user;
       "/P", Pdf.Integer (i32toi p)]
  in
    match process_cryption false true pdf (Pdfcryptprimitives.ARC4 (40, 2)) user_pw 2 user owner p id 40 None with
    | Some pdf ->
        let crypt_dict_num = Pdf.addobj pdf crypt_dict in
          {pdf with
            Pdf.trailerdict =
              Pdf.add_dict_entry
                pdf.Pdf.trailerdict "/Encrypt" (Pdf.Indirect crypt_dict_num)}
    | None -> raise (Pdf.PDFError "Encryption 40 failed")

let encrypt_pdf_40bit user_pw owner_pw banlist pdf =
  let p = p_of_banlist banlist
  in let owner = mk_owner 2 owner_pw user_pw 40
  in let id, pdf = get_or_add_id pdf in
    let user = mk_user false user_pw owner p id 2 40 in
      encrypt_pdf_40bit_inner owner user p user_pw id pdf

(* 128bit encryption *)
let encrypt_pdf_128bit_inner owner user p user_pw id pdf =
  let crypt_dict =
    Pdf.Dictionary
      ["/Filter", Pdf.Name "/Standard";
       "/V", Pdf.Integer 2;
       "/R", Pdf.Integer 3;
       "/O", Pdf.String owner;
       "/U", Pdf.String user;
       "/Length", Pdf.Integer 128;
       "/P", Pdf.Integer (i32toi p)]
  in
    match process_cryption false true pdf (Pdfcryptprimitives.ARC4 (128, 3)) user_pw 3 user owner p id 128 None with
    | Some pdf ->
        let crypt_dict_num = Pdf.addobj pdf crypt_dict in
          {pdf with
            Pdf.trailerdict =
              Pdf.add_dict_entry pdf.Pdf.trailerdict "/Encrypt" (Pdf.Indirect crypt_dict_num)}
    | None -> raise (Pdf.PDFError "Encryption 128 failed")

let encrypt_pdf_128bit_inner_r4 owner user p user_pw id pdf encrypt_metadata =
  let crypt_dict =
    Pdf.Dictionary
      ["/Filter", Pdf.Name "/Standard";
       "/V", Pdf.Integer 4;
       "/CF",
          Pdf.Dictionary
            ["/StdCF",
              Pdf.Dictionary
                ["/Length", Pdf.Integer 16;
                 "/AuthEvent", Pdf.Name "/DocOpen";
                 "/CFM", Pdf.Name "/V2"]];
       "/EncryptMetadata", Pdf.Boolean encrypt_metadata;
       "/Length", Pdf.Integer 128;
       "/R", Pdf.Integer 4;
       "/O", Pdf.String owner;
       "/U", Pdf.String user;
       "/P", Pdf.Integer (i32toi p);
       "/StrF", Pdf.Name "/StdCF";
       "/StmF", Pdf.Name "/StdCF"]
  in
    match process_cryption (not encrypt_metadata) true pdf (Pdfcryptprimitives.ARC4 (128, 4)) user_pw 4 user owner p id 128 None with
    | Some pdf ->
        let crypt_dict_num = Pdf.addobj pdf crypt_dict in
          {pdf with
            Pdf.trailerdict =
              Pdf.add_dict_entry pdf.Pdf.trailerdict "/Encrypt" (Pdf.Indirect crypt_dict_num)}
    | None -> raise (Pdf.PDFError "Encryption 128 r4 failed")

let encrypt_pdf_128bit user_pw owner_pw banlist pdf =
  let p = p_of_banlist banlist
  in let owner = mk_owner 3 owner_pw user_pw 128
  in let id, pdf = get_or_add_id pdf in
    let user = mk_user false user_pw owner p id 3 128 in
      encrypt_pdf_128bit_inner owner user p user_pw id pdf

(* AES Encryption. *)
let encrypt_pdf_AES_inner owner user p user_pw id encrypt_metadata pdf =
  let crypt_dict =
    Pdf.Dictionary
      ["/Filter", Pdf.Name "/Standard";
       "/V", Pdf.Integer 4;
       "/CF",
          Pdf.Dictionary
            ["/StdCF",
              Pdf.Dictionary
                ["/Length", Pdf.Integer 16;
                 "/AuthEvent", Pdf.Name "/DocOpen";
                 "/CFM", Pdf.Name "/AESV2"]];
       "/EncryptMetadata", Pdf.Boolean encrypt_metadata;
       "/Length", Pdf.Integer 128;
       "/R", Pdf.Integer 4;
       "/O", Pdf.String owner;
       "/U", Pdf.String user;
       "/P", Pdf.Integer (i32toi p);
       "/StrF", Pdf.Name "/StdCF";
       "/StmF", Pdf.Name "/StdCF"]
  in
    match 
      process_cryption
        (not encrypt_metadata) true pdf Pdfcryptprimitives.AESV2 user_pw 4 user owner p id 128 None
    with
    | Some pdf ->
        let crypt_dict_num = Pdf.addobj pdf crypt_dict in
          {pdf with
            Pdf.trailerdict =
              Pdf.add_dict_entry pdf.Pdf.trailerdict "/Encrypt" (Pdf.Indirect crypt_dict_num)}
    | None -> raise (Pdf.PDFError "Encryption AES failed")

let encrypt_pdf_AES encrypt_metadata user_pw owner_pw banlist pdf =
  let p = p_of_banlist banlist
  in let owner = mk_owner 4 owner_pw user_pw 128
  in let id, pdf = get_or_add_id pdf in
    let user = mk_user (not encrypt_metadata) user_pw owner p id 4 128 in
      encrypt_pdf_AES_inner owner user p user_pw id encrypt_metadata pdf

let encrypt_pdf_AES256_inner iso encrypt_metadata owner user p perms oe ue id key pdf =
  if !crypt_debug then Printf.printf "encrypt_pdf_AES256_inner, ISO = %b\n" iso;
  let crypt_dict =
    Pdf.Dictionary
      ["/Filter", Pdf.Name "/Standard";
       "/V", Pdf.Integer 5;
       "/CF",
          Pdf.Dictionary
            ["/StdCF",
              Pdf.Dictionary
                ["/Length", Pdf.Integer 32;
                 "/AuthEvent", Pdf.Name "/DocOpen";
                 "/CFM", Pdf.Name "/AESV3"]];
       "/EncryptMetadata", Pdf.Boolean encrypt_metadata;
       "/Length", Pdf.Integer 256;
       "/R", Pdf.Integer (if iso then 6 else 5);
       "/O", Pdf.String owner;
       "/U", Pdf.String user;
       "/P", Pdf.Integer (i32toi p);
       "/StrF", Pdf.Name "/StdCF";
       "/StmF", Pdf.Name "/StdCF";
       "/Perms", Pdf.String perms;
       "/OE", Pdf.String oe;
       "/UE", Pdf.String ue]
  in
    match
      process_cryption
        (not encrypt_metadata) true pdf (Pdfcryptprimitives.AESV3 iso) "" (if iso then 6 else 5) user owner p id 256 (Some key)
    with
    | Some pdf ->
        let crypt_dict_num = Pdf.addobj pdf crypt_dict in
          {pdf with
            Pdf.trailerdict =
              Pdf.add_dict_entry pdf.Pdf.trailerdict "/Encrypt" (Pdf.Indirect crypt_dict_num)}
    | None -> raise (Pdf.PDFError "256 bit Encryption AES failed")

(* Algorithm 3.10 *)
let perms_of_p ?digest iso encrypt_metadata p utf8pw o oe u =
  let extendedp = lor64 0xFFFFFFFF00000000L (i64ofi32 p) in
    let b = Array.make 16 0 in
      for n = 0 to 7 do
        b.(n) <- i64toi (land64 0x00000000000000FFL (lsr64 (land64 (lsl64 0xFFL (n * 8)) extendedp) (n * 8)))
      done;
      b.(8) <- int_of_char (if encrypt_metadata then 'T' else 'F');
      b.(9) <- int_of_char 'a';
      b.(10) <- int_of_char 'd';
      b.(11) <- int_of_char 'b';
      for n = 12 to 15 do b.(n) <- 0 done;
      let key =
        (int_array_of_string (string_of_bytes (file_encryption_key_aesv3 ?digest iso utf8pw o oe u)))
      in
        Pdfcryptprimitives.aes_encrypt_data_ecb 8 key (bytes_of_string (string_of_int_array b))

(* Algorithm 3.8. Returns u, ue. *)
let make_ue iso file_encryption_key user_pw user_validation_salt user_key_salt =
  let hash =
    if iso then shamix user_pw None else (fun x -> Pdfcryptprimitives.sha256 (Pdfio.input_of_string x))
  in
  let u =
    String.concat
      ""
      [(hash (String.concat "" [user_pw; user_validation_salt])); user_validation_salt; user_key_salt]
  in
    let ue = 
      Pdfcryptprimitives.aes_encrypt_data ~firstblock:(int_array_of_string zero_iv) 8
        (int_array_of_string (hash (String.concat "" [user_pw; user_key_salt])))
        file_encryption_key
    in
      u, String.sub (string_of_bytes ue) 16 32

(* Algorithm 3.9 *)
let make_oe iso file_encryption_key owner_pw owner_validation_salt owner_key_salt u =
  let hash =
    if iso then shamix owner_pw (Some u) else (fun x -> Pdfcryptprimitives.sha256 (Pdfio.input_of_string x))
  in
    let o =
      String.concat
       ""
       [(hash (String.concat "" [owner_pw; owner_validation_salt; u])); owner_validation_salt; owner_key_salt]
    in
      let digest =
        int_array_of_string (hash (String.concat "" [owner_pw; owner_key_salt; u]))
      in
        let oe = Pdfcryptprimitives.aes_encrypt_data ~firstblock:(int_array_of_string zero_iv) 8 digest file_encryption_key in
          o, String.sub (string_of_bytes oe) 16 32, digest

let mksalt () =
  let s = Array.make 8 0 in
  for x = 0 to 7 do s.(x) <- Random.int 255 done;
    string_of_int_array s

let mkfilekey () =
  let s = Array.make 32 0 in
    for x = 0 to 31 do s.(x) <- Random.int 255 done;
    string_of_int_array s

let encrypt_pdf_AES256_call iso encrypt_metadata user_pw owner_pw banlist pdf =
  let user_pw = make_utf8 user_pw
  and owner_pw = make_utf8 owner_pw
  and user_validation_salt = mksalt ()
  and user_key_salt = mksalt ()
  and owner_validation_salt = mksalt ()
  and owner_key_salt = mksalt ()
  and file_encryption_key = bytes_of_string (mkfilekey ())
  and p = p_of_banlist banlist in
  let u, ue =
    make_ue
      iso file_encryption_key user_pw user_validation_salt user_key_salt
  in
    let o, oe, digest =
      make_oe
        iso file_encryption_key owner_pw
        owner_validation_salt owner_key_salt u
    in
      let id, pdf = get_or_add_id pdf in
        let perms =
          perms_of_p ~digest:digest iso encrypt_metadata p owner_pw o oe u
        in
          encrypt_pdf_AES256_inner
            iso encrypt_metadata o u p (string_of_bytes perms) oe ue id
            (string_of_bytes file_encryption_key) pdf

let encrypt_pdf_AES256 =
  encrypt_pdf_AES256_call false

let encrypt_pdf_AES256ISO =
  encrypt_pdf_AES256_call true

(* Is a file encrypted? *)
let is_encrypted pdf =
  match Pdf.lookup_direct pdf "/Encrypt" pdf.Pdf.trailerdict with
  | Some _ -> true
  | None -> false

(* recrypt_pdf pdf password re-encrypts a PDF document which was decrypted with
the user or owner password given using that same user password *)
let recrypt_pdf_user pdf pw =
  let (crypt_type, u, o, p, id, ue, oe), encrypt_metadata, perms =
    match pdf.Pdf.saved_encryption with
      None -> raise (Pdf.PDFError "recrypt_pdf: no saved encryption")
    | Some x -> (x.Pdf.from_get_encryption_values, x.Pdf.encrypt_metadata, x.Pdf.perms)
  in
    match crypt_type with
    | Pdfcryptprimitives.AESV3 iso ->
        let oe =
          match oe with
            Some oe -> oe
          | None -> raise (Pdf.PDFError "recrypt_pdf: bad /oe")
        and ue =
          match ue with
            Some ue -> ue
          | None -> raise (Pdf.PDFError "recrypt_pdf: bad /ue")
        in
          let key =
            if authenticate_user_password_aesv3 iso (make_utf8 pw) u
              then file_encryption_key_aesv3_user iso (make_utf8 pw) u ue
              else raise (Pdf.PDFError "recrypt_pdf: failed AESV3 fek.")
          in
            encrypt_pdf_AES256_inner
              iso encrypt_metadata o u p perms oe ue id
              (string_of_bytes key) pdf
    | Pdfcryptprimitives.AESV2 ->
        encrypt_pdf_AES_inner o u p pw id encrypt_metadata pdf
    | Pdfcryptprimitives.ARC4 (40, _) ->
        encrypt_pdf_40bit_inner o u p pw id pdf
    | Pdfcryptprimitives.ARC4 (128, 4) ->
        encrypt_pdf_128bit_inner_r4 o u p pw id pdf encrypt_metadata
    | Pdfcryptprimitives.ARC4 (128, _) ->
        encrypt_pdf_128bit_inner o u p pw id pdf
    | _ -> raise (Pdf.PDFError "recrypt_pdf: bad encryption")

(* recrypt_pdf_owner  password re-encrypts a PDF document which was decrypted with
the user or owner password given using that same owner password *)
let recrypt_pdf_owner pdf owner_pw =
  let (crypt_type, u, o, p, id, ue, oe), encrypt_metadata, perms =
    match pdf.Pdf.saved_encryption with
      None ->
        raise (Pdf.PDFError "recrypt_pdf: no saved encryption")
    | Some x ->
        (x.Pdf.from_get_encryption_values, x.Pdf.encrypt_metadata, x.Pdf.perms)
  in
    let key, pw =
      match
        key_or_user_password_from_owner
          ~encryption_values:(crypt_type, u, o, oe) owner_pw pdf
      with
        None -> raise (Pdf.PDFError "Recrypt with owner password failed.")
      | Some (key, pw) -> (key, pw) 
    in
      match crypt_type with
      | Pdfcryptprimitives.AESV3 iso ->
          let oe =
            match oe with
              Some oe -> oe
            | None -> raise (Pdf.PDFError "recrypt_pdf: bad /oe")
          and ue =
            match ue with
              Some ue -> ue
            | None -> raise (Pdf.PDFError "recrypt_pdf: bad /ue")
          in
            encrypt_pdf_AES256_inner
              iso encrypt_metadata o u p perms oe ue id key pdf
      | Pdfcryptprimitives.AESV2 ->
          encrypt_pdf_AES_inner o u p pw id encrypt_metadata pdf
      | Pdfcryptprimitives.ARC4 (40, _) ->
          encrypt_pdf_40bit_inner o u p pw id pdf
      | Pdfcryptprimitives.ARC4 (128, 4) ->
          encrypt_pdf_128bit_inner_r4 o u p pw id pdf encrypt_metadata
      | Pdfcryptprimitives.ARC4 (128, _) ->
          encrypt_pdf_128bit_inner o u p pw id pdf
      | _ -> raise (Pdf.PDFError "recrypt_pdf_owner: bad encryption")

let recrypt_pdf ?(renumber=true) pdf pw =
  let pdf =
    if renumber then Pdf.renumber (Pdf.changes pdf) pdf else pdf
  in
    try
      try recrypt_pdf_user pdf pw with _ -> recrypt_pdf_owner pdf pw
    with
      _ -> raise (Pdf.PDFError "recrypt_pdf failed. Wrong password?")

