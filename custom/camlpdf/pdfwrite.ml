open Pdfutil
open Pdfio

let write_debug = ref false

(* The file header. We include four larger-than-127 bytes as requested by the
standard to help FTP programs distinguish binary/text transfer modes. *)
let header pdf =
  Printf.sprintf
    "%%PDF-%i.%i\n%%\128\129\130\131\n"
    pdf.Pdf.major
    pdf.Pdf.minor

let output_string_of_xref i n =
  let s = string_of_int n in
    let l = String.length s in
      for x = 0 to 10 - l - 1 do i.output_char '0' done;
      i.output_string s;
      i.output_string " 00000 n \n" 

(* Write the cross-reference table to a channel. *)
let write_xrefs xrefs i =
  i.output_string "xref\n";
  i.output_string "0 ";
  i.output_string (string_of_int (length xrefs + 1));
  i.output_string " \n";
  i.output_string "0000000000 65535 f \n";
  iter (output_string_of_xref i) xrefs

(* Convert a string to one suitable for output. The function [escape] escapes
parentheses and backslashes. *)
let b = Buffer.create 30

let make_pdf_string s =
  Buffer.clear b;
  Buffer.add_char b '(';
  String.iter
    (function
       | ('(' | ')' | '\\') as c -> Buffer.add_char b '\\'; Buffer.add_char b c
       | '\n' -> Buffer.add_char b '\\'; Buffer.add_char b 'n'
       | '\r' -> Buffer.add_char b '\\'; Buffer.add_char b 'r'
       | '\t' -> Buffer.add_char b '\\'; Buffer.add_char b 't'
       | '\b' -> Buffer.add_char b '\\'; Buffer.add_char b 'b'
       | '\012' -> Buffer.add_char b '\\'; Buffer.add_char b 'f'
       | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b ')';
  Buffer.contents b

let make_pdf_string_hex shex =
  Buffer.clear b;
  Buffer.add_char b '<';
  Buffer.add_string b shex;
  Buffer.add_char b '>';
  Buffer.contents b

(* We have two kinds of flat data to write: Strings and streams (we cannot
represent streams as strings, since there is a langauge limit on the length of
strings. *)
type writeout =
  | WString of string
  | WStream of Pdf.stream

(* We want real numbers with no exponents (format compliance), and no trailing
zeroes (compactness). (Jan 2012 - have added back in special case for whole
numbers. Can still get trailing zeroes on small values e.g 0.00001 => 0.000010,
but no printf way to prevent this).

If we can do it fast enough, what we need to do is print with printf at 5
decimal places (spec says this is ok) and remove any trailing zeroes. *)
let format_real x =
  let fl = floor x in
    if fl = x then string_of_int (int_of_float fl) else
      if x < 0.0001 && x > -. 0.0001
        then Printf.sprintf "%f" x
        else string_of_float x

(* Character codes in a name < 33 or > 126 are replaced with hashed combinations
(e.g #20 for space). If the name contains the null character, an exception is
raised. *)
let hexchar = function
  | 0 -> '0' | 1 -> '1' | 2 -> '2' | 3 -> '3' | 4 -> '4' | 5 -> '5' | 6 -> '6'
  | 7 -> '7' | 8 -> '8' | 9 -> '9' | 10 -> 'A' | 11 -> 'B' | 12 -> 'C'
  | 13 -> 'D' | 14 -> 'E' | 15 -> 'F'
  | _ -> raise (Failure "hexchar")

let make_pdf_name_inner b s =
  for x = 1 to String.length s - 1 do (* skip / *)
    match String.get s x with
    | '\000' ->
      raise (Pdf.PDFError "Name cannot contain the null character")
    | h when h < '\033' || h > '\126' || Pdf.is_delimiter h || h = '#' ->
      Buffer.add_char b '#';
      Buffer.add_char b (hexchar ((int_of_char h) / 16));
      Buffer.add_char b (hexchar ((int_of_char h) mod 16));
    | h ->
      Buffer.add_char b h
  done

(* See if a name needs altering by [make_pdf_name_inner]. We ignore the first
character, since a '/' is a delimter, and this is fine... *)
let rec needs_processing_inner s p l =
  (p <= l - 1) &&
    (match String.unsafe_get s p with
    | '\000' -> raise (Pdf.PDFError "Name cannot contain the null character")
    | x when x < '\033' || x > '\126' || Pdf.is_delimiter x || x = '#' -> true
    | _ -> needs_processing_inner s (p + 1) l)

let needs_processing s =
  let l = String.length s in
    if l < 2 then false else
      needs_processing_inner s 1 l

let b = Buffer.create 30

let make_pdf_name n =
  if needs_processing n then
    if n = "" || String.unsafe_get n 0 <> '/' then raise (Pdf.PDFError "bad name") else
      begin
        Buffer.clear b;
        Buffer.add_char b '/';
        make_pdf_name_inner b n;
        Buffer.contents b
      end
  else
    n

(* Calculate a strings and streams representing the given pdf datatype instance,
assuming it has no unresolved indirect references. *)
let rec strings_of_array f changetable = function
  | [] -> ()
  | [x] -> strings_of_pdf f changetable x
  | h::(h'::_ as tail) ->
      strings_of_pdf f changetable h;
      begin match h' with
        Pdf.Name _ | Pdf.String _ | Pdf.Array _ | Pdf.Dictionary _ -> ()
      | _ ->
          match h with
            Pdf.String _ | Pdf.Array _ | Pdf.Dictionary _ -> ()
          | _ -> f (WString " ")
      end;
      strings_of_array f changetable tail

and strings_of_dictionary f changetable = function
  | [] -> ()
  | [(k, v)] ->
      f (WString (make_pdf_name k));
      begin match v with
        Pdf.Name _ | Pdf.String _ | Pdf.Array _ | Pdf.Dictionary _ -> ()
      | _ -> f (WString " ")
      end;
      strings_of_pdf f changetable v
  | (k, v)::t ->
      f (WString (make_pdf_name k));
      begin match v with
        Pdf.Name _ | Pdf.String _ | Pdf.Array _ | Pdf.Dictionary _ -> ()
      | _ -> f (WString " ")
      end;
      strings_of_pdf f changetable v;
      strings_of_dictionary f changetable t

and strings_of_pdf f changetable = function
  | Pdf.Null ->  f (WString "null")
  | Pdf.Boolean b -> f (WString (string_of_bool b))
  | Pdf.Integer n ->  f (WString (string_of_int n))
  | Pdf.Real r -> f (WString (format_real r))
  | Pdf.String s -> f (WString (make_pdf_string s))
  | Pdf.StringHex shex -> f (WString (make_pdf_string_hex shex))
  | Pdf.Name n -> f (WString (make_pdf_name n))
  | Pdf.Array elts ->
      f (WString "[");
      strings_of_array f changetable elts;
      f (WString "]");
  | Pdf.Dictionary entries ->
      f (WString "<<");
      strings_of_dictionary f changetable entries;
      f (WString ">>");
  | Pdf.Stream {contents = (dict, data)} ->
      strings_of_pdf f changetable dict;
      f (WString "\010stream\010");
      f (WStream data);
      f (WString "\010endstream");
  | Pdf.Indirect n ->
      let n' =
        try Hashtbl.find changetable n with Not_found -> n
      in
        f (WString (string_of_int n'));
        f (WString " 0 R")

let strings_of_pdf_return obj =
  let strings = ref [] in
    strings_of_pdf
      (function x -> strings := x::!strings)
      (Hashtbl.create 0)
      obj;
    rev !strings

(* Produce a single string from a PDF object. Only use for things which will
always fall under the string size limit. *)
let b = Buffer.create 100

let string_of_pdf s =
  Buffer.clear b;
  strings_of_pdf
    (function (WString x) -> Buffer.add_string b x |  _ -> ())
    (Hashtbl.create 0)
    s;
  Buffer.contents b

(* Inter-module recursion, for debug *)
let _ = Pdfcrypt.string_of_pdf := string_of_pdf
let _ = Pdfcodec.string_of_pdf := string_of_pdf
let _ = Pdf.string_of_pdf := string_of_pdf

let debug_whole_pdf pdf =
  Printf.printf "trailerdict = %s\n" (string_of_pdf pdf.Pdf.trailerdict);
  Pdf.objiter (fun i o -> Printf.printf "%i = %s\n" i (string_of_pdf o)) pdf

(* Calculate strings, one for each indirect object in the body. *)
let strings_of_object (n, pdfobject) =
  let strings = ref [] in
  strings := [WString (string_of_int n); WString " 0 obj\n"];
  strings_of_pdf
    (function x -> strings := x::!strings)
    (Hashtbl.create 0)
    pdfobject;
  strings := WString "\nendobj\n"::!strings;
  rev !strings

let strings_of_pdf_object f (_, pdfobject) n' changetable =
  f (WString (string_of_int n'));
  f (WString " 0 obj\n");
  strings_of_pdf f changetable pdfobject;
  f (WString "\nendobj\n")

(* Output a stream. *)
let output_stream o s =
  Pdf.getstream s;
  match s with
  | Pdf.Stream {contents = _, Pdf.Got arr} ->
      if bytes_size arr > 0 then
        getinit o arr 0 (bytes_size arr)
  | _ -> raise (Pdf.PDFError "output_stream")

(* Encrypting a PDF while writing *)
type encryption_method =
  | PDF40bit
  | PDF128bit
  | AES128bit of bool (* true = encrypt metadata, false = don't. *)
  | AES256bit of bool (* as above *)
  | AES256bitISO of bool (* as above *)
  | AlreadyEncrypted (* Used as a flag to prevent garbage collection *)

type encryption = 
  {encryption_method : encryption_method;
   owner_password : string;
   user_password : string;
   permissions : Pdfcrypt.permission list}

let crypt_if_necessary pdf = function
  | None -> pdf
  | Some enc ->
      let f =
        match enc.encryption_method with
        | PDF40bit -> Pdfcrypt.encrypt_pdf_40bit
        | PDF128bit -> Pdfcrypt.encrypt_pdf_128bit
        | AES128bit em -> Pdfcrypt.encrypt_pdf_AES em
        | AES256bit em -> Pdfcrypt.encrypt_pdf_AES256 em
        | AES256bitISO em -> Pdfcrypt.encrypt_pdf_AES256ISO em
        | AlreadyEncrypted -> fun _ _ _ pdf -> pdf
      in
        f enc.user_password enc.owner_password enc.permissions pdf

let flatten_W o = function
 | WString s -> o.output_string s
 | WStream data -> output_stream o (Pdf.Stream {contents = Pdf.Null, data})

(* Functions for object streams. NB no attempt is made to catch objects which
shouldn't be in a stream - this is the responsibility of the caller. *)
let bake_object_streams compress pdf numbers =
  iter
    (fun (tostream, objects) ->
       let data, first =
         let output, d = Pdfio.input_output_of_bytes 32000 in
           let strings =
             map (fun x -> string_of_pdf (Pdf.lookup_obj pdf x) ^ " ") objects
           in
             iter (Pdf.removeobj pdf) objects;
             let lengths = map String.length strings in
               let byte_offsets =
                 0 :: all_but_last (cumulative_sum 0 lengths)
               in
                 iter2
                   (fun o boff ->
                      output.Pdfio.output_string (string_of_int o);
                      output.Pdfio.output_string " ";
                      output.Pdfio.output_string (string_of_int boff);
                      output.Pdfio.output_string " ")
                   objects
                   byte_offsets;
                 let first = output.Pdfio.pos_out () in
                   iter output.Pdfio.output_string strings;
                   (extract_bytes_from_input_output output d, first)
       in
         let dict =
           Pdf.Dictionary
             [("/Type", Pdf.Name "/ObjStm");
              ("/Length", Pdf.Integer (bytes_size data));
              ("/N", Pdf.Integer (length objects));
              ("/First", Pdf.Integer first)]
         in
           let obj = Pdf.Stream {contents = (dict, Pdf.Got data)} in
             if compress then Pdfcodec.encode_pdfstream pdf Pdfcodec.Flate obj;
             Pdf.addobj_given_num pdf (tostream, obj))
    numbers

let print_data instream nonstream =
  if !write_debug then
    begin
      Printf.printf "Reserved for object streams: 1 to %i\n" (length instream);
      Printf.printf "Streams: ";
      print_ints (map fst instream);
      Printf.printf "\n";
      Printf.printf "Objects in streams: ";
      print_ints (flatten (map snd instream));
      flprint "\n";
      Printf.printf "Nonstream objects: ";
      print_ints nonstream;
      flprint "\n"
    end

(* Modify PDF to reinstate object streams from the saved hints. Then returns
the xref stream. Hints are now invalid, of course. Numbering scheme:
1...n   object streams
n+1...m objects in object streams
m+1...p objects not in object streams
p+1 xref stream *)
let reinstate_object_streams compress we_will_be_encrypting pdf =
  if !write_debug then
    flprint "pdf_to_output, reinstate streams, trying stream preservation\n";
  (* Adobe Reader can't cope with the document catalog being in a stream in an
  encrypted file, despite the ISO standard clearly allowing it. Make sure that,
  if we're encrypting, we remove any existing hint *)
  if we_will_be_encrypting then
    Hashtbl.remove pdf.Pdf.objects.Pdf.object_stream_ids pdf.Pdf.root;
  (* Build stream_objnum (streamnum, objects_for_this_stream) pairs. Take
  account of missing objects! *)
  let objects_for_streams =
    let table = null_hash ()
    and keys = null_hash () in
      Hashtbl.iter
        (fun objnum instream ->
           if not (Hashtbl.mem keys instream) then
             Hashtbl.add keys instream ();
           Hashtbl.add table instream objnum)
        pdf.Pdf.objects.Pdf.object_stream_ids;
      let lists = ref [] in
        Hashtbl.iter
          (fun instream _ ->
            lists =| (instream, Hashtbl.find_all table instream))
          keys;
      !lists
  in
  (* The nonstream objects are any not included in the objects_for_streams *)
  let nonstream_objects =
    let all_in_stream = null_hash () in
      iter
        (fun x -> Hashtbl.add all_in_stream x ())
        (flatten (map snd objects_for_streams));
      option_map
        (fun x -> if not (Hashtbl.mem all_in_stream x) then Some x else None)
        (Pdf.objnumbers pdf)
  in
  print_data objects_for_streams nonstream_objects;
  (* Now renumber the PDF such that we have (1...n, [n + 1....m]) used for the
  and renumber the nonstream objects to (m + 1).... Also renumber
  objects_for_streams and nonstream_objects *)
  let n = length objects_for_streams in
  if !write_debug then Printf.printf "n = %i\n" n;
  let m = n + fold_left ( + ) 0 (map length (map snd objects_for_streams)) in
  if !write_debug then Printf.printf "m = %i\n" m;
  let changetable = null_hash () in
  (* Add all the objects_for_streams *)
  iter2
    (Hashtbl.add changetable)
    (map fst objects_for_streams)
    (indx objects_for_streams);
  (* Add all the changes for the objects within streams *)
  let x = flatten (map snd objects_for_streams) in
    iter2 (Hashtbl.add changetable) x (indxn (n + 1) x);
  (* Add all the changes for the nonstream objects *)
  iter2
    (Hashtbl.add changetable)
    nonstream_objects
    (indxn (m + 1) nonstream_objects);
  let pdf' = Pdf.renumber changetable pdf in
    pdf.Pdf.root <- pdf'.Pdf.root;
    pdf.Pdf.objects <- pdf'.Pdf.objects;
    pdf.Pdf.trailerdict <- pdf'.Pdf.trailerdict;
  (* Apply the changes to objects_for_streams and nonstream_objects *)
  let renumbered_nonstream_objects =
    map (Hashtbl.find changetable) nonstream_objects
  in
  let renumbered_objects_for_streams =
    combine
      (indx (map (Hashtbl.find changetable) (map fst objects_for_streams)))
      (map_lol (Hashtbl.find changetable) (map snd objects_for_streams))
  in
  print_data renumbered_objects_for_streams renumbered_nonstream_objects;
  (* Now build the object streams and bake them into the PDF *)
  bake_object_streams compress pdf renumbered_objects_for_streams;
  renumbered_objects_for_streams

(* Build the xref stream from collected data. The input xref positions 1..n and
m+1...p. We build xreferences to

a) The 1...n ones from xrefs
b) The n+1...m ones from renumbered_objects_for_streams
c) The m+1...p ones from xrefs

This stream is then returned - it will be written as object p + 1.  *)

(* Maximum bytes required to represent the numbers in a list *)
let max_bytes_required l =
  if l = [] then raise (Failure "max_bytes_required") else
    let r = ref (fold_left max min_int l)
    and b = ref 0 in
      while let v = !r > 0 in r := !r lsr 8; v do b += 1 done;
      max 1 !b

(* Output nbyyes bytes from x, highest byte first to the list reference given *)
let output_bytes nbytes x o =
  for pos = nbytes - 1 downto 0 do
    o.output_byte ((x land (255 lsl (pos * 8))) lsr (pos * 8))
  done

let make_xref_stream pdf xrefs renumbered_objects_for_streams =
  let entries = ref [(0, 65535, 0)] in
    (* 1...n ones from xrefs *)
    let type1s, type1s_tranche2 =
      cleave xrefs (length renumbered_objects_for_streams)
    in
      entries =@ rev (map (fun x -> (1, x, 0)) type1s);
      (* n+1...m ones from renumbered_objects_for_streams *)
      iter
        (fun (snum, objnums) ->
           entries =@ rev (map (fun i -> (2, snum, i)) (indx0 objnums)))
        renumbered_objects_for_streams;
      (* m+1...p ones from xrefs *)
      entries =@ rev (map (fun x -> (1, x, 0)) type1s_tranche2);
      let w1 = max_bytes_required (map (fun (x, _, _) -> x) !entries)
      and w2 = max_bytes_required (map (fun (_, x, _) -> x) !entries)
      and w3 = max_bytes_required (map (fun (_, _, x) -> x) !entries) in
        let data =
          let o, bytes = Pdfio.input_output_of_bytes 4096 in
            iter
              (function (typ, a, b) ->
                 output_bytes w1 typ o;
                 output_bytes w2 a o;
                 output_bytes w3 b o)
              (rev !entries);
            Pdfio.extract_bytes_from_input_output o bytes
        in
          let dict =
            Pdf.Dictionary
              (fold_right
                 (fun (k, v) d -> add k v d)
                 [("/Type", Pdf.Name "/XRef");
                  ("/Root", Pdf.Indirect pdf.Pdf.root);
                  ("/Size", Pdf.Integer (length !entries));
                  ("/W",
                    Pdf.Array [Pdf.Integer w1; Pdf.Integer w2; Pdf.Integer w3]);
                  ("/Length", Pdf.Integer (bytes_size data))]
                 (match pdf.Pdf.trailerdict with
                    Pdf.Dictionary d -> d
                  | _ -> []))
          in
            let xrefstream = Pdf.Stream {contents = (dict, Pdf.Got data)} in
              Pdfcodec.encode_pdfstream
                pdf
                Pdfcodec.Flate
                ~predictor:Pdfcodec.PNGUp
                ~predictor_columns:(w1 + w2 + w3)
                xrefstream;
              xrefstream

(* Build hints for object streams from nothing, optionally preserving existing
streams. *)
let generate_object_stream_hints we_will_be_encrypting pdf preserve_existing =
  if !write_debug then
    Printf.printf
      "generate_object_stream_hints: %i existing hints, preserve_existing = %b"
    (Hashtbl.length pdf.Pdf.objects.Pdf.object_stream_ids) preserve_existing;
  if not preserve_existing then
    Hashtbl.clear pdf.Pdf.objects.Pdf.object_stream_ids;
  (* Adobe Reader can't cope with the document catalog being in a stream in an
  encrypted file, despite the ISO standard clearly allowing it. Make sure that,
  if we're encrypting, we remove any existing hint *)
  if !write_debug then
    Printf.printf "***** root (catalog) is object %i\n" pdf.Pdf.root;
  if we_will_be_encrypting then
    Hashtbl.remove pdf.Pdf.objects.Pdf.object_stream_ids pdf.Pdf.root;
  let biggest_hint =
    max
      (fold_left
         max
         min_int
         (map snd (list_of_hashtbl pdf.Pdf.objects.Pdf.object_stream_ids)))
      0
  in
    if !write_debug then
      Printf.printf "Biggest existing hint is %i\n" biggest_hint;
    let possibles =
      option_map
        (fun x ->
           if not (Hashtbl.mem pdf.Pdf.objects.Pdf.object_stream_ids x)
             then Some x
             else None)
        (Pdf.objnumbers pdf)
    in
      if !write_debug then
        Printf.printf
          "Found %i possible new objects to put into streams\n"
          (length possibles);
      let for_streams, indirect_lengths =
        let indirect_lengths = null_hash () in
          (option_map
            (fun x ->
               match Pdf.lookup_obj pdf x with
               | Pdf.Stream {contents = (Pdf.Dictionary d, _)} ->
                   begin
                     begin match lookup "/Length" d with
                     | Some (Pdf.Indirect i) ->
                         Hashtbl.add indirect_lengths i ()
                     | _ -> ()
                     end;
                     None
                   end
               | _ -> Some x)
            possibles,
            indirect_lengths)
      in
        if !write_debug then Printf.printf
          "Got %i for_streams and %i indirect lengths\n"
          (length for_streams) (Hashtbl.length indirect_lengths);
        (* Remove indirect lengths and catalog. *)
        let final =
          option_map
            (fun x ->
              if
                Hashtbl.mem indirect_lengths x ||
                (x = pdf.Pdf.root && we_will_be_encrypting)
              then
                None
              else
                Some x)
            for_streams
        in
          if !write_debug then
            Printf.printf
              "%i final objects for new object streams\n" (length final);
          let groups = splitinto 250 (sort compare final) in
            iter2
              (fun items groupnum ->
                 iter
                   (fun i ->
                      if !write_debug then
                        Printf.printf
                          "Hinting object %i as being in group %i\n" i groupnum;
                      Hashtbl.add
                        pdf.Pdf.objects.Pdf.object_stream_ids i groupnum)
                   items)
              groups
              (indxn (biggest_hint + 1) groups)

(* Used when recrypting *)
let dummy_encryption =
  {encryption_method = AlreadyEncrypted;
   owner_password = "";
   user_password = "";
   permissions = []}

(* Flatten a PDF document to an Pdfio.output. *)
let pdf_to_output
  ?(preserve_objstm = false) ?(generate_objstm = false)
  ?(compress_objstm = true) ?(recrypt = None) linearize encrypt pdf o
=
  if !write_debug then
  begin flprint "****pdf_to_output\n"; debug_whole_pdf pdf end;
  if !write_debug then
    Printf.printf "pdf_to_output: preserve %b, generate %b, linearize %b\n"
    preserve_objstm generate_objstm linearize;
  if linearize then
    raise
      (Pdf.PDFError
        "Linearization not supported since v1.8. Use an external linearizer.");
  let renumbered_objects_for_streams, preserve_objstm =
    if generate_objstm then
      generate_object_stream_hints
      (match encrypt with Some _ -> true | _ -> false) pdf preserve_objstm;
    if
      (preserve_objstm || generate_objstm) &&
      Hashtbl.length pdf.Pdf.objects.Pdf.object_stream_ids > 0
    then
      (reinstate_object_streams
        compress_objstm
        (match encrypt with Some _ -> true | _ ->
          match recrypt with Some _ -> true | _ -> false)
        pdf,
        true)
    else
      ([], false) (* Weren't asked to preserve, or nothing to put in streams *)
  in
  if !write_debug then
  begin flprint "****after object streams built\n"; debug_whole_pdf pdf end;
    let encrypt =
      match recrypt with
        None -> encrypt
      | Some _ -> Some dummy_encryption
    in
    let pdf =
      if preserve_objstm || generate_objstm then pdf else
        begin match encrypt with
        | Some e when e.encryption_method = AlreadyEncrypted ->
            pdf (* Already been renumbered *)
        | Some _  ->
            (* Need to renumber before encrypting.
            Will remove once encryption-on-demand-on-writing is done...*)
            Pdf.renumber (Pdf.changes pdf) pdf
        | _ -> pdf
        end
    in
      if !write_debug then flprint "Finished renumber\n";
  if !write_debug then
    begin flprint "****after renumbering\n"; debug_whole_pdf pdf end;
      let pdf =
        match recrypt with
          None -> pdf
        | Some pw ->
            Pdfcrypt.recrypt_pdf
              ~renumber:(not (preserve_objstm || generate_objstm)) pdf pw
      in
  if !write_debug then
    begin flprint "****just before crypt_if_necessary\n"; debug_whole_pdf pdf end;
      let pdf = crypt_if_necessary pdf encrypt in
        if !write_debug then
          begin
            flprint "crypt_if_necessary done...\n";
            if Pdfcrypt.is_encrypted pdf then flprint "FILE IS ENCRYPTED\n"
          end;
  if !write_debug then
    begin flprint "****crypted, ready to write\n"; debug_whole_pdf pdf end;
        o.output_string (header pdf);
        let xrefs = ref []
        and objiter =
          if Pdfcrypt.is_encrypted pdf || preserve_objstm || generate_objstm
            then Pdf.objiter_inorder
            else Pdf.objiter
        and changetable =
          if Pdfcrypt.is_encrypted pdf || preserve_objstm || generate_objstm
            then Hashtbl.create 0
            else Pdf.changes pdf
        and currobjnum = ref 1
        in
          if !write_debug then flprint "About to write objects\n";
          objiter
            (fun ob p ->
               xrefs =| o.pos_out ();
               strings_of_pdf_object
                 (flatten_W o) (ob, p)
                 (if preserve_objstm then ob else !currobjnum) changetable;
               incr currobjnum)
            pdf;
          if !write_debug then flprint "finished writing objects\n";
          let xrefstart = o.pos_out () in
          if preserve_objstm || generate_objstm then
            begin
              let xrefstream =
                make_xref_stream pdf (rev !xrefs) renumbered_objects_for_streams
              in
                if !write_debug then
                  begin
                    flprint "Result of making xref stream\n";
                    flprint (string_of_pdf xrefstream);
                    flprint "OBJSTREAM trailer section...\n"
                  end;
                let thisnum =
                  match Pdf.lookup_direct pdf "/Size" xrefstream with
                  | Some (Pdf.Integer i) -> i
                  | _ -> failwith "bad xref stream generated\n"
                in
                  o.output_string (string_of_int thisnum);
                  o.output_string " 0 obj\n";
                  strings_of_pdf (flatten_W o) changetable xrefstream;
                  o.output_string "\nendobj\n";
                  o.output_string "startxref\n";
                  o.output_string (string_of_int xrefstart);
                  o.output_string "\n%%EOF\n"
            end
          else
            begin
              if !write_debug then
                flprint "NORMAL NON-OBJSTREAM trailer section\n";
              write_xrefs (rev !xrefs) o;
              o.output_string "trailer\n";
              let trailerdict' =
                match pdf.Pdf.trailerdict with
                | Pdf.Dictionary trailerdict ->
                    Pdf.Dictionary
                      (add "/Size" (Pdf.Integer (length !xrefs + 1))
                        (add "/Root" (Pdf.Indirect pdf.Pdf.root) trailerdict))
                | _ ->
                    raise
                      (Pdf.PDFError
                         "Pdf.pdf_to_output: Bad trailer dictionary")
              in
                strings_of_pdf (flatten_W o) changetable trailerdict';
                if !write_debug then flprint "all done...\n";
                o.output_string "\nstartxref\n";
                o.output_string (string_of_int xrefstart);
                o.output_string "\n%%EOF\n"
            end

let change_id pdf f =
  match pdf.Pdf.trailerdict with
  | Pdf.Dictionary d ->
      {pdf with
         Pdf.trailerdict =
           Pdf.Dictionary
             (add "/ID" (Pdf.generate_id pdf f (fun () -> Random.float 1.)) d)}
  | _ -> raise (Pdf.PDFError "Bad trailer dictionary")

(* Write a PDF to a channel. Don't use mk_id when the file is encrypted.*)
let pdf_to_channel
  ?(preserve_objstm = false) ?(generate_objstm = false)
  ?(compress_objstm = true) ?(recrypt = None)
  linearize encrypt mk_id pdf ch
=
  let pdf = if mk_id then change_id pdf "" else pdf in
    pdf_to_output
      ~preserve_objstm ~generate_objstm ~compress_objstm ~recrypt
      linearize encrypt pdf (output_of_channel ch)

(* Similarly to a named file. If mk_id is set, the /ID entry in the document's
trailer dictionary is updated using the current date and time and the filename.
Don't use mk_id when the file is encrypted. If [preserve_objstm] is set,
existing object streams will be preserved. If [generate_objstm] is set, new
ones will be generated in addition. To get totally fresh object streams, set
[preserve_objstm=false, generate_objstm=true]. *)
let pdf_to_file_options
  ?(preserve_objstm = false) ?(generate_objstm = false)
  ?(compress_objstm = true) ?(recrypt = None)
  linearize encrypt mk_id pdf f
=
  let pdf' = if mk_id then change_id pdf f else pdf
  and ch = open_out_bin f in
    try
      pdf_to_channel
        ~preserve_objstm ~generate_objstm ~compress_objstm ~recrypt
        linearize encrypt false pdf' ch;
      close_out ch
    with
      e -> close_out ch; raise e

let pdf_to_file pdf f =
  pdf_to_file_options
    ~preserve_objstm:true ~generate_objstm:false ~compress_objstm:true
    false None true pdf f

