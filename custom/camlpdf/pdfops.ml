open Pdfutil
open Pdfio

let debug = ref false

(* Graphics operators. *)
type t =
  | Op_w of float (* Set line width *)
  | Op_J of int (* Set line cap *)
  | Op_j of int (* Set line join *)
  | Op_M of float (* Set mitre limit *)
  | Op_d of float list * float (* Set dash pattern (dash, phase) *)
  | Op_ri of string (* Set rendering intent. *)
  | Op_i of int (* Set flatness. *)
  | Op_gs of string (* Set graphics state from dictionary *)
  | Op_q (* Save graphics state to stack *)
  | Op_Q (* Restore graphics state from stack *)
  | Op_cm of Pdftransform.transform_matrix (*r Modify CTM by concatenation *)
  | Op_m of float * float (* Begin a new subpath *)
  | Op_l of float * float (* Append a straight segment *)
  | Op_c of float * float * float * float * float * float (* Cubic bezier *)
  | Op_v of float * float * float * float (* Similar. *)
  | Op_y of float * float * float * float (* Similar. *)
  | Op_h (* Close subpath *)
  | Op_re of float * float * float * float (* Append rectangle *)
  | Op_S (* Stroke a path *)
  | Op_s (* Close and stroke path *) 
  | Op_f (* Fill path, non-zero *)
  | Op_F (* Same. *)
  | Op_f' (* f* operator. Fill path, even-odd. *)
  | Op_B (* Fill and stroke path, non-zero *)
  | Op_B' (* B* operator. Fill and stroke path, even-odd *)
  | Op_b (* Close fill and stroke, non-zero *)
  | Op_b' (* b* operator. Close fill and stroke, even-odd *)
  | Op_n (* Path no-op *)
  | Op_W (* Clipping path, even-odd *)
  | Op_W' (* Clipping path, non-zero *)
  | Op_BT (* Begin a text object *)
  | Op_ET (* End a text object *)
  | Op_Tc of float (* Set character spacing *)
  | Op_Tw of float (* Set word spacing *)
  | Op_Tz of float (* Set horizontal scaling *)
  | Op_TL of float (* Set leading *)
  | Op_Tf of string * float (* Set font size *)
  | Op_Tr of int (* Set text rendering mode *)
  | Op_Ts of float (* Set text rise *)
  | Op_Td of float * float (* Move to next line *)
  | Op_TD of float * float (* Ditto, but set leading *)
  | Op_Tm of Pdftransform.transform_matrix (* Set text and line matrices *)
  | Op_T' (* T* operator. Move text to the next line *)
  | Op_Tj of string (* Show text string *)
  | Op_Tj_hex of string
  | Op_TJ of Pdf.pdfobject (* Show many text strings *)
  | Op_' of string (* Move to next line and show text *)
  | Op_'' of float * float * string (* Ditto, extra parameters *)
  | Op_d0 of float * float (* Set glpyh width info *)
  | Op_d1 of float * float * float * float * float * float (* Similar *)
  | Op_CS of string (* Set colour space. *)
  | Op_cs of string (* Same for nonstroking operations *)
  | Op_SC of float list (* Set colour in current colour space. *)
  | Op_sc of float list (* Same for nonstroking operations *)
  | Op_SCN of float list (* Set colour in current colour space. *)
  | Op_scn of float list (* Same for nonstroking operations *)
  | Op_SCNName of string * float list (* A named Op_SCN *)
  | Op_scnName of string * float list (* Same for Op_scn *)
  | Op_G of float (* set gray *)
  | Op_g of float (* set gray nonstroking *)
  | Op_RG of float * float * float (* Set stroking colour *)
  | Op_rg of float * float * float (* Set painting colour *)
  | Op_K of float * float * float * float (* Set CMYK stroking *)
  | Op_k of float * float * float * float (* Set CMYK nonstroking *)
  | Op_sh of string (* Shading pattern *)
  | InlineImage of (Pdf.pdfobject * bytes) (* Inline image dictionary/data *)
  | Op_Do of string (* Introduce an XObject *)
  | Op_MP of string (* Marked content point *)
  | Op_DP of string * Pdf.pdfobject (* same with property list *)
  | Op_BMC of string (* begin marked content sequence *)
  | Op_BDC of string * Pdf.pdfobject (* same with property list *)
  | Op_EMC (* end of marked content sequence *)
  | Op_BX (* Start compatibility mode *)
  | Op_EX (* End compatibility mode *)
  | Op_Unknown of string (* Unknown operand / operator sequence *)

type lexeme =
  | Op of string
  | Obj of Pdfgenlex.t
  | PdfObj of Pdf.pdfobject
  | LexInlineImage of (Pdf.pdfobject * bytes)
  | LexComment
  
(* Lexing *)
let lexemes_of_op f = function
  | Op_w w -> f (Obj (Pdfgenlex.LexReal w)); f (Op "w")
  | Op_J j -> f (Obj (Pdfgenlex.LexInt j)); f (Op "J")
  | Op_j j -> f (Obj (Pdfgenlex.LexInt j)); f (Op "j")
  | Op_M m -> f (Obj (Pdfgenlex.LexReal m)); f (Op "M")
  | Op_d (fl, y) ->
      f (Obj Pdfgenlex.LexLeftSquare);
      iter (fun x -> f (Obj (Pdfgenlex.LexReal x))) fl;
      f (Obj Pdfgenlex.LexRightSquare);
      f (Obj (Pdfgenlex.LexReal y)); f (Op "d")
  | Op_ri s -> f (Obj (Pdfgenlex.LexName s)); f (Op "ri")
  | Op_i i -> f (Obj (Pdfgenlex.LexInt i)); f (Op "i")
  | Op_gs s -> f (Obj (Pdfgenlex.LexName s)); f (Op "gs")
  | Op_q -> f (Op "q")
  | Op_Q -> f (Op "Q")
  | Op_cm t ->
      f (Obj (Pdfgenlex.LexReal t.Pdftransform.a));
      f (Obj (Pdfgenlex.LexReal t.Pdftransform.b));
      f (Obj (Pdfgenlex.LexReal t.Pdftransform.c));
      f (Obj (Pdfgenlex.LexReal t.Pdftransform.d));
      f (Obj (Pdfgenlex.LexReal t.Pdftransform.e));
      f (Obj (Pdfgenlex.LexReal t.Pdftransform.f));
      f (Op "cm")
  | Op_m (a, b) ->
      f (Obj (Pdfgenlex.LexReal a)); f (Obj (Pdfgenlex.LexReal b)); f (Op "m")
  | Op_l (a, b) ->
      f (Obj (Pdfgenlex.LexReal a)); f (Obj (Pdfgenlex.LexReal b)); f (Op "l")
  | Op_c (a, b, c, d, e, k) ->
      f (Obj (Pdfgenlex.LexReal a)); f (Obj (Pdfgenlex.LexReal b));
      f (Obj (Pdfgenlex.LexReal c)); f (Obj (Pdfgenlex.LexReal d));
      f (Obj (Pdfgenlex.LexReal e)); f (Obj (Pdfgenlex.LexReal k)); f (Op "c");
  | Op_v (a, b, c, d) ->
      f (Obj (Pdfgenlex.LexReal a)); f (Obj (Pdfgenlex.LexReal b));
      f (Obj (Pdfgenlex.LexReal c)); f (Obj (Pdfgenlex.LexReal d)); f (Op "v")
  | Op_y (a, b, c, d) -> 
      f (Obj (Pdfgenlex.LexReal a)); f (Obj (Pdfgenlex.LexReal b));
      f (Obj (Pdfgenlex.LexReal c)); f (Obj (Pdfgenlex.LexReal d)); f (Op "y")
  | Op_h -> f (Op "h")
  | Op_re (a, b, c, d) -> 
      f (Obj (Pdfgenlex.LexReal a)); f (Obj (Pdfgenlex.LexReal b));
      f (Obj (Pdfgenlex.LexReal c)); f (Obj (Pdfgenlex.LexReal d)); f (Op "re")
  | Op_S -> f (Op "S")
  | Op_s -> f (Op "s")
  | Op_f -> f (Op "f")
  | Op_F -> f (Op "F")
  | Op_f' -> f (Op "f*")
  | Op_B -> f (Op "B")
  | Op_B' -> f (Op "B*")
  | Op_b -> f (Op "b")
  | Op_b' -> f (Op "b*")
  | Op_n -> f (Op "n")
  | Op_W -> f (Op "W")
  | Op_W' -> f (Op "W*")
  | Op_BT -> f (Op "BT")
  | Op_ET -> f (Op "ET")
  | Op_Tc c -> f (Obj (Pdfgenlex.LexReal c)); f (Op "Tc")
  | Op_Tw w -> f (Obj (Pdfgenlex.LexReal w)); f (Op "Tw")
  | Op_Tz z -> f (Obj (Pdfgenlex.LexReal z)); f (Op "Tz")
  | Op_TL l -> f (Obj (Pdfgenlex.LexReal l)); f (Op "TL") 
  | Op_Tf (k, s) ->
      f (Obj (Pdfgenlex.LexName k)); f (Obj (Pdfgenlex.LexReal s)); f (Op "Tf")
  | Op_Tr i -> f (Obj (Pdfgenlex.LexInt i)); f (Op "Tr")
  | Op_Ts k -> f (Obj (Pdfgenlex.LexReal k)); f (Op "Ts")
  | Op_Td (k, k') ->
      f (Obj (Pdfgenlex.LexReal k)); f (Obj (Pdfgenlex.LexReal k')); f (Op "Td")
  | Op_TD (k, k') ->
      f (Obj (Pdfgenlex.LexReal k)); f (Obj (Pdfgenlex.LexReal k')); f (Op "TD")
  | Op_Tm t ->
       f (Obj (Pdfgenlex.LexReal t.Pdftransform.a));
       f (Obj (Pdfgenlex.LexReal t.Pdftransform.b));
       f (Obj (Pdfgenlex.LexReal t.Pdftransform.c));
       f (Obj (Pdfgenlex.LexReal t.Pdftransform.d));
       f (Obj (Pdfgenlex.LexReal t.Pdftransform.e));
       f (Obj (Pdfgenlex.LexReal t.Pdftransform.f));
       f (Op "Tm")
  | Op_T' -> f (Op "T*")
  | Op_Tj s -> f (Obj (Pdfgenlex.LexString s)); f (Op "Tj")
  | Op_Tj_hex s -> f (Obj (Pdfgenlex.LexStringHex s)); f (Op "Tj")
  | Op_TJ pdfobject -> f (PdfObj pdfobject); f (Op "TJ")
  | Op_' s -> f (Obj (Pdfgenlex.LexString s)); f (Op "'")
  | Op_'' (k, k', s) -> 
      f (Obj (Pdfgenlex.LexReal k));
      f (Obj (Pdfgenlex.LexReal k'));
      f (Obj (Pdfgenlex.LexString s));
      f (Op "\"")
  | Op_d0 (k, k') ->
      f (Obj (Pdfgenlex.LexReal k)); f (Obj (Pdfgenlex.LexReal k')); f (Op "d0")
  | Op_d1 (a, b, c, d, e, k) ->
      f (Obj (Pdfgenlex.LexReal a)); f (Obj (Pdfgenlex.LexReal b));
      f (Obj (Pdfgenlex.LexReal c)); f (Obj (Pdfgenlex.LexReal d));
      f (Obj (Pdfgenlex.LexReal e)); f (Obj (Pdfgenlex.LexReal k)); f (Op "d1")
  | Op_CS s -> f (Obj (Pdfgenlex.LexName s)); f (Op "CS")
  | Op_cs s -> f (Obj (Pdfgenlex.LexName s)); f (Op "cs")
  | Op_SC fs -> iter (fun x -> f (Obj (Pdfgenlex.LexReal x))) fs; f (Op "SC")
  | Op_sc fs -> iter (fun x -> f (Obj (Pdfgenlex.LexReal x))) fs; f (Op "sc")
  | Op_SCN fs -> iter (fun x -> f (Obj (Pdfgenlex.LexReal x))) fs; f (Op "SCN")
  | Op_scn fs -> iter (fun x -> f (Obj (Pdfgenlex.LexReal x))) fs; f (Op "scn")
  | Op_SCNName (s, fs) ->
      iter (fun x -> f (Obj (Pdfgenlex.LexReal x))) fs;
      f (Obj (Pdfgenlex.LexName s)); f (Op "SCN")
  | Op_scnName (s, fs) ->
      iter (fun x -> f (Obj (Pdfgenlex.LexReal x))) fs;
      f (Obj (Pdfgenlex.LexName s)); f (Op "scn")
  | Op_G k -> f (Obj (Pdfgenlex.LexReal k)); f (Op "G")
  | Op_g k -> f (Obj (Pdfgenlex.LexReal k)); f (Op "g") 
  | Op_RG (r, g, b) ->
      f (Obj (Pdfgenlex.LexReal r)); f (Obj (Pdfgenlex.LexReal g));
      f (Obj (Pdfgenlex.LexReal b)); f (Op "RG")
  | Op_rg (r, g, b) ->
      f (Obj (Pdfgenlex.LexReal r)); f (Obj (Pdfgenlex.LexReal g));
      f (Obj (Pdfgenlex.LexReal b)); f (Op "rg")
  | Op_K (c, m, y, k) ->
      f (Obj (Pdfgenlex.LexReal c)); f (Obj (Pdfgenlex.LexReal m));
      f (Obj (Pdfgenlex.LexReal y)); f (Obj (Pdfgenlex.LexReal k)); f (Op "K")
  | Op_k (c, m, y, k) ->
      f (Obj (Pdfgenlex.LexReal c)); f (Obj (Pdfgenlex.LexReal m));
      f (Obj (Pdfgenlex.LexReal y)); f (Obj (Pdfgenlex.LexReal k)); f (Op "k")
  | Op_sh s -> f (Obj (Pdfgenlex.LexName s)); f (Op "sh")
  | InlineImage (dict, data) -> f (LexInlineImage (dict, data))
  | Op_Do s -> f (Obj (Pdfgenlex.LexName s)); f (Op "Do")
  | Op_MP s -> f (Obj (Pdfgenlex.LexName s)); f (Op "MP")
  | Op_DP (s, obj) ->
      f (Obj (Pdfgenlex.LexName s)); f (PdfObj obj); f (Op "DP")
  | Op_BMC s -> f (Obj (Pdfgenlex.LexName s)); f (Op "BMC")
  | Op_BDC (s, obj) ->
      f (Obj (Pdfgenlex.LexName s)); f (PdfObj obj); f (Op "BDC")
  | Op_EMC -> f (Op "EMC")
  | Op_BX -> f (Op "BX")
  | Op_EX -> f (Op "EX")
  | Op_Unknown _ -> ()

let lexemes_of_ops ops =
  let ls = ref [] in
    iter (lexemes_of_op (fun x -> ls := x::!ls)) ops;
    rev !ls

let lexemelists_of_ops (ops : t list) =
  map
    (fun op ->
       let ls = ref [] in
         lexemes_of_op (fun x -> ls := x::!ls) op; rev !ls)
    ops

(* Find a string representing some lexemes *)
let rec filterspecial = function
  | [] -> false
  | Pdf.Name ("/ASCIIHexDecode" | "/ASCII85Decode" | "/AHx" | "/A85")::_ -> true
  | _::t -> filterspecial t

let string_of_lexeme = function
  | LexComment -> ""
  | Obj o -> Pdfread.string_of_lexeme o 
  | Op op -> op
  | PdfObj obj -> Pdfwrite.string_of_pdf obj
  | LexInlineImage (dict, data) ->
      let dict_string = Pdfwrite.string_of_pdf dict in
        let dict_string' =
          (* Remove the dictionary markers. *)
          implode (rev (drop' 2 (rev (drop' 2 (explode dict_string)))))
        in let data_string =
          string_of_bytes data
        in let space =
          let filters =
            match
              Pdf.lookup_direct_orelse (Pdf.empty ()) "/F" "/Filter" dict
            with
            | Some (Pdf.Array filters) -> filters
            | Some (Pdf.Name f) -> [Pdf.Name f]
            | _ -> []
          in
            if filterspecial filters then "" else " "
        in
          "BI\n" ^ dict_string' ^ " ID" ^ space ^ data_string ^ "\nEI\n"

let b = Buffer.create 30

let string_of_lexemes lexemes =
  Buffer.clear b;
  iter
    (fun l ->
       let str = string_of_lexeme l in
         (* Add a space character if neither the current last character in the
          * buffer nor the first character of the new string is a delimiter *)
         if
           Buffer.length b > 0 &&
           not (Pdf.is_delimiter (Buffer.nth b (Buffer.length b - 1))) &&
           String.length str > 0 &&
           not (Pdf.is_delimiter str.[0])
         then
           Buffer.add_char b ' ';
         Buffer.add_string b str)
    lexemes;
  Buffer.contents b

(* Make a string of an operation, for debug purposes only. *)
let string_of_ops ops =
  string_of_lexemes (lexemes_of_ops ops)

let string_of_op op = string_of_ops [op]

exception LexingEnd

(* Lex a name. *)
let lex_name i =
  nudge i;
  Obj (Pdfgenlex.LexName ("/" ^ Pdfread.getuntil_white_or_delimiter_string i))

(* This is raised when we can't deal with some content. This should only happen
in the case of a malformed operator stream, not on any legitimate content. *)
exception Couldn'tHandleContent

let nocontent i =
  Printf.eprintf "Failed to understand content on page\n";
  if !Pdfread.read_debug then
    begin
      Pdfio.debug_next_n_chars 20 i;
      flprint "\n"
    end;
  raise Couldn'tHandleContent

(* Lex a number *)
let lex_number i =
  match Pdfread.lex_number i with
  | Pdfgenlex.LexReal r -> Obj (Pdfgenlex.LexReal r)
  | Pdfgenlex.LexInt i -> Obj (Pdfgenlex.LexReal (float_of_int i))
  | _ -> nocontent i

(* Lex and parse a dictionary to a Pdf.pdfobject. This constitutes a single
lexeme in terms of this module. *)
let get_dictionary i =
  PdfObj (snd (Pdfread.parse (Pdfread.lex_dictionary i)))

(* Given a colourspace and the number of bits per component, give the number of
bytes per pixel in the stored image data. *)
let rec components pdf resources t =
  match t with
  | Pdf.Name ("/CalGray" | "/DeviceGray" | "/G") -> 1
  | Pdf.Name ("/CalRGB" | "/DeviceRGB" | "/RGB") -> 3
  | Pdf.Name ("/CalCMYK" | "/DeviceCMYK" | "/CMYK") -> 4
  | Pdf.Name "/Pattern" ->
      raise (Pdf.PDFError "Can't use /Pattern here")
  | Pdf.Name space ->
      begin match Pdf.lookup_direct pdf "/ColorSpace" resources with
      | Some csdict ->
          begin match Pdf.lookup_direct pdf space csdict with
          | Some space' -> components pdf resources space'
          | None -> raise (Pdf.PDFError "ColorSpace not found")
          end
      | None -> raise (Pdf.PDFError "ColorSpace dict not found")
      end
  | Pdf.Array [Pdf.Name "/Lab"; _] -> 3
  | Pdf.Array [Pdf.Name "/ICCBased"; iccstream] ->
      begin match Pdf.lookup_direct pdf "/N" iccstream with
      | Some (Pdf.Integer n) -> n
      | _ -> raise (Pdf.PDFError "Bad iccstream")
      end
  | Pdf.Array (Pdf.Name "/DeviceN"::Pdf.Array items::_) ->
      (* 4th July 2017. Changed from looking at alternate to counting items. *)
      length items
  | Pdf.Array [Pdf.Name "/Separation"; _; _; _]
  | Pdf.Array (Pdf.Name ("/Indexed" | "/I")::_::_) -> 1
  | Pdf.Array [Pdf.Name "/CalRGB"; _] -> 3
  | Pdf.Array [Pdf.Name "/CalCMYK"; _] -> 4
  | Pdf.Array [Pdf.Name "/CalGray"; _] -> 1
  | Pdf.Array [Pdf.Name "/Pattern"; alternate] ->
      components pdf resources (Pdf.direct pdf alternate)
  | cs ->
     Printf.eprintf "%s\n" (Pdfwrite.string_of_pdf cs);
     raise (Pdf.PDFError "Unknown colourspace")

(* Lex an inline image. We read the dictionary, and then the stream. *)
let lex_inline_image pdf resources i =
  if !debug then Printf.eprintf "lex_inline_image at %i\n" (i.pos_in ());
  try
  let dict =
    let lexemes = Pdfread.lex_dictionary i in
      snd
        (Pdfread.parse
          ([Pdfgenlex.LexLeftDict] @ lexemes @ [Pdfgenlex.LexRightDict]))
  in
    if !debug then Printf.eprintf "dict was %s\n" (Pdfwrite.string_of_pdf dict);
    (* Read ID token *)
    Pdfread.dropwhite i;
    let c = char_of_int (i.input_byte ()) in
    let c' = char_of_int (i.input_byte ()) in
    match c, c' with
    | 'I', 'D' ->
      (* Skip a byte if not ASCII85 / ASCIIHex as one of the filters. *)
      let toskip =
        let filters =
          match Pdf.lookup_direct_orelse pdf "/F" "/Filter" dict with
          | Some (Pdf.Array filters) -> filters
          | Some (Pdf.Name f) -> [Pdf.Name f]
          | _ -> []
        in
          not (filterspecial filters)
      in
        if toskip then ignore (i.input_byte ());
        if !debug then Printf.eprintf "**got ID header, skipped possble byte";
        let bytes =
          let bpc =
            match
              Pdf.lookup_direct_orelse pdf "/BPC" "/BitsPerComponent" dict
            with
            | Some (Pdf.Integer bpc) -> bpc
            | _ ->
                Printf.eprintf "no BPC\n";
                nocontent i
          in
          let cspace =
            match Pdf.lookup_direct_orelse pdf "/CS" "/ColorSpace" dict with
            | Some
                (Pdf.Name
                   ("/DeviceGray" | "/DeviceRGB" | "/DeviceCMYK") as n) -> n
            | Some (Pdf.Name ("/G" | "/RGB" | "/CMYK") as n) -> n
            | Some ((Pdf.Array _) as n) -> n
            | Some (Pdf.Name cspace) ->
                if !debug then Printf.eprintf "resources is %s\n" (Pdfwrite.string_of_pdf resources);
                begin match Pdf.lookup_direct pdf "/ColorSpace" resources with
                | Some (Pdf.Dictionary _ as d) ->
                    begin match Pdf.lookup_direct pdf cspace d with
                    | Some c -> c
                    | _ ->
                        Printf.eprintf "no colourspace A\n";
                        nocontent i
                    end
                | _ ->
                    Printf.eprintf "no colourspace B\n";
                    nocontent i
                end
            | None ->
                (* Could it be an image mask? *)
                begin match
                  Pdf.lookup_direct_orelse pdf "/IM" "/ImageMask" dict
                with
                | Some (Pdf.Boolean true) -> Pdf.Name "/DeviceGray"
                | _ ->
                    Printf.eprintf "no colourspace C\n";
                    nocontent i
                end
            | _ ->
                Printf.eprintf "no colourspace D\n";
                nocontent i
          in let width =
            match Pdf.lookup_direct_orelse pdf "/W" "/Width" dict with
            | Some (Pdf.Integer w) -> w
            | _ ->
                Printf.eprintf "no or malformed /W";
                nocontent i 
          in let height =
            match Pdf.lookup_direct_orelse pdf "/H" "/Height" dict with
            | Some (Pdf.Integer h) -> h
            | _ ->
                Printf.eprintf "no or malformed /H";
                nocontent i
          in
            let bitwidth =
              components pdf resources cspace * bpc * width
            in
              let bytewidth =
                if bitwidth mod 8 = 0 then bitwidth / 8 else bitwidth / 8 + 1
              in
                bytewidth * height
        in
          let data =
            match
              Pdf.lookup_direct_orelse (Pdf.empty ()) "/F" "/Filter" dict
            with
            | None | Some (Pdf.Array []) ->
                begin try let data = mkbytes bytes in
                  if bytes > 0 then
                    for x = 0 to bytes_size data - 1 do
                      bset_unsafe data x (i.input_byte ());
                    done;
                  data
                with
                | e -> Printf.eprintf "%s" (Printexc.to_string e); raise e
                end
            | Some (Pdf.Name ("/DCT" | "/DCTDecode") | Pdf.Array [Pdf.Name ("/DCT" | "/DCTDecode")]) ->
                (* FIXME. The case of DCT combined with another one is possible e.g ["/DCT"; "/A85"]. Need to re-work. But have not seen an example yet. *)
                begin try Pdfjpeg.get_jpeg_data i with
                  e ->
                    Printf.eprintf "Couldn't read inline image JPEG data %s\n" (Printexc.to_string e);
                    raise e
                end
            | Some _ ->
                try
                  match Pdfcodec.decode_from_input i dict with
                  | None ->
                      Printf.eprintf "decode_from_input failed\n";
                      nocontent i
                  | Some data -> data 
                with
                  | Pdfcodec.DecodeNotSupported d ->
                      Printf.eprintf "Content DecodeNotSupported: %s\n" d;
                      nocontent i
                  | Pdfcodec.Couldn'tDecodeStream r ->
                      raise (Pdf.PDFError ("Inline image, bad data: " ^ r))
                  | e -> raise e
          in
            (* Read EI token *)
            Pdfread.dropwhite i;
            let c = char_of_int (i.input_byte ()) in
              let c' = char_of_int (i.input_byte ()) in
                begin match c, c' with
                | 'E', 'I' ->
                    (* Remove filter, predictor, if it wasn't JPEG. *)
                    let dict' =
                      match
                        Pdf.lookup_direct_orelse
                        (Pdf.empty ()) "/F" "/Filter" dict
                      with
                      (* FIXME as above *)
                      | Some (Pdf.Name ("/DCT" | "/DCTDecode") | Pdf.Array [Pdf.Name ("/DCT" | "/DCTDecode")]) -> dict
                      | _ -> 
                          fold_left
                            Pdf.remove_dict_entry
                            dict
                            ["/Filter"; "/F"; "/DecodeParms"; "/DP"] 
                    in
                      dict', data
                | x, y ->
                   Printf.eprintf "bad end to inline image %C, %C\n" x y;
                   nocontent i
                end
    | _ ->
        Printf.eprintf "Did not recognise beginning of inline image ID\n";
        nocontent i
  with
    e ->
      Printf.eprintf "inline image reading failed: %s\n" (Printexc.to_string e);
      nocontent i

(* Lex a keyword. *)
let lex_keyword pdf resources i =
  match Pdfread.getuntil_white_or_delimiter_string i with
  | "true" -> Obj (Pdfgenlex.LexBool true)
  | "false" -> Obj (Pdfgenlex.LexBool false)
  | "BI" -> LexInlineImage (lex_inline_image pdf resources i)
  | "ID" | "EI" -> nocontent i (* lex_inline_image should consume these *)
  | "" -> nocontent i
  | opstring -> Op opstring

(* Lex a string. *)
let lex_string i =
  match Pdfread.lex_string i with
  | Pdfgenlex.LexString str -> Obj (Pdfgenlex.LexString str)
  | _ -> nocontent i

(* Lex a hexadecimal string. *)
let lex_hexstring i =
  match Pdfread.lex_hexstring i with
  | Pdfgenlex.LexString str -> Obj (Pdfgenlex.LexString str)
  | _ -> nocontent i

(* Lex one token *)
let lex_next pdf resources i =
  try
    Pdfread.dropwhite i;
    match peek_byte i with
    | x when x = Pdfio.no_more ->
        raise LexingEnd
    | chr ->
        match char_of_int chr with
        | '/' -> lex_name i
        | '+' | '-' | '.' | '0'..'9' -> lex_number i
        | 'A'..'Z' | 'a'..'z' | '\'' | '\"' -> lex_keyword pdf resources i
        | '(' -> lex_string i
        | '[' -> nudge i; Obj (Pdfgenlex.LexLeftSquare)
        | ']' -> nudge i; Obj (Pdfgenlex.LexRightSquare)
        | '<' -> 
            begin match nudge i; let c = unopt (peek_char i) in rewind i; c with
            | '<' -> get_dictionary i
            | _ -> lex_hexstring i
            end
        | '%' -> ignore (Pdfread.lex_comment i); LexComment
        | _ -> raise (Pdf.PDFError "Lexing failure in content stream")
  with
    | Pdf.PDFError r -> 
        raise (Pdf.PDFError ("Pdfpages.lex_next => " ^ r))
    | Failure _ (*"unopt"*) | End_of_file ->
        raise LexingEnd 
    | Couldn'tHandleContent ->
        raise (Pdf.PDFError "Malformed page content")

let print_lexeme = function
  | Obj p -> Pdfread.print_lexeme p
  | Op s -> print_string s; print_newline ()
  | PdfObj p -> print_string "PDF OBJECT\n"
  | LexInlineImage _ -> print_string "INLINE IMAGE\n"
  | LexComment -> print_string "COMMENT\n"

(* Lex a graphics stream *)
let lex_stream pdf resources i =
  let lexemes = ref [] in
    try
      while true do
        match lex_next pdf resources i with
        | LexComment -> ()
        | lexeme -> lexemes := lexeme::!lexemes
      done;
      []
    with
      LexingEnd -> rev !lexemes

(* Split the lexemes into sections (zero or more operands followed by an
operator) and parse each. Section is reversed. *)
let split s =
  let rec split_inner prev = function
  | (Op _ | LexInlineImage _) as f::r -> f::prev, r
  | [] -> prev, []
  | x::r -> split_inner (x::prev) r
  in
    split_inner [] s

(* Parse a single operator and its operands, provided as a lexeme list. The
string from which these lexemes were extracted is provided so that Op_Unknown
instances can be generated. The compatibility level is also provided, and may be
updated. *)
let parse_operator compatibility = function
  | Op "W"::r -> Op_W, r
  | Op "W*"::r -> Op_W', r
  | Op "q"::r -> Op_q, r
  | Op "Q"::r -> Op_Q, r
  | Op "h"::r -> Op_h, r
  | Op "n"::r -> Op_n, r
  | Op "f*"::r -> Op_f', r
  | Op "f"::r -> Op_f, r
  | Op "F"::r -> Op_F, r
  | Op "BT"::r -> Op_BT, r
  | Op "ET"::r -> Op_ET, r
  | Op "B"::r -> Op_B, r
  | Op "B*"::r -> Op_B', r
  | Op "b"::r -> Op_b, r
  | Op "b*"::r -> Op_b', r
  | Op "S"::r -> Op_S, r
  | Op "s"::r -> Op_s, r
  | Op "T*"::r -> Op_T', r
  | Op "BX"::r -> incr compatibility; Op_BX, r
  | Op "EX"::r -> decr compatibility; Op_EX, r
  | Obj (Pdfgenlex.LexReal tx)::Obj (Pdfgenlex.LexReal ty)::Op "Td"::r ->
      Op_Td (tx, ty), r
  | Obj (Pdfgenlex.LexReal tx)::Obj (Pdfgenlex.LexReal ty)::Op "TD"::r ->
      Op_TD (tx, ty), r
  | Obj (Pdfgenlex.LexReal width)::Op "w"::r -> Op_w width, r
  | Obj (Pdfgenlex.LexReal cap)::Op "J"::r -> Op_J (int_of_float cap), r
  | Obj (Pdfgenlex.LexReal join)::Op "j"::r -> Op_j (int_of_float join), r
  | Obj (Pdfgenlex.LexReal x)::Obj (Pdfgenlex.LexReal y)::Op "m"::r ->
      Op_m (x, y), r
  | Obj (Pdfgenlex.LexReal x)::Obj (Pdfgenlex.LexReal y)::Op "l"::r ->
      Op_l (x, y), r
  | Obj (Pdfgenlex.LexReal leading)::Op "TL"::r -> Op_TL leading, r
  | Obj (Pdfgenlex.LexName n)::Obj (Pdfgenlex.LexReal s)::Op "Tf"::r ->
      Op_Tf (n, s), r
  | Obj (Pdfgenlex.LexString s)::Op "Tj"::r -> Op_Tj s, r
  | Obj (Pdfgenlex.LexReal r)::
    Obj (Pdfgenlex.LexReal g)::
    Obj (Pdfgenlex.LexReal b)::Op "RG"::rest ->
      Op_RG (r, g, b), rest
  | Obj (Pdfgenlex.LexReal r)::
    Obj (Pdfgenlex.LexReal g)::
      Obj (Pdfgenlex.LexReal b)::Op "rg"::rest ->
      Op_rg (r, g, b), rest
  | Obj (Pdfgenlex.LexReal g)::Op "G"::r -> Op_G g, r
  | Obj (Pdfgenlex.LexReal g)::Op "g"::r -> Op_g g, r
  | Obj (Pdfgenlex.LexReal c)::Obj (Pdfgenlex.LexReal m)::
    Obj (Pdfgenlex.LexReal y)::Obj (Pdfgenlex.LexReal k)::
    Op "k"::r -> Op_k (c, m, y, k), r
  | Obj (Pdfgenlex.LexReal c)::Obj (Pdfgenlex.LexReal m)::
    Obj (Pdfgenlex.LexReal y)::Obj (Pdfgenlex.LexReal k)::
    Op "K"::r -> Op_K (c, m, y, k), r
  | Obj (Pdfgenlex.LexReal a)::Obj (Pdfgenlex.LexReal b)::
    Obj (Pdfgenlex.LexReal c)::Obj (Pdfgenlex.LexReal d)::
    Obj (Pdfgenlex.LexReal e)::Obj (Pdfgenlex.LexReal f):: Op "cm"::r ->
       Op_cm
         {Pdftransform.a = a; Pdftransform.b = b; Pdftransform.c = c;
          Pdftransform.d = d; Pdftransform.e = e; Pdftransform.f = f}, r
  | Obj (Pdfgenlex.LexReal a)::Obj (Pdfgenlex.LexReal b)::
    Obj (Pdfgenlex.LexReal c)::Obj (Pdfgenlex.LexReal d)::
    Obj (Pdfgenlex.LexReal e)::Obj (Pdfgenlex.LexReal f)::Op "Tm"::r ->
       Op_Tm
         {Pdftransform.a = a; Pdftransform.b = b; Pdftransform.c = c;
          Pdftransform.d = d; Pdftransform.e = e; Pdftransform.f = f}, r
  | Obj (Pdfgenlex.LexName n)::Op "MP"::r -> Op_MP n, r
  | Obj (Pdfgenlex.LexName n)::PdfObj p::Op "DP"::r -> Op_DP (n, p), r
  | Obj (Pdfgenlex.LexName n)::Obj o::Op "DP"::r ->
      let p = snd (Pdfread.parse [o]) in Op_DP (n, p), r
  | Obj (Pdfgenlex.LexName n)::Op "BMC"::r -> Op_BMC n, r
  | Obj (Pdfgenlex.LexName n)::PdfObj p::Op "BDC"::r -> Op_BDC (n, p), r
  | Obj (Pdfgenlex.LexName n)::Obj o::Op "BDC"::r ->
      let p = snd (Pdfread.parse [o]) in Op_BDC (n, p), r
  | Op "EMC"::r -> Op_EMC, r
  | Obj (Pdfgenlex.LexName n)::Op "gs"::r -> Op_gs n, r
  | Obj (Pdfgenlex.LexName n)::Op "Do"::r -> Op_Do n, r
  | Obj (Pdfgenlex.LexName n)::Op "CS"::r -> Op_CS n, r
  | Obj (Pdfgenlex.LexName n)::Op "cs"::r -> Op_cs n, r
  | Obj (Pdfgenlex.LexReal x1)::Obj (Pdfgenlex.LexReal y1)::
    Obj (Pdfgenlex.LexReal x2)::Obj (Pdfgenlex.LexReal y2)::
    Obj (Pdfgenlex.LexReal x3)::Obj (Pdfgenlex.LexReal y3)::
     Op "c"::r -> Op_c (x1, y1, x2, y2, x3, y3), r
  | Obj (Pdfgenlex.LexReal x2)::Obj (Pdfgenlex.LexReal y2)::
    Obj (Pdfgenlex.LexReal x3)::Obj (Pdfgenlex.LexReal y3)::
     Op "v"::r -> Op_v (x2, y2, x3, y3), r
  | Obj (Pdfgenlex.LexReal x1)::Obj (Pdfgenlex.LexReal y1)::
    Obj (Pdfgenlex.LexReal x3)::Obj (Pdfgenlex.LexReal y3)::
     Op "y"::r -> Op_y (x1, y1, x3, y3), r
  | Obj (Pdfgenlex.LexReal x)::Obj (Pdfgenlex.LexReal y)::
    Obj (Pdfgenlex.LexReal w)::Obj (Pdfgenlex.LexReal h)::
     Op "re"::r -> Op_re (x, y, w, h), r
  | Obj (Pdfgenlex.LexName n)::Op "ri"::r -> Op_ri n, r
  | Obj (Pdfgenlex.LexReal i)::Op "i"::r -> Op_i (int_of_float i), r
  | Obj (Pdfgenlex.LexReal m)::Op "M"::r -> Op_M m, r
  | Obj (Pdfgenlex.LexString s)::Op "\'"::r -> Op_' s, r
  | Obj (Pdfgenlex.LexReal aw)::
    Obj (Pdfgenlex.LexReal ac)::
    Obj (Pdfgenlex.LexString s)::Op "\""::r ->
      Op_'' (aw, ac, s), r
  | Obj (Pdfgenlex.LexReal wx)::Obj (Pdfgenlex.LexReal wy)::Op "d0"::r ->
      Op_d0 (wx, wy), r
  | Obj (Pdfgenlex.LexReal wx)::Obj (Pdfgenlex.LexReal wy)::
    Obj (Pdfgenlex.LexReal llx)::Obj (Pdfgenlex.LexReal lly)::
    Obj (Pdfgenlex.LexReal urx)::Obj (Pdfgenlex.LexReal ury)::Op "d1"::r ->
      Op_d1 (wx, wy, llx, lly, urx, ury), r
  | Obj (Pdfgenlex.LexName n)::Op "sh"::r -> Op_sh n, r
  | Obj (Pdfgenlex.LexReal tc)::Op "Tc"::r -> Op_Tc tc, r
  | Obj (Pdfgenlex.LexReal tw)::Op "Tw"::r -> Op_Tw tw, r
  | Obj (Pdfgenlex.LexReal tz)::Op "Tz"::r -> Op_Tz tz, r
  | Obj (Pdfgenlex.LexReal tr)::Op "Tr"::r -> Op_Tr (toint tr), r
  | Obj (Pdfgenlex.LexReal ts)::Op "Ts"::r -> Op_Ts ts, r
  | LexInlineImage d::r -> InlineImage d, r
  | ls ->
      let ls, more = split ls in
      (* More complicated things are parsed by reversing the lexemes so we may
      inspect the operator. *)
      let r =
        let reals_of_real_lexemes errtext lexemes =
          let real_of_real_lexeme errtext = function
            | Obj (Pdfgenlex.LexReal n) -> n
            | _ -> raise (Pdf.PDFError errtext)
          in
            (* Adobe Distiller 5.0.5 produces bad Op_scn *)
            try rev_map (real_of_real_lexeme errtext) lexemes with
              _ -> [0.;0.;0.;]
        in
          match ls with
          | Op "sc"::nums ->
              Op_sc (reals_of_real_lexemes "Malformed 'sc'" nums)
          | Op "SC"::nums ->
              Op_SC (reals_of_real_lexemes "Malformed 'SC'" nums)
          | Op "scn"::Obj (Pdfgenlex.LexName n)::rest ->
              Op_scnName (n, reals_of_real_lexemes "scn" rest)
          | Op "SCN"::Obj (Pdfgenlex.LexName n)::rest ->
              Op_SCNName (n, reals_of_real_lexemes "SCN" rest)
          | Op "scn"::nums ->
              Op_scn (reals_of_real_lexemes "Malformed 'scn'" nums)
          | Op "SCN"::nums ->
              Op_SCN (reals_of_real_lexemes "Malformed 'SCN'" nums)
          | Op "d"::
            Obj (Pdfgenlex.LexReal phase)::
            Obj Pdfgenlex.LexRightSquare::r ->
              begin match rev r with
              | Obj Pdfgenlex.LexLeftSquare::t ->
                  let reals =
                    map
                      (function
                       | (Obj (Pdfgenlex.LexReal i)) -> i
                       | _ ->
                         raise (Pdf.PDFError "malformed 'd' op"))
                      t
                  in
                    Op_d (reals, phase) 
              | _ -> raise (Pdf.PDFError "malformed 'd' op")
              end
          | Op "TJ"::Obj Pdfgenlex.LexRightSquare::r ->
              begin match rev r with
              | Obj Pdfgenlex.LexLeftSquare::t ->
                  let elements =
                    map
                      (function
                       | (Obj (Pdfgenlex.LexReal i)) -> Pdf.Real i
                       | (Obj (Pdfgenlex.LexString s)) -> Pdf.String s
                       | _ -> raise (Pdf.PDFError "malformed TJ elt"))
                      t
                  in
                    Op_TJ (Pdf.Array elements)
              | _ -> raise (Pdf.PDFError "malformed TJ op")
              end
          | Op _::_ as l -> Op_Unknown (string_of_lexemes l)
          | l ->
             Printf.eprintf "Empty or malformed graphics operation.";
             Op_Unknown (string_of_lexemes l)
      in
        r, more

let rec parse_lexemes compatibility ls ops =
  match ls with
  | [] -> rev ops
  | _ ->
      let op, remaining = parse_operator compatibility ls in
        parse_lexemes compatibility remaining (op::ops)

(* Parse, given a list of streams. The contents of a single PDF page can be
split over several streams, which must be concatenated at the lexeme level. *)

(* Concatenate bytess, padding with whitespace *)
let concat_bytess ss =
  let total_length = fold_left ( + ) 0 (map bytes_size ss) in
    let s' = mkbytes (total_length + length ss) in
      let p = ref 0 in
        iter
          (fun s ->
             for x = 0 to bytes_size s - 1 do
               bset_unsafe s' !p (bget s x);
               incr p
             done;
             bset_unsafe s' !p (int_of_char ' ');
             incr p)
          ss;
        s'

let parse_stream pdf resources streams =
  let stream = match streams with [s] -> s | _ -> concat_bytess streams in
  let i = input_of_bytes stream in
    let lexemes = lex_stream pdf resources i in
      parse_lexemes (ref 0) lexemes []

(* Parse the operators in a list of streams. *)
let parse_operators pdf resources streams =
  let rawstreams =
    map
      (fun c ->
        let c = Pdf.direct pdf c in
          Pdfcodec.decode_pdfstream pdf c;
          Pdf.bigarray_of_stream c)
      streams
  in
    parse_stream pdf resources rawstreams
    
(* Flattening *)
          
(* Give a bigarray representing a list of graphics operators. *)
let stream_of_lexemes (oplists : lexeme list list) =
  let strings =
    map string_of_lexemes oplists
  in
    (* Insert a space if the neither the last character of a string nor the
     * first character of the next is a delimiter *)
    let rec addspaces prev = function
      [] -> rev prev
    | [x] -> addspaces (x :: prev) []
    | x::y::r ->
        if
             String.length x > 0 && Pdf.is_delimiter x.[String.length x - 1]
          || String.length y > 0 && Pdf.is_delimiter y.[0]
        then
          addspaces (x :: prev) (y :: r)
        else
          addspaces (" " :: x :: prev) (y :: r)
    in
      let strings = addspaces [] strings in
        let total_length =
          let l = ref 0 in iter (fun s -> l := !l + String.length s) strings; !l
        in
          let s = mkbytes total_length
          in let strings = ref strings
          in let pos = ref 0 in
            while !strings <> [] do
              let str = hd !strings in
                let l = String.length str in
                  if l > 0 then
                    for n = 0 to l - 1 do
                      bset_unsafe s !pos (int_of_char str.[n]);
                      incr pos
                    done;
                  strings := tl !strings
            done;
            s

let print_stream s =
  if bytes_size s > 0 then 
    for x = 0 to bytes_size s - 1 do
      Printf.printf "%c" (char_of_int (bget s x))
    done;
  print_newline ()
    
(* Make a stream from a list of operators. *)
let stream_of_ops ops =
  let data = stream_of_lexemes (lexemelists_of_ops ops) in
    Pdf.Stream
      (ref
        (Pdf.Dictionary
          [("/Length", Pdf.Integer (bytes_size data))], Pdf.Got data))

