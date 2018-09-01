(* \chaptertitle{PDFText}{Reading and writing text} *)
open Pdfutil
open Pdfio

(* \section{Data type for fonts} *)

(* Type 3 Specific Glyph Data *)
type type3_glpyhs =
  {fontbbox : float * float * float * float;
   fontmatrix : Pdftransform.transform_matrix;
   charprocs : (string * Pdf.pdfobject) list;
   type3_resources : Pdf.pdfobject}

(* A font is either one of the standard 14 fonts, a simple font, or.. *)
type simple_fonttype =
  | Type1
  | MMType1
  | Type3 of type3_glpyhs
  | Truetype

type fontmetrics = float array (*r widths of glyphs 0..255 *)

(* The fontfile is an indirect reference into the document, rather than a
PDFobject itself. This preserves polymorphic equality (a pdfobject can contain
functional values *)
type fontfile =
  | FontFile of int
  | FontFile2 of int
  | FontFile3 of int

type fontdescriptor = {
    italicangle : float;
    ascent      : float;
    descent     : float;
    leading     : float;
    avgwidth    : float;
    maxwidth    : float;
    stemv       : float;
    fontfile    : fontfile option;
  }

type differences = (string * int) list

type encoding =
  | ImplicitInFontFile
  | StandardEncoding
  | MacRomanEncoding
  | WinAnsiEncoding
  | MacExpertEncoding
  | CustomEncoding of encoding * differences
  | FillUndefinedWithStandard of encoding

type simple_font =
  {fonttype : simple_fonttype;
   basefont : string;
   fontmetrics : fontmetrics option;
   fontdescriptor : fontdescriptor option;
   encoding : encoding}

type standard_font =
  | TimesRoman
  | TimesBold
  | TimesItalic
  | TimesBoldItalic
  | Helvetica
  | HelveticaBold
  | HelveticaOblique
  | HelveticaBoldOblique
  | Courier
  | CourierBold
  | CourierOblique
  | CourierBoldOblique
  | Symbol
  | ZapfDingbats

let string_of_standard_font = function
  | TimesRoman -> "Times-Roman"
  | TimesBold -> "Times-Bold"
  | TimesItalic -> "Times-Italic"
  | TimesBoldItalic -> "Times-BoldItalic"
  | Helvetica -> "Helvetica"
  | HelveticaBold -> "Helvetica-Bold"
  | HelveticaOblique -> "Helvetica-Oblique"
  | HelveticaBoldOblique -> "Helvetica-BoldOblique"
  | Courier -> "Courier"
  | CourierBold -> "Courier-Bold"
  | CourierOblique -> "Courier-Oblique"
  | CourierBoldOblique -> "Courier-BoldOblique"
  | Symbol -> "Symbol"
  | ZapfDingbats -> "ZapfDingbats"
  
type cid_system_info =
  {registry : string;
   ordering : string;
   supplement : int}

type composite_CIDfont =
  {cid_system_info : cid_system_info;
   cid_basefont : string;
   cid_fontdescriptor : fontdescriptor;
   cid_widths : (int * float) list;
   cid_default_width : int}

type cmap_encoding =
  | Predefined of string
  | CMap of int (* indirect reference to CMap stream *)

type font =
  | StandardFont of standard_font * encoding
  | SimpleFont of simple_font
  | CIDKeyedFont of string * composite_CIDfont * cmap_encoding (* string is top-level basefont *)

(* For Debug *)
let string_of_fonttype = function
  | Type1 -> "Type 1"
  | MMType1 -> "MM Type 1"
  | Type3 {charprocs = charprocs} ->
      "Type 3: charprocs for" ^
      fold_left ( ^ ) "" (map (function (s, _) -> s) charprocs)
  | Truetype -> "Truetype" 

let rec string_of_encoding = function
  | ImplicitInFontFile -> "ImplicitInFontFile"
  | StandardEncoding -> "StandardEncoding"
  | MacRomanEncoding -> "MacRomanEncoding"
  | WinAnsiEncoding -> "WinAnsiEncoding"
  | MacExpertEncoding -> "MacExpertEncoding"
  | CustomEncoding (enc, diff) ->
      Printf.sprintf
        "CustomEncoding with base %s and differences %s"
        (string_of_encoding enc)
        (fold_left ( ^ ) "" (map (function (name, code) -> Printf.sprintf "%i -> %s, " code name) diff))
  | FillUndefinedWithStandard enc ->
      Printf.sprintf "FillUndefinedWithStandard %s" (string_of_encoding enc)

let string_of_simple_font font =
  "fonttype = " ^ string_of_fonttype font.fonttype ^ "\n" ^
  "basefont " ^ font.basefont ^ "\n" ^
  "encoding is " ^ string_of_encoding font.encoding ^ "\n"

let string_of_font = function
  | StandardFont (s, _) -> "StandardFont " ^ string_of_standard_font s
  | SimpleFont s -> "SimpleFont " ^ string_of_simple_font s
  | CIDKeyedFont (s, _, _) -> "CIDKeyedFont " ^ s

let read_type3_data pdf font =
  {fontbbox =
     (let obj = Pdf.lookup_fail "No fontbbox" pdf "/FontBBox" font in
       Pdf.parse_rectangle obj);
   fontmatrix =
     Pdf.parse_matrix pdf "/FontMatrix" font;
   charprocs = 
     (match Pdf.lookup_fail "Bad Charprocs" pdf "/CharProcs" font with
      | Pdf.Dictionary l -> l
      | _ -> raise (Pdf.PDFError "Bad charprocs")); 
   type3_resources =
     (match Pdf.lookup_direct pdf "/Resources" font with
      | None -> Pdf.Dictionary []
      | Some d -> d)}

let simple_fonttype_of_string pdf font = function
  | "/Type1" -> Some Type1
  | "/MMType1" -> Some MMType1
  | "/Type3" ->
      Some (Type3 (read_type3_data pdf font))
  | "/TrueType" -> Some Truetype
  | _ -> None

let read_basefont pdf font =
  match Pdf.lookup_direct pdf "/BaseFont" font with
  | Some (Pdf.Name n) -> n
  | _ -> ""

let read_fontdescriptor pdf font =
  match Pdf.lookup_direct pdf "/FontDescriptor" font with
  | None -> None
  | Some fontdescriptor ->
      let ascent =
        match Pdf.lookup_direct pdf "/Ascent" fontdescriptor with
        | Some x -> Pdf.getnum x
        | None -> 0.
      in let descent =
        match Pdf.lookup_direct pdf "/Descent" fontdescriptor with
        | Some x -> Pdf.getnum x
        | None -> 0.
      in let leading =
        match Pdf.lookup_direct pdf "/Leading" fontdescriptor with
        | Some x -> Pdf.getnum x
        | None -> 0.
      in let avgwidth =
        match Pdf.lookup_direct pdf "/AvgWidth" fontdescriptor with
        | Some x -> Pdf.getnum x
        | None -> 0.
      in let maxwidth =
        match Pdf.lookup_direct pdf "/MaxWidth" fontdescriptor with
        | Some x -> Pdf.getnum x
        | None -> 0.
      in let fontfile =
        match Pdf.find_indirect "/FontFile" fontdescriptor with
        | Some i -> Some (FontFile i)
        | None ->
            match Pdf.find_indirect "/FontFile2" fontdescriptor with
            | Some i -> Some (FontFile2 i)
            | None ->
                match Pdf.find_indirect "/FontFile3" fontdescriptor with
                | Some i -> Some (FontFile3 i)
                | None -> None
      in let stemv =
        match Pdf.lookup_direct pdf "/StemV" fontdescriptor with
        | Some x -> Pdf.getnum x
        | None   -> 0.
      in let italicangle =
        match Pdf.lookup_direct pdf "/ItalicAngle" fontdescriptor with
        | Some x -> Pdf.getnum x
        | None   -> 0.
      in
        Some {
          ascent      = ascent;
          descent     = descent;
          italicangle = italicangle;
          leading     = leading;
          stemv       = stemv;
          avgwidth    = avgwidth;
          maxwidth    = maxwidth;
          fontfile    = fontfile;
        }

(* Read the widths from a font. Normally in the font descriptor, but in Type3
fonts at the top level. *)
let read_metrics pdf font =
  let fontdescriptor =
    match Pdf.lookup_direct pdf "/Subtype" font with
    | Some (Pdf.Name "/Type3") -> Some font
    | _ -> Pdf.lookup_direct pdf "/FontDescriptor" font
  in
    match fontdescriptor with
    | None -> None
    | Some fontdescriptor ->
        let firstchar =
          match Pdf.lookup_direct pdf "/FirstChar" font with
          | Some (Pdf.Integer i) ->
              if i <= 255 && i >= 0 then i else
                raise (Pdf.PDFError "Bad /Firstchar")
          | _ -> raise (Pdf.PDFError "No /FirstChar")
        in let lastchar =
          match Pdf.lookup_direct pdf "/LastChar" font with
          | Some (Pdf.Integer i) ->
              if i <= 255 && i >= 0 then i else
                raise (Pdf.PDFError "Bad /Lastchar")
          | _ -> raise (Pdf.PDFError "No /LastChar")
        in let missingwidth =
          match Pdf.lookup_direct pdf "/MissingWidth" fontdescriptor with
          | Some (Pdf.Integer w) -> float w
          | Some (Pdf.Real w) -> w
          | _ -> 0.
        in
          let elts =
            match Pdf.lookup_direct pdf "/Widths" font with
            | Some (Pdf.Array elts) -> elts
            | _ -> raise (Pdf.PDFError "No /Widths")
          in
            if length elts <> lastchar - firstchar + 1
              then raise (Pdf.PDFError "Bad /Widths")
              else
                let before =
                  many missingwidth firstchar
                in let given =
                  map
                    (fun elt ->
                       match Pdf.direct pdf elt with
                       | Pdf.Integer i -> float i
                       | Pdf.Real f -> f
                       | _ -> raise (Pdf.PDFError "Bad /Width entry"))
                    elts
                in let after =
                  many missingwidth (255 - lastchar)
                in
                  Some (Array.of_list (before @ given @ after))

(* Parse a /Differences entry to get a list of (name, number) pairs *)
let pairs_of_differences pdf differences =
  let rec groups_of_differences prev elts =
    match elts with
    | [] -> prev
    | Pdf.Integer n::rest ->
        let stripname = function Pdf.Name n -> n | _ -> assert false in
          let names, more =
            cleavewhile (function Pdf.Name _ -> true | _ -> false) rest
          in
            groups_of_differences ((n, map stripname names)::prev) more
    | _ -> raise (Pdf.PDFError "Malformed /Differences")
  and mappings_of_group (x, es) =
    let additions = ilist 0 (length es - 1) in
      map2 (fun e a -> (x + a, e)) es additions
  in
    match differences with
    | Pdf.Array elts ->
        let direct_elements = map (Pdf.direct pdf) elts in
          let groups = groups_of_differences [] direct_elements in
            map
             (fun (k, v) -> (v, k))
             (flatten (map mappings_of_group groups))
    | _ -> raise (Pdf.PDFError "Bad /Differences")

let standard_font_of_name = function
  | "/Times-Roman" | "/TimesNewRoman" -> Some TimesRoman
  | "/Times-Bold" | "/TimesNewRoman,Bold" -> Some TimesBold
  | "/Times-Italic" | "/TimesNewRoman,Italic" -> Some TimesItalic
  | "/Times-BoldItalic" | "/TimesNewRoman,BoldItalic" -> Some TimesBoldItalic
  | "/Helvetica" | "/Arial" -> Some Helvetica
  | "/Helvetica-Bold" | "/Arial,Bold" -> Some HelveticaBold
  | "/Helvetica-Oblique" | "/Arial,Italic" -> Some HelveticaOblique
  | "/Helvetica-BoldOblique" | "/Arial,BoldItalic" -> Some HelveticaBoldOblique
  | "/Courier" | "/CourierNew" -> Some Courier
  | "/Courier-Bold" | "/CourierNew,Bold" -> Some CourierBold
  | "/Courier-Oblique" | "/CourierNew,Italic" -> Some CourierOblique
  | "/Courier-BoldOblique" | "/CourierNew,BoldItalic" -> Some CourierBoldOblique
  | "/Symbol" -> Some Symbol
  | "/ZapfDingbats" -> Some ZapfDingbats
  | _ -> None

(* Predicate: is it a standard 14 font? If it's been overriden (contains widths
etc, we treat it as a simple font. *)
let is_standard14font pdf font =
  match Pdf.lookup_direct pdf "/Subtype" font with
  | Some (Pdf.Name "/Type1") ->
      begin match Pdf.lookup_direct pdf "/BaseFont" font with
      | Some (Pdf.Name name) ->
          begin match standard_font_of_name name with
          | None -> false
          | Some _ ->
              (* Check to see if it's been overriden *)
              match Pdf.lookup_direct pdf "/Widths" font with
              | None -> true
              | _ -> false
          end
      | _ -> false
      end
  | _ -> false

(* Is a font embedded in the document? *)
let is_embedded pdf font =
  match Pdf.lookup_direct pdf "/FontDescriptor" font with
  | None -> false
  | Some fontdescriptor ->
      match
        Pdf.lookup_direct_orelse pdf "/FontFile" "/FontFile2" fontdescriptor
      with
      | Some _ -> true
      | None ->
          match Pdf.lookup_direct pdf "/FontFile3" fontdescriptor with
          | Some _ -> true
          | None -> false

(* Is a font symbolic? (Doesn't deal with standard 14 Zapf and Symbol) *)
let is_symbolic pdf font =
  match Pdf.lookup_direct pdf "/FontDescriptor" font with
  | None -> false
  | Some fontdescriptor ->
      match Pdf.lookup_direct pdf "/Flags" fontdescriptor with
      | Some (Pdf.Integer flags) -> flags land (1 lsl 3) > 0
      | _ -> raise (Pdf.PDFError "No /Flags in font descriptor")

(* For now, not for truetype fonts: add pg 399-401 later. Need to clarify what
happens if a standard-14 font is overriden. *)
let read_encoding pdf font =
  match Pdf.lookup_direct pdf "/Encoding" font with
  | Some (Pdf.Name "/MacRomanEncoding") -> MacRomanEncoding
  | Some (Pdf.Name "/MacExpertEncoding") -> MacExpertEncoding
  | Some (Pdf.Name "/WinAnsiEncoding") -> WinAnsiEncoding
  | Some (Pdf.Dictionary _ as encdict) ->
      begin match Pdf.lookup_direct pdf "/Subtype" font with
      | Some
          (Pdf.Name (("/Type1" | "/MMType1" | "/Type3" | "/TrueType") as fonttype))
        ->
          let encoding =
            let base_encoding =
              match Pdf.lookup_direct pdf "/BaseEncoding" encdict with
              | Some (Pdf.Name "/MacRomanEncoding") -> MacRomanEncoding
              | Some (Pdf.Name "/MacExpertEncoding") -> MacExpertEncoding
              | Some (Pdf.Name "/WinAnsiEncoding") -> WinAnsiEncoding
              | None ->
                  if is_embedded pdf font
                  then ImplicitInFontFile
                    else if is_symbolic pdf font
                      then ImplicitInFontFile
                      else StandardEncoding
              | _ -> raise (Pdf.PDFError "unknown /BaseEncoding")
            in
              begin match Pdf.lookup_direct pdf "/Differences" encdict with
              | Some differences ->
                  CustomEncoding
                    (base_encoding, pairs_of_differences pdf differences)
              | _ -> base_encoding
              end
          in
            if fonttype = "/Truetype"
              then FillUndefinedWithStandard encoding
              else encoding
      | _ -> raise (Pdf.PDFError "Bad font /Subtype")
      end
  | _ -> ImplicitInFontFile

let read_simple_font pdf font =
  match Pdf.lookup_direct pdf "/Subtype" font with
  | Some (Pdf.Name n) ->
      begin match simple_fonttype_of_string pdf font n with
      | Some fonttype ->
          let fontdescriptor = read_fontdescriptor pdf font in
            SimpleFont
              {fonttype = fonttype;
               basefont = read_basefont pdf font;
               fontmetrics = read_metrics pdf font;
               fontdescriptor = fontdescriptor;
               encoding = read_encoding pdf font}
      | None -> raise (Pdf.PDFError "Not a simple font")
      end
  | _ -> raise (Pdf.PDFError "No font /Subtype")

(* Read a base 14 font *)
let read_standard14font pdf font =
  match Pdf.lookup_direct pdf "/BaseFont" font with
  | Some (Pdf.Name name) ->
      begin match standard_font_of_name name with
      | None -> raise (Pdf.PDFError "Not a base 14 font")
      | Some f -> StandardFont (f, read_encoding pdf font)
      end
  | _ -> raise (Pdf.PDFError "Bad base 14 font")

(* Predicate: is it a simple font, assuming it's not a standard 14 font. *)
let is_simple_font pdf font =
  match Pdf.lookup_direct pdf "/Subtype" font with
  | Some (Pdf.Name ("/Type1" | "/MMType1" | "/Type3" | "/TrueType")) -> true
  | _ -> false

(* Predicate: is it a CIDKeyed font? *)
let is_cidkeyed_font pdf font =
  match Pdf.lookup_direct pdf "/Subtype" font with
  | Some (Pdf.Name "/Type0") -> true
  | _ -> false

(* Read a CID system info dictionary *)
let read_cid_system_info pdf dict =
  {registry =
     begin match Pdf.lookup_direct pdf "/Registry" dict with
     | Some (Pdf.String s) -> s
     | _ -> raise (Pdf.PDFError "No /Registry")
     end;
   ordering =
     begin match Pdf.lookup_direct pdf "/Ordering" dict with
     | Some (Pdf.String s) -> s
     | _ -> raise (Pdf.PDFError "No /Ordering")
     end;
   supplement =
     begin match Pdf.lookup_direct pdf "/Supplement" dict with
     | Some (Pdf.Integer i) -> i
     | _ -> raise (Pdf.PDFError "No /Supplement")
     end}

(* This returns the explicit pairs, which need to be combined
with the default value to look a width up. *)
let rec read_cid_widths = function
  | Pdf.Integer c::Pdf.Array ws::more ->
      let nums =
        map
          (function
           | Pdf.Integer i -> float i
           | Pdf.Real r -> r
           | _ -> raise (Pdf.PDFError "Bad /W array"))
        ws
      in
        combine (indxn c nums) nums @ read_cid_widths more
  | Pdf.Integer c_first::Pdf.Integer c_last::w::more ->
      let w =
        match w with
        | Pdf.Integer i -> float i
        | Pdf.Real r -> r
        | _ -> raise (Pdf.PDFError "Bad /W array")
      in
        if c_last = c_first then read_cid_widths more else (* Added 26/01/09 *) 
        if c_last < c_first
          then raise (Pdf.PDFError "Bad /W array")
          else
            let pairs =
              combine
                (ilist c_first c_last)
                (many w (c_last - c_first + 1))
            in
              pairs @ read_cid_widths more
  | [] -> []
  | _ -> raise (Pdf.PDFError "Malformed /W in CIDfont")

(* Read a composite CID font *)
(* FIXME: Doesn't support vertical modes (DW2 / W2) *)
let read_descendant pdf dict =
  let cid_system_info =
    match Pdf.lookup_direct pdf "/CIDSystemInfo" dict with
    | Some cid_dict -> read_cid_system_info pdf cid_dict
    | None -> raise (Pdf.PDFError "No CIDSystemInfo")
  in let cid_basefont =
    match Pdf.lookup_direct pdf "/BaseFont" dict with
    | Some (Pdf.Name n) -> n
    | _ -> raise (Pdf.PDFError "No /BaseFont")
  in let cid_fontdescriptor =
    match read_fontdescriptor pdf dict with
    | Some f -> f
    | None -> raise (Pdf.PDFError "No FontDescriptor in CIDkeyed font")
  in let cid_widths =
    match Pdf.lookup_direct pdf "/W" dict with
    | Some (Pdf.Array ws) -> read_cid_widths ws
    | _ -> []
  in let default_width =
    match Pdf.lookup_direct pdf "/DW" dict with
    | Some (Pdf.Integer d) -> d
    | _ -> 1000
  in
    {cid_system_info = cid_system_info;
     cid_basefont = cid_basefont;
     cid_fontdescriptor = cid_fontdescriptor;
     cid_widths = cid_widths;
     cid_default_width = default_width}

(* Read a CIDKeyed (Type 0) font *)
let read_cidkeyed_font pdf font =
  let basefont =
    match Pdf.lookup_direct pdf "/BaseFont" font with
    | Some (Pdf.Name b) -> b
    | _ -> raise (Pdf.PDFError "Bad /BaseFont")
  in let composite_CIDfont =
    match Pdf.lookup_direct pdf "/DescendantFonts" font with
    | Some (Pdf.Array [e]) ->
        read_descendant pdf (Pdf.direct pdf e)
    | _ -> raise (Pdf.PDFError "Bad descendant font")
  in let encoding =
    match Pdf.lookup_direct pdf "/Encoding" font with
    | Some (Pdf.Name e) -> Predefined e
    | Some (Pdf.Stream _) ->
        begin match Pdf.find_indirect "/Encoding" font with
        | Some n -> CMap n
        | None -> raise (Pdf.PDFError "malformed /Encoding")
        end
    | _ -> raise (Pdf.PDFError "malformed or missing /Encoding")
  in
    CIDKeyedFont (basefont, composite_CIDfont, encoding)

(* Reads a font *)
let read_font pdf fontdict =
  if is_standard14font pdf fontdict
    then read_standard14font pdf fontdict
    else if is_simple_font pdf fontdict
      then read_simple_font pdf fontdict
      else if is_cidkeyed_font pdf fontdict
        then read_cidkeyed_font pdf fontdict
        else raise (Pdf.PDFError "Unknown font type")

(* Write a font. Currently only works for our own special Type 3 fonts
generated by the Pdfcff module, but eventually, when we support font embedding,
it'll work for lots of kinds. *)
let write_encoding pdf = function
  | CustomEncoding (ImplicitInFontFile, diffs) ->
      let diffarray =
        Pdf.Array
          (flatten (map (function (name, number) -> [Pdf.Integer number; Pdf.Name name]) diffs))
      in
        let encodingdict =
          Pdf.Dictionary
            [("/Type", Pdf.Name "/Encoding");
             ("/Differences", diffarray)]
        in
          Pdf.addobj pdf encodingdict
  | _ -> raise (Pdf.PDFError "write_encoding: not supported")
 
let write_font pdf = function
  | SimpleFont
      {fonttype = Type3
         {fontbbox = fontbbox;
          fontmatrix = fontmatrix;
          charprocs = charprocs};
       encoding = encoding;
       fontdescriptor = Some fontdescriptor} ->
        let encoding_entry =
          match encoding with
          | ImplicitInFontFile -> []
          | _ -> [("/Encoding", Pdf.Indirect (write_encoding pdf encoding))]
        in
          let dict =
            Pdf.Dictionary
              ([("/Type", Pdf.Name "/Font");
               ("/Subtype", Pdf.Name "/Type3");
               ("/FontBBox", Pdf.Array [Pdf.Real 0.; Pdf.Real 0.; Pdf.Real 0.; Pdf.Real 0.]);
               ("/FontMatrix", Pdf.Array [Pdf.Real 0.; Pdf.Real 0.; Pdf.Real 0.; Pdf.Real 0.; Pdf.Real 0.; Pdf.Real 0.]);
               ("/CharProcs", Pdf.Dictionary (map (fun (s, _) -> (s, Pdf.Null)) charprocs));
               ("/FirstChar", Pdf.Integer 0);
               ("/LastChar", Pdf.Integer 0);
               ("/Widths", Pdf.Array [Pdf.Real 0.])] @ encoding_entry)
          in
            Pdf.addobj pdf dict
  | _ -> raise (Pdf.PDFError "write_font only supports type 3 fonts")

(* \section{Font encodings} *)

(* Parse a /ToUnicode CMap to extract font mapping. *)
type section =
  | BfChar of char list
  | BfRange of char list

let rec getuntilend prev = function
  | [] -> rev prev, []
  | 'e'::'n'::'d'::'b'::'f'::'c'::'h'::'a'::'r'::more -> rev prev, more
  | h::t -> getuntilend (h::prev) t

let rec getuntilend_range prev = function
  | [] -> rev prev, []
  | 'e'::'n'::'d'::'b'::'f'::'r'::'a'::'n'::'g'::'e'::more -> rev prev, more
  | h::t -> getuntilend_range (h::prev) t

let rec get_section = function
  | [] -> None
  | 'b'::'e'::'g'::'i'::'n'::'b'::'f'::'c'::'h'::'a'::'r'::more ->
      let numbers, rest = getuntilend [] more in
        Some (BfChar numbers, rest)
  | 'b'::'e'::'g'::'i'::'n'::'b'::'f'::'r'::'a'::'n'::'g'::'e'::more ->
      let numbers, rest = getuntilend_range [] more in
        Some (BfRange numbers, rest)
  | _::t -> get_section t

(* Read a character code. *)
let rec read_number = function
  | x::more when Pdf.is_whitespace x -> read_number more
  | '<'::a::'>'::more ->
      int_of_string (implode ['0'; 'x'; a]), more
  | '<'::a::b::'>'::more ->
      int_of_string (implode ['0'; 'x'; a; b]), more
  | '<'::a::b::c::'>'::more ->
      int_of_string (implode ['0'; 'x'; a; b; c]), more
  | '<'::a::b::c::d::'>'::more ->
      int_of_string (implode ['0'; 'x'; a; b; c; d]), more
  | [] -> raise Not_found
  | _ -> raise (Pdf.PDFError "Unknown number in /ToUnicode")

(* Read the bytes of the UTF-16BE unicode sequence as a string. *)
let fail () =
  raise (Pdf.PDFError "Bad unicode value")

let rec read_unicode = function
  | x::rest when Pdf.is_whitespace x -> read_unicode rest
  | '<'::rest ->
      let chars, rest  = cleavewhile (neq '>') rest in
        let is_hex_digit = function
          | x when (x >= '0' && x <= '9') || (x >= 'a' && x <= 'f') || (x >= 'A' && x <= 'F') -> true
          | _ -> false
        in
          iter
            (fun x -> if not (is_hex_digit x) then fail ())
            chars;
          if length chars > 0 && even (length chars) then
            let bytes =
              map
                (function
                  | [x; y] -> char_of_int (int_of_string (implode ['0'; 'x'; x; y]))
                  | _ -> assert false)
                (splitinto 2 chars)
            in
              let rest' =
                match rest with
                | [] -> []
                | _ -> tl rest
              in
                implode bytes, rest'
          else
            fail ()
  | _ -> fail ()

let rec get_sections chars =
  match get_section chars with
  | None -> []
  | Some (sec, restchars) ->
      sec::get_sections restchars

let pairs_of_section = function
  | BfChar numbers ->
      let results = ref []
      in let numbers = ref numbers in
        begin try
          while true do
            let number, rest = read_number !numbers in
              let str, rest = read_unicode rest in
                numbers := rest;
                results =| (number, str)
          done;
          []
        with
          Not_found -> rev !results
        end
  | BfRange numbers ->
      let results = ref []
      in let numbers = ref numbers in
        begin try
          while true do
            let src1, rest  = read_number !numbers in
              let src2, rest  = read_number rest in
                if src1 > src2 then raise (Pdf.PDFError "Bad /ToUnicode") else
                  match rest with
                  | '<'::_ ->
                      (* It's a single unicode string *)
                      let increment_final code d =
                        match code with
                        | "" -> ""
                        | s ->
                            let chars = rev (explode s) in
                              implode ((rev (tl chars)) @
                              [char_of_int (int_of_char (hd chars) + d)])
                      in
                        let code, rest = read_unicode rest in
                          results =@
                            rev
                              (combine
                                (ilist src1 src2)
                                (map (increment_final code) (ilist 0 (src2 - src1))));
                          numbers := rest
                  | '['::rest ->
                      (* It's several. *)
                      let rest = ref rest in
                        results =@
                          combine
                            (ilist src1 src2)
                            (map
                              (fun _ ->
                                 let num, rest' = read_unicode !rest in
                                   rest := rest';
                                   num)
                              (ilist 0 (src2 - src1)));
                      rest := (match !rest with [] -> [] | x -> tl x);
                      numbers := !rest
                  | _ -> raise (Pdf.PDFError "Bad BfRange")
          done;
          []
        with
          Not_found -> rev !results
        end

let rec parse_tounicode pdf tounicode =
  match tounicode with
  | Pdf.Stream {contents = (dict, Pdf.Got data)} ->
      Pdfcodec.decode_pdfstream pdf tounicode;
      begin match tounicode with
      | Pdf.Stream {contents = (dict, Pdf.Got data)} ->
          begin try
            flatten
              (map pairs_of_section
                (get_sections
                   (lose Pdf.is_whitespace (charlist_of_bytes data))))
          with
            e -> Printf.eprintf "/ToUnicode Parse Error : %s\n" (Printexc.to_string e); []
          end
      | _ -> assert false
      end
  | Pdf.Stream {contents = (_, Pdf.ToGet _)} ->
      Pdf.getstream tounicode;
      parse_tounicode pdf tounicode
  | e -> raise (Pdf.PDFError (Printf.sprintf "Bad /ToUnicode %s" (Pdfwrite.string_of_pdf e)))

(* Extracting of Text *)

(* A text extractor takes a character and returns a decoded PDF codepoint, a
glyphname, and a list of unicode codepoints. This may have to be extended
when we deal with composite fonts. *)
type text_extractor =
  {convert: int -> string * int list; (* Glyph name, List of unicode codepoints *)
   font: font}

(* Encode utf16be *)
let utf16be_of_codepoint u =
  if u < 0 || u > 0x10FFFF then
    raise (Invalid_argument "utf16be_of_codepoints")
  else
    (* Two bytes, bottom one first *)
    let bytes_of_double x = [x lsr 8; x land 255] in
      if u < 0x10000 then bytes_of_double u else
        let u' = u - 0x10000
        in let w1 = 0xD800
        in let w2 = 0xDC00 in
          let w1 = w1 lor (u' lsr 10)
          in let w2 = w2 lor (u' land 0b1111111111) in
            bytes_of_double w1 @ bytes_of_double w2

let utf16be_of_codepoints l =
  implode (['\254'; '\255'] @ map char_of_int (flatten (map utf16be_of_codepoint l)))

(* Return a list of codepoints from a UTF-16BE string. See RFC2871 *)
let fail2 () =
  raise (Invalid_argument "codepoints_of_utf16be")

let rec codepoints_of_utf16be_inner prev = function
  | [] -> rev prev
  | [w1] -> fail2 ()
  | [w1a; w1b] ->
      let w1 = (w1a lsl 8) lor w1b in
        if w1 < 0xD800 || w1 > 0xDFFF then
          codepoints_of_utf16be_inner (w1::prev) []
        else
          fail2 ()
  | [_; _; _] -> fail2 ()
  | w1a::w1b::w2a::w2b::more ->
      let w1 = (w1a lsl 8) lor w1b in
        if w1 < 0xD800 || w1 > 0xDFFF then
          codepoints_of_utf16be_inner (w1::prev) (w2a::w2b::more)
        else
          if w1 >= 0xD800 && w1 <= 0xDBFF then
            let w2 = (w2a lsl 8) lor w2b in
              if w2 >= 0xDC00 && w2 <= 0xDFFF then
                let ho = w1 land 0b1111111111
                in let lo = w2 lsr 6 in
                  codepoints_of_utf16be_inner
                    ((((ho lsl 10) lor lo) + 0x10000)::prev) more
              else
                fail2 ()
          else
            fail2 ()

let codepoints_of_utf16be str =
  codepoints_of_utf16be_inner [] (map int_of_char (explode str))

(* Build a hashtable for lookups based on an encoding *)
let rec add_encoding addvals = function
  | ImplicitInFontFile -> ()
  | StandardEncoding -> addvals Pdfglyphlist.name_to_standard
  | MacRomanEncoding -> addvals Pdfglyphlist.name_to_macroman
  | WinAnsiEncoding -> addvals Pdfglyphlist.name_to_win
  | MacExpertEncoding -> addvals Pdfglyphlist.name_to_macexpert
  | CustomEncoding (e, ds) ->
      add_encoding addvals e;
      addvals ds
  | FillUndefinedWithStandard e ->
      addvals Pdfglyphlist.name_to_standard;
      add_encoding addvals e

let table_of_encoding encoding =
  (*flprint "\nTABLE_OF_ENCODING: encoding is:\n";
  flprint (string_of_encoding encoding);
  flprint "\nEND OF ENCODING\n";*)
  let table = Hashtbl.create 203 in
    let addvals = iter (fun (k, v) -> Hashtbl.add table v k) in
      add_encoding addvals encoding;
      (*Printf.printf "table_of_encoding: built %i-sized table\n" (Hashtbl.length table);
      Hashtbl.iter (fun k v -> Printf.printf "%i -> %s, " k v) table;
      flprint "\n";*)
      table

let reverse_table_of_encoding encoding =
  let table = Hashtbl.create 203 in
    let addvals = iter (fun (k, v) -> Hashtbl.add table k v) in
      add_encoding addvals encoding;
      table

(* Method:
    1. If there's a /ToUnicode CMap, use it.
    2. If it is a standard 14 or simple font, use the encoding to get a glyph
    name, then look up the character in the glyph list.
    3. If it's a CID font, which we don't understand, just return.
The font here is the PDF font structure, not our font data type. If we need to
parse it, we do.

FIXME: InlineImages2.pdf - Acrobat can extract text, why can't we?
*)
let text_extractor_of_font pdf font =
  {convert =
     (let encoding =
        match read_font pdf font with
        | StandardFont (_, e) -> e
        | SimpleFont {encoding = e} -> e
        | _ -> ImplicitInFontFile (* No support *)
      in
        let table = table_of_encoding encoding in
          let tounicode_table, use_tounicode =
            match Pdf.lookup_direct pdf "/ToUnicode" font with
            | Some tounicode ->
                begin try
                  hashtable_of_dictionary <| parse_tounicode pdf tounicode, true
                with
                  e -> Printf.eprintf "bad tounicode (%s)\n" (Printexc.to_string e); (null_hash (), false)
                end
            | None -> null_hash (), false
          in
            if use_tounicode then
              function i ->
                try
                  begin try Hashtbl.find tounicode_table i with Not_found -> "/.notdef" end,
                  codepoints_of_utf16be (Hashtbl.find tounicode_table i)
                with
                  _ -> (* Failed *) ("/.notdef", [i])
            else
              function i ->
                try
                  let decoded = Hashtbl.find table i in
                    try
                      let codepoints = Hashtbl.find (Pdfglyphlist.glyph_hashes ()) decoded in
                        decoded, codepoints
                    with
                      _ -> (decoded, [i])
                with
                _ -> (* Failed *) ("/.notdef", [i]));
    font = read_font pdf font}

(* For now, the only composite font encoding scheme we understand is /Identity-H *)
let is_identity_h = function
  | CIDKeyedFont (_, _, Predefined "/Identity-H") -> true
  | _ -> false

let glyphnames_and_codepoints_of_text extractor text =
  if is_identity_h extractor.font then
    let chars = map int_of_char (explode text) in
      if odd (length chars) then raise (Pdf.PDFError "Bad Text") else
        map (fun (h, l) -> extractor.convert ((h lsl 8) lor l)) (pairs_of_list chars)
  else
    map (fun c -> extractor.convert (int_of_char c)) (explode text)

let codepoints_of_text extractor text =
  flatten (map snd (glyphnames_and_codepoints_of_text extractor text))

let glyphnames_of_text extractor text =
  map fst (glyphnames_and_codepoints_of_text extractor text)

(* Is a PDF string unicode (does it have a byte order marker at the beginning). *)
let is_unicode s =
  (String.length s >= 2) && s.[0] = '\254' && s.[1] = '\255'

(* Unicode codepoint from pdfdocencoding character number *)
let codepoint_of_pdfdocencoding_character i =
  if i < 0 || i > 255 then failwith "codepoint_of_pdfdocencoding_character out of range" else
    try
      match
        Hashtbl.find (Pdfglyphlist.glyph_hashes ())
          (Hashtbl.find Pdfglyphlist.reverse_name_to_pdf_hashes i)
      with
      | [codepoint] -> Some codepoint
      | _ -> Printf.eprintf "codepoint_of_pdfdocencoding: bad text string (char %i)\n" i; None
    with
      _ -> Printf.eprintf "codepoint_of_pdfdocencoding: bad text string (char %i)\n" i; None

(* Build a UTF-8 string from a list of unicode codepoints. *)
let get_utf8_chars c =
  if c <= 0x00_00_00_7F then [c]
  else if c <= 0x00_00_07_FF then
    [(c lsr 6) lor 0b11_00_00_00;
     c land 0b11_11_11 lor 0b10_00_00_00]
  else if c <= 0x00_00_FF_FF then
    [(c lsr 12) lor 0b11_10_00_00;
     (c lsr 6) land 0b00_11_11_11 lor 0b10_00_00_00;
     c land 0b00_11_11_11 lor 0b10_00_00_00]
  else if c <= 0x00_10_FF_FF then
    [(c lsr 18) lor 0b11_11_00_00;
     (c lsr 12) land 0b00_11_11_11 lor 0b10_00_00_00;
     (c lsr 6) land 0b00_11_11_11 lor 0b10_00_00_00;
     c land 0b00_11_11_11 lor 0b10_00_00_00]
  else raise (Pdf.PDFError "bad unicode codepoint")

let utf8_of_codepoints codepoints = 
  implode (map char_of_int (flatten (map get_utf8_chars codepoints)))

let codepoints_of_pdfdocstring s =
  if is_unicode s then
    codepoints_of_utf16be (String.sub s 2 (String.length s - 2))
  else
    option_map codepoint_of_pdfdocencoding_character (map int_of_char (explode s))

let utf8_of_pdfdocstring s =
  try utf8_of_codepoints (codepoints_of_pdfdocstring s) with
    e -> Printf.eprintf "utf8_of_pdfdocstring : %s\n" (Printexc.to_string e); ""

(* Build a PDFDocEncoding or UTF16BE string from a UTF8 encoded string *)
let rec codepoints_of_utf8 = function
  | [] -> []
  | c::cs
      when c lsr 7 = 0 ->
        c::codepoints_of_utf8 cs
  | c::c2::cs
      when c lsr 5 = 0b110 && c2 lsr 6 = 0b10 ->
            ((c land 0b000_11111) lsl 6)
        lor (c2 land 0b00_11_11_11)::codepoints_of_utf8 cs
  | c::c2::c3::cs
      when c lsr 4 = 0b1110 && c2 lsr 6 = 0b10 && c3 lsr 6 = 0b10 ->
            ((c land 0b0000_1111) lsl 12)
        lor ((c2 land 0b00_11_11_11) lsl 6)
        lor (c3 land 0b00_11_11_11)::codepoints_of_utf8 cs
  | c::c2::c3::c4::cs
      when c lsr 3 = 0b11110 && c2 lsr 6 = 0b10 && c3 lsr 6 = 0b10 && c4 lsr 6 = 0b10 ->
            ((c land 0b00000_111) lsl 18)
        lor ((c2 land 0b00_11_11_11) lsl 12)
        lor ((c3 land 0b00_11_11_11) lsl 6)
        lor (c4 land 0b00_11_11_11)::codepoints_of_utf8 cs
  | _ ->
      Printf.eprintf "Bad UTF8 in codepoints_of_utf8\n"; []

let codepoints_of_utf8 s = codepoints_of_utf8 (map int_of_char (explode s))

exception RequiresUnicode

(* Looks up each codepoint in the adobe glyphmap, and look up that name in
name_to_pdf above, raising the exception only if we find something that can't
be handled. *)
let rec pdfdocencoding_of_codepoints sofar = function
  | [] -> rev sofar
  | c::cs ->
       try
         pdfdocencoding_of_codepoints
           ((Hashtbl.find Pdfglyphlist.name_to_pdf_hashes (Hashtbl.find (Pdfglyphlist.reverse_glyph_hashes ()) [c]))::sofar)
           cs
       with
         Not_found -> raise RequiresUnicode

let pdfdocstring_of_codepoints codepoints =
  try implode (map char_of_int (pdfdocencoding_of_codepoints [] codepoints)) with
    RequiresUnicode -> utf16be_of_codepoints codepoints

let pdfdocstring_of_utf8 s =
  pdfdocstring_of_codepoints (codepoints_of_utf8 s)

(** Return the character code for a given unicode codepoint, if it exists in
this encoding. This is only really suitable for simple stuff like standard 14
fonts, or editing text in existing fonts. *)
let charcode_extractor_of_encoding encoding =
  let table = reverse_table_of_encoding encoding in
    function codepoint ->
      try
        let glyphname =
          Hashtbl.find (Pdfglyphlist.reverse_glyph_hashes ()) [codepoint]
        in
          Some (Hashtbl.find table glyphname)
      with
        Not_found -> None

