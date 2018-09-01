(* Convert a TrueType Font to a Type 3 font. *)
open Pdfutil
open Pdfio

let dbg = ref false (* text-based debug *)
let dbgg = ref false (* graphical debug *)

(* 32-bit signed fixed-point number (16.16) returned as two ints *)
let read_fixed b =
  let a = getval_31 b 16 in
    let b = getval_31 b 16 in
      a, b

(* 16-bit unsigned integer *)
let read_ushort b = getval_31 b 16

(* 32-bit unsigned integer *)
let read_ulong b = getval_32 b 32

(* Signed byte *)
let read_byte b = getval_31 b 8

(* Signed short *)
let read_short b = sign_extend 16 (getval_31 b 16)

(* fword *)
let read_fword = read_short

(* f2dot14 - 2 bit signed integer part, 14 bit unsigned fraction *)
let read_f2dot14 b =
  let v = read_ushort b in
    float_of_int (sign_extend 2 (v lsr 14)) +. (float_of_int (v land 0x3FFF) /. 16384.)

(* discard n bytes *)
let discard_bytes b n =
  for x = 1 to n do ignore (getval_31 b 8) done

(* longDateTime (stub) *)
let read_longdatetime b = discard_bytes b 8; 0

(* The empty glyph. *)
let notdef =
  {Pdfgraphics.elements = [];
   Pdfgraphics.fonts = [];
   Pdfgraphics.resources = Pdf.Dictionary []}

let string_of_tag t =
  let a = i32toi (Int32.shift_right t 24)
  and b = i32toi (Int32.logand 0x000000FFl (Int32.shift_right t 16))
  and c = i32toi (Int32.logand 0x000000FFl (Int32.shift_right t 8))
  and d = i32toi (Int32.logand 0x000000FFl t) in
    Printf.sprintf "%C%C%C%C" (char_of_int a) (char_of_int b) (char_of_int c) (char_of_int d)

let tag_cmap = 1668112752l
and tag_glyf = 1735162214l
and tag_maxp = 1835104368l
and tag_loca = 1819239265l
and tag_head = 1751474532l

let read_format_6_encoding_table b =
  let firstCode = read_ushort b in
  let entryCount = read_ushort b in
  let arr = Array.make (max 256 (firstCode + entryCount)) 0 in
    (* FIXME: This format can address glyphs > 255, but we don't support that
    elsewhere yet --- but we read the whole format table nonethless *)
    try
      for x = firstCode to firstCode + entryCount - 1 do
        arr.(x) <- read_ushort b
      done;
      arr
    with
    e -> failwith ("bad format 6 table: " ^ Printexc.to_string e ^ "\n")

let read_encoding_table fmt length version b =
  match fmt with
  | 0 -> Array.init 256 (function _ -> read_byte b)
  | 6 -> read_format_6_encoding_table b
  | n -> if !dbg then Printf.printf "read_encoding_table: format %i not known\n" n; [||]

let read_loca_table indexToLocFormat numGlyphs b =
  let fix_empties arr =
    for x = 1 to Array.length arr - 1 do
      if arr.(x) = arr.(x - 1) then arr.(x - 1) <- -1l
    done;
    if arr <> [||] then arr.(Array.length arr - 1) <- -1l
  in
    match indexToLocFormat with
    | 0 ->
        let arr = Array.init (numGlyphs + 1) (function _ -> i32ofi (read_ushort b * 2)) in
          fix_empties arr; arr
    | 1 ->
        let arr = Array.init (numGlyphs + 1) (function _ -> read_ulong b) in
          fix_empties arr; arr
    | _ -> raise (Pdf.PDFError "Unknown indexToLocFormat in read_loca_table")

(* NB. Global *)
let lastval = ref (false, false, false, false, false)
let repeats = ref 0

(* Read the flags for pts points *)
let rec read_flags flags b pts =
  if pts <= 0 then rev flags else
    let getsingle () =
      if !repeats > 0 then
        begin
          repeats := !repeats - 1;
          !lastval
        end
      else
        let flag = read_byte b in
          let oncurve, xshort, yshort, rp, xsame, ysame =
            flag land 0b00_00_00_01 = 0b1,
            flag land 0b00_00_00_10 = 0b10,
            flag land 0b00_00_01_00 = 0b1_00,
            flag land 0b00_00_10_00 = 0b10_00,
            flag land 0b00_01_00_00 = 0b1_00_00,
            flag land 0b00_10_00_00 = 0b10_00_00
        in
        if !dbg then
          Printf.printf
            "flag has on curve %b, xshort %b, yshort %b, repeat %b, xsame %b, ysame %b\n"
            oncurve xshort yshort rp xsame ysame;
        if rp then
          begin
            let rs = read_byte b in
            if !dbg then Printf.printf "There are %i repeats of this flag...\n" rs;
              repeats := rs;
              lastval := (oncurve, xshort, yshort, xsame, ysame);
              !lastval
          end
        else
          (oncurve, xshort, yshort, xsame, ysame)
    in
      read_flags (getsingle ()::flags) b (pts - 1)

let read_flags = read_flags []

(* NB. Global. *)
let prev = ref 0

(* NB. Set up prev before calling for each glyph *)
let read_coordinates b flags = 
  map
    (function (*short, same *)
      | true, false -> prev := !prev - read_byte b; !prev
      | true, true -> prev := !prev + read_byte b; !prev
      | false, false -> prev := !prev + read_short b; !prev
      | false, true -> !prev)
    flags

(* Returns a list of lines and quadratic beziers for the input contour. Returns floats. *)
type point = float * float

type part =
  | Line of point * point
  | QuadBezier of point * point * point

let rec break_coordinates_inner prev = function
  | (true, (x, y))::((true, (x', y')) as l)::r ->
       break_coordinates_inner (Line ((x, y), (x', y'))::prev) (l::r)
  | (true, (x, y))::(false, (x', y'))::((true, (x'', y'')) as l)::r ->
       break_coordinates_inner (QuadBezier ((x, y), (x', y'), (x'', y''))::prev) (l::r)
  | (true, (x, y))::(false, (x', y'))::((false, (x'', y'')) as l)::r ->
       let midpoint = ((x' +. x'') /. 2., (y' +. y'') /. 2.)  in
         break_coordinates_inner ((QuadBezier ((x, y), (x', y'), midpoint))::prev) ((true, midpoint)::l::r)
  | [_] -> rev prev
  | _ -> (*if !dbg then*) flprint "break_coordinated_inner: unrecognized sequence\n"; rev prev

(* FIXME: The truetype spec says that any combination of on and off points is
ok, but our conversion relies on the pattern (true + zero or more false) being
repeated. 

Strategy: We can special case when all are false (ignore it), and
otherwise cycle the list until a true is first. Then we have just reduced the
problem to not being able to do anything when all points are off the curve. *)
let rec all_points_off = function
    [] -> true
  | (false, _)::t -> all_points_off t
  | (true, _)::_ -> false 

let rec cycle_true_first = function
    [] -> assert false
  | (true, _) as h::t -> h::t
  | (false, _) as h::t -> cycle_true_first (t @ [h])

let break_coordinates = function
  | [] -> []
  | [_] -> []
  | l ->
     let tofloat (t, (x, y)) = (t, (float x, float y)) in
       if all_points_off l then [] else
         let closed =
           match cycle_true_first l with
           | [] -> assert false
           | h::t -> [h] @ t @ [h]
         in
           break_coordinates_inner [] (map tofloat closed)

let print_unbroken_coordinates (t, (x, y)) =
   Printf.printf "%b, %i, %i\n" t x y

let break_coordinates cs =
  if !dbg then begin flprint "This contour:\n"; iter print_unbroken_coordinates cs end;
  break_coordinates cs

let sofp (x, y) = Printf.sprintf "(%f, %f)" x y

let string_of_coordinate = function
  | Line (p, p') -> Printf.sprintf "Line %s --> %s\n" (sofp p) (sofp p')
  | QuadBezier (p, p', p'') -> Printf.sprintf "QuadBezier %s --> %s --> %s\n" (sofp p) (sofp p') (sofp p'')

let print_coordinates =
  iter (function x -> flprint (string_of_coordinate x))

(* Cubic bezier from quadratic bezier *)
let mkcubicbezier (p0x, p0y) (p1x, p1y) (p2x, p2y) =
  let cp1x = p0x +. ((2. /. 3.) *. (p1x -. p0x))
  and cp1y = p0y +. ((2. /. 3.) *. (p1y -. p0y))
  and cp2x = p2x +. ((2. /. 3.) *. (p1x -. p2x))
  and cp2y = p2y +. ((2. /. 3.) *. (p1y -. p2y)) in
    Pdfgraphics.Bezier ((p0x, p0y), (cp1x, cp1y), (cp2x, cp2y), (p2x, p2y))

let graphic_of_contours contours =
  {Pdfgraphics.elements = 
     [Pdfgraphics.Path
        ((Pdfgraphics.NonZero,
            map
              (function bits -> Pdfgraphics.Not_hole, Pdfgraphics.Closed, bits)
              (map_lol
                 (function
                  | Line (p, p') -> Pdfgraphics.Straight (p, p') 
                  | QuadBezier (p, p', p'') -> mkcubicbezier p p' p'')
                 contours)),
            {Pdfgraphics.path_transform = Pdftransform.i_matrix;
             Pdfgraphics.path_fill = Some (Pdfspace.DeviceGray, Pdfgraphics.Floats [1.]);
             Pdfgraphics.path_line = None;
             Pdfgraphics.path_linewidth = 0.;
             Pdfgraphics.path_joinstyle = 0;
             Pdfgraphics.path_capstyle = 0;
             Pdfgraphics.path_dash = ([0.], 0.);
             Pdfgraphics.path_mitrelimit = 0.;
             Pdfgraphics.path_transparency = {Pdfgraphics.fill_transparency = 1.; Pdfgraphics.line_transparency = 1.};
             Pdfgraphics.path_intent = "/RelativeColorimetric"})];
   Pdfgraphics.fonts = [];
   Pdfgraphics.resources = Pdf.Dictionary []}

let merge_components graphics =
  {Pdfgraphics.elements = flatten (map (fun g -> g.Pdfgraphics.elements) graphics);
   Pdfgraphics.fonts = [];
   Pdfgraphics.resources = Pdf.Dictionary []}

let arg_1_and_2_are_words = 0b1
and args_are_xy_values = 0b10
and round_xy_to_grid = 0b100
and we_have_a_scale = 0b1000
and more_components = 0b100000
and we_have_an_x_and_y_scale = 0b1000000
and we_have_a_two_by_two = 0b10000000
and we_have_instructions = 0b100000000
and use_my_metrics = 0b1000000000

(* FIXME: Missing things: WE_HAVE_A_2_BY_2 and WE_HAVE_AND_XY_SCALE missing.
Also, args_are_xy_values false ==> points not values - need examples of all
these three *)
let rec read_composite_glyph rg b =
  let graphics = ref [] in
  if !dbg then flprint "reading composite glyph...\n";
  let xmin = read_fword b in
  let ymin = read_fword b in
  let xmax = read_fword b in
  let ymax = read_fword b in
  if !dbg then Printf.printf "xmin, ymin, xmax, ymax = %i, %i, %i, %i\n" xmin ymin xmax ymax;
  let finished = ref false in
    while not !finished do
      let flags = read_ushort b in
      let glyphIndex = read_ushort b in
      if !dbg then Printf.printf "flags = %i, glyph_index = %i\n" flags glyphIndex;
        let argument1, argument2 =
          if (flags land arg_1_and_2_are_words) > 0 then
            let one = read_short b in
            let two = read_short b in
              one, two
          else
              let v = read_ushort b in
                v lsr 8, v land 255
        in
        if !dbg then Printf.printf "arg1 = %i, arg2 = %i\n" argument1 argument2;
          begin if (flags land we_have_a_scale) > 0 then
            let scale = read_f2dot14 b in
            if !dbg then Printf.printf "have read scale %f\n" scale;
              let currpos = Pdfio.bitstream_pos b in
                let graphic = rg glyphIndex in
                Pdfio.bitstream_seek b currpos;
                let transformed =
                  Pdfgraphics.transform_graphic
                    (Pdftransform.matrix_of_transform
                      [Pdftransform.Translate (float_of_int argument1, float_of_int argument2);
                       Pdftransform.Scale ((0., 0.), scale, scale)])
                    graphic
                in
                  graphics := transformed::!graphics
          else if (flags land we_have_an_x_and_y_scale) > 0 then
            let xscale = read_f2dot14 b in
            let yscale = read_f2dot14 b in
            if !dbg then Printf.printf "read scales %f, %f\n" xscale yscale;
          else if (flags land we_have_a_two_by_two) > 0 then
            let xscale = read_f2dot14 b in
            let scale01 = read_f2dot14 b in
            let scale10 = read_f2dot14 b in
            let yscale = read_f2dot14 b in
            if !dbg then Printf.printf "read scalesss %f, %f, %f, %f\n" xscale scale01 scale10 yscale
          else
            if !dbg then flprint "no scale\n";
            let currpos = Pdfio.bitstream_pos b in
              let graphic = rg glyphIndex in
                Pdfio.bitstream_seek b currpos;
                let moved_graphic =
                  Pdfgraphics.transform_graphic
                    (Pdftransform.mktranslate (float_of_int argument1) (float_of_int argument2))
                    graphic
                in
                  graphics := moved_graphic::!graphics
          end;
          if (flags land more_components) = 0 then set finished
    done;
    if !dbg then Printf.printf "We have read %i composite glyph components\n" (length !graphics);
    merge_components !graphics

and read_glyph rg b =
  let numberOfContours = read_short b in
    match numberOfContours with
    | 0 -> notdef
    | -1 -> read_composite_glyph rg b
    | x when x < -1 -> raise (Pdf.PDFError "Unknown numberOfContours")
    | _ ->
        let xMin = read_fword b in
        let yMin = read_fword b in
        let xMax = read_fword b in
        let yMax = read_fword b in
          if !dbg then
            Printf.printf "numberOfContours = %i, xmin = %i, ymin = %i, xmax = %i, ymax = %i\n" numberOfContours xMin yMin xMax yMax;
          let endPtsOfContours = Array.init numberOfContours (function _ -> read_ushort b) in
            if !dbg then
              (Printf.printf "endPtsOfContours:"; Array.iter (Printf.printf "%i ") endPtsOfContours; flprint "\n");
          let pointstoread =
            (endPtsOfContours.(0) + 1)::couple (fun x y -> y - x) (Array.to_list endPtsOfContours)
          in
          let instructionLength = read_ushort b in
          if !dbg then Printf.printf "instructionLength is %i\n" instructionLength;
          let _ = Array.init instructionLength (function _ -> read_byte b) in
            (* Read a list of lists of (on/off, x is short/long, y is short/long, xsame, ysame) triples *)
            if !dbg then Printf.printf "Reading flags...\n";
            let flags = 
              map
                (fun pts -> if !dbg then Printf.printf "to read: flags for %i pts on a contour:\n" pts; read_flags b pts)
                pointstoread
            in
              prev := 0;
              repeats := 0;
              if !dbg then Printf.printf "Reading x coordinates\n";
              let xcoords = map (fun flags -> read_coordinates b (map (fun (_, xs, _, xsame, _) -> (xs, xsame)) flags)) flags in
                prev := 0;
                repeats := 0;
                if !dbg then Printf.printf "Reading y coordinates\n";
                let ycoords = map (fun flags -> read_coordinates b (map (fun (_, _, ys, _, ysame) -> (ys, ysame)) flags)) flags in
                  let contours = List.map2 combine xcoords ycoords in
                    let pairs =
                      if length flags <> length contours
                        then raise (Pdf.PDFError "bad flags / contours in truetype font")
                        else combine (map_lol (fun (on, _, _, _, _) -> on) flags) contours
                    in
                      graphic_of_contours (map break_coordinates (map (function (a, b) -> combine a b) pairs))

let read_glyph rg b =
  try read_glyph rg b with
   e ->
     if !dbg then Printf.printf "Failed to read glyph with %s\n" (Printexc.to_string e);
     graphic_of_contours []

let parse_truetype_font data =
  (*if !dbgg then ignore (Graphics.open_graph " 1024x1024");*)
  let mk_b byte_offset = bitbytes_of_input (let i = input_of_bytes data in i.seek_in byte_offset; i) in
  let b = mk_b 0 in
  let major, minor = read_fixed b in
    if !dbg then Printf.printf "Truetype font version %i.%i\n" major minor;
    let numTables = read_ushort b in
    let searchRange = read_ushort b in
    let entrySelector = read_ushort b in
    let rangeShift = read_ushort b in
      if !dbg then Printf.printf "numTables = %i, searchRange = %i, entrySelector = %i, rangeShift = %i\n"
        numTables searchRange entrySelector rangeShift;
      let tables = ref [] in
        for x = 1 to numTables do
          let tag = read_ulong b in
          let checkSum = read_ulong b in
          let offset = read_ulong b in
          let ttlength = read_ulong b in
            if !dbg then Printf.printf "tag = %li = %s, checkSum = %li, offset = %li, ttlength = %li\n"
            tag (string_of_tag tag) checkSum offset ttlength;
            tables =| (tag, checkSum, offset, ttlength);
        done;
        let cmap =
          match keep (function (t, _, _, _) -> t = tag_cmap) !tables with
          | (_, _, o, l)::_ -> Some (o, l)
          | [] -> None
        in
        let glyphcodes = ref [] in
          begin match cmap with
          | None ->
              glyphcodes := ilist 0 255
          | Some (cmapoffset, cmaplength) -> 
              let b = mk_b (i32toi cmapoffset) in
                let cmap_version = read_ushort b in
                let num_encoding_tables = read_ushort b in
                  if !dbg then Printf.printf "cmap version %i. There are %i encoding tables\n"
                    cmap_version num_encoding_tables;
                  for x = 1 to num_encoding_tables do
                    let platform_id = read_ushort b in
                    let encoding_id = read_ushort b in
                    let subtable_offset = read_ulong b in
                      if !dbg then Printf.printf "subtable %i. platform_id = %i, encoding_id = %i, subtable_offset = %li\n"
                        x platform_id encoding_id subtable_offset;
                      let b = mk_b (i32toi cmapoffset + i32toi subtable_offset) in
                        let fmt = read_ushort b in
                        let lngth = read_ushort b in
                        let version = read_ushort b in
                          if !dbg then Printf.printf "subtable has format %i, length %i, version %i\n" fmt lngth version;
                          let got_glyphcodes = read_encoding_table fmt length version b in
                            if !glyphcodes = [] && got_glyphcodes <> [||] then
                              (if !dbg then flprint "USING THIS TABLE\n"; glyphcodes := Array.to_list got_glyphcodes);
                                 if !dbg && got_glyphcodes <> [||] then
                                   array_iter2 (Printf.printf "charcode %i --> glyphcode %i\n") (Array.of_list (ilist 0 255)) got_glyphcodes
                  done;
          end;
          let maxpoffset, maxplength =
            match keep (function (t, _, _, _) -> t = tag_maxp) !tables with
            | (_, _, o, l)::_ -> o, l
            | [] -> raise (Pdf.PDFError "No maxp table found in TrueType font")
          in
          let b = mk_b (i32toi maxpoffset) in
            let major, minor = read_fixed b in
            let numGlyphs = read_ushort b in
              if !dbg then Printf.printf "maxp table version %i.%i: This font has %i glyphs\n" major minor numGlyphs;
          let headoffset, headlength =
            match keep (function (t, _, _, _) -> t = tag_head) !tables with
            | (_, _, o, l)::_ -> o, l
            | [] -> raise (Pdf.PDFError "No maxp table found in TrueType font")
          in
            let b = mk_b (i32toi headoffset) in
              let _ (*major, minor*) = read_fixed b in
              let _ (*revmajor, revminor*) = read_fixed b in
              let _ (*checkSumAdjustment*) = read_ulong b in
              let _ (*magicNumber*) = read_ulong b in
              let _ (*flags*) = read_ushort b in
              let unitsPerEm = read_ushort b in
              let _ (*created*) = read_longdatetime b in
              let _ (*modified*) = read_longdatetime b in
              let _ (*xMin*) = read_fword b in
              let _ (*yMin*) = read_fword b in
              let _ (*xMax*) = read_fword b in
              let _ (*yMax*) = read_fword b in
              let _ (*macStyle*) = read_ushort b in
              let _ (*lowestRecPPEM*) = read_ushort b in
              let _ (*fontDirectionHint*) = read_short b in
              let indexToLocFormat = read_short b in
              let _ (*glyphDataFormat*) = read_short b in
                if !dbg then Printf.printf "head table: indexToLocFormat is %i\n" indexToLocFormat;
          let locaoffset, localength =
            match keep (function (t, _, _, _) -> t = tag_loca) !tables with
            | (_, _, o, l)::_ -> o, l
            | [] -> raise (Pdf.PDFError "No maxp table found in TrueType font")
          in
            let b = mk_b (i32toi locaoffset) in
              let offsets = read_loca_table indexToLocFormat numGlyphs b in
                if !dbg then
                  begin
                    Printf.printf "Have read loca table:\n";
                      array_iter2 (Printf.printf "Glyph %i is at offset %li\n") (Array.of_list (ilist 0 numGlyphs)) offsets;
                      flush stdout
                  end;
          let glyfoffset, glyflength =
            match keep (function (t, _, _, _) -> t = tag_glyf) !tables with
            | (_, _, o, l)::_ -> o, l
            | [] -> raise (Pdf.PDFError "No glyf table found in TrueType font")
          in
            let glyphcache = Hashtbl.create 300 in
            let glyphs =
              Array.to_list
                (Array.map
                  (function offset ->
                     if offset = -1l then notdef else
                       try Hashtbl.find glyphcache offset with
                         Not_found ->
                           let r = 
                             let b = mk_b (i32toi glyfoffset + i32toi offset) in
                               read_glyph
                                 (* Build the function reads a glyph from an offset. Gross. *)
                                 (fun glyphnum ->
                                         if !dbg then Printf.printf "READING GLYPH NUMBER %i\n" glyphnum;
                                      try
                                        let offset = offsets.(glyphnum) in
                                          if offset = -1l then notdef else
                                            try Hashtbl.find glyphcache offset with
                                              Not_found ->
                                                let b = mk_b (i32toi glyfoffset + i32toi offset) in
                                                  let r =
                                                    read_glyph (fun _ -> flprint "too much font recursion"; notdef (* FIXME *)) b
                                                  in
                                                    Hashtbl.add glyphcache offset r;
                                                    r
                                      with
                                        _ -> flprint "Failed component read\n"; notdef)
                                 b
                           in
                             Hashtbl.add glyphcache offset r;
                             r)
                  offsets)
            in
              if !dbg then Printf.printf "Read %i glyphs\n" (length glyphs);
              (map
                (function incode ->
                   (*Printf.printf "So, incode %i " incode;*)
                   let glyphcode = List.nth !glyphcodes incode in
                     (*Printf.printf "is glyphcode %i \n" glyphcode;*)
                     try List.nth glyphs glyphcode
                       with _ ->
                         if !dbg then
                           Printf.printf
                             "ERROR: no glyph for incode %i which is glyphcode %i - substituting notdef\n" incode glyphcode;
                         notdef)
                (ilist 0 255)),
              (let scale = 1.0 /. (float_of_int unitsPerEm) in
                 Pdftransform.matrix_of_op (Pdftransform.Scale ((0., 0.), scale, scale)))

(* Make a CharProc stream from a graphic. We need to filter out ops we don't
want. *)
let charprocbytes_of_graphic g matrix =
  Pdfops.stream_of_ops
    (Pdfops.Op_d1 (0., 0., 0., 0., 0., 0.)::
      lose
        (function
         | Pdfops.Op_q | Pdfops.Op_Q | Pdfops.Op_cm _ | Pdfops.Op_w _
         | Pdfops.Op_J _ | Pdfops.Op_j _ | Pdfops.Op_M _ | Pdfops.Op_ri _
         | Pdfops.Op_CS _ | Pdfops.Op_SCN _ | Pdfops.Op_cs _
         | Pdfops.Op_scn _ -> true
         | _ -> false)
        (Pdfgraphics.ops_of_simple_graphic (Pdfgraphics.transform_graphic matrix g)))

let to_type3 pdf font =
  match font with
  | Pdftext.SimpleFont
      ({Pdftext.fonttype = Pdftext.Truetype;
        Pdftext.encoding = original_encoding;
        Pdftext.fontdescriptor =
          Some ({Pdftext.fontfile = Some Pdftext.FontFile2 fontfileobj} as fontdescriptor)} as fontrec) ->
            let glyphs, fontmatrix =
              let str = Pdf.direct pdf (Pdf.Indirect fontfileobj) in
                Pdfcodec.decode_pdfstream pdf str;
                match str with
                | Pdf.Stream {contents = (_, Pdf.Got data)} ->
                    begin try parse_truetype_font data with
                      e -> Printf.printf "Error %s" (Printexc.to_string e); raise (Pdf.PDFError "TrueType failure")
                    end
                | _ -> raise (Pdf.PDFError "Truetype data not a stream")
            in
              let charprocs =
                let scalematrix =
                  Pdftransform.matrix_compose
                    fontmatrix
                    (Pdftransform.matrix_of_op (Pdftransform.Scale ((0., 0.), 1000., 1000.)))
                in
                  map2
                    (fun n g ->
                       "/" ^ string_of_int n, Pdf.Indirect (Pdf.addobj pdf (charprocbytes_of_graphic g scalematrix)))
                    (ilist 0 (length glyphs - 1))
                    glyphs
              and fontmatrix =
                Pdftransform.matrix_of_op (Pdftransform.Scale ((0., 0.), 0.001, 0.001))
              and fontbbox =
                (0., 0., 0., 0.)
              and encoding =
                Pdftext.CustomEncoding (Pdftext.ImplicitInFontFile, [])
              in
                Pdftext.SimpleFont
                  {fontrec with
                     Pdftext.fonttype = Pdftext.Type3
                       {Pdftext.fontbbox = fontbbox;
                        Pdftext.fontmatrix = fontmatrix;
                        Pdftext.charprocs = charprocs;
                        Pdftext.type3_resources = Pdf.Dictionary []};
                     Pdftext.encoding = encoding;
                     Pdftext.fontdescriptor = Some fontdescriptor}
  | _ ->
     raise (Pdf.PDFError "Pdftruetype.to_type3: This is not a TrueType font")

                      (*  let iround f = int_of_float (floor f) / 3 + 100 in
                      flprint "This glyph:\n";
                        iter
                          (fun (flags, coords) ->
                            flprint "This contour:\n";
                            iter2
                              (fun on (x, y) ->
                                 Printf.printf "on = %b, point = (%i, %i)\n" on x y;
                                 Graphics.set_color (if on then Graphics.blue else Graphics.red);
                              Graphics.draw_circle (iround (float x) + 200) ((iround (float y)) + 200) 10)
                              flags
                              coords;
                              let broken = break_coordinates (combine flags coords) in
                            print_coordinates broken;
                            if !dbgg then
                              begin
                                Graphics.set_color Graphics.black;
                                iter
                                   (function
                                   | Line ((x, y), (x', y')) ->
                                       Graphics.moveto (iround x + 200) (iround y + 200); 
                                       Graphics.lineto (iround x' + 200) (iround y' + 200)
                                   | QuadBezier ((x, y), (x', y'), (x'', y'')) ->
                                       let Pdfgraphics.Bezier ((ax, ay), (bx, by), (cx, cy), (dx, dy)) = mkcubicbezier (x, y) (x', y') (x'', y'') in
                                         Graphics.moveto (iround ax + 200) (iround ay + 200);
                                         Graphics.curveto
                                           (iround bx + 200, iround by + 200) (iround cx + 200, iround cy + 200) (iround dx + 200, iround dy + 200))
                                    broken
                              end;

                              
                              
                              )
                          pairs;
                                                          (*graphic_of_contours broken*)

                          if !dbgg then
                            begin
                              ignore (Graphics.wait_next_event [Graphics.Key_pressed]);
                              Graphics.clear_graph ();
                            end;*)
