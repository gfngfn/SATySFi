(* \chaptertitle{PDFImage}{PDF Images} *)
open Pdfutil
open Pdfio

type pixel_layout =
  | BPP1
  | BPP8
  | BPP24
  | BPP48

type t =
  | JPEG of bytes * float list option
  | JPEG2000 of bytes * float list option
  | JBIG2 of bytes * float list option
  | Raw of int * int * pixel_layout * bytes

let string_of_layout = function
  | BPP1 -> "BPP1"
  | BPP8 -> "BPP8"
  | BPP24 -> "BPP24"
  | BPP48 -> "BPP48"

let string_of_image = function
  | JPEG _ -> "JPEG"
  | JPEG2000 _ -> "JPEG2000"
  | JBIG2 _ -> "JBIG2"
  | Raw (w, h, layout, data) ->
      "RAW: " ^ string_of_int w ^ " " ^ string_of_int h
      ^ " " ^ string_of_layout layout ^ " bytes of data = "
      ^ string_of_int (bytes_size data)

let print_floats fs =
  for x = 1 to Array.length fs do
    print_float fs.(x - 1);
    print_string " "
  done;
  print_string "\n"

(* Decodes an image in-place, given an array of floats. Assumes that output of Decode fits into same number of bits as input of Decode. FIXME: When might this not be true? *)

(* Invert the bits in a bytes *)
let invert_bits s =
  for x = 0 to bytes_size s - 1 do
    bset s x (bget s x lxor 255)
  done

let decode fs bpc image =
  (*i Printf.printf "decode, %i floats, BPC %i\n" (Array.length fs) bpc; i*)
  match Array.length fs with
  | 0 -> ()
  | l when odd l -> raise (Pdf.PDFError "Bad /Decode")
  | l ->
      (* Check to see if it's the identity transform. If so, do nothing. *)
      let ident = ref true in
        for x = 0 to Array.length fs / 2 - 1 do
          if fs.(x * 2) <> 0. || fs.(x * 2 + 1) <> 1. then clear ident
        done;
        (*i Printf.printf "ident: %b\n" !ident; i*)
        if not !ident then
          let interpolate dmin dmax x =
            int_of_float (dmin +. (float x *. (dmax -. dmin) /. (2.0 ** float bpc -. 1.)))
          in
            let functions =
              Array.init (l / 2) (function i -> interpolate (fs.(i * 2)) (fs.(i * 2 + 1)))
            in
              match bpc with
              | 1 ->
                 (* For now, just recognise [1 0] *)
                 invert_bits image
              | 2 -> (*iflprint "DECODE: 2 bit\n"; i*)()
              | 4 -> (*iflprint "DECODE: 4 bit\n"; i*)()
              | 8 ->
                  (*i flprint "DECODE: 8 bit\n";
                  print_floats fs; i*)
                  for p = 0 to bytes_size image - 1 do
                    bset image p ((functions.(p mod (l / 2))) (bget image p))
                  done
              | 16 -> (*i flprint "16 bit"; i*) ()
              | _ -> raise (Pdf.PDFError "Bad /Decode")

let decode_defaults pdf resources entry image =
  match entry with
  | None ->
      begin match Pdf.lookup_direct pdf "/ColorSpace" image with
      | None -> None
      | Some cspace ->
          match Pdfspace.read_colourspace pdf resources cspace with 
          | Pdfspace.DeviceGray | Pdfspace.CalGray (_, _, _) | Pdfspace.Separation (_, _, _) ->
              Some (Pdf.Array [Pdf.Real 0.; Pdf.Real 1.])
          | Pdfspace.DeviceRGB | Pdfspace.CalRGB (_, _, _, _) ->
              Some (Pdf.Array [Pdf.Real 0.; Pdf.Real 1.; Pdf.Real 0.; Pdf.Real 1.;Pdf.Real 0.; Pdf.Real 1.])
          | Pdfspace.DeviceCMYK ->
              Some (Pdf.Array [Pdf.Real 0.; Pdf.Real 1.; Pdf.Real 0.; Pdf.Real 1.;Pdf.Real 0.; Pdf.Real 1.; Pdf.Real 0.; Pdf.Real 1.])
          | Pdfspace.Lab (_, _, range) ->
              Some (Pdf.Array ([Pdf.Real 0.; Pdf.Real 1.] @ map (function n -> Pdf.Real n) (Array.to_list range)))
          | Pdfspace.ICCBased {Pdfspace.icc_range = range} ->
              (*i flprint "Making default from ICCBased colour space\n"; i*)
              Some (Pdf.Array (map (function n -> Pdf.Real n) (Array.to_list range)))
          | Pdfspace.Indexed (_, _) ->
              (*i flprint "Making default from Indexed colour space\n"; i*)
              (*i let bpc =
                match Pdf.lookup_direct_orelse pdf "/BitsPerComponent" "/BPC" image with
                | Some (Pdf.Integer n) -> n
                | _ -> 0
              in i*) (* Commented out because we don't use it yet... *)
                (* For now, just make identity. FIXME: How should decode /
                 indexed work - do it in the actual extraction routine? *)
                Some (Pdf.Array [Pdf.Real 0.; Pdf.Real 1.])
                (*i let n = 2.0 ** float bpc -. 1.0 in
                  Some (Pdf.Array [Pdf.Real 0.; Pdf.Real n]) i*)
          | Pdfspace.Pattern -> None
          | Pdfspace.PatternWithBaseColourspace _ -> None (* FIXME: Check this *)
          | Pdfspace.DeviceN (arr, _, _, _) ->
              Some (Pdf.Array (flatten (many [Pdf.Real 0.; Pdf.Real 1.] (Array.length arr))))
      end
  | x ->
      match Pdf.lookup_direct pdf "/ColorSpace" image with
      | None -> x (* Because image masks don't have a colourspace *)
      | Some cspace ->
          match Pdfspace.read_colourspace pdf resources cspace with
          (* Again. A bodge. Need to sort out indexed decoding properly. *)
          | Pdfspace.Indexed (_, _) -> None
          | _ -> x

(* Decode until it is either plain or a type of decoding we can't deal with
natively. *) 
let rec decode_to_image pdf = function
  | Pdf.Stream {contents = d, s} as stream ->
      begin match Pdf.lookup_direct pdf "/Filter" d with
      | None
      | Some (Pdf.Array [])
      | Some (Pdf.Name ("/DCTDecode" | "/DCT" | "/JBIG2Decode" | "/JPXDecode"))
      | Some (Pdf.Array [Pdf.Name ("/DCTDecode" | "/DCT" | "/JBIG2Decode" | "/JPXDecode")]) -> ()
      | _ ->
          Pdfcodec.decode_pdfstream_onestage pdf stream;
          decode_to_image pdf stream 
      end
  | _ -> raise (Pdf.PDFError "decode_to_image: bad stream")

(* Basic CMYK to RGB conversion *)
let rgb_of_cmyk c m y k =
  let c = float c in let m = float m in let y = float y in let k = float k in
  let r = 255. -. fmin 255. ((c /.  255.) *. (255. -. k) +. k) 
  in let g = 255. -. fmin 255. ((m /.  255.) *. (255. -. k) +. k)
  in let b = 255. -. fmin 255. ((y /.  255.) *. (255. -. k) +. k) in
    toint r, toint g,  toint b

let read_cmyk_8bpp_as_rgb24 width height data =
  let data' = mkbytes (width * height * 3) in
    for p = 0 to width * height - 1 do
      let c = bget data (p * 4)
      in let m = bget data (p * 4 + 1)
      in let y = bget data (p * 4 + 2)
      in let k = bget data (p * 4 + 3) in
        let r, g, b = rgb_of_cmyk c m y k in
          bset data' (p * 3) r;
          bset data' (p * 3 + 1) g;
          bset data' (p * 3 + 2) b
    done;
    data'

let read_gray_8bpp_as_rgb24 width height data =
  let data' = mkbytes (width * height * 3) in
    for pout = 0 to width * height - 1 do
      bset data' (pout * 3) (bget data pout);
      bset data' (pout * 3 + 1) (bget data pout);
      bset data' (pout * 3 + 2) (bget data pout);
    done;
    data'

(* Input is 1bpp, rows padded to bytes. *)
let read_1bpp_as_rgb24 width height s =
  let s' = mkbytes (width * height * 3)
  in let s_bits = Pdfio.bitbytes_of_input (Pdfio.input_of_bytes s) in
    let pout = ref 0 in
      for row = 0 to height - 1 do
        let bits_to_do = ref width in
          while !bits_to_do > 0 do
            let bit = if Pdfio.getbit s_bits then 255 else 0 in
              bset s' !pout bit;
              bset s' (!pout + 1) bit;
              bset s' (!pout + 2) bit;
              decr bits_to_do;
              pout += 3
          done;
          Pdfio.align s_bits 
      done;
      s'

(* 4bpp, rows padded to bytes. *)
let read_4bpp_gray_as_rgb24 width height s =
  let s' = mkbytes (width * height * 3)
  in let s_bits = Pdfio.bitbytes_of_input (Pdfio.input_of_bytes s) in
    let pout = ref 0 in
      for row = 0 to height - 1 do
        let pix_to_do = ref width in
          while !pix_to_do > 0 do
            let a = if Pdfio.getbit s_bits then 1 else 0 in
            let b = if Pdfio.getbit s_bits then 1 else 0 in
            let c = if Pdfio.getbit s_bits then 1 else 0 in
            let d = if Pdfio.getbit s_bits then 1 else 0 in
              let col = (a * 8 + b * 4 + c * 2 + d) * (16 + 1) in
                bset s' !pout col;
                bset s' (!pout + 1) col;
                bset s' (!pout + 2) col;
                decr pix_to_do;
                pout += 3
          done;
          Pdfio.align s_bits 
      done;
      s'

let read_8bpp_indexed_as_rgb24 table width height s =
  let s' = mkbytes (width * height * 3) in
    for x = 0 to width * height - 1 do
      match Hashtbl.find table (bget s x) with
      | [r; g; b] ->
          bset s' (x * 3) r;
          bset s' (x * 3 + 1) g;
          bset s' (x * 3 + 2) b
      | _ -> raise (Pdf.PDFError "read_8bpp_indexed_as_rgb24")
    done;
    s'

(* Clamp a value to 0.0...1.0 *)
let clamp10 f = 
  if f > 1.0 then 1.0 else if f < 0.0 then 0.0 else f

(* LAB colour space support *)
let xyz_of_lab l' a' b' xw yw zw =
  let g x =
    if x >= 6. /. 29.
      then x *. x *. x
      else (108. /. 841.) *. (x -. 4. /. 29.)
  in
    let l = (l' +. 16.) /. 116. +. a' /. 500.
    in let m = (l' +. 16.) /. 116.
    in let n = (l' +. 16.) /. 116. -. b' /. 200. in
      clamp10 (xw *. g l),
      clamp10 (yw *. g m),
      clamp10 (zw *. g n)

(* For x, y, z on 0..1 calculate r, g, b on 0..1 *)
let rgb_of_xyz x y z =
  clamp10 (2.5623 *. x +. -.1.1661 *. y +. -.0.3962 *. z),
  clamp10 (-.1.0215 *. x +. 1.9778 *. y +. 0.0437 *. z),
  clamp10 (0.0752 *. x +. -.0.2562 *. y +. 1.1810 *. z)

(* Assume indexed /Range of -127...128. FIXME *)
let convert_lab_to_rgb width height (xw, yw, zw) q rs s =
  for p = 0 to width * height - 1 do
    let l = bget s (p * 3) - 127
    in let a = bget s (p * 3 + 1) - 127
    in let b = bget s (p * 3 + 2) - 127 in
      (*i Printf.printf "xw yw zw = %f, %f, %f\n" xw yw zw;
      Printf.printf "l a b = %i, %i, %i\n" l a b; i*)
      let l' = float l /. 255.
      in let a' = float a /. 255.
      in let b' = float b /. 255. in
        (*i Printf.printf "l' a' b' = %f, %f, %f\n" l' a' b'; i*)
        let x, y, z = xyz_of_lab l' a' b' xw yw zw in
        (*i Printf.printf "x y z = %f, %f, %f\n" x y z; i*)
          let r, g, b = rgb_of_xyz x y z in
        (*i Printf.printf "r g b = %f, %f, %f\n" r g b; i*)
            let r' = toint (r *. 255.)
            in let g' = toint (g *. 255.)
            in let b' = toint (b *. 255.) in
              (*i Printf.printf "r', g', b' = %i, %i, %i\n" r' g' b'; i*)
              bset s (p * 3) r';
              bset s (p * 3 + 1) g';
              bset s (p * 3 + 2) b'
  done

let read_8bpp_lab_indexed_as_rgb24 table width height p q rs data =
  let s = read_8bpp_indexed_as_rgb24 table width height data in
    convert_lab_to_rgb width height p q rs s;
    s

let read_8bpp_cmyk_indexed_as_rgb24 table width height s =
  let s' = mkbytes (width * height * 3) in
    for x = 0 to width * height - 1 do
      match Hashtbl.find table (bget s x) with
      | [c; m; y; k] ->
          let r, g, b = rgb_of_cmyk c m y k in
            bset s' (x * 3) r;
            bset s' (x * 3 + 1) g;
            bset s' (x * 3 + 2) b
      | _ -> raise (Pdf.PDFError "read_8bpp_indexed_as_rgb24")
    done;
    s'

let read_4bpp_indexed_as_rgb24 table width height s =
  let s' = mkbytes (width * height * 3) in
    let posin = ref 0
    in let posout = ref 0 in
      for row = 0 to height - 1 do
        for byte = 0 to (width + 1) / 2 - 1 do
          let p1 = bget s !posin lsr 4
          in let p2 = bget s !posin land 15 in
            begin match Hashtbl.find table p1 with
            | [r1; g1; b1] ->
                bset s' !posout r1; incr posout;
                bset s' !posout g1; incr posout;
                bset s' !posout b1; incr posout;
            | _ -> raise (Pdf.PDFError "read_4bpp_indexed_as_rgb24")
            end;
            begin
              if not (odd width && byte = (width + 1) / 2 - 1) then
              match Hashtbl.find table p2 with
              | [r2; g2; b2] ->
                   bset s' !posout r2; incr posout;
                   bset s' !posout g2; incr posout;
                   bset s' !posout b2; incr posout;
              | _ -> raise (Pdf.PDFError "read_4bpp_indexed_as_rgb24")
            end;
            incr posin
        done
      done;
      s'

let read_4bpp_cmyk_indexed_as_rgb24 table width height s =
  let s' = mkbytes (width * height * 3) in
    let posin = ref 0
    in let posout = ref 0 in
      for row = 0 to height - 1 do
        for byte = 0 to (width + 1) / 2 - 1 do
          let p1 = bget s !posin lsr 4
          in let p2 = bget s !posin land 15 in
            begin match Hashtbl.find table p1 with
            | [c; m; y; k] ->
                let r1, g1, b1 = rgb_of_cmyk c m y k in
                  bset s' !posout r1; incr posout;
                  bset s' !posout g1; incr posout;
                  bset s' !posout b1; incr posout;
            | _ -> raise (Pdf.PDFError "read_4bpp_cmyk_indexed_as_rgb24")
            end;
            begin
              if not (odd width && byte = (width + 1) / 2 - 1) then
              match Hashtbl.find table p2 with
              | [c; m; y; k] ->
                  let r2, g2, b2 = rgb_of_cmyk c m y k in
                    bset s' !posout r2; incr posout;
                    bset s' !posout g2; incr posout;
                    bset s' !posout b2; incr posout;
              | _ -> raise (Pdf.PDFError "read_4bpp_cmyk_indexed_as_rgb24")
            end;
            incr posin
        done
      done;
      s'

(* Separation, CMYK alternate, tint transform function. *)
let read_separation_cmyk_as_rgb24 f width height s = 
  let s' = mkbytes (width * height * 3) in
    for p = 0 to width * height - 1 do
      let v = bget s p in
        try
          match Pdffun.eval_function f [float v /. 255.] with
          | [c; y; m; k] ->
              let c = toint (c *. 255.)
              and m = toint (m *. 255.)
              and y = toint (y *. 255.)
              and k = toint (k *. 255.) in
                let r, g, b = rgb_of_cmyk c m y k in
                  bset s' (p * 3) r;
                  bset s' (p * 3 + 1) g;
                  bset s' (p * 3 + 2) b;
          | _ ->
            raise (Pdf.PDFError "Bad tint transform function")
        with
          Pdffun.BadFunctionEvaluation s ->
            raise (Pdf.PDFError ("Bad tint transform function " ^ s))
    done;
    s'

let rec read_raw_image size colspace bpc pdf resources width height dict data =
  match size, colspace, bpc with
  | size, (Pdfspace.DeviceRGB | Pdfspace.CalRGB _), Some (Pdf.Integer 8)
      when size >= width * height * 3 ->
        Raw (width, height, BPP24, data)
  | size, Pdfspace.DeviceCMYK, Some (Pdf.Integer 8)
      when size >= width * height * 4 ->
        Raw (width, height, BPP24, read_cmyk_8bpp_as_rgb24 width height data)
  | size, (Pdfspace.DeviceGray | Pdfspace.CalGray _), Some (Pdf.Integer 8)
      when size >= width * height ->
        Raw (width, height, BPP24, read_gray_8bpp_as_rgb24 width height data)
  | size, _, Some (Pdf.Integer 1)
      when size >= width * height / 8 ->
        Raw (width, height, BPP24, read_1bpp_as_rgb24 width height data)
  | size, Pdfspace.DeviceGray, Some (Pdf.Integer 4)
      when size >= width * height / 2 ->
        Raw (width, height, BPP24, read_4bpp_gray_as_rgb24 width height data)
  | size, Pdfspace.Indexed ((Pdfspace.DeviceRGB | Pdfspace.CalRGB _), table), Some (Pdf.Integer 8)
  | size,
    Pdfspace.Indexed
      ((Pdfspace.DeviceN (_, (Pdfspace.DeviceRGB | Pdfspace.CalRGB _ |
      Pdfspace.ICCBased {Pdfspace.icc_alternate = (Pdfspace.DeviceRGB |
      Pdfspace.CalRGB _)}), _, _) |
       Pdfspace.ICCBased {Pdfspace.icc_alternate = (Pdfspace.DeviceRGB |
       Pdfspace.CalRGB _)}) , table),
    Some (Pdf.Integer 8)
      when size >= width * height ->
        Raw (width, height, BPP24, read_8bpp_indexed_as_rgb24 table width height data)
  | size, Pdfspace.Indexed (Pdfspace.DeviceCMYK, table), Some (Pdf.Integer 8)
      when size >= width * height ->
        Raw (width, height, BPP24, read_8bpp_cmyk_indexed_as_rgb24 table width height data)
  | size, Pdfspace.Indexed ((Pdfspace.DeviceRGB | Pdfspace.CalRGB _), table), Some (Pdf.Integer 4)
  | size, Pdfspace.Indexed (Pdfspace.ICCBased {Pdfspace.icc_alternate = (Pdfspace.DeviceRGB | Pdfspace.CalRGB _)}, table), Some (Pdf.Integer 4)
      when size >= width * height / 2 ->
        Raw (width, height, BPP24, read_4bpp_indexed_as_rgb24 table width height data)
  | size, Pdfspace.Indexed ((Pdfspace.DeviceCMYK), table), Some (Pdf.Integer 4)
  | size, Pdfspace.Indexed (Pdfspace.ICCBased {Pdfspace.icc_alternate = (Pdfspace.DeviceCMYK)}, table), Some (Pdf.Integer 4)
      when size >= width * height / 2 ->
        Raw (width, height, BPP24, read_4bpp_cmyk_indexed_as_rgb24 table width height data)
  | size, Pdfspace.Indexed (Pdfspace.Lab (p, q, rs), table), Some (Pdf.Integer 8)
      when size >= width * height / 2 ->
        Raw (width, height, BPP24, read_8bpp_lab_indexed_as_rgb24 table width height p q rs data)
  | size, Pdfspace.Separation (_, Pdfspace.DeviceCMYK, fn), Some (Pdf.Integer 8)
      when size >= width * height ->
          Raw (width, height, BPP24, read_separation_cmyk_as_rgb24 fn width height data)
  | size, Pdfspace.ICCBased {Pdfspace.icc_alternate = cs}, _ ->
      read_raw_image size cs bpc pdf resources width height dict data
  | size, cs, bpc ->
     (*i Printf.printf "NO IMAGE:\n size:%i\n cspace\n%s\n bpc\n%s\n width %i\n height %i\n" size
     (Pdfspace.string_of_colourspace cs)
     (match bpc with None -> "NONE" | Some bpc -> Pdfwrite.string_of_pdf bpc)
     width
     height;
     flush stdout; i*)
     raise (Pdf.PDFError "No image\n")

let get_raw_image pdf resources width height dict data =
  try
  let size =
    bytes_size data
  in let colspace =
    (* If an image mask, it's /DeviceGray, effectively *)
    match Pdf.lookup_direct_orelse pdf "/ImageMask" "/IM" dict with
    | Some (Pdf.Boolean true) -> Pdfspace.DeviceGray
    | _ ->
      let colspace =
        Pdf.lookup_direct_orelse pdf "/ColorSpace" "/CS" dict
      in
        let space =
          match Pdf.lookup_direct pdf "/ColorSpace" resources, colspace with
          | Some (Pdf.Dictionary _ as d), Some (Pdf.Name c) ->
              begin match Pdf.lookup_direct pdf c d with
              | Some colspace -> colspace
              | _ -> (Pdf.Name c)
              end
          | _ ->
              match colspace with
              | Some c -> c
              | _ -> raise (Pdf.PDFError "PDf image: no colourspace")
        in
          Pdfspace.read_colourspace pdf resources space
  in let bpc =
    match Pdf.lookup_direct_orelse pdf "/BitsPerComponent" "/BPC" dict with
    | Some bpc -> Some bpc
    | None ->
        match Pdf.lookup_direct pdf "/ImageMask" dict with
        | Some (Pdf.Boolean true) -> Some (Pdf.Integer 1)
        | _ -> None
  in
    (*i flprint ("IMAGE SPACE:\n" ^ Pdfspace.string_of_colourspace colspace ^
     * "\n"); i*)
    read_raw_image size colspace bpc pdf resources width height dict data
  with
    e ->
      (*i Printf.eprintf (Pdfwrite.string_of_pdf (Pdf.direct pdf dict)); i*)
      raise e 

(* Print some debug information about an image. *)
let print_image pdf resources img =
  (*i Printf.printf "-----------------------------------------------------\n";
  Printf.printf "Image Dictionary:\n%s\n" (Pdfwrite.string_of_pdf img); i*)
  let w = match Pdf.lookup_direct pdf "/Width" img with Some (Pdf.Integer n) -> n | _ ->  0
  in let h = match Pdf.lookup_direct pdf "/Height" img with Some (Pdf.Integer n)-> n | _ -> 0 in
    Printf.printf "Width is %i, height %i\n" w h;
  begin match Pdf.lookup_direct pdf "/ColorSpace" img with
  | Some cs -> Printf.printf "Colourspace is...%s\n" (Pdfspace.string_of_colourspace (Pdfspace.read_colourspace pdf resources cs))
  | None -> Printf.printf "No Colourspace\n"
  end;
  begin match Pdf.lookup_direct pdf "/BitsPerComponent" img with
  | Some (Pdf.Integer n) -> Printf.printf "%i Bits Per Component\n" n
  | _ -> Printf.printf "No /BitsPerComponent\n"
  end;
  begin match Pdf.lookup_direct pdf "/Decode" img with
  | Some decode -> Printf.printf "Decode Array: %s\n" (Pdfwrite.string_of_pdf decode)
  | None -> Printf.printf "No /Decode Array\n"
  end

let get_image_24bpp pdf resources stream =
  (*i flprint "\n";
  print_image pdf resources (Pdf.direct pdf stream);
  flprint "\n"; i*)
  let stream = Pdf.direct pdf stream in
  let streamdict, data =
    Pdf.getstream stream;
    match stream with
    | Pdf.Stream {contents = (s, Pdf.Got d)} ->
        s, d
    | _ -> assert false (*r [Pdf.getstream] would have failed *)
  in
    let width = 
      match (Pdf.lookup_direct_orelse pdf "/Width" "/W" streamdict) with
      | Some (Pdf.Integer x) -> x
      | _ -> raise (Pdf.PDFError "Malformed /Image width")
    in let height =
      match (Pdf.lookup_direct_orelse pdf "/Height" "/H" streamdict) with
      | Some (Pdf.Integer x) -> x
      | _ -> raise (Pdf.PDFError "Malformed /Image height")
    in let bpc =
      match Pdf.lookup_direct_orelse pdf "/BitsPerComponent" "/BPC" streamdict with
      | Some (Pdf.Integer n) -> n
      | _ -> 0
    in
      decode_to_image pdf stream;
      match stream with
      | Pdf.Stream {contents = (Pdf.Dictionary d) as dict, Pdf.Got s} ->
          let get_decode () =
            let decode_entry = Pdf.lookup_direct_orelse pdf "/Decode" "/D" dict in
              let decode_entry = decode_defaults pdf resources decode_entry dict in
                  match decode_entry with
                  | Some (Pdf.Array nums) ->
                      Some (map (function (Pdf.Real n) -> n | _ -> 0.) nums)
                  | _ -> None
          in
            begin match Pdf.lookup_direct_orelse pdf "/Filter" "/F" dict with
            | None | Some (Pdf.Array []) ->
                let raw = get_raw_image pdf resources width height dict s
                in let decode_entry = Pdf.lookup_direct_orelse pdf "/Decode" "/D" dict in
                  (* Printf.printf "Decode entry before decode_defaults %s\n"
                  ((function None -> "None" | Some x -> (Pdfwrite.string_of_pdf
                  x)) decode_entry); i*)
                  let decode_entry = decode_defaults pdf resources decode_entry dict in
                  (*i Printf.printf "Decode entry after decode_defaults %s\n"
                  ((function None -> "None" | Some x -> (Pdfwrite.string_of_pdf
                  x)) decode_entry); i*)
                    let floats =
                      match decode_entry with
                      | Some (Pdf.Array elts) -> Array.of_list (map Pdf.getnum elts)
                      | None -> [||]
                      | _ -> raise (Pdf.PDFError "Bad /Decode")
                    in
                      begin match raw with
                      | Raw (_, _, _, data) -> if floats <> [||] then decode floats bpc data;
                      | _ -> ()
                      end;
                      raw
            | Some (Pdf.Name ("/DCTDecode" | "/DCT"))
            | Some (Pdf.Array [Pdf.Name ("/DCTDecode" | "/DCT")]) -> JPEG (s, get_decode ())
            | Some (Pdf.Name "/JBIG2Decode")
            | Some (Pdf.Array [Pdf.Name "/JBIG2Decode"]) -> JBIG2 (s, get_decode ())
            | Some (Pdf.Name "/JPXDecode")
            | Some (Pdf.Array [Pdf.Name "/JPXDecode"]) -> JPEG2000 (s, get_decode ())
            | _ -> raise (Pdf.PDFError "decode_to_image")
            end
      | _ -> assert false

(* For now, this just knows how to do 1bpp specially, since this causes enormous
memory headaches in pdf2xar *)
let get_image pdf resources stream =
  let stream = Pdf.direct pdf stream in
    let streamdict, data =
      Pdf.getstream stream;
      match stream with
      | Pdf.Stream {contents = (s, Pdf.Got d)} -> s, d
      | _ -> assert false (*r [Pdf.getstream] would have failed *)
    in
    let components =
      let colourspace =
        match Pdf.lookup_direct_orelse pdf "/ColorSpace" "/CS" stream with
        | None -> Pdf.Name "/DeviceGray" (* This is for image masks, in which the colourspace is implicitly 1 bpp *)
        | Some x -> x
        in
          Pdfops.components pdf resources colourspace 
  and bpc =
      match Pdf.lookup_direct_orelse pdf "/BitsPerComponent" "/BPC" streamdict
      with
      | Some (Pdf.Integer i) -> i
      | _ -> raise (Pdf.PDFError "bad /BitsPerComponent")
  and colorspace =
    (* If an image mask, it's /DeviceGray, effectively *)
    match Pdf.lookup_direct_orelse pdf "/ImageMask" "/IM" stream with
    | Some (Pdf.Boolean true) -> Pdfspace.DeviceGray
    | _ ->
      let colspace =
        Pdf.lookup_direct_orelse pdf "/ColorSpace" "/CS" stream
      in
        let space =
          match Pdf.lookup_direct pdf "/ColorSpace" resources, colspace with
          | Some (Pdf.Dictionary _ as d), Some (Pdf.Name c) ->
              begin match Pdf.lookup_direct pdf c d with
              | Some colspace -> colspace
              | _ -> (Pdf.Name c)
              end
          | _ ->
              match colspace with
              | Some c -> c
              | _ -> raise (Pdf.PDFError "PDf image: no colourspace")
        in
          Pdfspace.read_colourspace pdf resources space
  in
      (*i Printf.printf "get_image: %i components, %i bits per component\n" components bpc;
      Printf.printf "colorspace is %s\n" (Pdfspace.string_of_colourspace
      colorspace); i*)
      match bpc, components, colorspace with
      | (8 | 1), 1, (Pdfspace.DeviceGray | Pdfspace.DeviceRGB | Pdfspace.CalGray _ | Pdfspace.CalRGB _) ->
        (* 1bpp black and white and 8bpp greyscale special case *)
        begin
          let width = 
            match (Pdf.lookup_direct_orelse pdf "/Width" "/W" streamdict) with
            | Some (Pdf.Integer x) -> x
            | _ -> raise (Pdf.PDFError "Malformed /Image width")
          and height =
            match (Pdf.lookup_direct_orelse pdf "/Height" "/H" streamdict) with
            | Some (Pdf.Integer x) -> x
            | _ -> raise (Pdf.PDFError "Malformed /Image height")
          in
            decode_to_image pdf stream;
            begin match stream with
            | Pdf.Stream {contents = (Pdf.Dictionary d) as dict, Pdf.Got s} ->
          let get_decode () =
            let decode_entry = Pdf.lookup_direct_orelse pdf "/Decode" "/D" dict in
              let decode_entry = decode_defaults pdf resources decode_entry dict in
                match decode_entry with
                | Some (Pdf.Array nums) ->
                    Some (map (function Pdf.Real n -> n | Pdf.Integer i -> float i | _ -> 0.) nums)
                | _ -> None
          in
            begin match Pdf.lookup_direct_orelse pdf "/Filter" "/F" dict with
            | None | Some (Pdf.Array []) ->
                (* Now, fetch the raw data from the stream and, if necessary.
                apply the decode... *)
                let s =
                  if get_decode () = Some [1.; 0.] then
                    let s = copybytes s in
                      bytes_selfmap (if bpc = 1 then lnot else (function x -> 255 - x)) s;
                      s
                  else
                    s
                in       
                  Raw (width, height, (if bpc = 1 then BPP1 else BPP8), s)
            | Some (Pdf.Name ("/DCTDecode" | "/DCT"))
            | Some (Pdf.Array [Pdf.Name ("/DCTDecode" | "/DCT")]) -> JPEG (s, get_decode ())
            | Some (Pdf.Name "/JBIG2Decode")
            | Some (Pdf.Array [Pdf.Name "/JBIG2Decode"]) -> JBIG2 (s, get_decode ())
            | Some (Pdf.Name "/JPXDecode")
            | Some (Pdf.Array [Pdf.Name "/JPXDecode"]) -> JPEG2000 (s, get_decode ())
            | _ -> raise (Pdf.PDFError "decode_to_image")
            end
            | _ -> raise (Pdf.PDFError "get_image")
            end
     end
  | _ -> get_image_24bpp pdf resources stream

let get_image_raw_24bpp (pdf : Pdf.t) (resources : Pdf.pdfobject) (stream : Pdf.pdfobject) =
  (raise (Pdf.PDFError "get_image not implemented") : t)

let get_image_raw (pdf : Pdf.t) (resources : Pdf.pdfobject) (stream : Pdf.pdfobject) =
  (raise (Pdf.PDFError "get_image not implemented") : t)

(** Return a function which, when given an x and y coordinate, returns the pixel
byte values prior to any decoding, i.e in the raw input image data before /Decode,
/Index lookups and so on. Returns array of components as bytes.
This function is needed because color key masking is defined in terms of the
pixel values of undecoded images. *)
let get_image_unprocessed_pixel pdf resources stream =
  (* 1. Need to getstream / uncompress the image as normal (including any
  predictor), if not already done. *)
  Pdf.getstream stream;
  decode_to_image pdf stream;
  (* 1A. If it's JPEG, we can't oblige - Need a JPEG decoder to fix this... *)
  match stream with
  | Pdf.Stream {contents = d, Pdf.Got data} ->
      begin match Pdf.lookup_direct pdf "/Filter" d with
      | Some (Pdf.Name ("/DCTDecode" | "/DCT" | "/JBIG2Decode" | "/JPXDecode"))
      | Some (Pdf.Array [Pdf.Name ("/DCTDecode" | "/DCT" | "/JBIG2Decode" | "/JPXDecode")]) -> None
      | _ ->
        (* 2. Now, work out how many bytes per pixel - check this *)
        let bpc =
          match Pdf.lookup_direct_orelse pdf "/BitsPerComponent" "/BPC" stream with
          | Some (Pdf.Integer n) -> n
          | _ -> 0
        in let nocomponents =
          let colourspace =
            match Pdf.lookup_direct pdf "/ColorSpace" stream with
            | None -> Pdf.Dictionary []
            | Some x -> x
          in
            Pdfops.components pdf resources colourspace 
        in
          if bpc <> 8 then None else (* FIXME: Things other than bytes *)
          (* 3. Return the function, and making sure enough data / error handling done properly. *)
          Some
            (fun s ->
               try
                 (* Get nocomponents from correct position in data and put in an array... *)
                 let pos =
                   s * nocomponents
                 in
                   let a = Array.make nocomponents 0 in
                     for x = 0 to nocomponents - 1 do
                       a.(x) <- bget data (pos + x)
                     done;
                     a 
               with
                 _ -> raise (Pdf.PDFError "bad serial in get_image_unprocessed_pixel"))
      end 
  | _ -> None 


