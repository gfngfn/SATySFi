(* PDF Colour space parsing *)
open Pdfutil
open Pdfio

type point = float * float * float

type iccbased =
 {icc_n : int;
  icc_alternate : t;
  icc_range : float array;
  icc_metadata : Pdf.pdfobject option;
  icc_stream : Pdf.pdfobject}

(* Add CalCMYK - read as deviceCMYK *)
and t =
  | DeviceGray
  | DeviceRGB
  | DeviceCMYK
  | CalGray of point * point * float (* White, Black, Gamma *)
  | CalRGB of point * point * float array * float array (* White, Black, Gamma, Matrix *)
  | Lab of point * point * float array (* White, Black, Range *)
  | ICCBased of iccbased
  | Indexed of t * (int, int list) Hashtbl.t (* Base colourspace, values *)
  | Pattern
  | PatternWithBaseColourspace of t
  | Separation of string * t * Pdffun.t
  | DeviceN of string array * t * Pdffun.t * Pdf.pdfobject

let rec string_of_colourspace = function
  | DeviceGray -> "/DeviceGray"
  | DeviceRGB -> "/DeviceRGB"
  | DeviceCMYK -> "/DeviceCMYK"
  | CalGray (_, _, _) -> "/CalGray"
  | CalRGB (_, _, _, _) -> "/CalRGB"
  | Lab (_, _, _) -> "/Lab"
  | ICCBased {icc_alternate = a} ->
      "ICC Based - alternate is " ^ string_of_colourspace a
  | Indexed (a, _) ->
      "Indexed - base is " ^ string_of_colourspace a
  | Pattern -> "/Pattern"
  | PatternWithBaseColourspace a ->
      "PatternWithBaseColourspace - base is " ^ string_of_colourspace a
  | Separation (_, a, _) ->
      "Separation - base is " ^ string_of_colourspace a
  | DeviceN (_, a, _, _) ->
      "DeviceN - base is " ^ string_of_colourspace a

let name_of_colourspace = function
  | Separation (x, _, _) -> Some x
  | _ -> None

(* Read a tristimulus point. *)
let read_point pdf d n =
  match Pdf.lookup_direct pdf n d with
  | Some (Pdf.Array [a; b; c]) ->
      Pdf.getnum a, Pdf.getnum b, Pdf.getnum c
  | _ ->
      0., 0., 0.

let rec get_basic_table_colourspace c =
  match c with
  | Indexed (alt, _)
  (* FIXME Not actually checked the following two are correct *)
  | DeviceN (_, alt, _, _)
  | Separation (_, alt, _)
  | ICCBased {icc_alternate = alt} -> get_basic_table_colourspace alt
  | x -> x

(* Read a colour space. Raises [Not_found] on error. *)
let rec read_colourspace_inner pdf resources = function
  | Pdf.Indirect i ->
      read_colourspace_inner pdf resources (Pdf.direct pdf (Pdf.Indirect i))
  | Pdf.Name ("/DeviceGray" | "/G") -> DeviceGray
  | Pdf.Name ("/DeviceRGB" | "/RGB") -> DeviceRGB
  | Pdf.Name ("/DeviceCMYK" | "/CMYK") -> DeviceCMYK
  | Pdf.Name "/Pattern" -> Pattern
  | Pdf.Array [Pdf.Name "/Pattern"; base_colspace] ->
      PatternWithBaseColourspace (read_colourspace_inner pdf resources base_colspace)
  | Pdf.Array [onething] -> read_colourspace_inner pdf resources onething (* [PDFTests/illus_effects.pdf] [[/Pattern]] *)
  | Pdf.Name space ->
      begin match Pdf.lookup_direct pdf "/ColorSpace" resources with
      | Some csdict ->
          begin match Pdf.lookup_direct pdf space csdict with
          | Some space' ->
              read_colourspace_inner pdf resources space'
          | None -> raise Not_found
          end
      | None -> raise Not_found
      end
  | Pdf.Array [Pdf.Name "/CalGray"; dict] ->
      let whitepoint = read_point pdf dict "/WhitePoint"
      in let blackpoint = read_point pdf dict "/BlackPoint"
      in let gamma =
        match Pdf.lookup_direct pdf "/Gamma" dict with
        | Some n -> Pdf.getnum n
        | None -> 1.
      in
        CalGray (whitepoint, blackpoint, gamma)
  | Pdf.Array [Pdf.Name "/CalRGB"; dict] ->
      let whitepoint = read_point pdf dict "/WhitePoint"
      in let blackpoint = read_point pdf dict "/BlackPoint"
      in let gamma =
        match Pdf.lookup_direct pdf "/Gamma" dict with
        | Some (Pdf.Array [a; b; c]) ->
            [|Pdf.getnum a; Pdf.getnum b; Pdf.getnum c|]
        | _ ->
            [|1.; 1.; 1.|]
      in let matrix =
        match Pdf.lookup_direct pdf "/Matrix" dict with
        | Some (Pdf.Array [a; b; c; d; e; f; g; h; i]) ->
            [|Pdf.getnum a; Pdf.getnum b; Pdf.getnum c;
              Pdf.getnum d; Pdf.getnum e; Pdf.getnum f;
              Pdf.getnum g; Pdf.getnum h; Pdf.getnum i|]
        | _ ->
            [|1.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 1.|]
      in
        CalRGB (whitepoint, blackpoint, gamma, matrix)
  | Pdf.Array [Pdf.Name "/Lab"; dict] ->
      let whitepoint = read_point pdf dict "/WhitePoint"
      in let blackpoint = read_point pdf dict "/BlackPoint"
      in let range =
        match Pdf.lookup_direct pdf "/Range" dict with
        | Some (Pdf.Array [a; b; c; d]) ->
            [|Pdf.getnum a; Pdf.getnum b; Pdf.getnum c; Pdf.getnum d|]
        | _ ->
            [|-.100.; 100.; -.100.; 100.|]
      in
        Lab (whitepoint, blackpoint, range)
  | Pdf.Array [Pdf.Name "/ICCBased"; stream] ->
      begin match Pdf.direct pdf stream with
      | Pdf.Stream {contents = (dict, _)} ->
          let n =
            match Pdf.lookup_direct pdf "/N" dict with
            | Some (Pdf.Integer n) ->
                if n = 1 || n = 3 || n = 4 then n else raise Not_found
            | _ -> raise Not_found
          in
            let alternate =
              match Pdf.lookup_direct pdf "/Alternate" dict with
              | Some cs -> read_colourspace_inner pdf resources cs
              | _ ->
                 match n with
                 | 1 -> DeviceGray
                 | 3 -> DeviceRGB
                 | 4 -> DeviceCMYK
                 | _ -> assert false
            in let range =
              match Pdf.lookup_direct pdf "/Range" dict with
              | Some (Pdf.Array elts) when length elts = 2 * n ->
                 Array.of_list (map Pdf.getnum elts)
              | _ ->
                 Array.of_list (flatten (many [0.; 1.] n))
            in let metadata =
              Pdf.lookup_direct pdf "/Metadata" dict
            in
              ICCBased
                {icc_n = n;
                 icc_alternate = alternate;
                 icc_range = range;
                 icc_metadata = metadata;
                 icc_stream = stream}
      | _ -> raise Not_found
      end
  | Pdf.Array [Pdf.Name ("/Indexed" | "/I"); bse; hival; lookup_data] ->
      let hival =
        match hival with
        | Pdf.Integer h -> h
        | _ -> raise (Pdf.PDFError "Bad /Hival")
      in let bse =
        read_colourspace_inner pdf resources bse
      in
        let mktable_rgb data =
          try
            let table = Hashtbl.create (hival + 1)
            in let i = Pdfio.input_of_bytes data in
              for x = 0 to hival do
                let r = i.input_byte () in
                let g = i.input_byte () in
                let b = i.input_byte () in
                  Hashtbl.add table x [r; g; b]
              done;
              table
          with _ -> raise (Pdf.PDFError "Pdfspace: bad table")
        in let mktable_cmyk data =
          try
            let table = Hashtbl.create (hival + 1)
            in let i = Pdfio.input_of_bytes data in
              for x = 0 to hival do
                let c = i.input_byte () in
                let m = i.input_byte () in
                let y = i.input_byte () in
                let k = i.input_byte () in
                  Hashtbl.add table x [c; m; y; k]
              done;
              table
          with _ -> raise (Pdf.PDFError "Pdfspace: bad table")
        in
          let table =
            begin match Pdf.direct pdf lookup_data with
            | (Pdf.Stream _) as stream ->
                Pdfcodec.decode_pdfstream pdf stream;
                begin match stream with
                | (Pdf.Stream {contents = (_, Pdf.Got data)}) ->
                    begin match get_basic_table_colourspace bse with
                    | DeviceCMYK -> mktable_cmyk data
                    | DeviceRGB | CalRGB _ | Lab _ -> mktable_rgb data
                    (* FIXME: We need to read all colourspaces here, except
                    Pattern and Index, which aren't allowed. I think this just
                    means grey - everything else is found through
                    get_basic_table_colourspace. *)
                    | Pattern | PatternWithBaseColourspace _ | Indexed (_, _) ->
                        raise (Pdf.PDFError "Disallowed base colourspace in index colourspace")
                    | _ -> raise (Pdf.PDFError "Unsupported base colourspace in index colourspace")
                    end
                | _ -> raise (Pdf.PDFError "Indexed/Inconsistent")
                end
            | Pdf.String s ->
                let data = mkbytes (String.length s) in
                  for x = 0 to bytes_size data - 1 do
                    bset data x (int_of_char s.[x])
                  done;
                  begin match get_basic_table_colourspace bse with
                  | DeviceRGB | CalRGB _ | Lab _ -> mktable_rgb data
                    (* FIXME: Same comment as above *)
                  | DeviceCMYK -> mktable_cmyk data
                  | Pattern | PatternWithBaseColourspace _ | Indexed (_, _) ->
                      raise (Pdf.PDFError "Disallowed base colourspace in index colourspace")
                  | _ -> raise (Pdf.PDFError "Unsupported base colourspace in index colourspace")
                  end
            | _ -> raise (Pdf.PDFError ("PDFSpace: unknown indexed colourspace"))
            end
          in
            Indexed (bse, table)
  | Pdf.Array [Pdf.Name "/Separation"; Pdf.Name name; alternate; tint] ->
      let alt_space =
        read_colourspace_inner pdf resources alternate
      in let tint_transform =
        Pdffun.parse_function pdf tint
      in
        Separation (name, alt_space, tint_transform)
  | Pdf.Array [Pdf.Name "/DeviceN"; Pdf.Array names; alternate; tint] ->
      let names =
        Array.of_list (map (function Pdf.Name s -> s | _ -> raise Not_found) names)
      in let alternate =
        read_colourspace_inner pdf resources alternate
      in let tint =
        Pdffun.parse_function pdf tint
      in
        DeviceN (names, alternate, tint, Pdf.Dictionary [])
  | Pdf.Array [Pdf.Name "/DeviceN"; Pdf.Array names; alternate; tint; attributes] ->
      let names =
        Array.of_list (map (function Pdf.Name s -> s | _ -> raise Not_found) names)
      in let alternate =
        read_colourspace_inner pdf resources alternate
      in let tint =
        Pdffun.parse_function pdf tint
      in
        DeviceN (names, alternate, tint, attributes)
  | _ -> raise Not_found
       
let read_colourspace pdf resources space =
  try
    read_colourspace_inner pdf resources space
  with
    e ->
      (*i Printf.printf "SPACE:\n";
      Printf.printf "%s\n" (Pdfwrite.string_of_pdf space);
      Printf.printf "RESOURCES:\n";
      Printf.printf "%s\n" (Pdfwrite.string_of_pdf resources);
      flprint "\n"; i*)
      raise e

(* Flatten a colourspace to pdf objects. Unfinished. *)
let write_colourspace (pdf : Pdf.t) = function
  | DeviceGray -> Pdf.Name "/DeviceGray"
  | DeviceRGB -> Pdf.Name "/DeviceRGB"
  | DeviceCMYK -> Pdf.Name "/DeviceCMYK"
  | _ -> Printf.eprintf "write_colourspace space not suppported\n"; Pdf.Null

