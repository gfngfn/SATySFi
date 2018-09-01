(* \chaptertitle{Pdfgraphics}{Structured Graphics} *)
open Pdfutil
open Pdfio

type fpoint = float * float

type winding_rule = EvenOdd | NonZero

type segment =
  | Straight of fpoint * fpoint
  | Bezier of fpoint * fpoint * fpoint * fpoint

(* Each segment list may be marked as a hole or not. *)
type hole = Hole | Not_hole

(* A [subpath] is either closed or open. *)
type closure = Closed | Open

(* A [subpath] is the pair of a hole and a list of segments. *)
type subpath = hole * closure * segment list

(* A path is made from a number of subpaths. *)
type path = winding_rule * subpath list

type tiling = Tiling

type function_shading =
  {funshading_domain : float * float * float * float;
   funshading_matrix : Pdftransform.transform_matrix;
   funshading_function : Pdffun.t}

type radial_shading =
  {radialshading_coords : float * float * float * float * float * float;
   radialshading_domain : float * float;
   radialshading_function : Pdffun.t list;
   radialshading_extend : bool * bool}

type axial_shading =
  {axialshading_coords : float * float * float * float;
   axialshading_domain : float * float;
   axialshading_function : Pdffun.t list;
   axialshading_extend : bool * bool}

type shading_kind =
 | FunctionShading of function_shading
 | AxialShading of axial_shading
 | RadialShading of radial_shading
 | FreeFormGouraudShading
 | LatticeFormGouraudShading
 | CoonsPatchMesh
 | TensorProductPatchMesh

type shading =
 {shading_colourspace : Pdf.pdfobject;
  shading_background : Pdf.pdfobject option;
  shading_bbox : Pdf.pdfobject option;
  shading_antialias : bool;
  shading_matrix : Pdftransform.transform_matrix;
  shading_extgstate : Pdf.pdfobject;
  shading : shading_kind}

type pattern =
  | ColouredTilingPattern of tiling
  | UncolouredTilingPattern of tiling
  | ShadingPattern of shading

type colvals =
  | Floats of float list
  | Named of (string * float list)
  | Pattern of pattern

let rec string_of_colvals = function
  | Floats fs ->
      "Floats " ^ fold_left ( ^ ) "" (map (function x -> string_of_float x ^ " ") fs)
  | Named (n, fs) ->
      "Named " ^ n ^ " " ^ string_of_colvals (Floats fs)
  | Pattern p ->
      "Pattern"

type objectclass =
  | PathObject
  | TextObject
  | ClippingPathObject
  | PageDescriptionLevel
  | ShadingObject
  | InlineImageObject
  | ExternalObject

let string_of_objectclass = function
  | PathObject -> "PathObject"
  | TextObject -> "TextObject"
  | ClippingPathObject -> "ClippingPathObject"
  | PageDescriptionLevel -> "PageDescriptionLevel"
  | ShadingObject -> "ShadingObject"
  | InlineImageObject -> "InlineImageObject"
  | ExternalObject -> "ExternalObject"

type transparency_attributes =
  {fill_transparency : float;
   line_transparency : float}

type path_attributes =
  {path_transform : Pdftransform.transform_matrix;
   path_fill : (Pdfspace.t * colvals) option;
   path_line : (Pdfspace.t * colvals) option;
   path_linewidth : float;
   path_joinstyle : int;
   path_capstyle : int;
   path_dash : float list * float;
   path_mitrelimit : float;
   path_transparency : transparency_attributes;
   path_intent : string}

type text_attributes =
  {textmode : int}

type textblock_attributes =
  {textblock_transform : Pdftransform.transform_matrix}

type textblock =
  text_attributes * Pdfops.t

type softmask_subtype =
  Alpha | Luminosity

type transparency_group =
  {tr_group_colourspace : Pdf.pdfobject option; (* FIXME: This should be colourspace *)
   isolated : bool;
   knockout : bool;
   tr_graphic : t}

and softmask =
  {softmask_subtype : softmask_subtype;
   transparency_group : transparency_group;
   softmask_bbox : float * float * float * float;
   backdrop : float list option;
   softmask_transfer : Pdffun.t option}

and image_attributes =
  {image_transform : Pdftransform.transform_matrix;
   image_transparency : float; (* The /ca value *)
   image_softmask : softmask option}

and fontname = string * Pdf.pdfobject (*r Name, font *)

(* The main type for a graphic. It must be kept paired with the PDF it comes
from, since it will reference objects (fonts, images etc) in that PDF. *)
and graphic_elt =
  | Path of (path * path_attributes)
  | Text of textblock list * textblock_attributes
  | MCPoint of string
  | MCPointProperties of string * Pdf.pdfobject 
  | MCSection of string * graphic_elt list
  | MCSectionProperties of string * Pdf.pdfobject * graphic_elt list
  | Image of image_attributes * int (* object number *)
  | GraphicInlineImage of Pdf.pdfobject * bytes * Pdftransform.transform_matrix
  | Clip of path * graphic_elt list
  | Shading of path option * shading * Pdftransform.transform_matrix

and t =
  {elements : graphic_elt list; (* Page content *)
   fonts : fontname list; (* Fonts *)
   resources : Pdf.pdfobject} (* Anything else in /Resources *)

(* Calculate the bounding box (xmin, xmax, ymin, ymax) of a graphic. *)
let bbox_of_segment = function
  | Straight ((x1, y1), (x2, y2)) ->
      fmin x1 x2, fmax x1 x2, fmin y1 y2, fmax y1 y2
  | Bezier ((x1, y1), (x2, y2), (x3, y3), (x4, y4)) ->
      fmin (fmin x1 x2) (fmin x3 x4), fmax (fmax x1 x2) (fmax x3 x4),
      fmin (fmin y1 y2) (fmin y3 y4), fmax (fmax y1 y2) (fmax y3 y4)

let bbox_of_path (_, subpaths) =
  let segments =
    flatten (map (function (_, _, l) -> l) subpaths)
  in
    fold_left
      box_union_float
      (max_float, min_float, max_float, min_float)
      (map bbox_of_segment segments)

let rec bbox_of_graphic_inner (xmin, xmax, ymin, ymax) = function
  | [] -> xmin, xmax, ymin, ymax
  | (Path (p, _) | Clip (p, _))::t ->
      bbox_of_graphic_inner
        (box_union_float (xmin, xmax, ymin, ymax) (bbox_of_path p)) t
  | h::t -> bbox_of_graphic_inner (xmin, xmax, ymin, ymax) t

let bbox_of_graphic graphic =
  bbox_of_graphic_inner
    (max_float, min_float, max_float, min_float)
    graphic.elements

(* For debug purposes, build a string of a graphic. *)
let string_of_segment = function
  | Straight ((ax, ay), (bx, by)) ->
      Printf.sprintf "Straight line: (%f, %f) --> (%f, %f)\n" ax ay bx by
  | Bezier ((ax, ay), (bx, by), (cx, cy), (dx, dy)) ->
      Printf.sprintf
        "Bezier curve: (%f, %f) --> (%f, %f) --> (%f, %f) --> (%f, %f)\n"
        ax ay bx by cx cy dx dy

let string_of_subpath (h, o ,segments) =
  Printf.sprintf "Hole: %b, Open: %b, segments:%s\n"
    (h = Hole) (o = Open) (fold_left ( ^ ) "" (map string_of_segment segments))

let string_of_path (windingrule, subpaths) =
  Printf.sprintf "%s %s"
    (match windingrule with
    | EvenOdd -> "Even-odd\n"
    | NonZero -> "Non-zero\n")
    (fold_left ( ^ ) "" (map string_of_subpath subpaths))

let string_of_textblock (st, op) =
  "TEXTPIECE: " ^ Pdfops.string_of_op op ^ "\n"

let string_of_font (f, i) = f ^ " " ^ Pdfwrite.string_of_pdf i ^ "\n"

let string_of_colvals = function
  | Floats l -> Printf.sprintf "Floats (%i)" (length l)
  | Named (s, fl) -> Printf.sprintf "Named %s, Floats (%i)" s (length fl)
  | Pattern _ -> Printf.sprintf "Pattern"

let string_of_attributes a =
  let line =
    match a.path_line with
    | None -> "none"
    | Some (cs, vals) ->
        Printf.sprintf "line colourspace is %s, %s vals"
          (Pdfspace.string_of_colourspace cs)
          (string_of_colvals vals)
  and fill =
    match a.path_fill with
    | None -> "none"
    | Some (cs, vals) ->
        Printf.sprintf "fill colourspace is %s, %s vals"
          (Pdfspace.string_of_colourspace cs)
          (string_of_colvals vals)
  in
    line ^ "\n" ^ fill ^ "\n"

let rec string_of_graphic_elt = function
  | MCSection (n, g) ->
      Printf.sprintf "Marked content section %s...\n" n ^ "BEGIN\n" ^
      (fold_left ( ^ ) "" (map string_of_graphic_elt g))
      ^ "\nEND Marked content section\n"
  | MCSectionProperties (n, d, g) ->
      Printf.sprintf "Marked content section %s with properties %s...\n" n
      (Pdfwrite.string_of_pdf d) 
      ^ "BEGIN\n" ^
      (fold_left ( ^ ) "" (map string_of_graphic_elt g))
      ^ "\nEND Marked content section\n"
  | MCPoint n ->
      Printf.sprintf "Marked content point %s...\n" n
  | MCPointProperties (n, d) ->
      Printf.sprintf "Marked content point %s with properties %s...\n" n (Pdfwrite.string_of_pdf d)
  | Path (p, attributes) -> Printf.sprintf "Path: %s\nAttributes\n%s\n" (string_of_path p) (string_of_attributes attributes)
  | Text (ts, attr) ->
      "-----BEGIN TEXT - fonts:\n" ^
      fold_left ( ^ ) "" (map string_of_textblock ts) ^
      "-----END TEXT\n"
  | Image (tr, x) ->
      "Image " ^ string_of_int x ^ "\n"
  | GraphicInlineImage _ ->
      "Inline image\n"
  | Clip (p, g) ->
      "Clipview: path = " ^ string_of_path p ^ "\ngraphic is " ^
      fold_left ( ^ ) "" (map string_of_graphic_elt g)
  | Shading (clip, shading, tr) ->
      "Shading\n"

and string_of_graphic g =
  "Elements:\n" ^
  fold_left ( ^ ) "" (map string_of_graphic_elt g.elements) ^ 
  "Fonts:\n" ^
  fold_left ( ^ ) ""
    (map
      (fun (name, obj) -> name ^ " "  ^ Pdfwrite.string_of_pdf obj)
       g.fonts) ^
  "Resources:\n" ^
  Pdfwrite.string_of_pdf g.resources

type state =
  {mutable objectclass : objectclass; (*r Not strictly part of the state, but fits here. *)
   mutable clip : path option; (*r Ditto - stores a clipping path which is to be invoked on the next path operation. *)
   mutable intent : string;
   mutable fill : colvals;
   mutable linewidth : float;
   mutable line : colvals;
   mutable mitrelimit : float;
   mutable joinstyle : int;
   mutable capstyle : int;
   mutable colourspace_stroke : Pdfspace.t;
   mutable colourspace_nonstroke : Pdfspace.t;
   mutable dash : float list * float;
   mutable flatness : int;
   mutable transform : Pdftransform.transform_matrix;
   mutable extra_transform : Pdftransform.transform_matrix;
   mutable text_transform : Pdftransform.transform_matrix;
   mutable text_line_transform : Pdftransform.transform_matrix;
   mutable opacity_stroke : float;
   mutable opacity_nonstroke : float;
   mutable character_spacing : float;
   mutable word_spacing : float;
   mutable scale : float;
   mutable leading : float;
   mutable font_and_size : (string * float) option;
   mutable font_render : int;
   mutable font_rise : float;
   mutable blendmode : int;
   mutable softmask : softmask option;
   mutable in_xobject : int;
   mutable opdo_matrix : Pdftransform.transform_matrix}

let default_state () =
  {objectclass = PageDescriptionLevel;
   clip = None;
   intent = "/RelativeColorimetric";
   fill = Floats [1.];
   linewidth = 1.;
   line = Floats [1.];
   mitrelimit = 10.;
   joinstyle = 0;
   capstyle = 0;
   colourspace_stroke = Pdfspace.DeviceGray;
   colourspace_nonstroke = Pdfspace.DeviceGray;
   dash = [], 0.;
   flatness = 0;
   transform = Pdftransform.i_matrix;
   extra_transform = Pdftransform.i_matrix;
   text_transform = Pdftransform.i_matrix;
   text_line_transform = Pdftransform.i_matrix;
   opacity_stroke = 1.;
   opacity_nonstroke = 1.;
   character_spacing = 0.;
   word_spacing = 0.;
   scale = 100.;
   leading = 0.;
   font_and_size = None; (*r No initial value. *)
   font_render = 0;
   font_rise = 0.;
   blendmode = 1;
   softmask = None;
   in_xobject = 0;
   opdo_matrix = Pdftransform.i_matrix}

let state = ref (default_state ())

let string_of_state s =
  (*i "Object class: " ^ string_of_objectclass s.objectclass ^ "\n" ^ i*)
  "Stroke Colourspace: " ^ Pdfspace.string_of_colourspace s.colourspace_stroke ^ "\n" ^
  "Nonstroke Colourspace: " ^ Pdfspace.string_of_colourspace s.colourspace_nonstroke ^ "\n" ^
  "Stroke colours: " ^ string_of_colvals s.line ^ "\n" ^ 
  "NonStroke colours: " ^ string_of_colvals s.fill ^ "\n"

let path_attributes_fill_and_stroke () =
  {path_transform = (!state).transform;
   path_fill = Some ((!state).colourspace_nonstroke, (!state).fill);
   path_line = Some ((!state).colourspace_stroke, (!state).line);
   path_linewidth = (!state).linewidth;
   path_joinstyle = (!state).joinstyle;
   path_capstyle = (!state).capstyle;
   path_dash = (!state).dash;
   path_mitrelimit = (!state).mitrelimit;
   path_transparency =
     {fill_transparency = (!state).opacity_nonstroke;
      line_transparency = (!state).opacity_stroke};
   path_intent = (!state).intent}

let path_attributes_fill () =
  {path_transform = (!state).transform;
   path_fill = Some ((!state).colourspace_nonstroke, (!state).fill);
   path_line = None;
   path_linewidth = (!state).linewidth;
   path_joinstyle = (!state).joinstyle;
   path_capstyle = (!state).capstyle;
   path_dash = (!state).dash;
   path_mitrelimit = (!state).mitrelimit;
   path_transparency =
     {fill_transparency = (!state).opacity_nonstroke;
      line_transparency = 1.};
   path_intent = (!state).intent}

let path_attributes_stroke () =
  {path_transform = (!state).transform;
   path_fill = None;
   path_line = Some ((!state).colourspace_stroke, (!state).line);
   path_linewidth = (!state).linewidth;
   path_joinstyle = (!state).joinstyle;
   path_capstyle = (!state).capstyle;
   path_dash = (!state).dash;
   path_mitrelimit = (!state).mitrelimit;
   path_transparency =
     {fill_transparency = 1.;
      line_transparency = (!state).opacity_stroke};
   path_intent = (!state).intent}

let textstate () =
  {textmode = 0}

let nonzero = EvenOdd

let rec initial_colour pdf resources = function
  | Pdf.Name "/DeviceGray"
  | Pdf.Array (Pdf.Name "/CalGray"::_) ->
      Floats [0.]
  | Pdf.Name "/DeviceRGB"
  | Pdf.Array (Pdf.Name "/CalRGB"::_) ->
      Floats [0.; 0.; 0.]
  | Pdf.Name "/DeviceCMYK" ->
      Floats [0.; 0.; 0.; 1.]
  | Pdf.Name "/Pattern"
  | Pdf.Array [Pdf.Name "/Pattern"] ->
      Floats [0.]
  | Pdf.Array elts as cs ->
      begin match elts with
        | [Pdf.Name "/ICCBased"; iccstream] ->
             begin match Pdf.lookup_direct pdf "/Alternate" iccstream with
             | Some space -> initial_colour pdf resources space
             | None ->
                 begin match Pdf.lookup_direct pdf "/N" iccstream with
                 | Some (Pdf.Integer 1) -> Floats [0.]
                 | Some (Pdf.Integer 3) -> Floats [0.; 0.; 0.]
                 | Some (Pdf.Integer 4) -> Floats [0.; 0.; 0.; 0.]
                 | _ -> raise (Pdf.PDFError "Bad ICCBased Alternate")
                 end
             end
        | Pdf.Name "/DeviceN"::_::alternate::_ 
        | [Pdf.Name "/Separation"; _; alternate; _] ->
            initial_colour pdf resources alternate
        | [Pdf.Name "/Pattern"; alternate] ->
            initial_colour pdf resources alternate
        | _ -> Printf.eprintf "%s\n" (Pdfwrite.string_of_pdf cs); raise (Pdf.PDFError "Unknown colourspace A")
      end
  | Pdf.Indirect _ as indirect ->
      initial_colour pdf resources (Pdf.direct pdf indirect)
  | _ -> raise (Pdf.PDFError "Unknown colourspace B")

(* PartialPath (sp, cp, p, s) is starting point [sp], current point [cp] the
partial segment list [p], subpath [s] and graphic [g]. *)
type partial =
  | NoPartial
  | PartialText of textblock list
  | PartialPath of fpoint * fpoint * segment list * subpath list 

(* g is a [group_transparency] xobject *)
let rec read_transparency_group pdf g =
  let group =
    match Pdf.lookup_direct pdf "/Group" g with
    | Some gr -> gr
    | None -> raise (Pdf.PDFError "Pdfgraphics.read_transparency_group: no /Group found")
  in
    let colourspace =
      Pdf.lookup_direct pdf "/CS" group
    and isolated =
      match Pdf.lookup_direct pdf "/I" group with
      | Some (Pdf.Boolean b) -> b
      | _ -> false
    and knockout =
      match Pdf.lookup_direct pdf "/K" group with
      | Some (Pdf.Boolean b) -> b
      | _ -> false
    and graphic =
      let fakepage =
        let resources =
          match Pdf.lookup_direct pdf "/Resources" g with
          | Some (Pdf.Dictionary d) -> Pdf.Dictionary d
          | _ -> Pdf.Dictionary []
        and contents =
          [g]
        in
          {Pdfpage.content = contents;
           Pdfpage.mediabox = Pdf.Null;
           Pdfpage.resources = resources;
           Pdfpage.rotate = Pdfpage.Rotate0;
           Pdfpage.rest = Pdf.Dictionary []}
      in
        graphic_of_page pdf fakepage
    and a, b, c, d =
      Pdf.parse_rectangle (Pdf.lookup_fail "no bbox" pdf "/BBox" g)
    in
      {tr_group_colourspace = colourspace;
       isolated = isolated;
       knockout = knockout;
       tr_graphic = graphic}, a, b, c, d

and read_soft_mask pdf mask =
  match
    match Pdf.lookup_direct pdf "/S" mask with
    | Some (Pdf.Name "/Alpha") -> Some Alpha
    | Some (Pdf.Name "/Luminosity") -> Some Luminosity
    | _ -> None
  with
  | None -> None
  | Some subtype ->
      let transparency_group, a, b, c, d =
        match Pdf.lookup_direct pdf "/G" mask with
        | Some g -> read_transparency_group pdf g
        | None -> raise (Pdf.PDFError "Pdfgraphics.transparency group not found in soft mask")
      and backdrop =
        match Pdf.lookup_direct pdf "/BC" mask with
        | Some (Pdf.Array nums) -> Some (map Pdf.getnum nums)
        | _ -> None
      and transfer =
        match Pdf.lookup_direct pdf "/TR" mask with
        | Some (Pdf.Dictionary d) ->
            Some (Pdffun.parse_function pdf (Pdf.Dictionary d))
        | _ -> None
      in
        Some
          {softmask_subtype = subtype;
           transparency_group = transparency_group;
           backdrop =  backdrop;
           softmask_transfer = transfer;
           softmask_bbox = (a, b, c, d)}

and update_graphics_state_from_dict pdf resources gdict =
  begin match Pdf.lookup_direct pdf "/SMask" gdict with
  | Some softmask -> (!state).softmask <- read_soft_mask pdf softmask
  | None -> ()
  end;
  begin match Pdf.lookup_direct pdf "/CA" gdict with
  | Some (Pdf.Real o) -> (!state).opacity_stroke <- o
  | _ -> ()
  end;
  begin match Pdf.lookup_direct pdf "/ca" gdict with
  | Some (Pdf.Real o) -> (!state).opacity_nonstroke <- o
  | _ -> ()
  end;
  begin match Pdf.lookup_direct pdf "/BM" gdict with
  | Some (Pdf.Name n)
  | Some (Pdf.Array (Pdf.Name n::_)) ->
      (!state).blendmode <- 0 (* FIXME: Do properly *)
  | _ -> ()
  end;
  begin match Pdf.lookup_direct pdf "/LW" gdict with
    | Some (Pdf.Integer width) ->
        (!state).linewidth <- float width
    | Some (Pdf.Real width) ->
        (!state).linewidth <- width
    | _ -> ()
  end;
  begin match Pdf.lookup_direct pdf "/LC" gdict with
    | Some (Pdf.Integer style) ->
        (!state).capstyle <- style
    | _ -> ()
  end;
  begin match Pdf.lookup_direct pdf "/LC" gdict with
    | Some (Pdf.Integer join) ->
        (!state).joinstyle <- join
    | _ -> ()
  end;
  begin match Pdf.lookup_direct pdf "/ML" gdict with
    | Some (Pdf.Integer limit) ->
        (!state).mitrelimit <- float limit
    | Some (Pdf.Real limit) ->
        (!state).mitrelimit <- limit
    | _ -> ()
  end;
  begin match Pdf.lookup_direct pdf "/D" gdict with
  | Some (Pdf.Array [Pdf.Array dashes; phase]) ->
      let dashnums, phase =
        map
          (function
           | (Pdf.Integer n) -> float n
           | (Pdf.Real n) -> n
           | _ -> raise (Pdf.PDFError "Malformed dash."))
          dashes,
        match phase with
        | Pdf.Integer phase -> float phase
        | Pdf.Real phase -> phase
        | _ -> raise (Pdf.PDFError "Malformed dash phase.")
      in
        (!state).dash <- dashnums, phase
  | _ -> ()
  end

and statestack : state list ref = ref []

and copystate () =
  {!state with fill = (!state).fill}

and push_statestack () =
  (*i Printf.printf "push_statestack\n"; i*)
  statestack =| copystate ()

and pop_statestack () =
  (*i Printf.printf "pop_statestack: %i items in stack before pop\n" (length !statestack);
  Printf.printf "Before pop_statestack, line and fill spaces are %s and %s\n"
  (Pdfspace.string_of_colourspace (!state).colourspace_stroke) (Pdfspace.string_of_colourspace (!state).colourspace_nonstroke);  I*)
  begin match !statestack with
  | [] -> raise (Pdf.PDFError "Unbalanced q/Q Ops")
  | h::t -> statestack := t; state := h
  end(*i ;
  Printf.printf "After pop_statestack, line and fill spaces are %s and %s\n"
  (Pdfspace.string_of_colourspace (!state).colourspace_stroke) (Pdfspace.string_of_colourspace (!state).colourspace_nonstroke)  i*)

and read_tiling_pattern _ =
  ColouredTilingPattern Tiling

and read_function_shading pdf shading =
  let domain =
    match Pdf.lookup_direct pdf "/Domain" shading with
    | Some (Pdf.Array [a; b; c; d]) -> Pdf.getnum a, Pdf.getnum b, Pdf.getnum c, Pdf.getnum d
    | _ -> 0., 1., 0., 1.
  and matrix =
    Pdf.parse_matrix pdf "/Matrix" shading
  and func =
    Pdf.lookup_fail "No function found" pdf "/Function" shading
  in
    FunctionShading
      {funshading_domain = domain;
       funshading_matrix = matrix;
       funshading_function = Pdffun.parse_function pdf func}

and read_radial_shading pdf shading =
  let coords =
    match Pdf.lookup_direct pdf "/Coords" shading with
    | Some (Pdf.Array [a; b; c; d; e; f]) ->
        Pdf.getnum a, Pdf.getnum b, Pdf.getnum c, Pdf.getnum d, Pdf.getnum e, Pdf.getnum f
    | _ -> raise (Pdf.PDFError "Pdfgraphics.read_radial_shading: no coords in radial shading")
  and domain =
    match Pdf.lookup_direct pdf "/Domain" shading with
    | Some (Pdf.Array [a; b]) -> Pdf.getnum a, Pdf.getnum b
    | _ -> 0., 1.
  and func =
    match Pdf.lookup_direct pdf "/Function" shading with
    | Some (Pdf.Array fs) -> map (Pdffun.parse_function pdf) fs
    | Some f -> [Pdffun.parse_function pdf f]
    | _ -> raise (Pdf.PDFError "Pdfgraphics.read_radial_shading: no function in radial shading")
  and extend =
    match Pdf.lookup_direct pdf "/Extend" shading with
    | Some (Pdf.Array [Pdf.Boolean a; Pdf.Boolean b]) -> a, b
    | _ -> false, false
  in
    RadialShading
      {radialshading_coords = coords;
       radialshading_domain = domain;
       radialshading_function = func;
       radialshading_extend = extend}

and read_axial_shading pdf shading =
  let coords =
    match Pdf.lookup_direct pdf "/Coords" shading with
    | Some (Pdf.Array [a; b; c; d]) ->
        Pdf.getnum a, Pdf.getnum b, Pdf.getnum c, Pdf.getnum d
    | _ -> raise (Pdf.PDFError "Pdfgraphics.read_axial_shading: no coords in radial shading")
  and domain =
    match Pdf.lookup_direct pdf "/Domain" shading with
    | Some (Pdf.Array [a; b]) -> Pdf.getnum a, Pdf.getnum b
    | _ -> 0., 1.
  and func =
    match Pdf.lookup_direct pdf "/Function" shading with
    | Some (Pdf.Array fs) -> map (Pdffun.parse_function pdf) fs
    | Some f -> [Pdffun.parse_function pdf f]
    | _ -> raise (Pdf.PDFError "Pdfgraphics.read_axial_shading: no function in radial shading")
  and extend =
    match Pdf.lookup_direct pdf "/Extend" shading with
    | Some (Pdf.Array [Pdf.Boolean a; Pdf.Boolean b]) -> a, b
    | _ -> false, false
  in
    AxialShading
      {axialshading_coords = coords;
       axialshading_domain = domain;
       axialshading_function = func;
       axialshading_extend = extend}

(* Read a shading pattern *)
and read_shading pdf matrix extgstate shading =
  let colourspace =
    Pdf.lookup_fail "No colourspace in shading" pdf "/ColorSpace" shading
  and background =
    Pdf.lookup_direct pdf "/Background" shading
  and bbox =
    Pdf.lookup_direct pdf "/BBox" shading
  and antialias =
    match Pdf.lookup_direct pdf "/BBox" shading with
    | Some (Pdf.Boolean true) -> true
    | _ -> false
  in
    let shading =
      match Pdf.lookup_fail "no /ShadingType" pdf "/ShadingType" shading with
      | Pdf.Integer 1 -> read_function_shading pdf shading
      | Pdf.Integer 3 -> read_radial_shading pdf shading
      | Pdf.Integer 2 -> read_axial_shading pdf shading
      | Pdf.Integer 4 -> FreeFormGouraudShading
      | Pdf.Integer 5 -> LatticeFormGouraudShading
      | Pdf.Integer 6 -> CoonsPatchMesh
      | Pdf.Integer 7 -> TensorProductPatchMesh
      | _ -> raise (Pdf.PDFError "Pdfgraphics.unknown shadingtype")
    in
      {shading_colourspace = colourspace;
       shading_background = background;
       shading_bbox = bbox;
       shading_antialias = antialias;
       shading_matrix = matrix;
       shading_extgstate = extgstate;
       shading = shading}

and read_shading_pattern pdf p =
  let matrix = Pdf.parse_matrix pdf "/Matrix" p
  and extgstate =
    match Pdf.lookup_direct pdf "/ExtGState" p with
    | Some (Pdf.Dictionary _ as d) -> d
    | _ -> Pdf.Dictionary []
  in
    match Pdf.lookup_direct pdf "/Shading" p with
    | Some shading ->
        ShadingPattern (read_shading pdf matrix extgstate shading)
    | _ ->
        raise (Pdf.PDFError "No shading dictionary")

and read_pattern pdf page name =
  match Pdf.lookup_direct pdf "/Pattern" page.Pdfpage.resources with
  | None -> raise (Pdf.PDFError "No pattern dictionary")
  | Some patterndict ->
      match Pdf.lookup_direct pdf name patterndict with
      | None -> raise (Pdf.PDFError "Pattern not found")
      | Some pattern ->
          match Pdf.lookup_direct pdf "/PatternType" pattern with
          | Some (Pdf.Integer 1) ->
               read_tiling_pattern pattern
          | Some (Pdf.Integer 2) ->
               read_shading_pattern pdf pattern
          | _ -> raise (Pdf.PDFError "unknown pattern")

and process_op pdf page (partial, graphic) op =
  let ret = (partial, graphic) in
  (*i flprint (string_of_state !state); *)
  (*i flprint (Pdfpages.string_of_op op ^ "\n"); i*)
  match op with
  | Pdfops.Op_W ->
      (* Move the current partial path into Clip, and return *)
      begin match partial with
      | PartialPath (_, _, segments, subpaths) ->
          if segments = [] && subpaths = [] then ret else
            let path =
              if segments <> []
                then (Not_hole, Closed, rev segments)::subpaths
                else subpaths
            in
              (!state).clip <- Some (NonZero, path); ret
      | _ -> ret
      end
      (* FIXME: In NextClip needs to support possibly several clips, since we can do W n W n W n f, for instance? *)
  | Pdfops.Op_W' ->
      begin match partial with
      | PartialPath (_, _, segments, subpaths) ->
          if segments = [] && subpaths = [] then ret else
            let path =
              if segments <> []
                then (Not_hole, Closed, rev segments)::subpaths
                else subpaths
            in
              (!state).clip <- Some (EvenOdd, path); ret
      | _ -> ret
      end
  | Pdfops.InlineImage (dict, data) ->
      (NoPartial, GraphicInlineImage (dict, data, (!state).transform)::graphic)
  | Pdfops.Op_MP name ->
      begin match (!state).objectclass with
      | PageDescriptionLevel -> (NoPartial, MCPoint name::graphic)
      | TextObject -> ret (* FIXME -- Add it to the text partial. *)
      | _ -> ret (* Invalid, silently drop *)
      end
  | Pdfops.Op_DP (name, properties) ->
      begin match (!state).objectclass with
      | PageDescriptionLevel ->
          (NoPartial, MCPointProperties (name, properties)::graphic)
      | TextObject -> ret (* FIXME -- Add it to the text partial. *)
      | _ -> ret (* Invalid, silently drop *)
      end
  | Pdfops.Op_BX | Pdfops.Op_EX -> ret    
  | Pdfops.Op_ri n -> (!state).intent <- n; ret
  | Pdfops.Op_j j -> (!state).joinstyle <- j; ret
  | Pdfops.Op_J c -> (!state).capstyle <- c; ret
  | Pdfops.Op_w w -> (!state).linewidth <- w; ret
  | Pdfops.Op_M m -> (!state).mitrelimit <- m; ret
  | Pdfops.Op_q ->
      (*i flprint "Op_q\n"; i*)
      push_statestack ();
      ret
  | Pdfops.Op_Q ->
      (*i flprint "Op_Q\n"; i*)
      pop_statestack ();
      ret
  | Pdfops.Op_SC vals | Pdfops.Op_SCN vals ->
      (!state).line <- Floats vals;
      ret
  | Pdfops.Op_sc vals | Pdfops.Op_scn vals ->
      (!state).fill <- Floats vals;
      ret
  | Pdfops.Op_scnName (name, vals) ->
      begin match (!state).colourspace_nonstroke with
      | Pdfspace.Pattern | Pdfspace.PatternWithBaseColourspace _ ->
          begin try
            (!state).fill <- Pattern (read_pattern pdf page name);
            ret
          with
            _ -> ret
          end
      | _ -> 
          (!state).fill <- Named (name, vals);
          ret
      end
  | Pdfops.Op_SCNName (name, vals) ->
      begin match (!state).colourspace_stroke with
      | Pdfspace.Pattern | Pdfspace.PatternWithBaseColourspace _ ->
          begin try
            (!state).line <- Pattern (read_pattern pdf page name);
            ret
          with
            _ -> ret
          end
      | _ ->
          (!state).line <- Named (name, vals);
          ret
      end
  | Pdfops.Op_CS c ->
      (*i Printf.printf "Op_CS: %s\n" c; *)
      (!state).colourspace_nonstroke <- Pdfspace.read_colourspace pdf page.Pdfpage.resources (Pdf.Name c);
      ret
  | Pdfops.Op_cs c ->
      (*i flprint "Op_cs\n";
      Printf.printf "Pdfgraphics: Op_cs: %s\n" c; i*)
      (!state).colourspace_nonstroke <- Pdfspace.read_colourspace pdf page.Pdfpage.resources (Pdf.Name c);
      ret
  | Pdfops.Op_G gv ->
      (*i flprint "Op_G\n"; i*)
      (!state).colourspace_stroke <- Pdfspace.DeviceGray;
      (!state).line <- Floats [gv];
      ret
  | Pdfops.Op_g gv ->
      (*i flprint "Op_g\n"; i*)
      (!state).colourspace_nonstroke <- Pdfspace.DeviceGray;
      (!state).fill <- Floats [gv];
      ret
  | Pdfops.Op_RG (rv, gv, bv) ->
      (!state).colourspace_stroke <- Pdfspace.DeviceRGB;
      (!state).line <- Floats [rv; gv; bv];
      ret
  | Pdfops.Op_rg (rv, gv, bv) ->
      (!state).colourspace_nonstroke <- Pdfspace.DeviceRGB;
      (!state).fill <- Floats [rv; gv; bv];
      ret
  | Pdfops.Op_K (c, m, y, k) ->
      (!state).colourspace_stroke <- Pdfspace.DeviceCMYK;
      (!state).line <- Floats [c; y; m; k];
      ret
  | Pdfops.Op_k (c, m, y, k) ->
      (!state).colourspace_nonstroke <- Pdfspace.DeviceCMYK;
      (!state).fill <- Floats [c; y; m; k];
      ret
  | Pdfops.Op_gs name ->
      let ext_state_dict = Pdf.lookup_fail "Bad Op_gs" pdf "/ExtGState" page.Pdfpage.resources in
        let gdict = Pdf.lookup_fail "Bad Op_gs" pdf name ext_state_dict in
          update_graphics_state_from_dict pdf page.Pdfpage.resources gdict;
          ret
  | Pdfops.Op_m (x, y) ->
      (* Begin a new subpath. Get into path mode if not already there. If the last op was an
      [Op_m], it should have no effect. *)
      (!state).objectclass <- PathObject;
      begin match partial with
      | PartialPath (sp, cp, segs, subpaths) ->
          if segs = []
            then PartialPath ((x, y), (x, y), [], subpaths), graphic
            else PartialPath ((x, y), (x, y), [], (Not_hole, Open, rev segs)::subpaths), graphic
      | _ ->
          PartialPath ((x, y), (x, y), [], []), graphic
      end
  | Pdfops.Op_l (x, y) ->
      if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_l");
      begin match partial with 
      | PartialPath (sp, cp, segs, subpaths) ->
          PartialPath (sp, (x, y), Straight (cp, (x, y))::segs, subpaths), graphic
      | _ ->
          raise (Pdf.PDFError "Pdfgraphics: Op_l")
      end
  | Pdfops.Op_c (a, b, c, d, e, f) ->
      if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_c");
      begin match partial with 
      | PartialPath (sp, cp, segs, subpaths) ->
          let ep = (e, f) in
            let curve = Bezier (cp, (a, b), (c, d), ep) in
              PartialPath (sp, ep, curve::segs, subpaths), graphic
      | _ ->
          raise (Pdf.PDFError "Pdfgraphics: Op_c")
      end
  | Pdfops.Op_v (a, b, c, d) ->
      if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_v");
      begin match partial with 
      | PartialPath (sp, cp, segs, subpaths) ->
          let ep = (c, d) in
            let curve = Bezier (cp, cp, (a, b), ep) in
              PartialPath (sp, ep, curve::segs, subpaths), graphic
      | _ ->
          raise (Pdf.PDFError "Pdfgraphics: Op_v")
      end
  | Pdfops.Op_y (a, b, c, d) ->
      if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_y");
      begin match partial with 
      | PartialPath (sp, cp, segs, subpaths) ->
          let ep = (c, d) in
            let curve = Bezier (cp, (a, b), ep, ep) in
              PartialPath (sp, ep, curve::segs, subpaths), graphic
      | _ ->
          raise (Pdf.PDFError "Pdfgraphics: Op_y")
      end
  | Pdfops.Op_h ->
      if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_h - not in PathObject");
      begin match partial with 
      | PartialPath (sp, cp, segs, subpaths) ->
          PartialPath (sp, cp, [], (Not_hole, Closed, rev segs)::subpaths), graphic
      | _ ->
          raise (Pdf.PDFError "Pdfgraphics: Op_h - not a partial path")
      end
  | Pdfops.Op_s ->
      (* Close and stroke. Equivalent to h S *)
      process_ops pdf page ret [Pdfops.Op_h; Pdfops.Op_S]
  | Pdfops.Op_b ->
      (* Close, fill, stroke, nonzero. Equivalent to h B *)
      process_ops pdf page ret [Pdfops.Op_h; Pdfops.Op_B]
  | Pdfops.Op_b' ->
      (* Close, fill, stroke, evenodd. Equivalent to h B* *)
      process_ops pdf page ret [Pdfops.Op_h; Pdfops.Op_B']
  | Pdfops.Op_f | Pdfops.Op_F ->
      (* Close and Fill non-zero *)
      if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_f");
      let partial, graphic = process_op pdf page (partial, graphic) Pdfops.Op_h in
        (!state).objectclass <- PageDescriptionLevel;
        begin match partial with
        | PartialPath (sp, cp, segs, subpaths) ->
            (* segs is empty, due to [Op_h] *)
            PartialPath (sp, cp, [], []),
            Path ((NonZero, rev subpaths), path_attributes_fill ())::graphic
        | _ ->
           raise (Pdf.PDFError "Pdfgraphics: Op_f")
        end
  | Pdfops.Op_S ->
      (* Stroke *)
      if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_S");
      (!state).objectclass <- PageDescriptionLevel;
      begin match partial with
      | PartialPath (sp, cp, segs, subpaths) ->
          if segs = [] then
            PartialPath (sp, cp, [], []),
            Path ((EvenOdd, rev subpaths), path_attributes_stroke ())::graphic
          else
            PartialPath (sp, cp, [], []),
            Path ((EvenOdd, rev ((Not_hole, Open, rev segs)::subpaths)), path_attributes_stroke ())::graphic
      | _ ->
         raise (Pdf.PDFError "Pdfgraphics: Op_S")
      end
  | Pdfops.Op_B ->
      (* Fill and stroke, non-zero. *)
      if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_B");
      (!state).objectclass <- PageDescriptionLevel;
      begin match partial with
      | PartialPath (sp, cp, segs, subpaths) ->
          if segs = [] then
            PartialPath (sp, cp, [], []),
            Path ((NonZero, rev subpaths), path_attributes_fill_and_stroke ())::graphic
          else
            PartialPath (sp, cp, [], []),
            Path ((NonZero, rev ((Not_hole, Open, rev segs)::subpaths)), path_attributes_fill_and_stroke ())
            ::graphic
      | _ ->
        raise (Pdf.PDFError "Pdfgraphics: Op_B")
      end
  | Pdfops.Op_B' ->
      (* Fill and stroke, even-odd. *)
      if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_B*");
      let partial, graphic = process_op pdf page (partial, graphic) Pdfops.Op_h in
        (!state).objectclass <- PageDescriptionLevel;
        begin match partial with
        | PartialPath (sp, cp, segs, subpaths) ->
            if segs = [] then
              PartialPath (sp, cp, [], []),
              Path ((EvenOdd, rev subpaths), path_attributes_fill_and_stroke ())::graphic
            else
              PartialPath (sp, cp, [], []),
              Path ((EvenOdd, rev ((Not_hole, Open, rev segs)::subpaths)), path_attributes_fill_and_stroke ())
              ::graphic
        | _ ->
           raise (Pdf.PDFError "Pdfgraphics: Op_B*")
        end
  | Pdfops.Op_f' ->
      (* Fill, even-odd *)
      if (!state).objectclass <> PathObject then
        raise (Pdf.PDFError "Pdfgraphics: Op_f*");
      (!state).objectclass <- PageDescriptionLevel;
      begin match partial with
      | PartialPath (sp, cp, segs, subpaths) ->
          if segs = [] then
            PartialPath (sp, cp, [], []),
            Path ((EvenOdd, rev subpaths), path_attributes_fill ())::graphic
          else
            PartialPath (sp, cp, [], []),
            Path ((EvenOdd, rev ((Not_hole, Open, rev segs)::subpaths)), path_attributes_fill ())
            ::graphic
      | _ ->
         raise (Pdf.PDFError "Pdfgraphics: Op_f*")
      end
  | Pdfops.Op_n ->
      (* no-op *)
      (!state).objectclass <- PageDescriptionLevel;
      (* for now, until we support clipviews, clean up the polygon *)
      (NoPartial, graphic)
  | Pdfops.Op_re (x, y, w, h) ->
      (* Rectangle. *)
      let ops =
        [Pdfops.Op_m (x, y);
         Pdfops.Op_l (x +. w, y);
         Pdfops.Op_l (x +. w, y +. h);
         Pdfops.Op_l (x, y +. h);
         Pdfops.Op_h]
      in
        process_ops pdf page (partial, graphic) ops
  | Pdfops.Op_Do name ->
      begin match Pdf.lookup_direct pdf "/XObject" page.Pdfpage.resources with
      | Some d ->
          begin match Pdf.lookup_direct pdf name d with
          | Some xobj ->
              begin match Pdf.lookup_direct pdf "/Subtype" xobj with
              | Some (Pdf.Name "/Image") ->
                  let objnum =
                    match Pdf.find_indirect name d with
                    | None -> raise (Pdf.PDFError "image not found")
                    | Some i -> i
                  in
                    partial,
                    Image
                       ({image_transform = (!state).transform;
                         image_transparency = (!state).opacity_nonstroke;
                         image_softmask = (!state).softmask}
                        , objnum)::graphic
              | Some (Pdf.Name "/Form") ->
                  let elts = read_form_xobject pdf page xobj in
                    partial, rev elts @ graphic
              | _ -> raise (Pdf.PDFError "Unknown kind of xobject")
              end
          | _ -> raise (Pdf.PDFError "Unknown xobject")
          end
      | None -> raise (Pdf.PDFError "xobject not found")
      end
  | Pdfops.Op_cm tr ->
      (!state).transform <- Pdftransform.matrix_compose (!state).transform tr;
      ret
  |  (  Pdfops.Op_Tc _
      | Pdfops.Op_Tw _ 
      | Pdfops.Op_Tz _ 
      | Pdfops.Op_TL _ 
      | Pdfops.Op_Tf _ 
      | Pdfops.Op_Tr _ 
      | Pdfops.Op_Ts _ 
      | Pdfops.Op_Td _ 
      | Pdfops.Op_TD _ 
      | Pdfops.Op_Tm _ 
      | Pdfops.Op_T' 
      | Pdfops.Op_Tj _ 
      | Pdfops.Op_TJ _ 
      | Pdfops.Op_' _ 
      | Pdfops.Op_'' _ 
      | Pdfops.Op_d0 _ 
      | Pdfops.Op_d1 _) as op ->
       begin match partial with
       | PartialText t ->
           let st = textstate () in
             PartialText ((st, op)::t), graphic
       | _ ->
         (* If there's no partial text, this is an op affecting the text state but not in a text section. Such ops are allowed. FIXME: Deal with them properly - by ops altering the text state so this can be reflected in the initial state at the start of a text section *)
         ret
       end
  | Pdfops.Op_sh n ->
      let shading =
        let shadingdict = Pdf.lookup_fail "no /Shading" pdf "/Shading" page.Pdfpage.resources in
          let shading = Pdf.lookup_fail "named shading not found" pdf n shadingdict in
            read_shading pdf Pdftransform.i_matrix Pdf.Null shading
      and currentclip = (!state).clip in
        partial, Shading (currentclip, shading, (!state).transform)::graphic
  | Pdfops.Op_i flatness ->
      if flatness >= 0 && flatness <= 100 then (!state).flatness <- flatness;
      ret
  | Pdfops.Op_d (spec, phase) ->
      (!state).dash <- spec, phase;
      ret
  | Pdfops.Op_Unknown _ -> ret
  | _ -> Printf.eprintf "Operator shouldn't appear at this place"; ret

and getuntil_matching_emc level prev = function
  | (Pdfops.Op_BMC _ | Pdfops.Op_BDC (_, _)) as h::t ->
      getuntil_matching_emc (level + 1) (h::prev) t
  | Pdfops.Op_EMC::t ->
      if level < 0
        then raise (Pdf.PDFError "Too many EMCs\n")
        else if level = 0
          then rev prev, t
          else getuntil_matching_emc (level - 1) (Pdfops.Op_EMC::prev) t
  | h::t -> getuntil_matching_emc level (h::prev) t
  | [] -> raise (Pdf.PDFError "Missing EMC\n")

and getuntil_matching_Q level prev = function
  | Pdfops.Op_q::t -> getuntil_matching_Q (level + 1) (Pdfops.Op_q::prev) t
  | Pdfops.Op_Q::t ->
      if level = 0
        then rev prev, Pdfops.Op_Q::t
        else getuntil_matching_Q (level - 1) (Pdfops.Op_Q::prev) t
  | [] -> rev prev, []
  | h::t -> getuntil_matching_Q level (h::prev) t

and process_ops pdf page (partial, graphic) ops =
  match ops with
  | [] -> partial, rev graphic
  | Pdfops.Op_n::t ->
      (* If there's a NextClip, select all operators within the scope of this
      clip. That is, all operators until an [Op_Q] which puts the stack level
      below the current level or the end of the stream, whichever comes first.*)
      begin match (!state).clip with
      | None ->
          process_ops pdf page (partial, graphic) t
      | Some path ->
          (* We process the operators concerned, putting them inside a Clip,
          and then proceed with the remaining operators (including any [Op_Q]).
          However, to deal with the case of overlapping pairs of marked content
          sections and q/Q pairs (which is allowed). Currently just chuck BDC we don't understand. *)
          let toq, rest = getuntil_matching_Q 0 [] t in
            let _, elts =
              process_ops pdf page (NoPartial, []) toq
            in
              process_ops pdf page (NoPartial, Clip (path, elts)::graphic) rest
      end
  | Pdfops.Op_BMC n::t ->
      (* FIXME: Marked content regions / q/Q pairs overlapping problem *)
      begin try
        let ops, rest = getuntil_matching_emc 0 [] t in
          let partial, graphic' = process_ops pdf page (partial, []) ops in
            process_ops pdf page (partial, MCSection (n, graphic')::graphic) rest
      with
        _ -> process_ops pdf page (partial, graphic) t
      end
  | Pdfops.Op_BDC (n, d)::t ->
      (* FIXME: Marked content regions / q/Q pairs overlapping problem *)
      begin try
        let ops, rest = getuntil_matching_emc 0 [] t in
          let partial, graphic' = process_ops pdf page (partial, []) ops in
            process_ops pdf page (partial, MCSectionProperties (n, d, graphic')::graphic) rest
      with
        _ -> process_ops pdf page (partial, graphic) t
      end
  | Pdfops.Op_BT::t ->
     (* Can't nest text sections, so just get to ET *)
     let textops, rest = cleavewhile (neq Pdfops.Op_ET) t in
       begin match rest with
       | Pdfops.Op_ET::_ | [] ->
          (* We allow blank in case of wrongly nested EMC / ET etc *)
          let more = tail_no_fail rest in
          (* We need to process the ops, and capture the text operations, but state changes inside
          text sections (e.g colour changes) have global effect, so need to keep the state *)
          (!state).objectclass <- TextObject;
          let partial, _ =
            process_ops pdf page (PartialText [], graphic) textops
          in
            begin match partial with
            | PartialText t ->
                let textblock =
                  Text (rev t, {textblock_transform = (!state).transform})
                in
                  process_ops pdf page (partial, textblock::graphic) (Pdfops.Op_ET::more)
            | _ -> raise (Pdf.PDFError "Bad operations in text block")
            end
       | _ ->
         (*i Printf.printf "textops: %s\n\n" (Pdfops.string_of_ops textops);
         Printf.printf "rest: %s\n\n" (Pdfops.string_of_ops rest); i*)
         raise (Pdf.PDFError "No Matching Op_ET")
       end
  | Pdfops.Op_ET::t ->
     (!state).objectclass <- PageDescriptionLevel;
     process_ops pdf page (partial, graphic) t
  | h::t -> process_ops pdf page (process_op pdf page (partial, graphic) h) t

(* Load the fonts as (name, pdfobject) pairs *)
and fonts_of_page pdf page =
  match Pdf.lookup_direct pdf "/Font" page.Pdfpage.resources with
  | Some (Pdf.Dictionary fs) -> fs
  | _ -> []

(* Find the operations of a form xobject. *)
and read_form_xobject pdf page pdfobject =
  let content = [Pdf.direct pdf pdfobject] in
    let pagedict =
      match Pdf.direct pdf page.Pdfpage.resources with
      | Pdf.Dictionary rs -> rs
      | _ -> []
    and xobjdict =
      match Pdf.direct pdf pdfobject with
      | Pdf.Stream {contents = (dict, _)} ->
          begin match Pdf.lookup_direct pdf "/Resources" dict with
          | Some (Pdf.Dictionary rs) -> rs
          | _ -> []
          end
      | _ -> raise (Pdf.PDFError "bad stream in read_form_xobject")
    in
      let total_resources =
        Pdf.Dictionary (mergedict pagedict xobjdict)
      in
        let fake_page =
          {Pdfpage.content = [];
           Pdfpage.mediabox = Pdf.Null;
           Pdfpage.resources = total_resources;
           Pdfpage.rotate = Pdfpage.Rotate0;
           Pdfpage.rest = Pdf.Dictionary []}
        in
          let _, graphic_elts =
              (process_ops pdf fake_page (NoPartial, [])
              (Pdfops.parse_operators pdf total_resources content))
          in
            graphic_elts

(* Main function - build a graphic from a page *)
and graphic_of_page pdf page =
  statestack := [];
  state := default_state ();
  if Pdfcrypt.is_encrypted pdf then
    raise (Pdf.PDFError "Pdfgraphics: File is encrypted")
  else
    begin
      let _, elts =
        let ops =
          Pdfops.parse_operators pdf page.Pdfpage.resources page.Pdfpage.content
        in
          process_ops pdf page (NoPartial, []) ops
      in
        {elements = elts;
         fonts = fonts_of_page pdf page;
         resources = page.Pdfpage.resources}
    end

let graphic_of_ops ops =
   graphic_of_page
     (Pdf.empty ())
     {(Pdfpage.blankpage Pdfpaper.a4) with
         Pdfpage.content =
           [Pdf.Stream {contents =
              (Pdf.Dictionary [], Pdf.Got (bytes_of_string (Pdfops.string_of_ops ops)))}]}

(* \section{Building a page from a graphic} *)

let int_of_shading_kind = function
  | FunctionShading _ -> 1
  | AxialShading _ -> 2
  | RadialShading _ -> 3
  | FreeFormGouraudShading -> 4
  | LatticeFormGouraudShading -> 5
  | CoonsPatchMesh -> 6
  | TensorProductPatchMesh -> 7

let entries_of_shading pdf s =
  match s.shading with
  | RadialShading r ->
      let coords =
        let a, b, c, d, e, f = r.radialshading_coords in
          Pdf.Array
            [Pdf.Real a; Pdf.Real b; Pdf.Real c; Pdf.Real d; Pdf.Real e; Pdf.Real f]
      and domain =
        let a, b = r.radialshading_domain in
          Pdf.Array
            [Pdf.Real a; Pdf.Real b]
      and funcnum =
        match r.radialshading_function with
        | [f] ->
           Pdf.addobj pdf (Pdffun.pdfobject_of_function pdf f)
        | funs ->
           Pdf.addobj pdf (Pdf.Array (map (Pdffun.pdfobject_of_function pdf) funs))
      and extend =
        Pdf.Array
          [Pdf.Boolean (fst r.radialshading_extend);
           Pdf.Boolean (snd r.radialshading_extend)]
      in
        ["/Coords", coords;
         "/Domain", domain;
         "/Function", Pdf.Indirect funcnum;
         "/Extend", extend]
  | AxialShading a ->
      let coords =
        let a, b, c, d = a.axialshading_coords in
          Pdf.Array
            [Pdf.Real a; Pdf.Real b; Pdf.Real c; Pdf.Real d]
      and domain =
        let a, b = a.axialshading_domain in
          Pdf.Array
            [Pdf.Real a; Pdf.Real b]
      and funcnum =
        match a.axialshading_function with
        | [f] ->
           Pdf.addobj pdf (Pdffun.pdfobject_of_function pdf f)
        | funs ->
           Pdf.addobj pdf (Pdf.Array (map (Pdffun.pdfobject_of_function pdf) funs))
      and extend =
        Pdf.Array
          [Pdf.Boolean (fst a.axialshading_extend);
           Pdf.Boolean (snd a.axialshading_extend)]
      in
        ["/Coords", coords;
         "/Domain", domain;
         "/Function", Pdf.Indirect funcnum;
         "/Extend", extend]
  | _ -> []

let shading_object_of_shading pdf s =
  let background =
    match s.shading_background with
    | None -> []
    | Some b -> ["/Background", b]
  and bbox =
    match s.shading_bbox with
    | None -> []
    | Some b -> ["/BBox", b]
  in
    Pdf.Dictionary
      (["/ShadingType", Pdf.Integer (int_of_shading_kind s.shading);
       "/ColorSpace", s.shading_colourspace;
       "/AntiAlias", Pdf.Boolean s.shading_antialias]
      @ background @ bbox @ entries_of_shading pdf s)

let pattern_object_of_pattern xobject_level opdo_matrix pdf = function
  | ShadingPattern s ->
      begin try
        let shading_matrix =
          if xobject_level > 0 then
            let inverted = Pdftransform.matrix_invert opdo_matrix in
              Pdftransform.matrix_compose inverted s.shading_matrix
          else
            s.shading_matrix
        in
          Pdf.Dictionary
            ["/Type", Pdf.Name "/Pattern";
             "/PatternType", Pdf.Integer 2;
             "/Shading", shading_object_of_shading pdf s;
             "/Matrix", Pdf.make_matrix shading_matrix]
      with
        Pdftransform.NonInvertable -> raise (Pdf.PDFError "Pdfgraphics.Bad pattern")
      end
  | _ ->
      Printf.eprintf "Unknown pattern\n";
      Pdf.Dictionary []

(* Output a move and line/curve ops. *)
let ops_of_segs segs closure =
  let raw_seg_ops =
    map
      (function
       | Straight (_, (x, y)) -> Pdfops.Op_l (x, y)
       | Bezier (_, (bx, by), (cx, cy), (dx, dy)) -> Pdfops.Op_c (bx, by, cx, cy, dx, dy))
      segs
  and get_move = function
    | Straight ((x, y), _) | Bezier ((x, y), _, _, _) -> Pdfops.Op_m (x, y)
  in
    (* Predicate: Do we need to close this subpath? *)
    match segs with
    | [] -> []
    | h::_ -> get_move h::raw_seg_ops @ (if closure = Closed then [Pdfops.Op_h] else [])

let protect ops =
  [Pdfops.Op_q] @ ops @ [Pdfops.Op_Q]

let attribute_ops_of_path (_, a) =
  [Pdfops.Op_w a.path_linewidth;
   Pdfops.Op_J a.path_capstyle;
   Pdfops.Op_j a.path_joinstyle;
   begin match a.path_dash with (x, y) -> Pdfops.Op_d (x, y) end;
   Pdfops.Op_M a.path_mitrelimit;
   Pdfops.Op_ri a.path_intent]

let transform_ops_of_path (_, a) =
  [Pdfops.Op_cm a.path_transform]

let stroke_ops_of_path ((winding, _), a) =
  match winding, a.path_fill, a.path_line with
  | _, None, None -> Pdfops.Op_n
  | EvenOdd, Some _, Some _ -> Pdfops.Op_B'
  | EvenOdd, Some _, None -> Pdfops.Op_f'
  | NonZero, Some _, Some _ -> Pdfops.Op_B
  | NonZero, Some _, None -> Pdfops.Op_f
  | _, None, Some _ -> Pdfops.Op_S

let path_ops_of_path (_, subpaths) =
  flatten (map (fun (_, closure, segs) -> ops_of_segs segs closure) subpaths)

let ops_of_path pdf page (((winding, subpaths), a) as p) =
  (* Add a colourspace returning new resources and a new name, or return the name it's already held under. *)
  let name_of_colourspace cs resources =
    match cs with
    (*i | Pdf.Name (("/DeviceGray" | "/DeviceRGB" | "/DeviceCMYK" | "/Pattern") as str) -> resources, str i*)
    | Pdfspace.DeviceGray | Pdfspace.DeviceRGB | Pdfspace.DeviceCMYK | Pdfspace.Pattern -> resources, Pdfspace.string_of_colourspace cs
    | _ ->
      let existing_colourspacedict =
        match Pdf.lookup_direct pdf "/ColorSpace" resources with
        | Some ((Pdf.Dictionary _) as d) -> d
        | _ -> Pdf.Dictionary []
      in
        (* FIXME: For now, we just always create a new one. Must search to see if it's already there for efficiency. *)
        let name = Pdf.unique_key "cs" existing_colourspacedict in
          let newcolourspacedict = Pdf.add_dict_entry existing_colourspacedict name (Pdfspace.write_colourspace pdf cs) in
            Pdf.add_dict_entry resources "/ColorSpace" newcolourspacedict, name
  in
  let resources = page.Pdfpage.resources in
  let attribute_ops = attribute_ops_of_path p
  and transform = transform_ops_of_path p
  and stroke_op = stroke_ops_of_path p in
  let colours_stroke, resources =
    match a.path_line with
    | Some (cs, Floats vals) ->
        let resources', name = name_of_colourspace cs resources in
        [Pdfops.Op_CS name; Pdfops.Op_SCN vals], resources
    | Some (cs, Named (n, vals)) ->
        let resources', name = name_of_colourspace cs resources in
          [Pdfops.Op_CS name; Pdfops.Op_SCNName (n, vals)], resources'
    | _ -> [], resources
  in
  let colours_nonstroke, resources =
    match a.path_fill with
    | Some (cs, Floats vals) ->
        let resources', name = name_of_colourspace cs resources in
          [Pdfops.Op_cs name; Pdfops.Op_scn vals], resources'
    | Some (cs, Named (n, vals)) ->
        let resources', name = name_of_colourspace cs resources in
          [Pdfops.Op_cs name; Pdfops.Op_scnName (n, vals)], resources'
    | Some (_, Pattern p) ->
        (* Build /Pattern cs and reference to pattern, having built the
        pattern in the pattern dictionary *)
        let pattern = pattern_object_of_pattern (!state).in_xobject (!state).opdo_matrix pdf p in
         let resources, name =
           let existing_patterndict =
             match Pdf.lookup_direct pdf "/Pattern" resources with
             | Some ((Pdf.Dictionary _) as d) -> d
             | _ -> Pdf.Dictionary []
           in
             let name = Pdf.unique_key "pt" existing_patterndict in
               let newpatterndict = Pdf.add_dict_entry existing_patterndict name pattern in
                 Pdf.add_dict_entry page.Pdfpage.resources "/Pattern" newpatterndict, name
        in
          [Pdfops.Op_cs "/Pattern"; Pdfops.Op_scnName (name, [])], resources
    | _ -> [], resources
  in
  let gs, resources =
    if a.path_transparency.fill_transparency < 1. || a.path_transparency.line_transparency < 1.
      then
         let resources, name =
           let existing_extgstate =
             match Pdf.lookup_direct pdf "/ExtGState" resources with
             | Some ((Pdf.Dictionary _) as d) -> d
             | _ -> Pdf.Dictionary []
           in
             let name = Pdf.unique_key "gs" existing_extgstate
             and gsdict =
               Pdf.Dictionary
                 [("/ca", Pdf.Real a.path_transparency.fill_transparency);
                  ("/CA", Pdf.Real a.path_transparency.line_transparency)]
             in
               let new_extgstate = Pdf.add_dict_entry existing_extgstate name gsdict in
                 Pdf.add_dict_entry page.Pdfpage.resources "/ExtGState" new_extgstate, name
         in
            [Pdfops.Op_gs name], resources
      else
        [], resources
  in
   let path_ops = path_ops_of_path (winding, subpaths) in
     protect (gs @ transform @ attribute_ops @ colours_stroke @ colours_nonstroke @ path_ops @ [stroke_op]),
     resources

let ops_of_textstate st = []

let ops_of_textpiece (st, op) =
  ops_of_textstate st @ [op]

(* Upon entry to this, the transformation matrix is identity *)
let ops_of_text tr ops =
  protect ([Pdfops.Op_cm tr; Pdfops.Op_BT] @ (flatten <| map ops_of_textpiece ops) @ [Pdfops.Op_ET])

(* Transform a bounding box by a given matrix *)
let extreme_of_4 f a b c d =
  hd <| sort f [a; b; c; d]

let min_of_4 = extreme_of_4 compare

let max_of_4 = extreme_of_4 (fun a b -> ~-(compare a b))

let transform_bbox tr l b r t =
  let (x0, y0) = Pdftransform.transform_matrix tr (l, t)
  and (x1, y1) = Pdftransform.transform_matrix tr (l, b)
  and (x2, y2) = Pdftransform.transform_matrix tr (r, t)
  and (x3, y3) = Pdftransform.transform_matrix tr (r, b) in
    min_of_4 x0 x1 x2 x3,
    min_of_4 y0 y1 y2 y3,
    max_of_4 x0 x1 x2 x3,
    max_of_4 y0 y1 y2 y3

(* Build a transparency group xobject, add it to the pdf and return its object number *)
let rec pdfobject_of_transparency_group (a, b, c, d) pdf t =
  (!state).in_xobject <- (!state).in_xobject + 1;
  let r =
    let page = page_of_graphic pdf (0., 0., 0., 0.) t.tr_graphic
    and group_attributes =
      let cs =
        match t.tr_group_colourspace with 
        | None -> []
        | Some pdfobject -> ["/CS", pdfobject]
      in
        Pdf.Dictionary
          (["/Type", Pdf.Name "/Group";
            "/S", Pdf.Name "/Transparency";
            "/I", Pdf.Boolean t.isolated;
            "/K", Pdf.Boolean t.knockout] @ cs)
    in
      let extras =
        ["/Type", Pdf.Name "/XObject";
         "/Subtype", Pdf.Name "/Form";
         "/BBox", Pdf.Array [Pdf.Real a; Pdf.Real b; Pdf.Real c; Pdf.Real d];
         "/Resources", page.Pdfpage.resources;
         "/Group", group_attributes]
      in
        match page.Pdfpage.content with
        | Pdf.Stream ({contents = Pdf.Dictionary dict, Pdf.Got data})::_ ->
            Pdf.addobj pdf (Pdf.Stream ({contents = Pdf.Dictionary (extras @ dict), Pdf.Got data}))
        | _ -> raise (Pdf.PDFError "Pdfgraphics: Bad page content")
  in
    (!state).in_xobject <- (!state).in_xobject - 1;
    r

and pdfobject_of_softmask pdf m =
  let bc =
    match m.backdrop with
    | None -> []
    | Some fs -> ["/BC", Pdf.Array (map (function x -> Pdf.Real x) fs)]
  and tr =
    match m.softmask_transfer with
    | None -> []
    | Some f -> ["/TR", Pdffun.pdfobject_of_function pdf f]
  in
    Pdf.addobj pdf
      (Pdf.Dictionary
        (["/Type", Pdf.Name "/Mask";
         "/S", Pdf.Name (match m.softmask_subtype with Alpha -> "/Alpha" | Luminosity -> "/Luminosity");
         "/G", Pdf.Indirect (pdfobject_of_transparency_group m.softmask_bbox pdf m.transparency_group)]
        @ bc @ tr))

and ops_of_image pdf page (a, i) =
  (!state).opdo_matrix <- a.image_transform;
  let resources = page.Pdfpage.resources in
    let ops, resources =
      let opgs, resources =
        if a.image_transparency < 1. || a.image_softmask <> None
          then
             let resources, name =
               let existing_extgstate =
                 match Pdf.lookup_direct pdf "/ExtGState" page.Pdfpage.resources with
                 | Some ((Pdf.Dictionary _) as d) -> d
                 | _ -> Pdf.Dictionary []
               in
                 let name = Pdf.unique_key "gs" existing_extgstate
                 and gsdict =
                   let softmask =
                     match a.image_softmask with
                     | None -> []
                     | Some m -> ["/SMask", Pdf.Indirect (pdfobject_of_softmask pdf m)]
                   in
                     Pdf.Dictionary
                       ([("/ca", Pdf.Real a.image_transparency)] @ softmask)
                 in
                   let new_extgstate = Pdf.add_dict_entry existing_extgstate name gsdict in
                     Pdf.add_dict_entry resources "/ExtGState" new_extgstate, name
             in
                [Pdfops.Op_gs name], resources
          else
            [], resources
      in
        [Pdfops.Op_cm a.image_transform] @ opgs @ [Pdfops.Op_Do ("/Im" ^ string_of_int i)], resources
    in
      protect ops, resources

and ops_of_shading pdf page path shading transform =
  let resources', name =
    (* Add new entry to shading dictionary, return its name, new resources *)
    let existing_shadingdict =
      match Pdf.lookup_direct pdf "/Shading" page.Pdfpage.resources with
      | Some ((Pdf.Dictionary _) as d) -> d
      | _ -> Pdf.Dictionary []
    in
      let name = Pdf.unique_key "sh" existing_shadingdict
      and objnum = Pdf.addobj pdf (shading_object_of_shading pdf shading) in
        let shadingref = Pdf.Indirect objnum in
          let new_shadingdict = Pdf.add_dict_entry existing_shadingdict name shadingref in
            let r =
              Pdf.add_dict_entry page.Pdfpage.resources "/Shading" new_shadingdict
            in
              r, name
  in
    let ops =
      let pathops, clipops =
       match path with
       | None -> [], []
       | Some p ->
           path_ops_of_path p, [Pdfops.Op_W; Pdfops.Op_n] (* FIXME: Even-odd vs Non-Zero *)
      in
        pathops @ clipops @ [Pdfops.Op_cm transform; Pdfops.Op_sh name]
    in
      protect ops, resources'

and ops_of_graphic_acc pdf page oplists = function
  | [] ->
      flatten (rev oplists), page
  | Path p::t ->
      let ops, resources' = ops_of_path pdf page p in
        let page' = {page with Pdfpage.resources = resources'} in
          ops_of_graphic_acc pdf page' (ops::oplists) t
  | Image (attr, i)::t ->
      let ops, resources' = ops_of_image pdf page (attr, i) in
        let page' = {page with Pdfpage.resources = resources'} in
          ops_of_graphic_acc pdf page' (ops::oplists) t
  | Text (ts, {textblock_transform = tr})::t ->
      let ops = ops_of_text tr ts in
        ops_of_graphic_acc pdf page (ops::oplists) t
  | MCSection (n, graphic)::t ->
      let oplist, page' =
        ops_of_graphic_acc pdf page [] graphic
      in
        ops_of_graphic_acc pdf page' (([Pdfops.Op_BMC n] @ oplist @ [Pdfops.Op_EMC])::oplists) t
  | MCSectionProperties (n, d, graphic)::t ->
      let oplist, page' =
        ops_of_graphic_acc pdf page [] graphic
      in
        ops_of_graphic_acc pdf page' (([Pdfops.Op_BDC (n, d)] @ oplist @ [Pdfops.Op_EMC])::oplists) t
  | MCPoint n::t ->
      ops_of_graphic_acc pdf page ([Pdfops.Op_MP n]::oplists) t
  | MCPointProperties (n, d)::t ->
      ops_of_graphic_acc pdf page ([Pdfops.Op_DP (n, d)]::oplists) t
  | GraphicInlineImage (dict, data, tr)::t ->
      ops_of_graphic_acc pdf page (protect [Pdfops.Op_cm tr; Pdfops.InlineImage (dict, data)]::oplists) t
  | Clip ((w, _) as p, elts)::t ->
      let ops, page' =
        let path_ops =
          [Pdfops.Op_cm (!state).transform] @ path_ops_of_path p 
        and clipviewops =
          [if w = NonZero then Pdfops.Op_W else Pdfops.Op_W'; Pdfops.Op_n]
        and insideclipops, page' =
          ops_of_graphic_acc pdf page [] elts
        in
          protect (path_ops @ clipviewops @ insideclipops), page'
      in
        ops_of_graphic_acc pdf page' (ops::oplists) t
   | Shading (path, shading, transform)::t ->
       let ops, resources' = ops_of_shading pdf page path shading transform in
         let oplists'= protect ops::oplists
         and page' = {page with Pdfpage.resources = resources'} in
           ops_of_graphic_acc pdf page' oplists' t

(* Build a page from a graphic in the same PDF. *)
and image_numbers_of_elts prev = function
  | Image (_, i)::t -> image_numbers_of_elts (i::prev) t
  | MCSection (_, elts)::t
  | MCSectionProperties (_, _, elts)::t
  | Clip (_, elts)::t ->
      let these = image_numbers_of_elts [] elts in
        image_numbers_of_elts (these @ prev) t
  | _::t -> image_numbers_of_elts prev t
  | [] -> prev

and make_xobjects pdf elts =
  let numbers = image_numbers_of_elts [] elts in
    setify <| map (function n -> ("/Im" ^ string_of_int n), Pdf.Indirect n) numbers

and make_resources pdf g page' =
  let resources =
    match g.resources with
    | Pdf.Dictionary rs -> rs
    | _ -> []
  and fontdict =
    Pdf.Dictionary g.fonts
  and xobjdict =
    let objs = make_xobjects pdf g.elements in
      Pdf.Dictionary objs
  and resources_frompage =
    match page'.Pdfpage.resources with
    | Pdf.Dictionary d -> d
    | _ -> raise (Pdf.PDFError "bad resources in page in make_resources")
  in
    let resources = remove "/Shading" resources in
    let resources = remove "/Pattern" resources in
    let resources = remove "/ExtGState" resources in
    let resources = remove "/ColorSpace" resources in
      (* [fold_right] so that entries overwrite *)
      Pdf.Dictionary
        (fold_right
           (fun (k, v) d -> add k v d)
           ["/Font", fontdict; "/XObject", xobjdict]
           (resources_frompage @ resources))

and page_of_graphic pdf (xmin, ymin, xmax, ymax) graphic =
  let page =
    Pdfpage.custompage (Pdf.Array [Pdf.Real xmin; Pdf.Real ymin; Pdf.Real xmax; Pdf.Real ymax])
  in
    let ops, page' = ops_of_graphic_acc pdf page [] graphic.elements in
      (* We're not including the ExtGState because it's in the page', so need
      to merge with resources *)
      let resources = make_resources pdf graphic page' in
        {page' with
           Pdfpage.content = [Pdfops.stream_of_ops ops];
           Pdfpage.resources = resources}

let ops_of_simple_graphic graphic =
  fst (ops_of_graphic_acc (Pdf.empty ()) (Pdfpage.blankpage Pdfpaper.a4) [] graphic.elements)

(* FIXME Add in here a function to copy a page/graphic from one document to another *)

let streams_of_simple_graphic g =
  (page_of_graphic (Pdf.empty ()) (0., 0., 600., 400.) g).Pdfpage.content

(* Transforming a graphic *)
let transform_segment tr s =
  let f = Pdftransform.transform_matrix tr in
    match s with
    | Straight (x, y) -> Straight (f x, f y)
    | Bezier (a, b, c, d) -> Bezier (f a, f b, f c, f d)

let transform_subpath tr (h, c, segments) =
  (h, c, map (transform_segment tr) segments)

let transform_path tr (w, subpaths) =
  (w, map (transform_subpath tr) subpaths)

let transform_element tr = function
  | Path (pth, attr) -> Path (transform_path tr pth, attr)
  | x -> x (* FIXME: Add rest of elements. *)

let transform_graphic tr g =
  {g with elements = map (transform_element tr) g.elements} 


