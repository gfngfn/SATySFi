(** Structured Graphics. This will (eventually) be a module allowing for the raising of a page's contents to a tree form, the manipulation of that tree and its writing back to the page, with no possible loss of fidelity.

It is only a little experiment at the moment... *)

open Pdfutil
open Pdfio

(** Point. *)
type fpoint = float * float

(** Winding rule. *)
type winding_rule = EvenOdd | NonZero

(** A segment (a straight line or bezier curve) *)
type segment =
  | Straight of fpoint * fpoint
  | Bezier of fpoint * fpoint * fpoint * fpoint

(** Each segment list may be marked as a hole or not. *)
type hole = Hole | Not_hole

(* A [subpath] is either closed or open. *)
type closure = Closed | Open

(** A [subpath] is the pair of a hole and a list of segments. *)
type subpath = hole * closure * segment list

(** A path is made from a number of subpaths. *)
type path = winding_rule * subpath list

val string_of_path : path -> string

(** Colour values *)
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

type transparency_attributes =
  {fill_transparency : float;
   line_transparency : float}

(** Path attributes. *)
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

type image_attributes =
  {image_transform : Pdftransform.transform_matrix;
   image_transparency : float;
   image_softmask : softmask option} (* The /ca value *)

and softmask_subtype =
  Alpha | Luminosity

and transparency_group =
  {tr_group_colourspace : Pdf.pdfobject option;
   isolated : bool;
   knockout : bool;
   tr_graphic : t}

and softmask =
  {softmask_subtype : softmask_subtype;
   transparency_group : transparency_group;
   softmask_bbox : float * float * float * float;
   backdrop : float list option;
   softmask_transfer : Pdffun.t option}

and fontname = string * Pdf.pdfobject

(** For now, just support for reading paths out. Eventually a tree-structure for
an op stream. *)
and graphic_elt =
  | Path of (path * path_attributes)
  | Text of textblock list * textblock_attributes
  | MCPoint of string
  | MCPointProperties of string * Pdf.pdfobject 
  | MCSection of string * graphic_elt list
  | MCSectionProperties of string * Pdf.pdfobject * graphic_elt list
  | Image of image_attributes * int
  | GraphicInlineImage of Pdf.pdfobject * bytes * Pdftransform.transform_matrix
  | Clip of path * graphic_elt list
  | Shading of path option * shading * Pdftransform.transform_matrix

and t =
  {elements : graphic_elt list; (* Page content *)
   fonts : fontname list; (* Fonts *)
   resources : Pdf.pdfobject} (* Anything else in /Resources *)

(** Bounding box xmin, xmax, ymin, yman of a graphic *)
val bbox_of_graphic : t -> float * float * float * float

(** Make a graphic from operations. *)
val graphic_of_page : Pdf.t -> Pdfpage.t -> t

(** Make a graphic from a simple string. *)
val graphic_of_ops : Pdfops.t list -> t

(** Flatten a graphic to a list of operations and replace the operations in a
page by them, returning the new page. *)
val page_of_graphic : Pdf.t -> (float * float * float * float) -> t -> Pdfpage.t

(** Debug string of a [graphic] *)
val string_of_graphic : t -> string

(** Operations from a simple graphic (i.e no need for resources etc.) *)
val ops_of_simple_graphic : t -> Pdfops.t list

(** Pdfdoc.content entry from a simple graphic (i.e no need for resources etc.) *)
val streams_of_simple_graphic : t -> Pdf.pdfobject list

(** Transform a graphic by a matrix. *)
val transform_graphic : Pdftransform.transform_matrix -> t -> t

