(* \chaptertitle{Shapes}{Stroking lines and making shapes} *)

(* This module provides for the stroking of lines, and production of shape
primitives (circles, regular polygons etc). *)
open Pdfutil

(* \section{Common geometric functions} *)

(* The factor by which we multiply the radius to find the length of the bezier
control lines when approximating quarter arcs to make semicircles and circles.
*)
let kappa = ((sqrt 2. -. 1.) /. 3.) *. 4.

(* Calculate rotation from [p] to [p'] about [c] with the shorter arc-length.
When arc-lengths are equal, the result may be either. *)
let rotation (cx, cy) (px, py) (px', py') =
  let px = px -. cx and py = py -. cy
  and px' = px' -. cx and py' = py' -. cy in
    let a = px *. py' -. py *. px'
    and b = px *. px' +. py *. py' in
      atan2 a b

(* The absolute angle to a point [p] from a centre [c]. The angle is the
rotation clockwise (i.e the first quadrant encountered has positive [x] and [y]
values) from East. When the point is [(0, 0)], the result is [0].*)
let angle_to (cx, cy) (px, py) =
  let r = atan2 (py -. cy) (px -. cx) in
    if r < 0. then r +. 2. *. pi else r

(* Restrict an angle [a] to one of those at $s, 2s, 3s\ldots$. We find the two
candidate angles, and see which [a] is numerically closer to. The candidate
points are taken modulo $2\pi$ for this to work. *)
let restrict_angle s a =
  let p = mod_float (floor (a /. s) *. s) (2. *. pi) in
    let p' = mod_float (p +. s) (2. *. pi) in
      if abs_float (p -. a) < abs_float (p' -. a) then p else p'

(* \section{Some Useful Shapes} *)

(* Make a quarter-circle from a single bezier curve from [s] to $(s + \pi / 2)
\bmod 2\pi$ with centre [c] and radius [r]. We cheat by making the standard
quarter from [(1, 0)] to [(0, 1)] and rotating using the [Transform] module.
*)
let quarter s (cx, cy) r =
  let standard_quarter_points =
    [(1., 0.); (1., kappa); (kappa, 1.); (0., 1.)]
  and transform =
    [Pdftransform.Translate(cx, cy);
     Pdftransform.Scale((0., 0.), r, r);
     Pdftransform.Rotate((0., 0.), s)]
  in
    match
      map (Pdftransform.transform transform) standard_quarter_points
    with
    | [p; q; r; s] -> Pdfgraphics.Bezier(p, q, r, s)
    | _ -> raise (Pdf.PDFError ("Shapes.quarter: inconsistency"))

(* The anticlockwise variant. *)
let quarter_anticlockwise s c r =
  match quarter s c r with
  | Pdfgraphics.Bezier(p, q, r, s) -> Pdfgraphics.Bezier(s, r, q, p)
  | _ -> raise (Pdf.PDFError "Shapes.quarter_anticlockwise: inconsistency")

(* Some of the following functions generate what is supposed to be a connected
list of segments. However, since they operate by calculating each segment
seperately, floating point inaccuracies can arise, making the end of one
segment misalign with the start of the next. This function corrects the defect
by copying the end of one segment to the beginning of the next. We only need to
deal with bezier segments for now. *)
let rec joinsegs segments =
  match segments with
  | [] -> []
  | [x] -> [x]
  | Pdfgraphics.Bezier(_, _, _, d) as s::Pdfgraphics.Bezier(_, b', c', d')::rest ->
      s::joinsegs (Pdfgraphics.Bezier(d, b', c', d')::rest)
  | _ -> raise (Pdf.PDFError "PDFShapes.joinsegs: Segment not supported")

(* This version sets the start and end points to p1 and p2 respectively. Used
for ensuring round joins join correctly to the rails they connect *)
let joinsegs_ends p1 p2 segments =
  match joinsegs segments with
  | [] -> []
  | [Pdfgraphics.Bezier(a, b, c, d)] -> [Pdfgraphics.Bezier(p1, b, c, p2)]
  | segs ->
    match extremes_and_middle segs with
    | Pdfgraphics.Bezier(_, b, c, d), m, Pdfgraphics.Bezier(a', b', c', _) ->
        Pdfgraphics.Bezier(p1, b, c, d)::m @ [Pdfgraphics.Bezier(a', b', c', p2)]
    | _ -> raise (Pdf.PDFError "PDFShapes.joinsegs_ends: Segment not supported")

(* The shorter arc made from bezier curves from [p1] to [p2] with centre [c].
The arc is formed from zero or more quarter arcs rotated accordingly, and at
most one partial arc produced by truncating a quarter arc, again rotated. If
[p1=p2], no segments are produced. If the two curves defined by the arguments
are of equal length, the one chosen is undefined. *)
(*i let arc p1 p2 c =
  let ninety = pi /. 2.
  and angletogo = rotation c p1 p2 (*r signed angle to turn through *)
  and abs_angle = angle_to c p1 (*r absolute angle to the first point *)
  and r = distance_between p1 c in (*r radius of the resultant arc *)
    let quarter, ninety_abs =
      if angletogo > 0.
        then quarter, ninety
        else quarter_anticlockwise, ~-.ninety
    in
      let segments = ref []
      and angletogo = ref (abs_float angletogo) (*r Have dealt with sign. *)
      and abs_angle = ref abs_angle in
        while !angletogo > 0. do
          if !angletogo >= ninety then
            begin
              angletogo := !angletogo -. ninety;
              segments := (quarter !abs_angle c r)::!segments;
              abs_angle := mod_float (!abs_angle +. ninety_abs) (2. *. pi)
            end
          else
            (* Calculate a partial arc to finish, if required. *)
            if !angletogo > 0. then
              begin
                let q = quarter !abs_angle c r in
                  let portion_needed = !angletogo /. ninety in
                    let portion, _ = Polygon.bezier_split portion_needed q in
                      segments := portion::!segments;
                angletogo := 0.
              end;
        done;
        joinsegs_ends p1 p2 (rev !segments) i*)

(* Approximate a circle using four bezier curves.*)
let circle x y r =
  Pdfgraphics.NonZero,
    [(Pdfgraphics.Not_hole,
      Pdfgraphics.Closed,
     joinsegs
       [quarter 0. (x, y) r;
       quarter (pi /. 2.) (x, y) r;
       quarter pi (x, y) r;
       quarter (3. *. pi /. 2.) (x, y) r ])]

let rectangle x y w h =
  (Pdfgraphics.EvenOdd,
    ([(Pdfgraphics.Not_hole,
       Pdfgraphics.Closed,
      [Pdfgraphics.Straight ((x, y), (x +. w, y));
       Pdfgraphics.Straight ((x +. w, y), (x +. w, y +. h));
       Pdfgraphics.Straight ((x +. w, y +. h), (x, y +. h));
       Pdfgraphics.Straight ((x, y +. h), (x, y))])]))

