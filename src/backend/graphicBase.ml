
open LengthInterface


type color =
  | DeviceGray of float
  | DeviceRGB  of float * float * float
  | DeviceCMYK of float * float * float * float
[@@deriving show { with_path = false }]

type 'a path_element =
  | LineTo              of 'a
  | CubicBezierTo       of point * point * 'a

type path =
  | GeneralPath of point * (point path_element) list * (unit path_element) option

type vector = point

type matrix = float * float * float * float

type bbox_corners = point * point


let ( +@% ) ((x, y) : point) ((vx, vy) : vector) : point =
  (x +% vx, y +% vy)


let shift_path_element (v : vector) (pe : point path_element) : point path_element =
  match pe with
  | LineTo(pt)                  -> LineTo(pt +@% v)
  | CubicBezierTo(pt1, pt2, pt) -> CubicBezierTo(pt1 +@% v, pt2 +@% v, pt +@% v)


let shift_path (v : vector) (path : path) : path =
  let GeneralPath(pt0, pes, cycle_opt) = path in
  let cycle_opt =
    cycle_opt |> Option.map (function
      | LineTo(()) as l             -> l
      | CubicBezierTo(pt1, pt2, ()) -> CubicBezierTo(pt1 +@% v, pt2 +@% v, ())
    )
  in
  GeneralPath(pt0 +@% v, pes |> List.map (shift_path_element v), cycle_opt)


let linear_transform_point (mat : matrix) ((x, y) : point) : point =
  let (a, b, c, d) = mat in
  (x *% a +% y *% b, x *% c +% y *% d)


(* Linear transform centered at point (cx, cy) *)
let centered_linear_transform_point ((a, b, c, d) : matrix) ~center:((cx, cy) : point) ((x, y) : point) : point =
  let x = !=> x in
  let y = !=> y in
  let cx = !=> cx in
  let cy = !=> cy in
  let relx = x -. cx in
  let rely = y -. cy in
  (!<= (a *. relx +. b *. rely +. cx), !<= (c *. relx +. d *. rely +. cy))


let centered_linear_transform_bbox (mat : matrix) ~(center : point) (bbox : bbox_corners) : bbox_corners =
  let ((xmin, ymin), (xmax, ymax)) = bbox in
  let (x1, y1) = centered_linear_transform_point mat ~center (xmin, ymin) in
  let (x2, y2) = centered_linear_transform_point mat ~center (xmin, ymax) in
  let (x3, y3) = centered_linear_transform_point mat ~center (xmax, ymin) in
  let (x4, y4) = centered_linear_transform_point mat ~center (xmax, ymax) in
  let xmin = x1 |> Length.min x2 |> Length.min x3 |> Length.min x4 in
  let xmax = x1 |> Length.max x2 |> Length.max x3 |> Length.max x4 in
  let ymin = y1 |> Length.min y2 |> Length.min y3 |> Length.min y4 in
  let ymax = y1 |> Length.max y2 |> Length.max y3 |> Length.max y4 in
  ((xmin, ymin), (xmax, ymax))


let make_rectangle ((x1, y1) : point) (wid : length) (hgt : length) : path =
  let x2 = x1 +% wid in
  let y2 = y1 +% hgt in
  GeneralPath((x1, y1), [ LineTo(x1, y2); LineTo(x2, y2); LineTo(x2, y1); ], Some(LineTo(())))


let linear_transform_path_element (mat : matrix) (pe : point path_element) : point path_element =
  let trans = linear_transform_point mat in
  match pe with
  | LineTo(pt)                  -> LineTo(trans pt)
  | CubicBezierTo(pt1, pt2, pt) -> CubicBezierTo(trans pt1, trans pt2, trans pt)


let linear_transform_path (mat : matrix) (path : path) : path =
  let trans = linear_transform_point mat in
  let GeneralPath(pt0, pes, cycleopt) = path in
  let cycleopt_s =
    cycleopt |> Option.map (function
      | LineTo(()) as l             -> l
      | CubicBezierTo(pt1, pt2, ()) -> CubicBezierTo(trans pt1, trans pt2, ())
    )
  in
  GeneralPath(trans pt0, pes |> List.map (linear_transform_path_element mat), cycleopt_s)


let bezier_bbox ((x0, y0) : point) ((x1, y1) : point) ((x2, y2) : point) ((x3, y3) : point) : bbox_corners =

  let bezier_point t r0 r1 r2 r3 =
    if t < 0. then r0 else
      if 1. < t then r3 else
        let c1 = 3. *. (-. r0 +. r1) in
        let c2 = 3. *. (r0 -. 2. *. r1 +. r2) in
        let c3 = -. r0 +. 3. *. (r1 -. r2) +. r3 in
        r0 +. t *. (c1 +. t *. (c2 +. t *. c3))
  in

  let aux r0 r1 r2 r3 =
    let a = -. r0 +. 3. *. (r1 -. r2) +. r3 in
    let b = 2. *. (r0 -. 2. *. r1 +. r2) in
    let c = -. r0 +. r1 in
    if a = 0. then
      [ r0; r3 ]
    else
      let det = b *. b -. 4. *. a *. c in
      if det < 0. then
        [ r0; r3 ]
      else
        let delta = sqrt det in
        let t_plus  = (-. b +. delta) /. (2. *. a) in
        let t_minus = (-. b -. delta) /. (2. *. a) in
        [r0; r3; bezier_point t_plus r0 r1 r2 r3; bezier_point t_minus r0 r1 r2 r3]
  in

  let xopts = aux (!=> x0) (!=> x1) (!=> x2) (!=> x3) in
  let xmax = xopts |> List.fold_left max (!=> x0) in
  let xmin = xopts |> List.fold_left min (!=> x0) in
  let yopts = aux (!=> y0) (!=> y1) (!=> y2) (!=> y3) in
  let ymax = yopts |> List.fold_left max (!=> y0) in
  let ymin = yopts |> List.fold_left min (!=> y0) in
  ((!<= xmin, !<= ymin), (!<= xmax, !<= ymax))


let update_min ((x0, y0) : point) ((x1, y1) : point) : point =
  (Length.min x0 x1, Length.min y0 y1)


let update_max ((x0, y0) : point) ((x1, y1) : point) : point =
  (Length.max x0 x1, Length.max y0 y1)


let unite_bbox (bbox1 : bbox_corners) (bbox2 : bbox_corners) : bbox_corners =
  let (pt_min1, pt_max1) = bbox1 in
  let (pt_min2, pt_max2) = bbox2 in
  (update_min pt_min1 pt_min2, update_max pt_max1 pt_max2)


let update_bbox_by_path_element ((ptmin, ptmax) : bbox_corners) (pt_from : point) (pe : point path_element) : bbox_corners * point =
  match pe with
  | LineTo(pt_to) ->
      let bbox = (update_min ptmin pt_to, update_max ptmax pt_to) in
      (bbox, pt_to)

  | CubicBezierTo(pt1, pt2, pt_to) ->
      let (ptminbz, ptmaxbz) = bezier_bbox pt_from pt1 pt2 pt_to in
      let bbox = (update_min ptmin ptminbz, update_max ptmax ptmaxbz) in
      (bbox, pt_to)


let get_path_bbox (path : path) : bbox_corners =
  let GeneralPath(pt0, pes, cycle_opt) = path in
  let bbox_init = (pt0, pt0) in
  let (bbox, pt_from) =
    pes |> List.fold_left (fun (bbox, pt_from) pe ->
      update_bbox_by_path_element bbox pt_from pe
    ) (bbox_init, pt0)
  in
  match cycle_opt with
  | None
  | Some(LineTo(())) ->
      bbox

  | Some(CubicBezierTo(pt1, pt2, ())) ->
      let (bbox, _) = update_bbox_by_path_element bbox pt_from (CubicBezierTo(pt1, pt2, pt0)) in
      bbox


let get_path_list_bbox (paths : path list) : bbox_corners =
  let bbox_init =
    match paths with
    | []                          -> assert false  (* Does not deal with the empty path list *)
    | GeneralPath(pt0, _, _) :: _ -> (pt0, pt0)
  in
  paths |> List.fold_left (fun (ptmin0, ptmax0) path ->
    let (ptmin1, ptmax1) = get_path_bbox path in
    (update_min ptmin0 ptmin1, update_max ptmax0 ptmax1)
  ) bbox_init
