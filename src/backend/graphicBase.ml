
open MyUtil
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
  | Rectangle   of point * point


let (+@%) (x, y) (vx, vy) =
  (x +% vx, y +% vy)


let shift_path_element v pe =
  match pe with
  | LineTo(pt)                  -> LineTo(pt +@% v)
  | CubicBezierTo(pt1, pt2, pt) -> CubicBezierTo(pt1 +@% v, pt2 +@% v, pt +@% v)


let shift_path v path =
  match path with
  | Rectangle(pt1, pt2) ->
      Rectangle(pt1 +@% v, pt2 +@% v)

  | GeneralPath(pt0, pelst, cycleopt) ->
      let cycleopt_s =
        cycleopt |> option_map (function
          | LineTo(()) as l             -> l
          | CubicBezierTo(pt1, pt2, ()) -> CubicBezierTo(pt1 +@% v, pt2 +@% v, ())
        )
      in
        GeneralPath(pt0 +@% v, pelst |> List.map (shift_path_element v), cycleopt_s)
