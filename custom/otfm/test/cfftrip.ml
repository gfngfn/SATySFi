
open Result

let pp = Format.fprintf

type error = [ Otfm.error | `Msg of string ]


let string_of_file inf =
  try
    let ic = if inf = "-" then stdin else open_in_bin inf in
    let close ic = if inf <> "-" then close_in ic else () in
    let buf_size = 65536 in
    let b = Buffer.create buf_size in
    let s = Bytes.create buf_size in
    try
      while true do
        let c = input ic s 0 buf_size in
        if c = 0 then raise Exit else
        Buffer.add_substring b (Bytes.unsafe_to_string s) 0 c
      done;
      assert false
    with
    | Exit -> close ic; Ok (Buffer.contents b)
    | Failure _ -> close ic; Error (`Msg (Printf.sprintf "%s: input file too large" inf))
    | Sys_error e -> close ic; (Error (`Msg e));
  with
  | Sys_error e -> (Error (`Msg e))


let charstring topdict gid =
    match Otfm.charstring_absolute topdict.Otfm.charstring_info gid with
    | Error(e)      -> Error(e :> error)
    | Ok(None)      -> Error(`Msg (Printf.sprintf "no CharString for GID %d" gid))
    | Ok(Some(pcs)) ->
        match Otfm.charstring_bbox pcs with
        | None       -> Error(`Msg "no bounding box")
        | Some(bbox) -> Ok((bbox, pcs))


type point_kind = OL | OM | OA | OB | OC


let svgx x = x
let svgy y = 1000 - y


let output_bbox fmt fout (xmin, xmax, ymin, ymax) =
  let sxmins = svgx xmin in
  let symins = min (svgy ymin) (svgy ymax) in
  let wid = svgx xmax - sxmins in
  let hgt = abs (svgy ymax - svgy ymin) in
  Format.fprintf fmt "bbox: %d, %d, %d, %d\n" xmin xmax ymin ymax;
  Printf.fprintf fout "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"red\" />" sxmins symins wid hgt


let output_path_element fout csptacc pe =
  match pe with
  | Otfm.LineTo((x, y)) ->
      Printf.fprintf fout "L%d,%d " (svgx x) (svgy y);
      (OL, x, y) :: csptacc

  | Otfm.BezierTo((xA, yA), (xB, yB), (xC, yC)) ->
      Printf.fprintf fout "C%d,%d %d,%d %d,%d " (svgx xA) (svgy yA) (svgx xB) (svgy yB) (svgx xC) (svgy yC);
      (OC, xC, yC) :: (OB, xB, yB) :: (OA, xA, yA) :: csptacc


let output_path fout ((x, y), pelst) =
  Printf.fprintf fout "<path d=\"M%d,%d " (svgx x) (svgy y);
  let csptacc =
    pelst |> List.fold_left (output_path_element fout) [(OM, x, y)]
  in
  Printf.fprintf fout "Z\" />";
  csptacc |> List.rev |> List.fold_left (fun i (o, x, y) ->
    let color =
      match o with
      | OM -> "purple"
      | OL -> "orange"
      | OA -> "red"
      | OB -> "blue"
      | OC -> "green"
    in
    Printf.fprintf fout "<circle cx=\"%d\" cy=\"%d\" r=\"5\" fill=\"%s\" />" (svgx x) (svgy y) color;
    Printf.fprintf fout "<text x=\"%d\" y=\"%d\"><tspan fill=\"gray\" font-size=\"7pt\">%d</tspan></text>" (svgx (x + 4)) (svgy y) i;
    i + 1
  ) 0 |> ignore


let main fmt =
  let ( >>= ) x f =
    match x with
    | Ok(v)    -> f v
    | Error(e) -> Error(e :> error)
  in
  let (filename, gid, outname) =
    try (Sys.argv.(1), int_of_string Sys.argv.(2), Sys.argv.(3)) with
    | Invalid_argument(_) -> begin print_endline "illegal argument"; exit 1 end
  in
  let src =
    match string_of_file filename with
    | Ok(src)       -> src
    | Error(`Msg e) -> begin print_endline e; exit 1 end
  in
  Otfm.decoder (`String(src)) >>= function
  | Otfm.SingleDecoder(d) ->
      begin
        print_endline "finish initializing decoder";
        Otfm.cff d >>= fun cffinfo ->
        let (x1, y1, x2, y2) = cffinfo.Otfm.font_bbox in
        pp fmt "FontBBox: (%d, %d, %d, %d)\n" x1 y1 x2 y2;
        pp fmt "IsFixedPitch: %B\n" cffinfo.Otfm.is_fixed_pitch;
        pp fmt "ItalicAngle: %d\n" cffinfo.Otfm.italic_angle;
        pp fmt "UnderlinePosition: %d\n" cffinfo.Otfm.underline_position;
        pp fmt "UnderlineThickness: %d\n" cffinfo.Otfm.underline_thickness;
        pp fmt "PaintType: %d\n" cffinfo.Otfm.paint_type;
        pp fmt "StrokeWidth: %d\n" cffinfo.Otfm.stroke_width;

        charstring cffinfo gid >>= fun (bbox, pcs) ->

        let fout = open_out outname in
        Printf.fprintf fout "<?xml version=\"1.0\" encoding=\"utf-8\"?>";
        Printf.fprintf fout "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">";
        let sx1 = svgx x1 in
        let sy1 = svgx y1 in
        let sx2 = svgx x2 in
        let sy2 = svgx y2 in
        Printf.fprintf fout "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"%d\" height=\"%d\" viewBox=\"%d %d %d %d\">" (sx2 - sx1) (sy2 - sy1) sx1 sy1 sx2 sy2;
        Printf.fprintf fout "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"none\" stroke=\"blue\" />" sx1 sy1 (sx2 - sx1) (sy2 - sy1);

        pcs |> List.iter (output_path fout);
        output_bbox fmt fout bbox;
(*
        let path_sample = ((500, 100), [ Otfm.BezierTo((700, 200), (600, 300), (400, 400)); ]) in
        output_path fout path_sample;
        begin
          match Otfm.charstring_bbox [path_sample] with
          | None       -> Format.fprintf fmt "bbox = none\n"
          | Some(bbox) -> output_bbox fmt fout bbox
        end;
*)
        Printf.fprintf fout "</svg>";
        close_out fout;

        match cffinfo.Otfm.cid_info with
        | None ->
            pp fmt "Not a CIDFont\n";
            Ok()

        | Some(cidinfo) ->
            pp fmt "CIDFont\n";
            pp fmt "Registry: '%s'\n" cidinfo.Otfm.registry;
            pp fmt "Ordering: '%s'\n" cidinfo.Otfm.ordering;
            pp fmt "Supplement: %d\n" cidinfo.Otfm.supplement;
            Ok()
      end

  | Otfm.TrueTypeCollection(_) ->
      begin
        print_endline "TrueType Collection";
        Ok()
      end

let () =
  match main Format.std_formatter with
  | Ok()                    -> ()
  | Error(#Otfm.error as e) -> Format.eprintf "@[%a@]@." Otfm.pp_error e
  | Error(`Msg msg)         -> Format.eprintf "@[%s@]@." msg
