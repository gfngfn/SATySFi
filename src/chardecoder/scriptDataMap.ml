
open MyUtil
open CharBasis


let read_east_asian_width _ data =
  match data with
  | "H"  -> EAWHalfWidth
  | "F"  -> EAWFullWidth
  | "Na" -> EAWNarrow
  | "W"  -> EAWWide
  | "A"  -> EAWAmbiguous
  | "N"  -> EAWNeutral
  | _    -> assert false


let read_script eaw_map cp data =
  match data with
  | "Han"      -> HanIdeographic
  | "Hiragana" -> HiraganaOrKatakana
  | "Katakana" -> HiraganaOrKatakana
  | "Latin"    -> Latin
(* temporary; should add more scripts *)

  | "Common" ->
      begin
        try
          match eaw_map |> BatIMap.find cp with
          | EAWNarrow
          | EAWHalfWidth
          | EAWAmbiguous
          | EAWNeutral
              -> CommonNarrow

          | EAWFullWidth
          | EAWWide
              -> CommonWide
        with Not_found -> CommonNarrow
      end

  | _ -> OtherScript


type t = script BatIMap.t


let empty : t = BatIMap.empty ~eq:(=)


let make_from_file ~script:(abspath_S : abs_path) ~east_asian_width:(abspath_EAW : abs_path) : t =
  let eaw_map =
    let channel_EAW = AbsPathIo.open_in abspath_EAW in
    let eaw_list = DataParser.main DataLexer.expr (Lexing.from_channel channel_EAW) in
    Stdlib.close_in channel_EAW;
    eaw_list |> CharBasis.map_of_list read_east_asian_width
  in
  let script_map =
    let channel_S = AbsPathIo.open_in abspath_S in
    let script_list = DataParser.main DataLexer.expr (Lexing.from_channel channel_S) in
    Stdlib.close_in channel_S;
    script_list |> CharBasis.map_of_list (read_script eaw_map)
  in
  script_map


let find (uch : Uchar.t) (script_map : t) : script option =
  try
    let script = script_map |> BatIMap.find (Uchar.to_int uch) in
    Some(script)
  with
  | Not_found ->
      None
