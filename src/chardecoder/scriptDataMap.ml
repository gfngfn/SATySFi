
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
  let eaw_list =
    AbsPathIo.open_in abspath_EAW (fun inc_EAW ->
      DataParser.main DataLexer.expr (Lexing.from_channel inc_EAW)
    )
  in
  let eaw_map = eaw_list |> CharBasis.map_of_list read_east_asian_width in
  let script_list =
    AbsPathIo.open_in abspath_S (fun inc_S ->
      DataParser.main DataLexer.expr (Lexing.from_channel inc_S)
    )
  in
  let script_map = script_list |> CharBasis.map_of_list (read_script eaw_map) in
  script_map


let find (uch : Uchar.t) (script_map : t) : script option =
  try
    let script = script_map |> BatIMap.find (Uchar.to_int uch) in
    Some(script)
  with
  | Not_found ->
      None
