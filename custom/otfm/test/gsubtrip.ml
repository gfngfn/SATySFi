open Result

let str = Format.sprintf

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
    | Failure _ -> close ic; Error (`Msg (str "%s: input file too large" inf))
    | Sys_error e -> close ic; (Error (`Msg e));
  with
  | Sys_error e -> (Error (`Msg e))


type error = [ Otfm.error | `Msg of string ]

let ( >>= ) x f =
  match x with
  | Ok(v)    -> f v
  | Error(e) -> Error(e :> error)

let return v = Ok(v)

let err e = Error(e)

let pickup lst predicate e =
  match lst |> List.filter predicate with
  | []     -> err e
  | v :: _ -> return v


type pair_position =
  | Pair1 of Otfm.glyph_id * (Otfm.glyph_id * Otfm.value_record * Otfm.value_record) list
  | Pair2 of Otfm.class_value * (Otfm.class_value * Otfm.value_record * Otfm.value_record) list


let skip gid _ = gid

let f_single acc (gid, gidto) = (gid, [gidto]) :: acc

let f_alt acc (gid, gidaltlst) = (gid, gidaltlst) :: acc

let f_lig acc (gidfst, liginfolst) = (gidfst, liginfolst) :: acc

let f_pair1 acc (gidfst, pairinfolst) = Pair1(gidfst, pairinfolst) :: acc

(*
let f_pair2 clsdeflst1 clsdeflst2 lst (clsval, pairinfolst) =
  Pair2(clsval, pairinfolst) :: lst
*)
let f_pair2 _ _ lst _ = lst


let decode_gsub scripttag type3tag type4tag d =
  Otfm.gsub_script d >>= fun scriptlst ->
  Format.printf "@[<v2>  all GSUB Script tags:@ @[[";
  scriptlst |> List.iter (fun gs -> Format.printf "%s,@ " (Otfm.gsub_script_tag gs));
  Format.printf "]@]@,";
  pickup scriptlst (fun gs -> Otfm.gsub_script_tag gs = scripttag)
    (`Msg(str "GSUB does not contain Script tag '%s'" scripttag)) >>= fun script ->
  Otfm.gsub_langsys script >>= fun (langsys_DFLT, _) ->
  Otfm.gsub_feature langsys_DFLT >>= fun (_, featurelst) ->
  Format.printf "all GSUB Feature tags for '%s', 'DFLT':@ @[[" scripttag;
  featurelst |> List.iter (fun gf -> Format.printf "%s,@ " (Otfm.gsub_feature_tag gf));
  Format.printf "]@]@,@]";
  pickup featurelst (fun gf -> Otfm.gsub_feature_tag gf = type4tag)
    (`Msg(str "GSUB does not contain Feature tag '%s' for '%s', 'DFLT'" type4tag scripttag)) >>= fun feature_type4 ->
  Otfm.gsub feature_type4 skip skip f_lig [] >>= fun type4ret ->
  Format.printf "finish '%s'\n" type4tag;
  pickup featurelst (fun gf -> Otfm.gsub_feature_tag gf = type3tag)
    (`Msg(str "GSUB does not contain Feature tag '%s' for '%s', 'DFLT'" type3tag scripttag)) >>= fun feature_type3 ->
  Format.printf "middle of '%s'\n" type3tag;
  Otfm.gsub feature_type3 f_single f_alt skip [] >>= fun type3ret ->
  Format.printf "finish '%s'\n" type3tag;
  return (type3ret, type4ret)


let decode_gpos d =
  Otfm.gpos_script d >>= fun scriptlst ->
  pickup scriptlst (fun script -> Otfm.gpos_script_tag script = "latn")
    (`Msg("GPOS does not contain Script tag 'latn'")) >>= fun script_latn ->
  Otfm.gpos_langsys script_latn >>= fun (langsys_DFLT, _) ->
  Otfm.gpos_feature langsys_DFLT >>= fun (_, featurelst) ->
  pickup featurelst (fun feature -> Otfm.gpos_feature_tag feature = "kern")
    (`Msg("GPOS does not contain Feature tag 'kern' for 'latn'")) >>= fun feature_kern ->
  Otfm.gpos feature_kern f_pair1 f_pair2 [] >>= fun gposres ->
  return gposres


let main filename script type3tag type4tag =
  let src =
    match string_of_file filename with
    | Ok(src)       -> src
    | Error(`Msg e) -> begin print_endline e; exit 1 end
  in
  Otfm.decoder (`String(src)) >>= function
  | Otfm.SingleDecoder(d) ->
      begin
        print_endline "finish initializing decoder";
        decode_gsub script type3tag type4tag d >>= fun (type3ret, type4ret) ->
        decode_gpos d >>= fun gposret ->
        print_endline "finish decoding";
        return (type3ret, type4ret, gposret)
      end

  | Otfm.TrueTypeCollection(_) ->
      let () = print_endline "TTC file" in
      return ([], [], [])


let () =
  let (filename, script, type3tag, type4tag) =
    try (Sys.argv.(1), Sys.argv.(2), Sys.argv.(3), Sys.argv.(4)) with
    | Invalid_argument(_) -> begin print_endline "illegal argument"; exit 1 end
  in
  match main filename script type3tag type4tag with
  | Error(#Otfm.error as e) ->
      Format.printf "error1\n";
      Format.printf "@[%a@]@.\n" Otfm.pp_error e

  | Error(`Msg(msg)) ->
      Format.printf "error2\n";
      Format.printf "!!!! %s !!!!\n" msg

  | Ok(gid_altset_assoc, gidfst_ligset_assoc, clsfst_pairposlst_assoc) ->
      begin
        Format.printf "GSUB '%s':\n" type3tag;
        gid_altset_assoc |> List.rev |> List.iter (fun (gid, gidlst) ->
          Format.printf "%d -> [" gid;
          gidlst |> List.iter (fun gidalt -> Format.printf " %d" gid);
          print_endline "]";
        );
        Format.printf "GSUB '%s':\n" type4tag;
        gidfst_ligset_assoc |> List.rev |> List.iter (fun (gidfst, ligset) ->
          Format.printf "%d -> [" gidfst;
          ligset |> List.iter (fun (gidtail, gidlig) ->
            gidtail |> List.iter (fun gid -> Format.printf " %d" gid);
            Format.printf " ----> %d; " gidlig;
          );
          print_endline "]";
        );
        print_endline "GPOS:";
        clsfst_pairposlst_assoc |> List.rev |> List.iter (function
        | Pair1(gidfst, pairposset) ->
            print_endline ("[1] " ^ (string_of_int gidfst) ^ " -> (length: " ^
              (string_of_int (List.length pairposset)) ^ ")")
        | Pair2(clsfst, pairposset) ->
            print_endline ("[2] " ^ (string_of_int clsfst) ^ " -> (length: " ^
              (string_of_int (List.length pairposset)) ^ ")")
        );
      end
