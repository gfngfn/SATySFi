
open MyUtil
open CharBasis

module YS = Yojson.SafePos
module MYU = MyYojsonUtil

exception InvalidPatternElement of Range.t

module ExceptionMap = Map.Make(String)

type exception_map = (string list) ExceptionMap.t

type number = int

type beginning =
  | TopOfWord
  | ArbitraryBeginning of number

type final =
  | EndOfWord
  | ArbitraryFinal

type pattern = beginning * (Uchar.t * number) list * final

type t = exception_map * pattern list

type answer =
  | Single    of uchar_segment list
  | Fractions of (uchar_segment list) list


let read_exception_list (json : YS.json) : exception_map =
  let jsonlst = json |> YS.Util.to_list in
  jsonlst |> List.fold_left (fun mapacc json ->
    match json with
    | (_, `Tuple[json1; json2]) ->
        let wordfrom = json1 |> YS.Util.to_string in
        let jsonlstto = json2 |> YS.Util.to_list in
        let fraclstto = jsonlstto |> List.map YS.Util.to_string in
        mapacc |> ExceptionMap.add wordfrom fraclstto

    | _ ->
        raise (YS.Util.Type_error("Expects pair", json))

  ) ExceptionMap.empty


let numeric (uch : Uchar.t) : number option =
  let cp = Uchar.to_int uch in
  let cp0 = Char.code '0' in
  let cp9 = Char.code '9' in
    if cp0 <= cp && cp <= cp9 then
      Some(cp - cp0)
    else
      None


let convert_pattern (rng : Range.t) (strpat : string) : pattern =
  let uchlstraw = InternalText.to_uchar_list (InternalText.of_utf8 strpat) in
  let (beginning, uchlstsub) =
      match uchlstraw with
      | [] ->
          raise (InvalidPatternElement(rng))

      | uch0 :: uchtail ->
          if uch0 = Uchar.of_char '.' then
            (TopOfWord, uchtail)
          else
            match numeric uch0 with
            | None      -> (ArbitraryBeginning(0), uchlstraw)
            | Some(num) -> (ArbitraryBeginning(num), uchtail)
  in
  let (final, uchlst) =
    match List.rev uchlstsub with
    | [] ->
        raise (InvalidPatternElement(rng))

    | uchL :: uchrest ->
        if uchL = Uchar.of_char '.' then
          (EndOfWord, List.rev uchrest)
        else
          (ArbitraryFinal, uchlstsub)
  in
  let pairlst =
    uchlst |> list_fold_adjacent (fun acc uch _ optnext ->
      match numeric uch with
      | Some(_) ->
          acc

      | None ->
          begin
            match optnext with
            | None ->
                Alist.extend acc (uch, 0)

            | Some(uchnext) ->
                let pair =
                  match numeric uchnext with
                  | None      -> (uch, 0)
                  | Some(num) -> (uch, num)
                in
                  Alist.extend acc pair
          end
    ) Alist.empty |> Alist.to_list
  in
(*
  (* begin: for debug *)
  let pp_pair fmt (uch, num) = Format.fprintf fmt "%s(%n)" (InternalText.to_utf8 (InternalText.of_uchar uch)) num in
  Format.printf "LoadHyph>";
  (match beginning with
  | TopOfWord -> ()
  | ArbitraryBeginning(num) -> Format.printf "(%d)" num);
  Format.printf "%a\n" (Format.pp_print_list pp_pair) pairlst;
  (* end: for debug *)
*)
    (beginning, pairlst, final)


let read_pattern_list (json : YS.json) : pattern list =
  let jsons = json |> YS.Util.to_list  in
  jsons |> List.map (fun ((pos, _) as json) ->
    let rng = MYU.make_range pos in
    convert_pattern rng (YS.Util.to_string json)
  )


let read_assoc (assoc : MYU.assoc) : t =
  let excpmap = assoc |> MYU.find "exceptions" |> read_exception_list in
  let hyphpatlst = assoc |> MYU.find "patterns" |> read_pattern_list in
  (excpmap, hyphpatlst)


let main (abspath : abs_path) : t =
  let pathstr = get_abs_path_string abspath in
  try
    let json = YS.from_file ~fname:pathstr pathstr in
      (* -- may raise 'Sys_error'  -- *)
    let assoc = json |> MYU.make_assoc in
    read_assoc assoc
  with
  | Yojson.Json_error(msg) -> MYU.syntax_error pathstr msg


let empty = (ExceptionMap.empty, [])


let match_prefix (opt : (number ref * number) option) (pairlst : (Uchar.t * number) list) (clst : (uchar_segment * number ref) list) : unit =
  let rec aux acc pairlst clst =
  match (pairlst, clst) with
  | (_ :: _, []) ->
      ()

  | ([], _) ->
      acc |> Alist.to_list |> List.iter (fun (numref, num) -> numref := max (!numref) num)

  | ((uchp, num) :: pairtail, ((uchw, _), numref) :: ctail) ->
      if Uchar.equal uchp uchw then
        aux (Alist.extend acc (numref, num)) pairtail ctail
      else
        ()
  in
  let accinit =
    match opt with
    | None       -> Alist.empty
    | Some(pair) -> Alist.extend Alist.empty pair
  in
    aux accinit pairlst clst


let match_every (numbeginning : number) pairlst clst =
  let rec aux (refoptprev : (number ref) option) pairlst clst =
  match clst with
  | [] ->
      ()

  | (_, numref) :: ctail ->
      let opt =
        match refoptprev with
        | None             -> None
        | Some(numrefprev) -> Some((numrefprev, numbeginning))
      in
      match_prefix opt pairlst clst;
      aux (Some(numref)) pairlst ctail
  in
    aux None pairlst clst


let make_fraction fracacc =
  fracacc |> Alist.to_list


(* --
   'lookup_patterns':
     determines hyphen pattern of the given word.
     this implemenmtation is currently very inefficient. -- *)
let lookup_patterns (lmin : int) (rmin : int) (patlst : pattern list) (uchseglst : uchar_segment list) : (uchar_segment list) list =
  let len = List.length uchseglst in
  let clst = uchseglst |> List.map (fun uchseg -> (uchseg, ref 0)) in
  let () =
    patlst |> List.iter (fun (beginning, pairlst, _final) ->
      match beginning with
      | TopOfWord               -> match_prefix None pairlst clst
      | ArbitraryBeginning(num) -> match_every num pairlst clst
    )
  in
  let (_, acc, fracaccopt) =
    clst |> List.fold_left (fun (i, acc, fracaccopt) (uchseg, numref) ->
      if (!numref) mod 2 = 1 && i + 1 >= lmin && len - (i + 1) >= rmin then
      (* -- if able to break the word with hyphen immediately after the current position -- *)
        let fracacc =
          match fracaccopt with
          | Some(fracacc) -> fracacc
          | None          -> Alist.empty
        in
          let sfrac = make_fraction (Alist.extend fracacc uchseg) in
            (i + 1, Alist.extend acc sfrac, None)
      else
        match fracaccopt with
        | Some(fracacc) -> (i + 1, acc, Some(Alist.extend fracacc uchseg))
        | None          -> (i + 1, acc, Some(Alist.extend Alist.empty uchseg))
    ) (0, Alist.empty, None)
  in
    match fracaccopt with
    | Some(fracacc) -> Alist.extend acc (make_fraction fracacc) |> Alist.to_list
    | None          -> acc |> Alist.to_list


let lookup (lmin : int) (rmin : int) ((excpmap, patlst) : t) (uchseglst : uchar_segment list) : answer =
  let fraclst =
    let uchlst = uchseglst |> List.map (fun (u, _) -> u) in
    match excpmap |> ExceptionMap.find_opt (InternalText.to_utf8 (InternalText.of_uchar_list uchlst)) with
    | Some(sfraclst) ->
        sfraclst |> List.map (fun sfrac ->
          let uchlst = InternalText.to_uchar_list (InternalText.of_utf8 sfrac) in
            uchlst |> List.map (fun uch -> (uch, []))
        )

    | None ->
        lookup_patterns lmin rmin patlst uchseglst
  in
  match fraclst with
  | frac :: [] -> Single(frac)
  | _          -> Fractions(fraclst)
