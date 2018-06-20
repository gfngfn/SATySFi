
open MyUtil
open Config


type dir_path = string
type file_path = string

exception InvalidYOJSON               of file_path * string
exception OtherThanDictionary         of file_path
exception NotProvidingExceptionList   of file_path
exception ExceptionListOtherThanArray of file_path
exception InvalidExceptionElement     of file_path
exception NotProvidingPatternList     of file_path
exception PatternListOtherThanArray   of file_path
exception InvalidPatternElement       of file_path


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
  | Single    of Uchar.t list
  | Fractions of (Uchar.t list) list


let read_exception_list (srcpath : file_path) (jsonarr : Yojson.Safe.json) : exception_map =
  match jsonarr with
  | `List(jsonlst) ->
      jsonlst |> List.fold_left (fun mapacc json ->
        match json with
        | `Tuple[`String(wordfrom); `List(jsonlstto)] ->
            let fraclstto =
              jsonlstto |> List.map (function
                | `String(fracto) -> fracto
                | _               -> raise (InvalidExceptionElement(srcpath))
              )
            in
            mapacc |> ExceptionMap.add wordfrom fraclstto

        | _ ->
            raise (InvalidExceptionElement(srcpath))
      ) ExceptionMap.empty

  | _ ->
      raise (ExceptionListOtherThanArray(srcpath))


let numeric (uch : Uchar.t) : number option =
  let cp = Uchar.to_int uch in
  let cp0 = Char.code '0' in
  let cp9 = Char.code '9' in
    if cp0 <= cp && cp <= cp9 then
      Some(cp - cp0)
    else
      None


let convert_pattern (srcpath : file_path) (strpat : string) : pattern =
  let uchlstraw = InternalText.to_uchar_list (InternalText.of_utf8 strpat) in
  let (beginning, uchlstsub) =
      match uchlstraw with
      | [] ->
          raise (InvalidPatternElement(srcpath))

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
        raise (InvalidPatternElement(srcpath))

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


let read_pattern_list (srcpath : file_path) (jsonarr : Yojson.Safe.json) : pattern list =
  match jsonarr with
  | `List(jsonlst) ->
      jsonlst |> List.map (function
        | `String(strpat) -> convert_pattern srcpath strpat
        | _               -> raise (InvalidPatternElement(srcpath))
      )

  | _ ->
      raise (PatternListOtherThanArray(srcpath))


let read_assoc (srcpath : file_path) (assoc : (string * Yojson.Safe.json) list) : t =
  let excpmap =
    match assoc |> List.assoc_opt "exceptions" with
    | None          -> raise (NotProvidingExceptionList(srcpath))
    | Some(jsonarr) -> read_exception_list srcpath jsonarr
  in
  let hyphpatlst =
    match assoc |> List.assoc_opt "patterns" with
    | None          -> raise (NotProvidingPatternList(srcpath))
    | Some(jsonarr) -> read_pattern_list srcpath jsonarr
  in
    (excpmap, hyphpatlst)


let main (filename : file_path) : t =
  let srcpath = resolve_dist_path (Filename.concat "dist/hyph" filename) in
    try
      let json = Yojson.Safe.from_file srcpath in
          (* -- may raise 'Sys_error', or 'Yojson.Json_error' -- *)
        match json with
        | `Assoc(assoc) -> read_assoc srcpath assoc
        | json_other    -> raise (OtherThanDictionary(srcpath))
    with
    | Yojson.Json_error(msg) -> raise (InvalidYOJSON(srcpath, msg))


let empty = (ExceptionMap.empty, [])


let match_prefix (opt : (number ref * number) option) (pairlst : (Uchar.t * number) list) (clst : (Uchar.t * number ref) list) : unit =
  let rec aux acc pairlst clst =
  match (pairlst, clst) with
  | (_ :: _, []) ->
      ()

  | ([], _) ->
      acc |> Alist.to_list |> List.iter (fun (numref, num) -> numref := max (!numref) num)

  | ((uchp, num) :: pairtail, (uchw, numref) :: ctail) ->
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


let rec match_every (numbeginning : number) pairlst clst =
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
let lookup_patterns (lmin : int) (rmin : int) (patlst : pattern list) (uchlst : Uchar.t list) : (Uchar.t list) list =
  let len = List.length uchlst in
  let clst = uchlst |> List.map (fun uch -> (uch, ref 0)) in
  let () =
    patlst |> List.iter (fun (beginning, pairlst, final) ->
      match beginning with
      | TopOfWord               -> match_prefix None pairlst clst
      | ArbitraryBeginning(num) -> match_every num pairlst clst
    )
  in
  let (_, acc, fracaccopt) =
    clst |> List.fold_left (fun (i, acc, fracaccopt) (uch, numref) ->
      if (!numref) mod 2 = 1 && i + 1 >= lmin && len - (i + 1) >= rmin then
      (* -- if able to break the word with hyphen immediately after the current position -- *)
        let fracacc =
          match fracaccopt with
          | Some(fracacc) -> fracacc
          | None          -> Alist.empty
        in
          let sfrac = make_fraction (Alist.extend fracacc uch) in
            (i + 1, Alist.extend acc sfrac, None)
      else
        match fracaccopt with
        | Some(fracacc) -> (i + 1, acc, Some(Alist.extend fracacc uch))
        | None          -> (i + 1, acc, Some(Alist.extend Alist.empty uch))
    ) (0, Alist.empty, None)
  in
    match fracaccopt with
    | Some(fracacc) -> Alist.extend acc (make_fraction fracacc) |> Alist.to_list
    | None          -> acc |> Alist.to_list


let lookup (lmin : int) (rmin : int) ((excpmap, patlst) : t) (uchlst : Uchar.t list) : answer =
  let fraclst =
    match excpmap |> ExceptionMap.find_opt (InternalText.to_utf8 (InternalText.of_uchar_list uchlst)) with
    | Some(sfraclst) -> sfraclst |> List.map (fun sfrac -> InternalText.to_uchar_list (InternalText.of_utf8 sfrac))
    | None           -> lookup_patterns lmin rmin patlst uchlst
  in
  match fraclst with
  | frac :: [] -> Single(frac)
  | _          -> Fractions(fraclst)
